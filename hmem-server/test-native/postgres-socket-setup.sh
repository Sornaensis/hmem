#!/usr/bin/env bash
# Regression coverage for Linux hmem-ctl init socket configuration.
# Builds hmem-ctl in the repository, then runs it only against temporary HOME
# directories and fake PostgreSQL commands. pg_ctl deliberately fails after the
# configuration phase so no database server or migration connection is used.
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$repo_root"

if [[ -n "${HMEM_CTL:-}" ]]; then
  hmem_ctl=$HMEM_CTL
else
  stack build hmem-server:hmem-ctl >/dev/null
  install_root=$(stack path --local-install-root)
  hmem_ctl=$install_root/bin/hmem-ctl
fi

if [[ ! -x "$hmem_ctl" ]]; then
  echo "hmem-ctl is not executable: $hmem_ctl" >&2
  exit 1
fi

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
fake_bin=$tmp/bin
mkdir -p "$fake_bin"

for command in initdb createdb psql; do
  cat >"$fake_bin/$command" <<'EOF'
#!/usr/bin/env bash
exit 97
EOF
  chmod +x "$fake_bin/$command"
done

cat >"$fake_bin/pg_ctl" <<'EOF'
#!/usr/bin/env bash
# hmem-ctl must configure postgresql.conf before attempting startup.
exit 23
EOF
chmod +x "$fake_bin/pg_ctl"

run_init_until_fake_startup_failure() {
  local home=$1
  if env HOME="$home" PATH="$fake_bin:$PATH" "$hmem_ctl" init >"$home/init.out" 2>&1; then
    echo "hmem-ctl init unexpectedly passed fake pg_ctl" >&2
    return 1
  fi
  grep -Fq "pg_ctl" "$home/init.out"
}

# Model the state left by the Debian failure: initdb completed, its generated
# config contains only the commented system socket default, and config.yaml was
# never reached. Include quote, backslash, and whitespace characters in HOME to
# exercise PostgreSQL configuration-string quoting.
partial_home="$tmp/home with 'quote\\slash"
partial_data="$partial_home/.hmem/data/postgresql"
mkdir -p "$partial_data"
printf '17\n' >"$partial_data/PG_VERSION"
cat >"$partial_data/postgresql.conf" <<'EOF'
# PostgreSQL generated configuration
shared_buffers = 128MB
#unix_socket_directories = '/var/run/postgresql'
EOF

run_init_until_fake_startup_failure "$partial_home"
config=$partial_data/postgresql.conf
socket_dir="$partial_home/.hmem/run"
escaped_socket=${socket_dir//\\/\\\\}
escaped_socket=${escaped_socket//\'/\'\'}
expected="unix_socket_directories = '$escaped_socket'"

grep -Fqx "shared_buffers = 128MB" "$config"
grep -Fqx "$expected" "$config"
[[ -d "$socket_dir" ]]
[[ $(grep -Fc "# hmem Unix socket configuration" "$config") -eq 1 ]]

# Re-running init after another startup failure remains idempotent.
run_init_until_fake_startup_failure "$partial_home"
[[ $(grep -Fc "# hmem Unix socket configuration" "$config") -eq 1 ]]
[[ $(grep -Fc "$expected" "$config") -eq 1 ]]

# Do not override an unrelated explicit socket policy in an existing cluster.
# PostgreSQL parameter names are case-insensitive and the equals sign is
# optional, so cover both that spelling and the compact equals form.
assert_explicit_setting_preserved() {
  local label=$1
  local setting_line=$2
  local custom_home="$tmp/$label-home"
  local custom_data="$custom_home/.hmem/data/postgresql"
  local custom_config="$custom_data/postgresql.conf"

  mkdir -p "$custom_data"
  printf '17\n' >"$custom_data/PG_VERSION"
  printf 'shared_buffers = 256MB\n%s\n' "$setting_line" >"$custom_config"

  run_init_until_fake_startup_failure "$custom_home"
  grep -Fqx "shared_buffers = 256MB" "$custom_config"
  grep -Fqx "$setting_line" "$custom_config"
  ! grep -Fq "# hmem Unix socket configuration" "$custom_config"
}

assert_explicit_setting_preserved \
  "mixed-case-no-equals" \
  "UnIx_SoCkEt_DiReCtOrIeS '/tmp/site-managed-postgres' # site policy"
assert_explicit_setting_preserved \
  "compact-equals" \
  "unix_socket_directories='/tmp/site-managed-postgres'"

printf 'native PostgreSQL socket setup regression: PASS\n'
