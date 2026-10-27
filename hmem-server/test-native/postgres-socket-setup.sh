#!/usr/bin/env bash
# Regression coverage for the non-Windows hmem-ctl PostgreSQL TCP-only default.
# Every hmem-ctl run uses a temporary HOME and pg_ctl deliberately fails before
# a server starts, so the test never touches a live native installation.
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

cat >"$fake_bin/initdb" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
data_dir=
while (($#)); do
  case $1 in
    -D)
      data_dir=$2
      shift 2
      ;;
    *)
      shift
      ;;
  esac
done
[[ -n "$data_dir" ]]
mkdir -p "$data_dir"
printf '17\n' >"$data_dir/PG_VERSION"
cat >"$data_dir/postgresql.conf" <<'CONFIG'
# fake initdb generated configuration
shared_buffers = 128MB
#unix_socket_directories = '/var/run/postgresql'
include_dir = 'conf.d'
CONFIG
EOF
chmod +x "$fake_bin/initdb"

for command in createdb psql; do
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
  mkdir -p "$home"
  if env HOME="$home" PATH="$fake_bin:$PATH" "$hmem_ctl" init >"$home/init.out" 2>&1; then
    echo "hmem-ctl init unexpectedly passed fake pg_ctl" >&2
    return 1
  fi
  grep -Fq "pg_ctl" "$home/init.out"
}

write_managed_prefix() {
  cat <<'EOF'
# hmem managed native PostgreSQL TCP-only default
unix_socket_directories = ''

EOF
}

# Exercise a genuinely fresh temporary HOME: fake initdb creates the cluster,
# hmem adds its ordinary network settings, then prepends the managed default.
fresh_home=$tmp/fresh-home
run_init_until_fake_startup_failure "$fresh_home"
fresh_config=$fresh_home/.hmem/data/postgresql/postgresql.conf
fresh_expected=$tmp/fresh-expected.conf
{
  write_managed_prefix
  cat <<'EOF'
# fake initdb generated configuration
shared_buffers = 128MB
#unix_socket_directories = '/var/run/postgresql'
include_dir = 'conf.d'

# hmem configuration
port = 54320
listen_addresses = 'localhost'
EOF
} >"$fresh_expected"
cmp "$fresh_expected" "$fresh_config"

# A rerun after startup failure must neither duplicate nor reorder bytes.
fresh_hash=$(sha256sum "$fresh_config")
run_init_until_fake_startup_failure "$fresh_home"
[[ $(sha256sum "$fresh_config") == "$fresh_hash" ]]
[[ $(grep -Fc "# hmem managed native PostgreSQL TCP-only default" "$fresh_config") -eq 1 ]]

# Repair a pre-existing partial cluster while preserving its complete original
# content byte-for-byte, including an explicit later override and no final LF.
partial_home=$tmp/partial-home
partial_data=$partial_home/.hmem/data/postgresql
partial_original=$tmp/partial-original.conf
partial_expected=$tmp/partial-expected.conf
mkdir -p "$partial_data"
printf '17\n' >"$partial_data/PG_VERSION"
printf "%s" "shared_buffers = 256MB
include = 'site.conf'
unix_socket_directories = '/tmp/site-managed-postgres'" >"$partial_original"
cp "$partial_original" "$partial_data/postgresql.conf"
chmod 640 "$partial_data/postgresql.conf"
{
  write_managed_prefix
  cat "$partial_original"
} >"$partial_expected"
run_init_until_fake_startup_failure "$partial_home"
cmp "$partial_expected" "$partial_data/postgresql.conf"
[[ $(stat -c '%a' "$partial_data/postgresql.conf") == 640 ]]

# Preserve an explicit postgresql.conf symlink while atomically replacing its
# externally managed target, including the target's original permissions.
symlink_home=$tmp/symlink-home
symlink_data=$symlink_home/.hmem/data/postgresql
symlink_external=$tmp/external-postgresql-config
symlink_target=$symlink_external/postgresql.conf
symlink_original=$tmp/symlink-original.conf
symlink_expected=$tmp/symlink-expected.conf
mkdir -p "$symlink_data" "$symlink_external"
printf '17\n' >"$symlink_data/PG_VERSION"
printf '%s\n' \
  'shared_buffers = 320MB' \
  "unix_socket_directories = '/tmp/external-override'" >"$symlink_original"
cp "$symlink_original" "$symlink_target"
chmod 640 "$symlink_target"
if ln -s "$symlink_target" "$symlink_data/postgresql.conf" 2>/dev/null; then
  symlink_destination=$(readlink "$symlink_data/postgresql.conf")
  {
    write_managed_prefix
    cat "$symlink_original"
  } >"$symlink_expected"
  run_init_until_fake_startup_failure "$symlink_home"
  [[ -L "$symlink_data/postgresql.conf" ]]
  [[ $(readlink "$symlink_data/postgresql.conf") == "$symlink_destination" ]]
  cmp "$symlink_expected" "$symlink_target"
  cmp "$symlink_expected" "$symlink_data/postgresql.conf"
  [[ $(stat -c '%a' "$symlink_target") == 640 ]]
else
  echo "PostgreSQL config symlink preservation: SKIP (symlinks unavailable)"
fi

# A marker by itself is not a completed managed prefix. Repair it once, then
# recognize the complete prefix at byte zero on subsequent runs.
marker_home=$tmp/marker-only-home
marker_data=$marker_home/.hmem/data/postgresql
marker_original=$tmp/marker-only-original.conf
marker_expected=$tmp/marker-only-expected.conf
mkdir -p "$marker_data"
printf '17\n' >"$marker_data/PG_VERSION"
printf '%s\nshared_buffers = 64MB\n' \
  '# hmem managed native PostgreSQL TCP-only default' >"$marker_original"
cp "$marker_original" "$marker_data/postgresql.conf"
{
  write_managed_prefix
  cat "$marker_original"
} >"$marker_expected"
run_init_until_fake_startup_failure "$marker_home"
cmp "$marker_expected" "$marker_data/postgresql.conf"
marker_hash=$(sha256sum "$marker_data/postgresql.conf")
run_init_until_fake_startup_failure "$marker_home"
[[ $(sha256sum "$marker_data/postgresql.conf") == "$marker_hash" ]]

# Simulate ENOSPC-like short writes with RLIMIT_FSIZE. The sibling temporary
# file may fail, but the live configuration must remain byte-for-byte intact,
# retain its permissions, and be repairable on the next normal run.
interrupted_home=$tmp/interrupted-home
interrupted_data=$interrupted_home/.hmem/data/postgresql
interrupted_original=$tmp/interrupted-original.conf
interrupted_expected=$tmp/interrupted-expected.conf
mkdir -p "$interrupted_data"
printf '17\n' >"$interrupted_data/PG_VERSION"
{
  printf 'shared_buffers = 192MB\n'
  printf '%*s' 940 '' | tr ' ' '#'
  printf '\n'
} >"$interrupted_original"
original_bytes=$(wc -c <"$interrupted_original")
prefix_bytes=$(write_managed_prefix | wc -c)
[[ $original_bytes -le 1024 ]]
[[ $((original_bytes + prefix_bytes)) -gt 1024 ]]
cp "$interrupted_original" "$interrupted_data/postgresql.conf"
chmod 640 "$interrupted_data/postgresql.conf"

if (trap '' XFSZ; ulimit -c 0; ulimit -f 1; exec env HOME="$interrupted_home" PATH="$fake_bin:$PATH" \
    "$hmem_ctl" init >/dev/null 2>&1); then
  echo "hmem-ctl init unexpectedly succeeded under the file-size limit" >&2
  exit 1
fi
cmp "$interrupted_original" "$interrupted_data/postgresql.conf"
[[ $(stat -c '%a' "$interrupted_data/postgresql.conf") == 640 ]]
if compgen -G "$interrupted_data/.postgresql.conf.hmem.tmp*" >/dev/null; then
  echo "hmem-ctl left a failed PostgreSQL configuration temporary file" >&2
  exit 1
fi

{
  write_managed_prefix
  cat "$interrupted_original"
} >"$interrupted_expected"
run_init_until_fake_startup_failure "$interrupted_home"
cmp "$interrupted_expected" "$interrupted_data/postgresql.conf"
[[ $(stat -c '%a' "$interrupted_data/postgresql.conf") == 640 ]]

# When real PostgreSQL tools are installed, validate parser precedence against
# an actual initdb cluster: the managed default parses as empty, top-level and
# included settings override it, and postgresql.auto.conf remains final.
real_pg_bin=
if command -v initdb >/dev/null 2>&1 && command -v postgres >/dev/null 2>&1; then
  candidate=$(dirname "$(command -v initdb)")
  if [[ -x "$candidate/postgres" ]]; then
    real_pg_bin=$candidate
  fi
fi
if [[ -z "$real_pg_bin" ]]; then
  for candidate in /usr/lib/postgresql/*/bin; do
    if [[ -x "$candidate/initdb" && -x "$candidate/postgres" ]]; then
      real_pg_bin=$candidate
      break
    fi
  done
fi

if [[ -n "$real_pg_bin" ]]; then
  real_home=$tmp/real-parser-home
  real_data=$real_home/.hmem/data/postgresql
  mkdir -p "$(dirname "$real_data")"
  "$real_pg_bin/initdb" -D "$real_data" \
    --auth=trust --no-instructions --no-locale -E UTF8 >/dev/null
  cp "$real_data/postgresql.conf" "$tmp/real-original.conf"
  {
    write_managed_prefix
    cat "$tmp/real-original.conf"
  } >"$tmp/real-expected.conf"

  run_init_until_fake_startup_failure "$real_home"
  cmp "$tmp/real-expected.conf" "$real_data/postgresql.conf"
  [[ -z "$("$real_pg_bin/postgres" -D "$real_data" -C unix_socket_directories)" ]]

  printf "\nunix_socket_directories = '/tmp/hmem-top-level-override'\n" \
    >>"$real_data/postgresql.conf"
  [[ $("$real_pg_bin/postgres" -D "$real_data" -C unix_socket_directories) \
      == /tmp/hmem-top-level-override ]]

  mkdir -p "$real_data/conf.d"
  printf "unix_socket_directories = '/tmp/hmem-include-override'\n" \
    >"$real_data/conf.d/socket.conf"
  printf "\ninclude_dir = 'conf.d'\n" >>"$real_data/postgresql.conf"
  [[ $("$real_pg_bin/postgres" -D "$real_data" -C unix_socket_directories) \
      == /tmp/hmem-include-override ]]

  printf "unix_socket_directories = '/tmp/hmem-auto-override'\n" \
    >"$real_data/postgresql.auto.conf"
  [[ $("$real_pg_bin/postgres" -D "$real_data" -C unix_socket_directories) \
      == /tmp/hmem-auto-override ]]
  echo "real PostgreSQL parser precedence: PASS"
else
  echo "real PostgreSQL parser precedence: SKIP (initdb/postgres unavailable)"
fi

printf 'native PostgreSQL TCP-only setup regression: PASS\n'
