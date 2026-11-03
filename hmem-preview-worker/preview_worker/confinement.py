"""Verify required Linux confinement primitives before loading untrusted bytes."""
import os
from pathlib import Path
from .policy import require


def check():
    require(os.name == "posix" and hasattr(os, "geteuid"), "confinement_unavailable")
    status = {}
    for line in Path("/proc/self/status").read_text().splitlines():
        key, _, value = line.partition(":")
        status[key] = value.strip()
    uid = os.geteuid()
    caps = int(status.get("CapEff", "1"), 16)
    bound = int(status.get("CapBnd", "1"), 16)
    no_new = int(status.get("NoNewPrivs", "0"))
    seccomp = int(status.get("Seccomp", "0"))
    memory = Path("/sys/fs/cgroup/memory.max").read_text().strip()
    pids = Path("/sys/fs/cgroup/pids.max").read_text().strip()
    require(uid != 0 and caps == bound == 0 and no_new == 1 and seccomp == 2, "confinement_unavailable")
    require(memory.isdigit() and 0 < int(memory) <= 512 * 1024**2
            and pids.isdigit() and 0 < int(pids) <= 64, "confinement_unavailable")
    root_ro = False
    for line in Path("/proc/mounts").read_text().splitlines():
        fields = line.split()
        if len(fields) >= 4 and fields[1] == "/":
            root_ro = "ro" in fields[3].split(",")
    interfaces = sorted(x.name for x in Path("/sys/class/net").iterdir())
    routes = Path("/proc/net/route").read_text().strip().splitlines()
    require(root_ro and interfaces == ["lo"] and len(routes) <= 1, "confinement_unavailable")
    forbidden = [key for key, value in os.environ.items() if value and key != "GPG_KEY"
                 and any(word in key.upper() for word in ("TOKEN", "PASSWORD", "SECRET", "CREDENTIAL", "AUTHORIZATION"))]
    require(not forbidden, "confinement_credentials")
    return {"uid": uid, "cap_eff": caps, "cap_bnd": bound, "no_new_privs": no_new,
            "seccomp": seccomp, "memory_max": int(memory), "pids_max": int(pids),
            "root_read_only": root_ro, "interfaces": interfaces}
