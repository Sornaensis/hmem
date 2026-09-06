from __future__ import annotations

import argparse
import hashlib
import importlib.metadata
import json
import os
import re
import subprocess
import sys
from pathlib import Path
from typing import Any


PIN_RE = re.compile(r"^([A-Za-z0-9_.-]+)==([^ \\\r\n]+)")
LICENSE_FILE_RE = re.compile(r"(^|/)(licen[cs]e|copying|notice|authors?)([._-]|$)", re.I)


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def normalized(name: str) -> str:
    return re.sub(r"[-_.]+", "-", name).lower()


def parse_pins(path: Path) -> dict[str, str]:
    pins: dict[str, str] = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        match = PIN_RE.match(line)
        if match:
            pins[normalized(match.group(1))] = match.group(2)
    return pins


def record_file(path: Path) -> dict[str, Any]:
    return {"path": str(path), "size": path.stat().st_size, "sha256": sha256_file(path)}


def wheel_records(pins: dict[str, str]) -> tuple[list[dict[str, Any]], list[str]]:
    records: list[dict[str, Any]] = []
    unresolved: list[str] = []
    distributions = {normalized(dist.metadata["Name"]): dist for dist in importlib.metadata.distributions()}
    for name, pinned_version in sorted(pins.items()):
        dist = distributions.get(name)
        if dist is None:
            raise ValueError(f"locked distribution is not installed: {name}")
        installed_version = dist.version
        if installed_version != pinned_version:
            raise ValueError(f"version mismatch for {name}: {installed_version} != {pinned_version}")
        metadata = dist.metadata
        expression = metadata.get("License-Expression")
        license_field = metadata.get("License")
        classifiers = sorted(
            value for value in metadata.get_all("Classifier", []) if value.startswith("License ::")
        )
        files: list[dict[str, Any]] = []
        for entry in dist.files or []:
            relative = str(entry).replace("\\", "/")
            if not LICENSE_FILE_RE.search(relative):
                continue
            located = Path(dist.locate_file(entry))
            if located.is_file():
                files.append(record_file(located))
        files.sort(key=lambda item: item["path"])
        if not expression:
            unresolved.append(
                f"{name}=={installed_version}: no License-Expression; human review of recorded metadata/files required"
            )
        if not (expression or license_field or classifiers or files):
            unresolved.append(f"{name}=={installed_version}: no license evidence shipped in wheel metadata")
        records.append(
            {
                "name": metadata["Name"],
                "version": installed_version,
                "license_expression": expression,
                "license_field": license_field,
                "license_classifiers": classifiers,
                "license_files": files,
            }
        )
    return records, unresolved


def debian_records() -> list[dict[str, Any]]:
    output = subprocess.run(
        ["dpkg-query", "-W", "-f=${binary:Package}\t${Version}\n"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    records: list[dict[str, Any]] = []
    for line in output.splitlines():
        package, version = line.split("\t", 1)
        copyright_path = Path("/usr/share/doc") / package.split(":", 1)[0] / "copyright"
        record: dict[str, Any] = {"package": package, "version": version}
        if copyright_path.is_file():
            record["copyright"] = record_file(copyright_path.resolve())
        else:
            record["copyright"] = None
        records.append(record)
    return sorted(records, key=lambda item: item["package"])


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--requirements", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--toolchain-image", required=True)
    parser.add_argument("--base-image", required=True)
    args = parser.parse_args()

    pins = parse_pins(args.requirements)
    wheels, unresolved = wheel_records(pins)
    python_license = Path("/usr/local/lib/python3.11/LICENSE.txt")
    if not python_license.is_file():
        raise ValueError("Python runtime license file is absent")
    os_release: dict[str, str] = {}
    for line in Path("/etc/os-release").read_text(encoding="utf-8").splitlines():
        if "=" in line:
            key, value = line.split("=", 1)
            os_release[key] = value.strip('"')
    debian = debian_records()
    missing_debian = [item["package"] for item in debian if item["copyright"] is None]
    if missing_debian:
        unresolved.append(
            "installed Debian packages without /usr/share/doc/<package>/copyright: "
            + ", ".join(missing_debian)
        )
    result = {
        "schema_version": 1,
        "toolchain_image": args.toolchain_image,
        "base_image": args.base_image,
        "requirements_sha256": sha256_file(args.requirements),
        "python": {"version": sys.version.split()[0], "license_file": record_file(python_license)},
        "base_os": os_release,
        "locked_wheels": wheels,
        "installed_debian_packages": debian,
        "redistribution_obligations": [
            "Retain the copyright, license, attribution, and NOTICE material required by each redistributed component.",
            "For redistributed GPL/LGPL base-image components, satisfy the applicable corresponding-source and relinking terms.",
            "Review package-specific recorded terms before distributing the build-only toolchain image; this evidence is not a production license grant.",
        ],
        "redistribution_gate": "incomplete" if unresolved else "evidence_complete",
        "unresolved_redistribution_items": unresolved,
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, indent=2, sort_keys=True) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
