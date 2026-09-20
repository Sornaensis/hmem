#!/usr/bin/env python3
"""Assemble the offline BuildKit managed-bundle context from locked local assets.

The independent Haskell checker is the authority for manifest, model, runtime,
numerical fixture, and license bytes. This script only stages an exact, symlink-
free copy and records what was staged. It never downloads serving assets.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import stat
import sys
import uuid

sys.dont_write_bytecode = True


class PreparationError(ValueError):
    pass


def is_link_or_junction(path: Path) -> bool:
    return path.is_symlink() or getattr(path, "is_junction", lambda: False)()


def checked_destinations(output: Path, report: Path,
                         sources: tuple[Path, ...]) -> tuple[Path, Path]:
    # Requiring an existing private parent avoids creating directories through
    # an unchecked alias. Inspect the lexical ancestry before resolving it:
    # resolve() alone would silently accept a junction into a source tree.
    for path in (output, report):
        if not path.is_absolute() or ".." in path.parts or path.name in ("", ".", ".."):
            raise PreparationError("bundle destinations require absolute paths without traversal")
        if path.exists() or is_link_or_junction(path):
            raise PreparationError("bundle destination must not exist")
    if output.parent != report.parent or output == report:
        raise PreparationError("bundle and report require distinct files in one private parent")
    parent = output.parent
    if not parent.is_dir():
        raise PreparationError("bundle private parent must already exist")
    cursor = Path(parent.anchor)
    for part in parent.parts[1:]:
        cursor /= part
        if is_link_or_junction(cursor):
            raise PreparationError("bundle destination ancestry must not contain a link or junction")
    canonical_parent = parent.resolve(strict=True)
    for path in (canonical_parent / output.name, canonical_parent / report.name):
        if any(path == source or source in path.parents for source in sources):
            raise PreparationError("bundle destination aliases a repository or source asset tree")
    return canonical_parent / output.name, canonical_parent / report.name


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(4 * 1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def regular_tree(root: Path) -> list[Path]:
    if is_link_or_junction(root) or not root.is_dir():
        raise PreparationError(f"asset root must be a real directory: {root}")
    files: list[Path] = []
    for current, dirs, names in os.walk(root, followlinks=False):
        parent = Path(current)
        for name in dirs:
            child = parent / name
            if is_link_or_junction(child) or not child.is_dir():
                raise PreparationError(f"non-directory or symlink in asset tree: {child}")
        for name in names:
            child = parent / name
            mode = child.lstat().st_mode
            if not stat.S_ISREG(mode):
                raise PreparationError(f"non-regular or symlinked asset: {child}")
            files.append(child.relative_to(root))
    return sorted(files)


def copy_exact(source: Path, destination: Path) -> list[dict[str, object]]:
    inventory: list[dict[str, object]] = []
    for relative in regular_tree(source):
        original = source / relative
        staged = destination / relative
        staged.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(original, staged)
        if original.stat().st_size != staged.stat().st_size:
            raise PreparationError(f"copied asset size drift: {relative}")
        source_hash = sha256_file(original)
        staged_hash = sha256_file(staged)
        if source_hash != staged_hash:
            raise PreparationError(f"copied asset checksum drift: {relative}")
        inventory.append({"path": relative.as_posix(), "bytes": staged.stat().st_size, "sha256": staged_hash})
    return inventory


def run_checker(root: Path, model: Path, runtime: Path) -> None:
    scripts = root / "scripts" / "managed-embedding-gpu"
    sys.path.insert(0, str(scripts))
    from prepare_image import CommandError, run  # Existing bounded, owned-process runner.

    command = [
        "stack", "script", "--resolver", "lts-24.2",
        "--package", "aeson", "--package", "yaml", "--package", "crypton",
        "--package", "directory", "--package", "filepath",
        "--package", "bytestring", "--package", "text",
        "--package", "containers", "--package", "scientific",
        "--package", "temporary", "--package", "process",
        "--package", "vector",
        str(root / "scripts" / "check-managed-embedding-provenance.hs"),
        "--", "--root", str(root), "--model-root", str(model), "--runtime-root", str(runtime),
    ]
    try:
        stdout, _stderr = run(command, timeout_seconds=900, output_cap=65536)
    except CommandError as exc:
        reason = exc.reason if exc.reason in {
            "exit_code", "timeout", "output_cap", "startup_failure", "pipe_failure"
        } else "command_failure"
        # Only return short, path-shaped checker findings or a Stack error code.
        # Never echo the command, environment, arbitrary stderr, or secret text.
        safe_findings = []
        for line in exc.stdout.decode("utf-8", "replace").splitlines()[-12:]:
            match = re.fullmatch(r"  - ([a-z][a-z ]{2,80}): ([A-Za-z0-9_./-]{1,180})", line)
            if match:
                safe_findings.append(f"{match.group(1)}: {match.group(2)}")
        codes = re.findall(r"\[S-[0-9]{4}\]", exc.stderr.decode("utf-8", "replace"))
        detail = "; ".join(safe_findings[-3:]) or (codes[-1] if codes else "no safe detail")
        raise PreparationError(f"independent checker failed ({reason}): {detail}") from None
    except Exception:
        raise PreparationError("independent checker failed (runner error)") from None
    if "Managed embedding GPU provenance check passed." not in stdout:
        raise PreparationError("independent checker did not report a pass")


def prepare(root: Path, model: Path, runtime: Path, output: Path, report: Path) -> dict[str, object]:
    if any(is_link_or_junction(path) for path in (root, model, runtime)):
        raise PreparationError("input roots must not be symlinks")
    root = root.resolve(strict=True)
    model = model.resolve(strict=True)
    runtime = runtime.resolve(strict=True)
    output, report = checked_destinations(output, report, (root, model, runtime))
    manifest = root / "config" / "managed-embedding-provenance.yaml"
    if not manifest.is_file() or manifest.is_symlink():
        raise PreparationError("locked repository manifest is missing or symlinked")
    stage = output.with_name(output.name + ".tmp-" + uuid.uuid4().hex)
    stage.mkdir(mode=0o700)
    published = False
    report_created = False
    accepted = False
    try:
        # Check source first, then check the independent copied bytes again.
        run_checker(root, model, runtime)
        (stage / "manifest").mkdir()
        shutil.copyfile(manifest, stage / "manifest" / manifest.name)
        model_inventory = copy_exact(model, stage / "model")
        runtime_inventory = copy_exact(runtime, stage / "tei-runtime")
        run_checker(root, stage / "model", stage / "tei-runtime")
        if sha256_file(manifest) != sha256_file(stage / "manifest" / manifest.name):
            raise PreparationError("staged manifest checksum drift")
        evidence: dict[str, object] = {
            "schema_version": 1,
            "checker": "scripts/check-managed-embedding-provenance.hs",
            "manifest_sha256": sha256_file(manifest),
            "model": model_inventory,
            "tei_runtime": runtime_inventory,
            "model_file_count": len(model_inventory),
            "runtime_file_count": len(runtime_inventory),
        }
        payload = (json.dumps(evidence, sort_keys=True, indent=2) + "\n").encode("utf-8")
        if len(payload) > 16384:
            raise PreparationError("preparation evidence exceeds 16 KiB")
        stage.rename(output)
        published = True
        with report.open("xb") as handle:
            report_created = True
            handle.write(payload)
            handle.flush()
            os.fsync(handle.fileno())
        accepted = True
        return evidence
    finally:
        if stage.exists():
            shutil.rmtree(stage)
        if published and not accepted:
            shutil.rmtree(output)
        if report_created and not accepted:
            report.unlink()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", required=True, type=Path)
    parser.add_argument("--model-root", required=True, type=Path)
    parser.add_argument("--runtime-root", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--report", required=True, type=Path)
    args = parser.parse_args()
    try:
        result = prepare(args.root, args.model_root, args.runtime_root, args.output, args.report)
    except (OSError, PreparationError) as exc:
        print(f"managed bundle preparation failed: {exc}", file=sys.stderr)
        return 1
    print(f"managed bundle verified: {result['model_file_count']} model files, {result['runtime_file_count']} runtime files")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
