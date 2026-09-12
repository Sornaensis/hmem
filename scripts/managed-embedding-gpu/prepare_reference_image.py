from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import time
import urllib.request
import uuid
from pathlib import Path
from typing import Any

from prepare_image import CommandError, run


TASK_ID = "aa30a81c-2adf-4f54-b1eb-cfeaa39bac77"
REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_ROOT = Path(__file__).resolve().parent
FIXTURE_ROOT = REPO_ROOT / "hmem-server/test/fixtures/embedding-gpu-viability"
DEFAULT_METADATA = FIXTURE_ROOT / "reference-wheel-metadata-v1.json"
DEFAULT_METADATA_PROVENANCE = FIXTURE_ROOT / "reference-wheel-size-provenance-v1.json"
DEFAULT_REQUIREMENTS = SCRIPT_ROOT / "reference-requirements.lock"
DEFAULT_DOCKERFILE = SCRIPT_ROOT / "Dockerfile.reference-cuda"
DEFAULT_DRIVER = SCRIPT_ROOT / "reference_cuda.py"
ARTIFACT_ROOT = Path(r"D:\hmem-artifacts\aa30a81c-2adf-4f54-b1eb-cfeaa39bac77\gpu-foundation-v1")
METADATA_SHA256 = "dfa20a10e156eb79a56bf03166c4c59ff838d2f0d3f559eabe4485967722b287"
METADATA_PROVENANCE_SHA256 = "78073664ee350b0b28f1f4b860a9d22b13d97e9d2fb73f2c83913a319d5def61"
LOCK_SHA256 = "3a9d75851a22b4cf880fc81b6c35ca15659401d3aabf56e3ab758445a87b9c02"
BASE_IMAGE = "docker.io/library/python@sha256:2856e6af199e8128161abd320575eb9b341f3b76f017b5d0c9cd364f60d8a050"
BASE_IMAGE_DIGEST = "sha256:2856e6af199e8128161abd320575eb9b341f3b76f017b5d0c9cd364f60d8a050"
DERIVED_TAG = "hmem-reference-cuda:aa30a81c-ba60c4e"
GENERATIONS = ("v1", "v2", "v3", "v4")
PACKAGE_COUNT = 39
EXPECTED_WHEEL_BYTES = 3_883_897_561
MAX_WHEEL_BYTES = 6 * 1024 * 1024 * 1024
MINIMUM_FREE_BYTES = 18 * 1024 * 1024 * 1024
DOWNLOAD_TOTAL_SECONDS = 7200
DOWNLOAD_SOCKET_SECONDS = 60
PULL_SECONDS = 1800
BUILD_SECONDS = 3600
COMMAND_OUTPUT_BYTES = 8 * 1024 * 1024
INSTALL_MEMORY_BYTES = 12 * 1024 * 1024 * 1024
INSTALL_MEMORY_SWAP_BYTES = 16 * 1024 * 1024 * 1024
INSTALL_CPUS = 6
BUILD_NPROC_RLIMIT = 512
BUILD_SHM_BYTES = 2 * 1024 * 1024 * 1024
MAX_DERIVED_IMAGE_BYTES = 12 * 1024 * 1024 * 1024


class PreparationError(ValueError):
    pass


def require(condition: bool, message: str) -> None:
    if not condition:
        raise PreparationError(message)


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        while chunk := handle.read(4 * 1024 * 1024):
            digest.update(chunk)
    return digest.hexdigest()


def read_json(path: Path) -> Any:
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError) as exc:
        raise PreparationError(f"cannot read JSON {path}: {exc}") from exc


def validate_inputs(metadata_path: Path, provenance_path: Path, lock_path: Path) -> list[dict[str, Any]]:
    require(sha256_file(metadata_path) == METADATA_SHA256, "reference wheel metadata checksum drift")
    require(sha256_file(provenance_path) == METADATA_PROVENANCE_SHA256, "reference wheel metadata provenance checksum drift")
    require(sha256_file(lock_path) == LOCK_SHA256, "reference requirements checksum drift")
    metadata = read_json(metadata_path)
    provenance = read_json(provenance_path)
    require(provenance.get("kind") == "official_torch_wheel_size_correction", "unexpected metadata correction provenance kind")
    require(provenance.get("task_id") == TASK_ID, "metadata correction task identity drift")
    require(provenance.get("closure_bytes") == EXPECTED_WHEEL_BYTES, "metadata correction closure size drift")
    require(provenance.get("retrieval", {}).get("headers", {}).get("Content-Length") == "1039389795", "Torch size provenance drift")
    require(provenance.get("retrieval", {}).get("headers", {}).get("x-amz-meta-checksum-sha256") == "c301dc280458afd95450af794924c98fe07522dd148ff384739b810e3e3179f2", "Torch checksum provenance drift")
    require(metadata.get("metadata_only") is True, "wheel metadata flag drift")
    require(metadata.get("closure_errors") == [], "wheel dependency closure has errors")
    target = metadata.get("target")
    require(isinstance(target, dict), "wheel target is missing")
    require(target.get("python_full_version") == "3.11.9", "wheel Python target drift")
    require(target.get("platform_system") == "Linux" and target.get("platform_machine") == "x86_64", "wheel platform target drift")
    packages = metadata.get("packages")
    require(isinstance(packages, list) and len(packages) == PACKAGE_COUNT, f"expected {PACKAGE_COUNT} wheel records")
    filenames = [item.get("filename") for item in packages if isinstance(item, dict)]
    require(len(filenames) == PACKAGE_COUNT and len(set(filenames)) == PACKAGE_COUNT, "wheel filenames must be unique")
    total = 0
    expected_lock = set()
    for item in packages:
        require(set(("name", "version", "filename", "url", "size", "sha256")) <= set(item), "wheel record is incomplete")
        require(isinstance(item["filename"], str) and Path(item["filename"]).name == item["filename"], "unsafe wheel filename")
        require(item["filename"].endswith(".whl"), "non-wheel artifact in metadata")
        require(isinstance(item["url"], str) and item["url"].startswith("https://"), "wheel URL must use HTTPS")
        require(isinstance(item["size"], int) and item["size"] > 0, "invalid wheel size")
        require(isinstance(item["sha256"], str) and re.fullmatch(r"[0-9a-f]{64}", item["sha256"]) is not None, "invalid wheel checksum")
        total += item["size"]
        expected_lock.add(f"{item['name'].lower()}=={item['version']} --hash=sha256:{item['sha256']}")
    require(total == EXPECTED_WHEEL_BYTES, f"wheel closure size drift: {total}")
    require(total <= MAX_WHEEL_BYTES, f"wheel closure exceeds byte cap: {total}")
    lock_lines = {
        line.strip().lower()
        for line in lock_path.read_text(encoding="utf-8").splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    }
    require(lock_lines == expected_lock, "requirements lock and wheel metadata differ")
    return packages


def download_wheel(item: dict[str, Any], directory: Path, deadline: float, attempt_id: str) -> dict[str, Any]:
    destination = directory / item["filename"]
    if destination.exists():
        require(destination.is_file(), f"wheel destination is not a file: {destination}")
        require(destination.stat().st_size == item["size"] and sha256_file(destination) == item["sha256"], f"existing wheel drift: {destination}")
        return {"filename": item["filename"], "bytes": item["size"], "sha256": item["sha256"], "reused": True}
    require(time.monotonic() < deadline, "download total deadline expired")
    temporary = destination.with_name(destination.name + f".part-{attempt_id}")
    digest = hashlib.sha256()
    observed_bytes = 0
    request = urllib.request.Request(item["url"], headers={"User-Agent": "hmem-reference-prefetch/1"})
    try:
        with urllib.request.urlopen(request, timeout=DOWNLOAD_SOCKET_SECONDS) as response, temporary.open("xb") as handle:
            content_length = response.headers.get("Content-Length")
            if content_length is not None:
                require(int(content_length) == item["size"], f"HTTP size drift for {item['filename']}")
            while True:
                require(time.monotonic() < deadline, "download total deadline expired")
                chunk = response.read(4 * 1024 * 1024)
                if not chunk:
                    break
                observed_bytes += len(chunk)
                require(observed_bytes <= item["size"], f"download exceeded locked size for {item['filename']}")
                digest.update(chunk)
                handle.write(chunk)
            handle.flush()
            os.fsync(handle.fileno())
        require(observed_bytes == item["size"], f"download size drift for {item['filename']}: {observed_bytes}")
        require(digest.hexdigest() == item["sha256"], f"download checksum drift for {item['filename']}")
        os.replace(temporary, destination)
    except BaseException:
        # The uniquely named partial is retained as failed-attempt evidence.
        raise
    return {"filename": item["filename"], "bytes": observed_bytes, "sha256": digest.hexdigest(), "reused": False}


def copy_frozen(source: Path, destination: Path) -> None:
    expected = sha256_file(source)
    if destination.exists():
        require(destination.is_file() and sha256_file(destination) == expected, f"build-context input drift: {destination}")
        return
    shutil.copyfile(source, destination)
    require(sha256_file(destination) == expected, f"build-context copy drift: {destination}")


def inspect_image(reference: str) -> dict[str, Any]:
    stdout, _ = run(["docker", "image", "inspect", reference], 30, COMMAND_OUTPUT_BYTES)
    value = json.loads(stdout)
    require(isinstance(value, list) and len(value) == 1 and isinstance(value[0], dict), "Docker inspect did not return one image")
    return value[0]


def authenticate_derived_image(committed_id: str, derived_tag: str) -> tuple[dict[str, Any], dict[str, Any], dict[str, str]]:
    derived = inspect_image(derived_tag)
    require(derived.get("Id") == committed_id, "derived tag does not resolve to BuildKit image ID")
    require(isinstance(derived.get("Id"), str) and re.fullmatch(r"sha256:[0-9a-f]{64}", derived["Id"]) is not None, "derived image ID is invalid")
    require(derived.get("Architecture") == "amd64" and derived.get("Os") == "linux", "derived image platform drift")
    config = derived.get("Config", {})
    require(config.get("User") == "65532:65532", "derived image user drift")
    require(config.get("Entrypoint") == ["/usr/local/bin/python3.11", "-I", "-B", "/scripts/reference_cuda.py"], "derived image entrypoint drift")
    labels = config.get("Labels") or {}
    require(labels.get("io.hmem.task") == TASK_ID and labels.get("io.hmem.reference.kind") == "independent-pytorch-cuda", "derived image labels drift")
    require(isinstance(derived.get("Size"), int) and derived["Size"] <= MAX_DERIVED_IMAGE_BYTES, "derived image exceeds disk-size cap")
    return derived, config, labels


def derived_image_record(derived_tag: str, derived: dict[str, Any], config: dict[str, Any], labels: dict[str, str]) -> dict[str, Any]:
    return {
        "tag": derived_tag, "id": derived["Id"], "repo_tags": derived.get("RepoTags"),
        "repo_digests": derived.get("RepoDigests"), "created": derived.get("Created"),
        "size": derived.get("Size"), "architecture": derived.get("Architecture"), "os": derived.get("Os"),
        "rootfs_diff_ids": derived.get("RootFS", {}).get("Layers"), "config": {
            "user": config.get("User"), "entrypoint": config.get("Entrypoint"), "environment": config.get("Env"),
            "labels": labels,
        },
    }


def link_wheels(packages: list[dict[str, Any]], wheelhouse: Path, context_wheelhouse: Path) -> None:
    context_wheelhouse.mkdir(parents=True, exist_ok=True)
    expected_names = {item["filename"] for item in packages}
    actual_names = {item.name for item in context_wheelhouse.iterdir() if item.is_file()}
    require(actual_names <= expected_names, f"unexpected wheel in build context: {sorted(actual_names-expected_names)}")
    for item in packages:
        source = wheelhouse / item["filename"]
        destination = context_wheelhouse / item["filename"]
        if destination.exists():
            require(destination.stat().st_size == item["size"] and sha256_file(destination) == item["sha256"], f"context wheel drift: {destination}")
            continue
        try:
            os.link(source, destination)
        except OSError:
            shutil.copyfile(source, destination)
        require(destination.stat().st_size == item["size"] and sha256_file(destination) == item["sha256"], f"context wheel copy drift: {destination}")


def generation_outputs(artifact_root: Path, generation: str) -> tuple[Path, Path, str]:
    require(generation in GENERATIONS, f"unsupported reference image generation: {generation}")
    if generation == "v1":
        image_root = artifact_root / "reference-image"
        tag = DERIVED_TAG
    else:
        image_root = artifact_root / f"reference-image-{generation}"
        tag = f"{DERIVED_TAG}-{generation}"
    return image_root, image_root / f"pinned-image-{generation}.json", tag


def build_command(image_root: Path, iid_path: Path, metadata_path: Path, tag: str = DERIVED_TAG) -> list[str]:
    return [
        "docker", "buildx", "build", "--load", "--progress=plain", "--network=none",
        "--pull=false", "--no-cache", "--platform", "linux/amd64",
        "--resource", f"memory={INSTALL_MEMORY_BYTES}",
        "--resource", f"memory-swap={INSTALL_MEMORY_SWAP_BYTES}",
        "--resource", f"cpu-period=100000", "--resource", f"cpu-quota={INSTALL_CPUS * 100000}",
        "--ulimit", f"nproc={BUILD_NPROC_RLIMIT}:{BUILD_NPROC_RLIMIT}", "--shm-size", str(BUILD_SHM_BYTES),
        "--iidfile", str(iid_path), "--metadata-file", str(metadata_path),
        "--file", str(image_root / "Dockerfile.reference-cuda"), "--tag", tag, str(image_root),
    ]


def write_new_json(path: Path, value: Any) -> None:
    payload = (json.dumps(value, indent=2, sort_keys=True) + "\n").encode("utf-8")
    require(len(payload) <= COMMAND_OUTPUT_BYTES, "preparation report exceeds output cap")
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("xb") as handle:
        handle.write(payload)
        handle.flush()
        os.fsync(handle.fileno())


def main() -> int:
    parser = argparse.ArgumentParser(description="Fetch, authenticate, and build the pinned independent CUDA reference image.")
    parser.add_argument("--metadata", type=Path, default=DEFAULT_METADATA)
    parser.add_argument("--metadata-provenance", type=Path, default=DEFAULT_METADATA_PROVENANCE)
    parser.add_argument("--requirements", type=Path, default=DEFAULT_REQUIREMENTS)
    parser.add_argument("--dockerfile", type=Path, default=DEFAULT_DOCKERFILE)
    parser.add_argument("--driver", type=Path, default=DEFAULT_DRIVER)
    parser.add_argument("--artifact-root", type=Path, default=ARTIFACT_ROOT)
    parser.add_argument("--generation", choices=GENERATIONS, default="v1")
    args = parser.parse_args()
    started = time.monotonic()
    attempt_id = uuid.uuid4().hex
    report: dict[str, Any] = {
        "schema_version": 1, "state": "started", "task_id": TASK_ID, "attempt_id": attempt_id,
        "generation": args.generation,
        "limits": {
            "attempts_per_url": 1, "download_socket_seconds": DOWNLOAD_SOCKET_SECONDS,
            "download_total_seconds": DOWNLOAD_TOTAL_SECONDS, "pull_seconds": PULL_SECONDS,
            "build_seconds": BUILD_SECONDS, "max_wheel_bytes": MAX_WHEEL_BYTES,
            "minimum_free_bytes": MINIMUM_FREE_BYTES, "command_output_bytes": COMMAND_OUTPUT_BYTES,
            "build_memory_bytes": INSTALL_MEMORY_BYTES,
            "build_memory_swap_bytes": INSTALL_MEMORY_SWAP_BYTES,
            "build_cpu_quota": INSTALL_CPUS * 100000, "build_cpu_period": 100000,
            "build_nproc_rlimit": BUILD_NPROC_RLIMIT,
            "build_shared_memory_bytes": BUILD_SHM_BYTES,
            "resource_effectiveness_inspected": False,
            "max_derived_image_bytes": MAX_DERIVED_IMAGE_BYTES,
        },
    }
    attempt_root: Path | None = None
    try:
        artifact_root = args.artifact_root.resolve()
        require(artifact_root == ARTIFACT_ROOT.resolve(), f"artifact root must be exactly {ARTIFACT_ROOT}")
        artifact_root.mkdir(parents=True, exist_ok=True)
        require(shutil.disk_usage(artifact_root).free >= MINIMUM_FREE_BYTES, "insufficient free disk for wheelhouse and image build")
        wheelhouse = artifact_root / "reference-wheelhouse"
        image_root, pinned, derived_tag = generation_outputs(artifact_root, args.generation)
        attempt_root = image_root / "attempts" / attempt_id
        wheelhouse.mkdir(parents=True, exist_ok=True)
        image_root.mkdir(parents=True, exist_ok=True)
        attempt_root.mkdir(parents=True, exist_ok=False)

        metadata_path = args.metadata.resolve()
        provenance_path = args.metadata_provenance.resolve()
        lock_path = args.requirements.resolve()
        dockerfile_path = args.dockerfile.resolve()
        driver_path = args.driver.resolve()
        packages = validate_inputs(metadata_path, provenance_path, lock_path)
        report["inputs"] = {
            "metadata": {"path": str(metadata_path), "sha256": sha256_file(metadata_path)},
            "metadata_provenance": {"path": str(provenance_path), "sha256": sha256_file(provenance_path)},
            "requirements": {"path": str(lock_path), "sha256": sha256_file(lock_path)},
            "dockerfile": {"path": str(dockerfile_path), "sha256": sha256_file(dockerfile_path)},
            "driver": {"path": str(driver_path), "sha256": sha256_file(driver_path)},
        }
        deadline = time.monotonic() + DOWNLOAD_TOTAL_SECONDS
        report["wheels"] = [download_wheel(item, wheelhouse, deadline, attempt_id) for item in packages]
        expected_wheels = {item["filename"] for item in packages}
        actual_wheels = {item.name for item in wheelhouse.iterdir() if item.is_file() and ".part-" not in item.name}
        require(actual_wheels == expected_wheels, f"wheelhouse file set drift: missing={sorted(expected_wheels-actual_wheels)}, extra={sorted(actual_wheels-expected_wheels)}")

        copy_frozen(dockerfile_path, image_root / "Dockerfile.reference-cuda")
        copy_frozen(lock_path, image_root / "reference-requirements.lock")
        copy_frozen(driver_path, image_root / "reference_cuda.py")
        link_wheels(packages, wheelhouse, image_root / "wheelhouse")

        pull_out, pull_err = run(["docker", "pull", "--platform", "linux/amd64", BASE_IMAGE], PULL_SECONDS, COMMAND_OUTPUT_BYTES)
        (attempt_root / "base-pull.log").write_text(pull_out + pull_err, encoding="utf-8")
        base = inspect_image(BASE_IMAGE)
        require(base.get("Id") == BASE_IMAGE_DIGEST, f"base image config identity drift: {base.get('Id')}")
        require(base.get("Architecture") == "amd64" and base.get("Os") == "linux", "base image platform drift")

        iid_path = attempt_root / "image.id"
        build_metadata_path = attempt_root / "build-metadata.json"
        command = build_command(image_root, iid_path, build_metadata_path, derived_tag)
        report["build_argv"] = command
        build_out, build_err = run(command, BUILD_SECONDS, COMMAND_OUTPUT_BYTES)
        (attempt_root / "image-build.log").write_text(build_out + build_err, encoding="utf-8")
        committed_id = iid_path.read_text(encoding="utf-8").strip()
        require(re.fullmatch(r"sha256:[0-9a-f]{64}", committed_id) is not None, "BuildKit returned an invalid image ID")
        derived, config, labels = authenticate_derived_image(committed_id, derived_tag)
        report["base_image"] = {"reference": BASE_IMAGE, "id": base.get("Id"), "repo_digests": base.get("RepoDigests")}
        report["derived_image"] = derived_image_record(derived_tag, derived, config, labels)
        report["state"] = "prepared"
        report["elapsed_seconds"] = time.monotonic() - started
        if pinned.exists():
            previous = read_json(pinned)
            require(previous.get("derived_image", {}).get("id") == derived["Id"], "previously pinned derived image differs")
            require(previous.get("inputs") == report["inputs"], "previously pinned build inputs differ")
        else:
            write_new_json(pinned, report)
        write_new_json(attempt_root / "preparation-report.json", report)
        print(json.dumps({"state": "prepared", "image_id": derived["Id"], "report": str(attempt_root / "preparation-report.json")}, sort_keys=True))
        return 0
    except (CommandError, OSError, PreparationError, subprocess.SubprocessError, ValueError) as exc:
        report["state"] = "failed"
        report["error"] = {"type": type(exc).__name__, "message": str(exc)}
        report["elapsed_seconds"] = time.monotonic() - started
        if attempt_root is not None:
            try:
                write_new_json(attempt_root / "preparation-report.json", report)
            except BaseException as report_exc:
                print(f"failed to preserve preparation report: {report_exc}", file=sys.stderr)
        print(f"reference image preparation failed: {type(exc).__name__}: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
