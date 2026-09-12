from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path, PurePosixPath
from typing import Any


TASK_ID = "aa30a81c-2adf-4f54-b1eb-cfeaa39bac77"
INDEX_DIGEST = "sha256:aedf3b34836dc57289583142adcf2b93836cda0736ac8e6ce43691b9c2c67170"
PLATFORM_DIGEST = "sha256:144aaa80ddcb520d49df83f915dc188ddd7cc6b1b3b9684a829c21dd39cbe3c5"
CONFIG_DIGEST = "sha256:affa793eda6c6c6583d9c4041372dd710d74a55ab0a89025a1f322dd71b92eb2"


class ContractError(ValueError):
    pass


def sha256_file(path: Path, chunk_bytes: int = 4 * 1024 * 1024) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        while chunk := handle.read(chunk_bytes):
            digest.update(chunk)
    return digest.hexdigest()


def read_json(path: Path) -> Any:
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError) as exc:
        raise ContractError(f"cannot read JSON {path}: {exc}") from exc


def _require(condition: bool, message: str) -> None:
    if not condition:
        raise ContractError(message)


def _safe_relative(value: str) -> Path:
    posix = PurePosixPath(value)
    _require(not posix.is_absolute(), f"artifact path must be relative: {value!r}")
    _require(".." not in posix.parts, f"artifact path escapes root: {value!r}")
    _require(value not in {"", "."}, "artifact path is empty")
    return Path(*posix.parts)


def load_contract(path: Path) -> dict[str, Any]:
    contract = read_json(path)
    validate_contract(contract)
    return contract


def validate_contract(contract: Any) -> None:
    _require(isinstance(contract, dict), "contract root must be an object")
    _require(contract.get("schema_version") == 1, "unsupported contract schema")
    _require(contract.get("task_id") == TASK_ID, "unexpected task identity")
    image = contract.get("image")
    _require(isinstance(image, dict), "image contract is missing")
    _require(image.get("index_digest") == INDEX_DIGEST, "unexpected OCI index digest")
    _require(
        image.get("platform_manifest_digest") == PLATFORM_DIGEST,
        "unexpected OCI platform manifest digest",
    )
    _require(image.get("config_digest") == CONFIG_DIGEST, "unexpected OCI config digest")
    _require(image.get("reference", "").endswith("@" + INDEX_DIGEST), "image must be digest-pinned")
    _require(image.get("platform") == {"os": "linux", "architecture": "amd64"}, "unexpected platform")

    artifacts = contract.get("model", {}).get("artifacts")
    _require(isinstance(artifacts, list) and len(artifacts) == 20, "model lock must contain 20 artifacts")
    paths = [entry.get("path") for entry in artifacts if isinstance(entry, dict)]
    _require(len(paths) == 20 and len(set(paths)) == 20, "model artifact paths must be unique")
    for entry in artifacts:
        _safe_relative(entry["path"])
        _require(_is_hex_sha256(entry.get("sha256")), f"invalid model SHA-256 for {entry.get('path')}")

    layers = image.get("layers")
    _require(isinstance(layers, list) and layers, "image layer lock is missing")
    _require(len({entry.get("digest") for entry in layers}) == len(layers), "image layer digests must be unique")
    for entry in layers:
        _require(_is_digest(entry.get("digest")), "invalid image layer digest")
        _require(isinstance(entry.get("bytes"), int) and entry["bytes"] > 0, "invalid image layer size")
    limits = contract.get("preparation_limits", {})
    _require(sum(entry["bytes"] for entry in layers) <= limits.get("max_compressed_image_bytes", 0), "image exceeds compressed-byte cap")
    _require(limits.get("attempts") == 1, "preparation must permit exactly one attempt")


def validate_index_manifest(contract: dict[str, Any], value: Any) -> None:
    _require(isinstance(value, dict), "OCI index must be an object")
    _require(value.get("mediaType") == "application/vnd.oci.image.index.v1+json", "unexpected OCI index media type")
    selected = [
        item
        for item in value.get("manifests", [])
        if item.get("platform") == contract["image"]["platform"]
    ]
    _require(len(selected) == 1, "OCI index must have exactly one linux/amd64 manifest")
    _require(selected[0].get("digest") == PLATFORM_DIGEST, "linux/amd64 manifest digest drift")


def validate_platform_manifest(contract: dict[str, Any], value: Any) -> None:
    _require(isinstance(value, dict), "OCI platform manifest must be an object")
    _require(value.get("mediaType") == "application/vnd.oci.image.manifest.v1+json", "unexpected OCI manifest media type")
    _require(value.get("config", {}).get("digest") == CONFIG_DIGEST, "OCI config digest drift")
    observed = [(item.get("digest"), item.get("size")) for item in value.get("layers", [])]
    expected = [(item["digest"], item["bytes"]) for item in contract["image"]["layers"]]
    _require(observed == expected, "OCI layer order, digest, or size drift")


def validate_locked_files(root: Path, entries: list[dict[str, Any]], exact: bool) -> list[dict[str, Any]]:
    _require(root.is_dir(), f"locked root does not exist: {root}")
    inventory: list[dict[str, Any]] = []
    expected_paths: set[str] = set()
    for entry in entries:
        relative = _safe_relative(entry["path"])
        candidate = root / relative
        _require(candidate.is_file(), f"locked file is missing: {candidate}")
        observed = sha256_file(candidate)
        _require(observed == entry["sha256"], f"checksum drift: {candidate}")
        expected_paths.add(relative.as_posix())
        inventory.append({"path": relative.as_posix(), "bytes": candidate.stat().st_size, "sha256": observed})
    if exact:
        actual = {
            item.relative_to(root).as_posix()
            for item in root.rglob("*")
            if item.is_file()
        }
        _require(actual == expected_paths, f"locked root file set drift: missing={sorted(expected_paths-actual)}, extra={sorted(actual-expected_paths)}")
    return inventory


def validate_source_facts(source_root: Path) -> None:
    flash = (source_root / "backends/candle/src/models/flash_qwen2.rs").read_text(encoding="utf-8")
    qwen = (source_root / "backends/candle/src/models/qwen2.rs").read_text(encoding="utf-8")
    caps = (source_root / "backends/candle/src/compute_cap.rs").read_text(encoding="utf-8")
    dockerfile = (source_root / "Dockerfile-cuda").read_text(encoding="utf-8")
    entrypoint = (source_root / "cuda-entrypoint.sh").read_text(encoding="utf-8")
    matrix = read_json(source_root / ".github/workflows/matrix.json")
    _require("is_causal: config.is_causal" in flash and "self.is_causal" in flash, "FlashQwen2 does not consume config.is_causal")
    _require("if vb.dtype() != DType::F16" in flash, "FlashQwen2 F16 requirement is missing")
    _require("pub is_causal: bool" in qwen, "Qwen2 explicit is_causal field is missing")
    _require("(120, 120) => true" in caps, "runtime/compile SM120 match is missing")
    _require("nvidia/cuda:12.9.1-runtime-ubuntu24.04" in dockerfile, "CUDA runtime drift")
    _require("DEFAULT_USE_FLASH_ATTENTION=True" in dockerfile, "Flash attention default drift")
    _require("command -v nvidia-smi" in entrypoint, "CUDA entrypoint device check drift")
    _require("DRIVER_CUDA=$(nvidia-smi" in entrypoint and "exec text-embeddings-router" in entrypoint, "CUDA entrypoint/router linkage drift")
    selected = [item for item in matrix if item.get("name") == "blackwell-120"]
    _require(len(selected) == 1 and selected[0].get("cudaComputeCap") == 120, "SM120 build matrix drift")


def atomic_write_json(path: Path, value: Any, max_bytes: int) -> None:
    payload = (json.dumps(value, indent=2, sort_keys=True, ensure_ascii=False) + "\n").encode("utf-8")
    _require(len(payload) <= max_bytes, f"JSON output exceeds cap: {len(payload)} > {max_bytes}")
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + f".tmp-{os.getpid()}")
    try:
        with temporary.open("xb") as handle:
            handle.write(payload)
            handle.flush()
            os.fsync(handle.fileno())
        os.replace(temporary, path)
    finally:
        if temporary.exists():
            temporary.unlink()


def _is_hex_sha256(value: Any) -> bool:
    return isinstance(value, str) and len(value) == 64 and all(ch in "0123456789abcdef" for ch in value)


def _is_digest(value: Any) -> bool:
    return isinstance(value, str) and value.startswith("sha256:") and _is_hex_sha256(value[7:])
