from __future__ import annotations

import argparse
import json
import math
import os
import re
import struct
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from gpu_contract import (
    ContractError,
    atomic_write_json,
    load_contract,
    read_json,
    sha256_file,
    validate_locked_files,
)
from prepare_image import run
import reference_cuda
from run_reference_validation import make_deadline_runner
from run_tei_startup import VramMonitor, parse_gpu_csv


TASK_ID = "aa30a81c-2adf-4f54-b1eb-cfeaa39bac77"
BASE_SHA = "ba60c4ecafdac00932fd030876041e347f222948"
IMAGE_ID = "sha256:cc98032cdc6ac1b6f4ad9197006d9347dd09096a6a8854e978f28ff4daaa8c9d"
HISTORICAL_REFERENCE_DRIVER = {
    "path": "/scripts/reference_cuda.py",
    "bytes": 29060,
    "sha256": "eac9e1235b46425bbad706a188bc83a371f0783c3a0724534e17fdbf6649b6a1",
}
PROTOCOL_SHA256 = "4b2eb01016a55f9089d8f2281f07eadb7e0b31dd3e4735d6388981f5fbbf2810"
DIAGNOSTIC_KIND = "loaded_weight_rounding_diagnostic_v1"
EXECUTION_KIND = "loaded_weight_rounding_diagnostic_execution_v1"
DIAGNOSTIC_LABEL_VALUE = "loaded-weight-rounding-v1"
TASK_LABEL = f"io.hmem.task={TASK_ID}"
TEI_TASK_LABEL = f"hmem.task={TASK_ID}"
TOTAL_SECONDS = 1200
CLEANUP_RESERVE_SECONDS = 120
OUTPUT_CAP_BYTES = 8 * 1024 * 1024
MEMORY_BYTES = 21_474_836_480
MEMORY_SWAP_BYTES = 25_769_803_776
CPUS = 6
PIDS = 256
TMPFS_BYTES = 2_147_483_648
RESERVED_VRAM_MIB = 2048
RUN_ID_RE = re.compile(r"^[0-9a-f]{32}$")


def descriptor(path: Path) -> dict[str, Any]:
    path = path.resolve()
    if not path.is_file():
        raise ContractError(f"required diagnostic input is not a file: {path}")
    return {"path": str(path), "bytes": path.stat().st_size, "sha256": sha256_file(path)}


def _path(value: str | Path) -> str:
    return os.path.normcase(os.path.normpath(str(value)))


def validate_output_root(output_root: Path, artifact_root: Path) -> str:
    output_root = output_root.resolve()
    expected_parent = (artifact_root / "diagnostics" / "weight-rounding").resolve()
    run_id = output_root.name
    if output_root.parent != expected_parent or RUN_ID_RE.fullmatch(run_id) is None:
        raise ContractError("diagnostic output root must be the declared weight-rounding path plus one UUID hex")
    if output_root.exists():
        raise ContractError("diagnostic output root must be create-new")
    return run_id


def largest_rounding_temporary(source_root: Path, artifacts: list[dict[str, Any]]) -> dict[str, Any]:
    largest: dict[str, Any] | None = None
    for artifact in artifacts:
        relative = artifact.get("path")
        if not isinstance(relative, str) or not relative.endswith(".safetensors"):
            continue
        path = source_root / relative
        with path.open("rb") as handle:
            raw_length = handle.read(8)
            if len(raw_length) != 8:
                raise ContractError(f"invalid safetensors header length: {relative}")
            header_length = struct.unpack("<Q", raw_length)[0]
            if header_length <= 0 or header_length > 16 * 1024 * 1024:
                raise ContractError(f"unsafe safetensors header length: {relative}")
            header_raw = handle.read(header_length)
        try:
            header = json.loads(header_raw)
        except (UnicodeDecodeError, json.JSONDecodeError) as exc:
            raise ContractError(f"invalid safetensors header: {relative}") from exc
        if not isinstance(header, dict):
            raise ContractError(f"invalid safetensors tensor inventory: {relative}")
        for name, value in header.items():
            if name == "__metadata__":
                continue
            if (
                not isinstance(value, dict)
                or value.get("dtype") != "F32"
                or not isinstance(value.get("shape"), list)
                or not value["shape"]
                or any(not isinstance(dimension, int) or dimension <= 0 for dimension in value["shape"])
                or not isinstance(value.get("data_offsets"), list)
                or len(value["data_offsets"]) != 2
            ):
                raise ContractError(f"unexpected safetensors entry for {relative}:{name}")
            start, end = value["data_offsets"]
            if not isinstance(start, int) or not isinstance(end, int) or start < 0 or end <= start or (end - start) % 4:
                raise ContractError(f"invalid safetensors offsets for {relative}:{name}")
            elements = math.prod(value["shape"])
            if elements <= 0 or elements * 4 != end - start:
                raise ContractError(f"safetensors shape/offset mismatch for {relative}:{name}")
            candidate = {
                "artifact": relative,
                "tensor": name,
                "shape": value["shape"],
                "elements": elements,
                "source_fp32_bytes": elements * 4,
                "temporary_fp16_bytes": elements * 2,
                "temporary_promoted_fp32_bytes": elements * 4,
                "round_and_promote_bytes": elements * 6,
                "chunked_delta_bytes": min(elements, 4_194_304) * 4,
                "chunked_finite_check_mask_bytes": min(elements, 4_194_304),
                "estimated_peak_temporary_bytes": elements * 6 + min(elements, 4_194_304) * 5,
            }
            if largest is None or candidate["estimated_peak_temporary_bytes"] > largest["estimated_peak_temporary_bytes"]:
                largest = candidate
    if largest is None:
        raise ContractError("no FP32 safetensors were available for the rounding estimate")
    return largest


def validate_temporary_headroom(gpu: dict[str, Any], estimate: dict[str, Any]) -> int:
    available_after_reserve = max(0, gpu["memory_free_mib"] - RESERVED_VRAM_MIB) * 1024 * 1024
    if estimate["estimated_peak_temporary_bytes"] > available_after_reserve:
        raise ContractError("insufficient fresh VRAM headroom for largest rounding temporary estimate")
    return available_after_reserve


def _finite_unit_vector(value: Any, label: str) -> list[float]:
    if not isinstance(value, list) or len(value) != 1536:
        raise ContractError(f"{label} must contain exactly 1536 coordinates")
    vector = [float(item) for item in value]
    if any(isinstance(item, bool) or not math.isfinite(number) for item, number in zip(value, vector)):
        raise ContractError(f"{label} coordinates must be finite numbers")
    norm = math.sqrt(math.fsum(number * number for number in vector))
    if abs(norm - 1.0) > 1e-4:
        raise ContractError(f"{label} unit-norm drift: {norm}")
    return vector


def _select_expected_evidence(probes: Path, reference: Path, tei_report: Path) -> dict[str, Any]:
    probe_cases = reference_cuda.validate_probes(read_json(probes))
    selected = [case for case in probe_cases if case.get("id") == "mixed_long"]
    if len(selected) != 1:
        raise ContractError("diagnostic inputs require exactly one mixed_long probe")
    case = selected[0]
    token_ids = case.get("token_ids")
    if not isinstance(token_ids, list) or len(token_ids) != 2048 or token_ids[-1] != 151643:
        raise ContractError("diagnostic mixed_long token identity drift")

    reference_value = read_json(reference)
    reference_cases = reference_value.get("cases") if isinstance(reference_value, dict) else None
    if not isinstance(reference_cases, list):
        raise ContractError("diagnostic retained reference cases are missing")
    reference_matches = [item for item in reference_cases if isinstance(item, dict) and item.get("id") == "mixed_long"]
    if len(reference_matches) != 1 or reference_matches[0].get("token_ids") != token_ids:
        raise ContractError("diagnostic retained reference mixed_long identity drift")
    reference_runtime = reference_value.get("runtime")
    if (
        not isinstance(reference_runtime, dict)
        or reference_runtime.get("script_sha256") != HISTORICAL_REFERENCE_DRIVER["sha256"]
    ):
        raise ContractError("diagnostic retained reference driver identity drift")

    tei_value = read_json(tei_report)
    candidate_cases = tei_value.get("candidate_cases") if isinstance(tei_value, dict) else None
    if not isinstance(candidate_cases, list):
        raise ContractError("diagnostic retained TEI candidate cases are missing")
    tei_matches = [item for item in candidate_cases if isinstance(item, dict) and item.get("id") == "mixed_long"]
    if len(tei_matches) != 1 or tei_matches[0].get("token_ids") != token_ids:
        raise ContractError("diagnostic retained TEI mixed_long identity drift")
    return {
        "case": {
            "id": "mixed_long",
            "role": case.get("role"),
            "utf8_sha256": case.get("utf8_sha256"),
            "token_ids": list(token_ids),
        },
        "reference_vector": _finite_unit_vector(reference_matches[0].get("vector"), "retained reference vector"),
        "tei_vector": _finite_unit_vector(tei_matches[0].get("vector"), "retained TEI vector"),
        "reference_driver": dict(HISTORICAL_REFERENCE_DRIVER),
    }


def validate_protocol_and_inputs(
    contract: dict[str, Any],
    probes: Path,
    reference: Path,
    tei_report: Path,
    protocol_path: Path,
    diagnostic_script: Path,
) -> tuple[dict[str, Any], dict[str, dict[str, Any]], dict[str, Any], dict[str, Any]]:
    protocol_descriptor = descriptor(protocol_path)
    if protocol_descriptor["sha256"] != PROTOCOL_SHA256:
        raise ContractError("diagnostic protocol checksum drift")
    protocol = read_json(protocol_path)
    identity = protocol.get("identity") if isinstance(protocol, dict) else None
    if (
        protocol.get("schema_version") != 1
        or protocol.get("kind") != "gpu_loaded_weight_rounding_diagnostic_protocol"
        or not isinstance(identity, dict)
        or identity.get("task_id") != TASK_ID
        or identity.get("base_sha") != BASE_SHA
        or protocol.get("runtime", {}).get("image_id") != IMAGE_ID
    ):
        raise ContractError("diagnostic protocol identity drift")
    if contract.get("task_id") != TASK_ID:
        raise ContractError("diagnostic contract task drift")
    evidence = protocol.get("evidence")
    if not isinstance(evidence, dict):
        raise ContractError("diagnostic evidence descriptors are missing")
    supplied = {
        "probe": descriptor(probes),
        "reference": descriptor(reference),
        "reference_execution": descriptor(reference.with_name(reference.name + ".execution.json")),
        "failed_tei": descriptor(tei_report),
    }
    for key, actual in supplied.items():
        expected = evidence.get(key)
        if (
            not isinstance(expected, dict)
            or _path(actual["path"]) != _path(expected.get("path", ""))
            or actual["bytes"] != expected.get("bytes")
            or actual["sha256"] != expected.get("sha256")
        ):
            raise ContractError(f"diagnostic {key} descriptor drift")
    script = descriptor(diagnostic_script)
    expected_script = Path(__file__).with_name("diagnose_weight_rounding.py").resolve()
    if diagnostic_script.resolve() != expected_script:
        raise ContractError("diagnostic script must be the reviewed repository file")
    source_root = Path(contract["model"]["source_root"]).resolve()
    validate_locked_files(source_root, contract["model"]["artifacts"], exact=True)
    estimate = largest_rounding_temporary(source_root, contract["model"]["artifacts"])
    expected_evidence = _select_expected_evidence(probes, reference, tei_report)
    return protocol_descriptor, supplied | {"diagnostic_script": script}, estimate, expected_evidence


def build_create_command(
    contract: dict[str, Any],
    probes: Path,
    reference: Path,
    tei_report: Path,
    protocol: Path,
    diagnostic_script: Path,
    output_root: Path,
    run_id: str,
) -> tuple[str, list[str]]:
    name = f"hmem-aa30-weight-rounding-{run_id[:12]}"
    command = [
        "docker", "create", "--name", name,
        "--label", TASK_LABEL,
        "--label", f"io.hmem.run={run_id}",
        "--label", f"io.hmem.diagnostic={DIAGNOSTIC_LABEL_VALUE}",
        "--gpus", "device=0",
        "--network", "none",
        "--memory", str(MEMORY_BYTES),
        "--memory-swap", str(MEMORY_SWAP_BYTES),
        "--cpus", str(CPUS),
        "--pids-limit", str(PIDS),
        "--user", "65532:65532",
        "--read-only",
        "--tmpfs", f"/tmp:rw,nosuid,nodev,size={TMPFS_BYTES}",
        "--cap-drop", "ALL",
        "--security-opt", "no-new-privileges",
        "--log-driver", "local",
        "--log-opt", "max-size=8m",
        "--log-opt", "max-file=1",
        "--log-opt", "compress=false",
        "--env", "HF_HUB_OFFLINE=1",
        "--env", "TRANSFORMERS_OFFLINE=1",
        "--env", "HF_DATASETS_OFFLINE=1",
        "--env", "HF_HOME=/tmp/hf",
        "--env", "HF_MODULES_CACHE=/tmp/hf/modules",
        "--env", "TOKENIZERS_PARALLELISM=false",
        "--env", "NVIDIA_DRIVER_CAPABILITIES=compute,utility",
        "--env", f"HMEM_DIAGNOSTIC_RUN_ID={run_id}",
        "--entrypoint", "/usr/local/bin/python3.11",
        "--mount", f"type=bind,src={contract['model']['source_root']},dst=/model,readonly",
        "--mount", f"type=bind,src={probes},dst=/inputs/probes.json,readonly",
        "--mount", f"type=bind,src={reference},dst=/inputs/reference-v3.json,readonly",
        "--mount", f"type=bind,src={tei_report},dst=/inputs/failed-tei.json,readonly",
        "--mount", f"type=bind,src={protocol},dst=/inputs/diagnostic-protocol.json,readonly",
        "--mount", f"type=bind,src={diagnostic_script},dst=/diagnostic/diagnose_weight_rounding.py,readonly",
        "--mount", f"type=bind,src={output_root},dst=/output",
        IMAGE_ID,
        "-I", "-B", "/diagnostic/diagnose_weight_rounding.py",
        "--source", "/model",
        "--probes", "/inputs/probes.json",
        "--reference", "/inputs/reference-v3.json",
        "--tei-report", "/inputs/failed-tei.json",
        "--protocol", "/inputs/diagnostic-protocol.json",
        "--output", "/output/diagnostic.json",
    ]
    return name, command


def parse_container_id(value: str) -> str:
    candidate = value.strip()
    if len(candidate) != 64 or any(character not in "0123456789abcdef" for character in candidate):
        raise ContractError("Docker did not return one full lowercase diagnostic container ID")
    return candidate


def authenticate_ownership(value: Any, run_id: str, name: str) -> dict[str, Any]:
    if not isinstance(value, list) or len(value) != 1 or not isinstance(value[0], dict):
        raise ContractError("Docker inspect did not return exactly one diagnostic container")
    item = value[0]
    config = item.get("Config", {})
    labels = config.get("Labels") or {}
    if item.get("Name") != "/" + name or config.get("Image") != IMAGE_ID:
        raise ContractError("diagnostic container name/image drift")
    if (
        labels.get("io.hmem.task") != TASK_ID
        or labels.get("io.hmem.run") != run_id
        or labels.get("io.hmem.diagnostic") != DIAGNOSTIC_LABEL_VALUE
    ):
        raise ContractError("diagnostic container ownership labels drift")
    return item


def _env_map(values: Any) -> dict[str, str]:
    if not isinstance(values, list):
        raise ContractError("diagnostic container environment is missing")
    result: dict[str, str] = {}
    for item in values:
        if not isinstance(item, str) or "=" not in item:
            raise ContractError("diagnostic container environment entry is invalid")
        key, value = item.split("=", 1)
        if key in result:
            raise ContractError(f"duplicate diagnostic environment variable: {key}")
        result[key] = value
    return result


def authenticate_container(
    value: Any,
    contract: dict[str, Any],
    probes: Path,
    reference: Path,
    tei_report: Path,
    protocol: Path,
    diagnostic_script: Path,
    output_root: Path,
    run_id: str,
    name: str,
) -> dict[str, Any]:
    item = authenticate_ownership(value, run_id, name)
    config = item.get("Config", {})
    host = item.get("HostConfig", {})
    expected_host = {
        "NetworkMode": "none",
        "Memory": MEMORY_BYTES,
        "MemorySwap": MEMORY_SWAP_BYTES,
        "NanoCpus": CPUS * 1_000_000_000,
        "PidsLimit": PIDS,
        "ReadonlyRootfs": True,
        "LogConfig": {"Type": "local", "Config": {"compress": "false", "max-file": "1", "max-size": "8m"}},
    }
    if any(host.get(key) != expected for key, expected in expected_host.items()):
        raise ContractError("diagnostic container host contract drift")
    if config.get("User") != "65532:65532" or config.get("Entrypoint") != ["/usr/local/bin/python3.11"]:
        raise ContractError("diagnostic container user/entrypoint drift")
    expected_cmd = [
        "-I", "-B", "/diagnostic/diagnose_weight_rounding.py",
        "--source", "/model", "--probes", "/inputs/probes.json",
        "--reference", "/inputs/reference-v3.json", "--tei-report", "/inputs/failed-tei.json",
        "--protocol", "/inputs/diagnostic-protocol.json", "--output", "/output/diagnostic.json",
    ]
    if config.get("Cmd") != expected_cmd:
        raise ContractError("diagnostic container command drift")
    if host.get("CapDrop") != ["ALL"] or "no-new-privileges" not in (host.get("SecurityOpt") or []):
        raise ContractError("diagnostic container security contract drift")
    tmpfs = host.get("Tmpfs") or {}
    if set(tmpfs) != {"/tmp"} or set(str(tmpfs["/tmp"]).split(",")) != {"rw", "nosuid", "nodev", f"size={TMPFS_BYTES}"}:
        raise ContractError("diagnostic tmpfs contract drift")
    requests = host.get("DeviceRequests") or []
    if len(requests) != 1 or requests[0].get("DeviceIDs") != ["0"] or ["gpu"] not in (requests[0].get("Capabilities") or []):
        raise ContractError("diagnostic GPU selection drift")
    env = _env_map(config.get("Env"))
    expected_env = {
        "HF_HUB_OFFLINE": "1",
        "TRANSFORMERS_OFFLINE": "1",
        "HF_DATASETS_OFFLINE": "1",
        "HF_HOME": "/tmp/hf",
        "HF_MODULES_CACHE": "/tmp/hf/modules",
        "TOKENIZERS_PARALLELISM": "false",
        "NVIDIA_DRIVER_CAPABILITIES": "compute,utility",
        "HMEM_DIAGNOSTIC_RUN_ID": run_id,
    }
    if any(env.get(key) != expected for key, expected in expected_env.items()):
        raise ContractError("diagnostic container environment drift")
    mounts = {mount.get("Destination"): mount for mount in item.get("Mounts", [])}
    expected_mounts = {
        "/model": (_path(contract["model"]["source_root"]), False),
        "/inputs/probes.json": (_path(probes), False),
        "/inputs/reference-v3.json": (_path(reference), False),
        "/inputs/failed-tei.json": (_path(tei_report), False),
        "/inputs/diagnostic-protocol.json": (_path(protocol), False),
        "/diagnostic/diagnose_weight_rounding.py": (_path(diagnostic_script), False),
        "/output": (_path(output_root), True),
    }
    if set(mounts) != set(expected_mounts):
        raise ContractError("diagnostic container mount set drift")
    for destination, (source, writable) in expected_mounts.items():
        mount = mounts[destination]
        if mount.get("Type") != "bind" or _path(mount.get("Source", "")) != source or mount.get("RW") is not writable:
            raise ContractError(f"diagnostic container mount drift for {destination}")
    return item


def recover_owned_container(name: str, run_id: str, *, runner=run) -> str | None:
    raw, _ = runner(
        [
            "docker", "ps", "-aq", "--no-trunc",
            "--filter", f"name=^/{name}$",
            "--filter", f"label={TASK_LABEL}",
            "--filter", f"label=io.hmem.run={run_id}",
            "--filter", f"label=io.hmem.diagnostic={DIAGNOSTIC_LABEL_VALUE}",
        ],
        15,
        65536,
    )
    if not raw.strip():
        return None
    container_id = parse_container_id(raw)
    inspected, _ = runner(["docker", "inspect", container_id], 15, 1048576)
    authenticate_ownership(json.loads(inspected), run_id, name)
    return container_id


def cleanup_failures(cleanup: dict[str, Any]) -> list[str]:
    failures = [
        key
        for key in ("recovery", "logs", "log_write", "authentication", "stop", "remove", "absence_check")
        if key in cleanup and (cleanup[key] is None or str(cleanup[key]).startswith("failed:"))
    ]
    if cleanup.get("absent") is not True:
        failures.append("absence")
    if cleanup.get("within_deadline") is False:
        failures.append("cleanup_deadline")
    if cleanup.get("vram_monitor_stopped") is False or cleanup.get("vram_monitor_failure"):
        failures.append("vram_monitor")
    return failures


def _require_output_descriptor(value: Any, path: str, expected: dict[str, Any], label: str) -> None:
    if (
        not isinstance(value, dict)
        or value.get("path") != path
        or value.get("bytes") != expected["bytes"]
        or value.get("sha256") != expected["sha256"]
    ):
        raise ContractError(f"diagnostic output {label} descriptor drift")


def _vector_metrics(left: list[float], right: list[float]) -> dict[str, float]:
    left_norm = math.sqrt(math.fsum(value * value for value in left))
    right_norm = math.sqrt(math.fsum(value * value for value in right))
    if left_norm <= 0.0 or right_norm <= 0.0:
        raise ContractError("diagnostic metric vector has zero norm")
    deltas = [a - b for a, b in zip(left, right)]
    return {
        "cosine": math.fsum(a * b for a, b in zip(left, right)) / (left_norm * right_norm),
        "l2_distance": math.sqrt(math.fsum(delta * delta for delta in deltas)),
        "maximum_coordinate_absolute_error": max(abs(delta) for delta in deltas),
        "left_norm": left_norm,
        "right_norm": right_norm,
    }


def _require_finite_number(value: Any, label: str) -> float:
    if isinstance(value, bool):
        raise ContractError(f"{label} must be a finite number")
    try:
        result = float(value)
    except (TypeError, ValueError) as exc:
        raise ContractError(f"{label} must be a finite number") from exc
    if not math.isfinite(result):
        raise ContractError(f"{label} must be a finite number")
    return result


def _require_metric(value: Any, expected: dict[str, float], label: str) -> None:
    if not isinstance(value, dict) or set(value) != set(expected):
        raise ContractError(f"diagnostic {label} metric fields drift")
    for key, expected_number in expected.items():
        observed = _require_finite_number(value.get(key), f"{label}.{key}")
        if not math.isclose(observed, expected_number, rel_tol=1e-12, abs_tol=1e-12):
            raise ContractError(f"diagnostic {label} metric drift: {key}")


def _validate_forward(value: Any, expected_tokens: list[int], label: str) -> list[float]:
    if not isinstance(value, dict) or value.get("token_ids") != expected_tokens:
        raise ContractError(f"diagnostic {label} token identity drift")
    vector = _finite_unit_vector(value.get("vector"), f"diagnostic {label} vector")
    reported_norm = _require_finite_number(value.get("norm"), f"{label}.norm")
    actual_norm = math.sqrt(math.fsum(number * number for number in vector))
    if not math.isclose(reported_norm, actual_norm, rel_tol=1e-12, abs_tol=1e-12):
        raise ContractError(f"diagnostic {label} reported norm drift")
    if _require_finite_number(value.get("pooled_norm"), f"{label}.pooled_norm") <= 0.0:
        raise ContractError(f"diagnostic {label} pooled norm must be positive")
    if _require_finite_number(value.get("elapsed_seconds"), f"{label}.elapsed_seconds") < 0.0:
        raise ContractError(f"diagnostic {label} elapsed time must be nonnegative")
    return vector


def _validate_rounding(value: Any) -> None:
    if not isinstance(value, dict):
        raise ContractError("completed diagnostic parameter rounding evidence is missing")
    representation = value.get("representation")
    expected_representation = {
        "baseline": "original CUDA FP32 checkpoint values",
        "rounded": "each unique learned CUDA parameter rounded FP32->F16->FP32; inference remains CUDA FP32",
        "buffers_modified": False,
    }
    if representation != expected_representation or value.get("buffers_unchanged") is not True:
        raise ContractError("completed diagnostic rounding representation drift")
    parameters = value.get("parameters")
    summary = value.get("summary")
    if not isinstance(parameters, list) or not parameters or not isinstance(summary, dict):
        raise ContractError("completed diagnostic parameter inventory is empty")
    names: list[str] = []
    total_elements = 0
    total_changed = 0
    maximum_delta = 0.0
    largest_name = None
    largest_elements = -1
    for item in parameters:
        if not isinstance(item, dict) or not isinstance(item.get("name"), str) or not item["name"]:
            raise ContractError("completed diagnostic parameter entry is invalid")
        name = item["name"]
        names.append(name)
        elements = item.get("numel")
        changed = item.get("changed_count")
        if (
            not isinstance(elements, int) or isinstance(elements, bool) or elements <= 0
            or not isinstance(changed, int) or isinstance(changed, bool) or changed < 0 or changed > elements
            or item.get("dtype") != "torch.float32" or item.get("device") != "cuda:0"
            or item.get("f16_temporary_finite") is not True
            or item.get("promoted_fp32_finite") is not True
            or not isinstance(item.get("version_before"), int)
            or not isinstance(item.get("version_after"), int)
            or item["version_after"] <= item["version_before"]
        ):
            raise ContractError(f"completed diagnostic parameter evidence drift: {name}")
        delta = _require_finite_number(item.get("maximum_absolute_delta"), f"rounding.{name}.maximum_absolute_delta")
        if delta < 0.0:
            raise ContractError(f"completed diagnostic negative rounding delta: {name}")
        total_elements += elements
        total_changed += changed
        maximum_delta = max(maximum_delta, delta)
        if elements > largest_elements:
            largest_name, largest_elements = name, elements
    if names != sorted(names) or len(names) != len(set(names)) or total_changed <= 0:
        raise ContractError("completed diagnostic parameter ordering or change evidence drift")
    expected_summary = {
        "parameter_count": len(parameters),
        "total_elements": total_elements,
        "total_changed_count": total_changed,
        "maximum_absolute_delta": maximum_delta,
        "largest_parameter_name": largest_name,
        "largest_parameter_elements": largest_elements,
        "largest_parameter_round_and_promote_bytes": largest_elements * 6,
        "largest_parameter_finite_check_mask_bytes": min(largest_elements, 4_194_304),
        "largest_parameter_delta_and_finite_check_bytes": min(largest_elements, 4_194_304) * 5,
        "largest_parameter_estimated_peak_temporary_bytes": largest_elements * 6 + min(largest_elements, 4_194_304) * 5,
        "delta_chunk_elements": 4_194_304,
        "finite_check_chunk_elements": 4_194_304,
        "finite_check_mask_element_bytes": 1,
        "delta_element_bytes": 4,
    }
    for key, expected in expected_summary.items():
        observed = summary.get(key)
        if isinstance(expected, float):
            if not math.isclose(_require_finite_number(observed, f"rounding.summary.{key}"), expected, rel_tol=1e-12, abs_tol=1e-12):
                raise ContractError(f"completed diagnostic rounding summary drift: {key}")
        elif observed != expected:
            raise ContractError(f"completed diagnostic rounding summary drift: {key}")
    if value.get("buffers_before") != value.get("buffers_after"):
        raise ContractError("completed diagnostic buffer metadata changed")


def validate_diagnostic_output(
    value: Any,
    run_id: str,
    protocol_descriptor: dict[str, Any],
    inputs: dict[str, dict[str, Any]],
    expected_evidence: dict[str, Any],
    expected_gpu: dict[str, Any] | None = None,
) -> str:
    if (
        not isinstance(value, dict)
        or value.get("schema_version") != 1
        or value.get("kind") != DIAGNOSTIC_KIND
        or value.get("task_id") != TASK_ID
        or value.get("base_sha") != BASE_SHA
        or value.get("run_id") != run_id
        or value.get("state") not in {"completed_diagnostic", "inconclusive_baseline_drift", "failed"}
    ):
        raise ContractError("diagnostic output identity drift")
    observed_protocol = value.get("protocol_artifact")
    _require_output_descriptor(
        observed_protocol, "/inputs/diagnostic-protocol.json", protocol_descriptor, "protocol"
    )
    observed_inputs = value.get("input_artifacts")
    if not isinstance(observed_inputs, dict):
        raise ContractError("diagnostic output input descriptors are missing")
    _require_output_descriptor(observed_inputs.get("probes"), "/inputs/probes.json", inputs["probe"], "probes")
    _require_output_descriptor(
        observed_inputs.get("retained_reference"),
        "/inputs/reference-v3.json",
        inputs["reference"],
        "retained reference",
    )
    _require_output_descriptor(
        observed_inputs.get("failed_tei_report"),
        "/inputs/failed-tei.json",
        inputs["failed_tei"],
        "failed TEI",
    )
    runtime = value.get("runtime")
    expected_runtime = {
        "python": "3.11.9",
        "torch": "2.7.1+cu128",
        "torch_cuda": "12.8",
        "transformers": "4.41.2",
        "reference_method": "original-eager-cuda-fp32-v2",
        "parameter_devices": ["cuda:0"],
        "parameter_dtypes": ["torch.float32"],
        "activation_devices": ["cuda:0"],
        "activation_dtypes": ["torch.float32"],
        "smoke_dtype": "torch.float32",
        "float32_matmul_precision": "highest",
        "cuda_matmul_allow_tf32": False,
        "cudnn_allow_tf32": False,
        "autocast_enabled": False,
        "torch_compile": False,
        "attention_implementation": "eager",
        "is_causal": False,
        "use_cache": False,
        "model_loads": 1,
    }
    if (
        not isinstance(runtime, dict)
        or any(runtime.get(key) != expected for key, expected in expected_runtime.items())
        or runtime.get("packages") != reference_cuda.PACKAGE_VERSIONS
        or "sm_120" not in runtime.get("torch_supported_architectures", [])
    ):
        raise ContractError("diagnostic output runtime drift")
    driver = runtime.get("reference_driver")
    driver_expected = expected_evidence.get("reference_driver")
    if not isinstance(driver_expected, dict):
        raise ContractError("diagnostic expected reference driver identity is missing")
    _require_output_descriptor(driver, "/scripts/reference_cuda.py", driver_expected, "reference driver")
    model_class = runtime.get("model_class")
    if (
        not isinstance(model_class, dict)
        or model_class.get("class") != "Qwen2Model"
        or model_class.get("module") != reference_cuda.ORIGINAL_MODEL_MODULE_NAME
        or model_class.get("path") != "/model/modeling_qwen.py"
        or model_class.get("sha256") != reference_cuda.MODELING_QWEN_SHA256
    ):
        raise ContractError("diagnostic output model class drift")
    device = runtime.get("device")
    if (
        not isinstance(device, dict)
        or device.get("index") != 0
        or device.get("compute_capability") != "12.0"
        or not device.get("uuid")
    ):
        raise ContractError("diagnostic output device drift")
    if expected_gpu is not None and (
        device.get("index") != expected_gpu.get("index")
        or device.get("uuid") != expected_gpu.get("uuid")
        or device.get("compute_capability") != expected_gpu.get("compute_capability")
    ):
        raise ContractError("diagnostic output host/container GPU identity drift")
    state = value["state"]
    expected_forwards = 1 if state == "inconclusive_baseline_drift" else 2 if state == "completed_diagnostic" else None
    if expected_forwards is not None and runtime.get("forwards_completed") != expected_forwards:
        raise ContractError("diagnostic output forward count drift")
    if state not in {"completed_diagnostic", "inconclusive_baseline_drift"}:
        return state
    expected_case = expected_evidence.get("case")
    if value.get("case") != expected_case or not isinstance(expected_case, dict):
        raise ContractError("diagnostic mixed_long case evidence drift")
    expected_tokens = expected_case.get("token_ids")
    if not isinstance(expected_tokens, list):
        raise ContractError("diagnostic expected token evidence is missing")
    reference_vector = expected_evidence.get("reference_vector")
    tei_vector = expected_evidence.get("tei_vector")
    if not isinstance(reference_vector, list) or not isinstance(tei_vector, list):
        raise ContractError("diagnostic expected vectors are missing")
    baseline_value = value.get("baseline")
    baseline = _validate_forward(baseline_value, expected_tokens, "baseline")
    if not isinstance(baseline_value, dict):
        raise ContractError("diagnostic baseline evidence is missing")
    _require_metric(
        baseline_value.get("metrics_to_retained_reference"),
        _vector_metrics(reference_vector, baseline),
        "baseline_vs_retained_reference",
    )
    exact_baseline = baseline == reference_vector
    if baseline_value.get("exact_match_retained_reference") is not exact_baseline:
        raise ContractError("diagnostic baseline exact-match guard drift")
    if state == "inconclusive_baseline_drift":
        if exact_baseline or any(value.get(key) is not None for key in ("parameter_rounding", "rounded", "comparisons")):
            raise ContractError("inconclusive diagnostic stage evidence drift")
        return state

    if not exact_baseline:
        raise ContractError("completed diagnostic baseline differs from retained reference")
    rounded = _validate_forward(value.get("rounded"), expected_tokens, "rounded")
    _validate_rounding(value.get("parameter_rounding"))
    comparisons = value.get("comparisons")
    required_comparisons = {
        "baseline_vs_rounded", "rounded_vs_retained_reference", "rounded_vs_retained_tei",
        "baseline_vs_retained_tei", "signed_coordinate_deltas", "coordinate_940",
    }
    if not isinstance(comparisons, dict) or set(comparisons) != required_comparisons:
        raise ContractError("completed diagnostic comparison evidence is incomplete")
    for key, left, right in (
        ("baseline_vs_rounded", baseline, rounded),
        ("rounded_vs_retained_reference", reference_vector, rounded),
        ("rounded_vs_retained_tei", tei_vector, rounded),
        ("baseline_vs_retained_tei", tei_vector, baseline),
    ):
        _require_metric(comparisons.get(key), _vector_metrics(left, right), key)
    signed = comparisons.get("signed_coordinate_deltas")
    expected_signed = [after - before for before, after in zip(baseline, rounded)]
    if not isinstance(signed, dict) or signed.get("meaning") != "rounded_minus_baseline" or signed.get("values") != expected_signed:
        raise ContractError("completed diagnostic signed coordinate evidence drift")
    index = 940
    before_error = abs(baseline[index] - tei_vector[index])
    after_error = abs(rounded[index] - tei_vector[index])
    expected_coordinate = {
        "zero_based_index": index,
        "baseline": baseline[index],
        "rounded": rounded[index],
        "retained_reference": reference_vector[index],
        "retained_tei": tei_vector[index],
        "rounded_minus_baseline": expected_signed[index],
        "rounded_minus_retained_reference": rounded[index] - reference_vector[index],
        "rounded_minus_retained_tei": rounded[index] - tei_vector[index],
        "baseline_minus_retained_tei": baseline[index] - tei_vector[index],
        "absolute_tei_error_before": before_error,
        "absolute_tei_error_after": after_error,
        "absolute_tei_error_reduction": before_error - after_error,
    }
    if comparisons.get("coordinate_940") != expected_coordinate:
        raise ContractError("completed diagnostic coordinate 940 evidence drift")
    return state


def main() -> int:
    parser = argparse.ArgumentParser(description="Run one bounded loaded-weight rounding diagnostic.")
    parser.add_argument("--contract", type=Path, required=True)
    parser.add_argument("--probes", type=Path, required=True)
    parser.add_argument("--reference", type=Path, required=True)
    parser.add_argument("--tei-report", type=Path, required=True)
    parser.add_argument("--protocol", type=Path, required=True)
    parser.add_argument("--diagnostic-script", type=Path, required=True)
    parser.add_argument("--output-root", type=Path, required=True)
    args = parser.parse_args()

    started = time.monotonic()
    execution_deadline = started + TOTAL_SECONDS - CLEANUP_RESERVE_SECONDS
    cleanup_deadline = started + TOTAL_SECONDS
    execution_run = make_deadline_runner(execution_deadline, "diagnostic execution")
    cleanup_run = make_deadline_runner(cleanup_deadline, "diagnostic cleanup")
    contract_path = args.contract.resolve()
    contract = load_contract(contract_path)
    probes = args.probes.resolve()
    reference = args.reference.resolve()
    tei_report = args.tei_report.resolve()
    protocol_path = args.protocol.resolve()
    diagnostic_script = args.diagnostic_script.resolve()
    output_root = args.output_root.resolve()
    run_id = validate_output_root(output_root, Path(contract["artifact_root"]).resolve())
    protocol_descriptor, inputs, temporary_estimate, expected_evidence = validate_protocol_and_inputs(
        contract, probes, reference, tei_report, protocol_path, diagnostic_script
    )
    output_root.mkdir(parents=True, exist_ok=False)
    output = output_root / "diagnostic.json"
    sidecar = output_root / "diagnostic.json.execution.json"
    report: dict[str, Any] = {
        "schema_version": 1,
        "kind": EXECUTION_KIND,
        "state": "started",
        "task_id": TASK_ID,
        "base_sha": BASE_SHA,
        "run_id": run_id,
        "controller": {
            "pid": os.getpid(),
            "started_utc": datetime.now(timezone.utc).isoformat(),
            "monotonic_started": started,
        },
        "limits": {
            "total_seconds": TOTAL_SECONDS,
            "cleanup_reserve_seconds": CLEANUP_RESERVE_SECONDS,
            "output_cap_bytes": OUTPUT_CAP_BYTES,
            "memory_bytes": MEMORY_BYTES,
            "memory_swap_bytes": MEMORY_SWAP_BYTES,
            "cpus": CPUS,
            "pids": PIDS,
            "tmpfs_bytes": TMPFS_BYTES,
            "reserved_vram_mib": RESERVED_VRAM_MIB,
        },
        "image_id": IMAGE_ID,
        "protocol_artifact": protocol_descriptor,
        "input_artifacts": inputs,
        "helpers": {
            name: descriptor(Path(__file__).with_name(name))
            for name in ("gpu_contract.py", "prepare_image.py", "run_reference_validation.py", "run_tei_startup.py")
        },
        "controller_source": descriptor(Path(__file__)),
        "largest_parameter_temporary_estimate": temporary_estimate,
    }
    print(json.dumps({"state": "launched", "run_id": run_id, "controller": report["controller"]}, sort_keys=True), flush=True)
    name, command = build_create_command(
        contract, probes, reference, tei_report, protocol_path, diagnostic_script, output_root, run_id
    )
    report["container_name"] = name
    report["create_argv"] = command
    container_id: str | None = None
    monitor: VramMonitor | None = None
    create_attempted = False
    first_error: BaseException | None = None
    query = [
        "nvidia-smi",
        "--query-gpu=index,uuid,name,driver_version,memory.total,memory.free,memory.used,compute_cap",
        "--format=csv,noheader,nounits",
    ]
    try:
        for label in (TASK_LABEL, TEI_TASK_LABEL):
            existing, _ = execution_run(["docker", "ps", "-aq", "--no-trunc", "--filter", f"label={label}"], 15, 65536)
            if existing.strip():
                raise ContractError("another task-owned reference, diagnostic, or TEI container exists")
        gpu_raw, _ = execution_run(query, 15, 65536)
        gpu = parse_gpu_csv(gpu_raw, "12.0", contract["runtime_contract"]["minimum_free_vram_mib"])
        report["host_gpu_before"] = gpu
        report["available_vram_after_reserve_bytes"] = validate_temporary_headroom(gpu, temporary_estimate)
        create_attempted = True
        try:
            created, _ = execution_run(command, 60, 65536)
            candidate = parse_container_id(created)
        except BaseException:
            candidate = recover_owned_container(name, run_id, runner=execution_run)
            if candidate is None:
                raise
        inspected_raw, _ = execution_run(["docker", "inspect", candidate], 15, 2 * 1024 * 1024)
        inspected_value = json.loads(inspected_raw)
        authenticate_ownership(inspected_value, run_id, name)
        container_id = candidate
        authenticate_container(
            inspected_value, contract, probes, reference, tei_report, protocol_path,
            diagnostic_script, output_root, run_id, name
        )
        report["container_id"] = container_id
        report["container_mounts"] = [
            {key: mount.get(key) for key in ("Type", "Source", "Destination", "RW", "Propagation")}
            for mount in inspected_value[0].get("Mounts", [])
        ]
        monitor_runtime = {"required_compute_capability": "12.0", "reserved_vram_mib": RESERVED_VRAM_MIB}
        monitor = VramMonitor(query, monitor_runtime, container_id, max_samples=TOTAL_SECONDS + 1)
        monitor.start()
        if not monitor.wait_first_sample(10.0):
            raise ContractError("diagnostic VRAM monitor did not produce an initial sample")
        monitor.check()
        remaining = execution_deadline - time.monotonic()
        if remaining < 3:
            raise ContractError("diagnostic execution deadline exhausted before start")
        stdout, stderr = execution_run(["docker", "start", "--attach", container_id], remaining, OUTPUT_CAP_BYTES)
        report["attach_output"] = {"stdout": stdout, "stderr": stderr}
        monitor.check()
        if not output.is_file():
            raise ContractError("diagnostic process did not create diagnostic.json")
        result = read_json(output)
        diagnostic_state = validate_diagnostic_output(
            result, run_id, protocol_descriptor, inputs, expected_evidence, report["host_gpu_before"]
        )
        report["diagnostic_output"] = descriptor(output)
        report["diagnostic_state"] = diagnostic_state
        if diagnostic_state != "completed_diagnostic":
            raise ContractError(f"diagnostic did not complete: {diagnostic_state}")
        report["state"] = "diagnostic_completed"
    except BaseException as exc:
        first_error = exc
        report["state"] = "failed"
        report["error"] = {"type": type(exc).__name__, "message": str(exc)}
    finally:
        cleanup_started = time.monotonic()
        cleanup: dict[str, Any] = {"container_id": container_id, "logs": None, "stop": None, "remove": None, "absent": None}
        try:
            if output.is_file() and "diagnostic_output" not in report:
                report["diagnostic_output"] = descriptor(output)
                value = read_json(output)
                report["diagnostic_state"] = validate_diagnostic_output(
                    value, run_id, protocol_descriptor, inputs, expected_evidence, report.get("host_gpu_before")
                )
        except BaseException as exc:
            report["diagnostic_output_error"] = f"{type(exc).__name__}: {exc}"
        if monitor is not None:
            cleanup["vram_monitor_stopped"] = monitor.stop(min(10.0, max(0.0, cleanup_deadline - time.monotonic())))
            cleanup["vram_monitor_failure"] = monitor.failure
            report["vram_samples"] = [report.get("host_gpu_before"), *monitor.samples]
        if container_id is None and create_attempted:
            try:
                container_id = recover_owned_container(name, run_id, runner=cleanup_run)
                cleanup["container_id"] = container_id
                cleanup["recovery"] = "owned" if container_id is not None else "absent"
                if container_id is None:
                    cleanup["absent"] = True
            except BaseException as exc:
                cleanup["recovery"] = f"failed: {type(exc).__name__}: {exc}"
        if container_id is not None:
            try:
                current_raw, _ = cleanup_run(["docker", "inspect", container_id], 10, 2 * 1024 * 1024)
                authenticate_ownership(json.loads(current_raw), run_id, name)
                cleanup["authentication"] = "ok"
            except BaseException as exc:
                cleanup["authentication"] = f"failed: {type(exc).__name__}: {exc}"
            if cleanup.get("authentication") == "ok":
                log_bytes: bytes | None = None
                try:
                    logs, log_err = cleanup_run(["docker", "logs", container_id], 10, OUTPUT_CAP_BYTES)
                    log_bytes = (logs + log_err).encode("utf-8")
                    cleanup["logs"] = "ok"
                except BaseException as exc:
                    cleanup["logs"] = f"failed: {type(exc).__name__}: {exc}"
                if log_bytes is not None:
                    try:
                        log_path = output_root / f"diagnostic-{run_id}.log"
                        with log_path.open("xb") as handle:
                            handle.write(log_bytes)
                            handle.flush()
                            os.fsync(handle.fileno())
                        cleanup["log_write"] = "ok"
                        cleanup["log"] = descriptor(log_path)
                    except BaseException as exc:
                        cleanup["log_write"] = f"failed: {type(exc).__name__}: {exc}"
                try:
                    cleanup_run(["docker", "stop", "--time", "10", container_id], 15, 65536)
                    cleanup["stop"] = "ok"
                except BaseException as exc:
                    cleanup["stop"] = f"failed: {type(exc).__name__}: {exc}"
                try:
                    cleanup_run(["docker", "rm", "--force", container_id], 10, 65536)
                    cleanup["remove"] = "ok"
                except BaseException as exc:
                    cleanup["remove"] = f"failed: {type(exc).__name__}: {exc}"
            try:
                by_id, _ = cleanup_run(["docker", "ps", "-aq", "--no-trunc", "--filter", f"id={container_id}"], 5, 65536)
                by_labels, _ = cleanup_run(
                    [
                        "docker", "ps", "-aq", "--no-trunc",
                        "--filter", f"label={TASK_LABEL}",
                        "--filter", f"label=io.hmem.run={run_id}",
                        "--filter", f"label=io.hmem.diagnostic={DIAGNOSTIC_LABEL_VALUE}",
                    ],
                    5,
                    65536,
                )
                cleanup["absent"] = not by_id.strip() and not by_labels.strip()
            except BaseException as exc:
                cleanup["absence_check"] = f"failed: {type(exc).__name__}: {exc}"
        cleanup["seconds"] = time.monotonic() - cleanup_started
        cleanup["within_deadline"] = time.monotonic() <= cleanup_deadline
        cleanup["failures"] = cleanup_failures(cleanup)
        report["cleanup"] = cleanup
        report["total_seconds"] = time.monotonic() - started
        if cleanup["failures"]:
            report["state"] = "cleanup_failed"
        atomic_write_json(sidecar, report, max_bytes=16 * 1024 * 1024)
    print(json.dumps({"state": report["state"], "report": str(sidecar)}, sort_keys=True))
    return 0 if first_error is None and report["state"] == "diagnostic_completed" else 1


if __name__ == "__main__":
    raise SystemExit(main())
