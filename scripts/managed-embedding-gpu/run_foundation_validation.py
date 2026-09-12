from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import sys
import time
import uuid
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from compare_gpu_vectors import accepted, compare_cases, metrics, validate_vector
from gpu_contract import ContractError, atomic_write_json, load_contract, read_json, sha256_file, validate_locked_files
from prepare_image import CommandError, run
from run_tei_startup import (
    TASK_LABEL,
    VramMonitor,
    authenticate_container,
    build_create_command,
    cleanup_authenticated_container,
    parse_container_id,
    parse_gpu_csv,
    read_router_runtime,
    recover_owned_container,
    validate_info,
)
import reference_cuda


REFERENCE_TASK_LABEL = "io.hmem.task=aa30a81c-2adf-4f54-b1eb-cfeaa39bac77"
REFERENCE_METHOD = "original-sdpa-math-cuda-f16-v3"
REFERENCE_IMAGE_ID = "sha256:24ece9d9b0ff4710e0c9b955e3c67c520b18b9504256043af1f7a324a8a51582"
EXPECTED_ATTENTION_NAMES = [f"layers.{index}.self_attn" for index in range(28)]
EXPECTED_ROTARY_NAMES = [f"layers.{index}.self_attn.rotary_emb" for index in range(28)]
EXPECTED_SDPA_FLAGS = {
    "math_sdp_enabled": True,
    "flash_sdp_enabled": False,
    "mem_efficient_sdp_enabled": False,
    "cudnn_sdp_enabled": False,
    "cuda_matmul_allow_tf32": False,
    "cudnn_allow_tf32": False,
    "float32_matmul_precision": "highest",
    "allow_fp16_reduced_precision_reduction": False,
    "fp16_bf16_reduction_math_sdp_allowed": False,
    "autocast_enabled": False,
}


def validate_probes(value: Any, contract: dict[str, Any]) -> list[dict[str, Any]]:
    if not isinstance(value, dict) or value.get("schema_version") != 1 or value.get("task_id") != contract["task_id"]:
        raise ContractError("invalid parity probe identity")
    if value.get("model") != {"id": contract["model"]["id"], "revision": contract["model"]["revision"]}:
        raise ContractError("parity probe model identity drift")
    cases = value.get("cases")
    if not isinstance(cases, list) or len(cases) < 4:
        raise ContractError("parity probes require at least four cases")
    seen = set()
    for case in cases:
        if not isinstance(case, dict) or not isinstance(case.get("id"), str) or case["id"] in seen:
            raise ContractError("parity probe IDs must be unique strings")
        seen.add(case["id"])
        text = case.get("text")
        ids = case.get("token_ids")
        if not isinstance(text, str) or hashlib.sha256(text.encode("utf-8")).hexdigest() != case.get("utf8_sha256"):
            raise ContractError(f"parity probe UTF-8 checksum drift for {case['id']}")
        if (
            not isinstance(ids, list)
            or not ids
            or ids[-1] != contract["model"]["eos_token_id"]
            or ids.count(contract["model"]["eos_token_id"]) != 1
        ):
            raise ContractError(f"parity probe EOS drift for {case['id']}")
        if any(not isinstance(token_id, int) or isinstance(token_id, bool) or token_id < 0 for token_id in ids):
            raise ContractError(f"parity probe token ID drift for {case['id']}")
        if len(ids) > contract["numerical_acceptance"]["maximum_compact_probe_tokens"]:
            raise ContractError(f"parity probe exceeds compact token cap for {case['id']}")
        if case.get("role") == "query":
            if text != contract["model"]["query_prefix"] + case.get("raw_text", ""):
                raise ContractError(f"query prefix drift for {case['id']}")
        elif case.get("role") != "document" or case.get("raw_text") != text:
            raise ContractError(f"document bytes drift for {case['id']}")
    if sum(case["role"] == "query" for case in cases) != 1:
        raise ContractError("parity probes require exactly one query case")
    return cases


def http_json(
    container_id: str,
    method: str,
    path: str,
    timeout: int,
    max_bytes: int,
    *,
    payload: Any | None = None,
    payload_file: str | None = None,
) -> tuple[int, Any]:
    if (payload is None) == (payload_file is None) and method != "GET":
        raise ContractError("exactly one HTTP payload source is required")
    command = [
        "docker", "exec", container_id, "curl", "--silent", "--show-error", "--max-time", str(timeout),
        "--request", method, "--header", "Accept: application/json", "--write-out", "\n%{http_code}",
    ]
    if method != "GET":
        command.extend(["--header", "Content-Type: application/json", "--data-binary"])
        command.append("@" + payload_file if payload_file is not None else json.dumps(payload, ensure_ascii=False, separators=(",", ":")))
    command.append("http://127.0.0.1:8080" + path)
    stdout, _ = run(command, timeout + 5, max_bytes)
    body, separator, status_text = stdout.rpartition("\n")
    if not separator or not status_text.isdigit():
        raise ContractError(f"HTTP response lacks status for {path}")
    try:
        value = json.loads(body)
    except json.JSONDecodeError as exc:
        raise ContractError(f"invalid JSON response from {path}: {exc}") from exc
    return int(status_text), value


def embed(container_id: str, texts: list[str], timeout: int, dimensions: int, norm_error: float) -> list[list[float]]:
    status, value = http_json(
        container_id,
        "POST",
        "/embed",
        timeout,
        64 * 1024 * 1024,
        payload={"inputs": texts, "normalize": True, "truncate": False, "prompt_name": None},
    )
    if status != 200 or not isinstance(value, list) or len(value) != len(texts):
        raise ContractError(f"unexpected /embed response: status={status}, count={len(value) if isinstance(value, list) else None}")
    return [validate_vector(vector, dimensions, norm_error) for vector in value]


def add_validation_mount(command: list[str], image_reference: str, result_root: Path) -> list[str]:
    index = command.index(image_reference)
    return command[:index] + ["--mount", f"type=bind,src={result_root},dst=/validation,readonly"] + command[index:]


def measured_recommendations(startup: float, short_requests: list[float], full_context: float) -> dict[str, Any]:
    if startup <= 0 or not short_requests or any(value <= 0 for value in short_requests) or full_context <= 0:
        raise ContractError("measured timing recommendations require positive observations")
    return {
        "startup_timeout_seconds": max(120, math.ceil(startup * 1.5)),
        "short_request_timeout_seconds": max(30, math.ceil(max(short_requests) * 2.0)),
        "full_context_request_timeout_seconds": max(300, math.ceil(full_context * 1.5)),
        "validated_concurrent_requests": 2,
        "validated_compact_batch_items": 4,
        "policy": "ceil startup/full x1.5, short maximum x2; retain frozen minima 120/30/300",
    }


def cleanup_failures(cleanup: dict[str, Any]) -> list[str]:
    failures = [
        key for key in ("logs", "log_write", "stop", "remove", "absence_check")
        if key in cleanup and (cleanup[key] is None or str(cleanup[key]).startswith("failed:"))
    ]
    if cleanup.get("absent") is not True:
        failures.append("absence")
    if cleanup.get("within_deadline") is False:
        failures.append("cleanup_deadline")
    if cleanup.get("vram_monitor_stopped") is False or cleanup.get("vram_monitor_failure"):
        failures.append("vram_monitor")
    return failures


def monitor_sample_capacity(runtime: dict[str, Any]) -> int:
    return (
        runtime["startup_timeout_seconds"] + 9 * runtime["short_request_timeout_seconds"]
        + 3 * runtime["full_context_request_timeout_seconds"] + runtime["cleanup_timeout_seconds"] + 121
    )


def metric_evidence(case_id: str, reference: list[float], candidate: list[float], thresholds: dict[str, Any]) -> dict[str, Any]:
    observed = metrics(reference, candidate)
    return {"id": case_id, **observed, "accepted": accepted(observed, thresholds), "vector": candidate}


def require_serial_gpu_ownership(*, runner=run) -> None:
    for task_label in (TASK_LABEL, REFERENCE_TASK_LABEL):
        existing, _ = runner(["docker", "ps", "-aq", "--no-trunc", "--filter", f"label={task_label}"], 15, 65536)
        if existing.strip():
            raise ContractError("another task-owned reference or TEI container exists")


def _validate_attention_inventory(runtime: dict[str, Any]) -> None:
    attention = runtime.get("attention_modules")
    if (
        not isinstance(attention, dict)
        or set(attention) != {"count", "class", "module", "names"}
        or attention.get("count") != 28
        or attention.get("class") != "Qwen2SdpaAttention"
        or attention.get("module") != reference_cuda.ORIGINAL_MODEL_MODULE_NAME
        or attention.get("names") != EXPECTED_ATTENTION_NAMES
    ):
        raise ContractError("reference original SDPA attention inventory drift")


def _validate_rotary_inventory(runtime: dict[str, Any]) -> None:
    rotary = runtime.get("rotary_modules")
    if not isinstance(rotary, list) or len(rotary) != 28:
        raise ContractError("reference rotary cache inventory drift")
    expected_buffers = [
        ("inv_freq", [64], 64),
        ("cos_cached", [131072, 128], 16_777_216),
        ("sin_cached", [131072, 128], 16_777_216),
    ]
    for expected_name, item in zip(EXPECTED_ROTARY_NAMES, rotary):
        if (
            not isinstance(item, dict)
            or set(item) != {"name", "class", "module", "max_seq_len_cached", "buffers"}
            or item.get("name") != expected_name
            or item.get("class") != "Qwen2RotaryEmbedding"
            or item.get("module") != reference_cuda.ORIGINAL_MODEL_MODULE_NAME
            or item.get("max_seq_len_cached") != 131072
        ):
            raise ContractError("reference rotary cache inventory drift")
        buffers = item.get("buffers")
        if not isinstance(buffers, list) or len(buffers) != len(expected_buffers):
            raise ContractError("reference rotary buffer inventory drift")
        for observed, (name, shape, numel) in zip(buffers, expected_buffers):
            if (
                not isinstance(observed, dict)
                or set(observed) != {"name", "shape", "numel", "dtype", "device", "version"}
                or observed.get("name") != f"{expected_name}.{name}"
                or observed.get("shape") != shape
                or observed.get("numel") != numel
                or observed.get("dtype") != "torch.float16"
                or observed.get("device") != "cuda:0"
                or isinstance(observed.get("version"), bool)
                or not isinstance(observed.get("version"), int)
                or observed["version"] < 0
            ):
                raise ContractError("reference rotary buffer inventory drift")


def _validate_attention_backend_evidence(case: dict[str, Any]) -> None:
    evidence = case.get("attention_backend_evidence")
    if not isinstance(evidence, dict) or set(evidence) != {
        "profiler", "effective_flags_before", "effective_flags_after",
        "scaled_dot_product_attention_calls", "implementation_operator_counts",
    }:
        raise ContractError("reference SDPA MATH backend evidence is missing")
    if evidence.get("profiler") != {
        "activities": ["CPU"], "record_shapes": False, "profile_memory": False, "with_stack": False,
    }:
        raise ContractError("reference bounded profiler settings drift")
    if (
        evidence.get("effective_flags_before") != EXPECTED_SDPA_FLAGS
        or evidence.get("effective_flags_after") != EXPECTED_SDPA_FLAGS
        or evidence.get("scaled_dot_product_attention_calls") != 28
        or evidence.get("implementation_operator_counts") != {"aten::_scaled_dot_product_attention_math": 28}
    ):
        raise ContractError("reference SDPA MATH dispatch evidence drift")


def reference_validation_summary(reference: dict[str, Any]) -> dict[str, Any]:
    runtime = reference["runtime"]
    cases = reference["cases"]
    return {
        "reference_method": runtime["reference_method"],
        "script_sha256": runtime["script_sha256"],
        "parameter_dtypes": runtime["parameter_dtypes"],
        "activation_dtypes": runtime["activation_dtypes"],
        "pooling_input_dtype": runtime["pooling_input_dtype"],
        "pooling_compute_dtype": runtime["pooling_compute_dtype"],
        "normalized_vector_dtype": runtime["normalized_vector_dtype"],
        "attention_implementation": runtime["attention_implementation"],
        "attention_module_count": runtime["attention_modules"]["count"],
        "rotary_module_count": len(runtime["rotary_modules"]),
        "effective_flags": {key: runtime[key] for key in EXPECTED_SDPA_FLAGS},
        "case_ids": [case["id"] for case in cases],
        "math_calls_per_case": [case["attention_backend_evidence"]["scaled_dot_product_attention_calls"] for case in cases],
        "implementation_operator_counts_per_case": [
            case["attention_backend_evidence"]["implementation_operator_counts"] for case in cases
        ],
    }


def validate_reference_report(reference: Any, probe_sha256: str, contract: dict[str, Any]) -> list[dict[str, Any]]:
    if not isinstance(reference, dict) or reference.get("schema_version") != 1 or reference.get("state") != "passed":
        raise ContractError("reference output is not a passing schema-v1 result")
    if reference.get("probe_sha256") != probe_sha256:
        raise ContractError("reference output probe checksum drift")
    model = reference.get("model")
    expected_model = contract["model"]
    if not isinstance(model, dict) or model.get("id") != expected_model["id"] or model.get("revision") != expected_model["revision"] or model.get("source") != "/model":
        raise ContractError("reference model identity drift")
    expected_artifacts = {(item["path"], item["sha256"]) for item in expected_model["artifacts"]}
    observed_artifacts = {(item.get("path"), item.get("sha256")) for item in model.get("artifacts", []) if isinstance(item, dict)}
    if observed_artifacts != expected_artifacts or model.get("config_sha256") != reference_cuda.CONFIG_SHA256:
        raise ContractError("reference model source inventory drift")
    runtime = reference.get("runtime")
    expected_runtime = {"python": "3.11.9", "torch": "2.7.1+cu128", "torch_cuda": "12.8", "transformers": "4.41.2"}
    if not isinstance(runtime, dict) or any(runtime.get(key) != value for key, value in expected_runtime.items()):
        raise ContractError("reference runtime identity drift")
    if runtime.get("packages") != reference_cuda.PACKAGE_VERSIONS:
        raise ContractError("reference package inventory drift")
    device = runtime.get("device")
    if not isinstance(device, dict) or device.get("index") != 0 or device.get("compute_capability") != "12.0" or not device.get("uuid"):
        raise ContractError("reference CUDA device identity drift")
    if (
        "sm_120" not in runtime.get("torch_supported_architectures", [])
        or runtime.get("parameter_devices") != ["cuda:0"]
        or runtime.get("reference_method") != REFERENCE_METHOD
        or runtime.get("parameter_dtypes") != ["torch.float16"]
        or runtime.get("activation_dtypes") != ["torch.float16"]
        or runtime.get("pooling_input_dtype") != "torch.float16"
        or runtime.get("pooling_compute_dtype") != "torch.float32"
        or runtime.get("normalized_vector_dtype") != "torch.float32"
        or runtime.get("smoke_dtype") != "torch.float16"
        or runtime.get("float32_matmul_precision") != "highest"
        or runtime.get("cuda_matmul_allow_tf32") is not False
        or runtime.get("cudnn_allow_tf32") is not False
        or runtime.get("allow_fp16_reduced_precision_reduction") is not False
        or runtime.get("fp16_bf16_reduction_math_sdp_allowed") is not False
        or runtime.get("autocast_enabled") is not False
        or runtime.get("torch_compile") is not False
        or runtime.get("attention_implementation") != "sdpa"
        or runtime.get("output_attentions") is not False
        or runtime.get("is_causal") is not False
        or runtime.get("use_cache") is not False
        or runtime.get("model_loads") != 1
        or runtime.get("forwards_completed") != 4
        or any(runtime.get(key) != expected for key, expected in EXPECTED_SDPA_FLAGS.items())
    ):
        raise ContractError("reference CUDA/F16/SDPA/noncausal execution drift")
    if runtime.get("script_sha256") != sha256_file(Path(__file__).with_name("reference_cuda.py")):
        raise ContractError("reference driver checksum drift")
    model_class = runtime.get("model_class")
    if (
        not isinstance(model_class, dict)
        or model_class.get("class") != "Qwen2Model"
        or model_class.get("module") != reference_cuda.ORIGINAL_MODEL_MODULE_NAME
        or model_class.get("path") != "/model/modeling_qwen.py"
        or model_class.get("sha256") != reference_cuda.MODELING_QWEN_SHA256
    ):
        raise ContractError("reference custom model class drift")
    tokenizer_class = runtime.get("tokenizer_class")
    if not isinstance(tokenizer_class, dict) or tokenizer_class.get("class") != "Qwen2TokenizerFast" or tokenizer_class.get("sha256") != reference_cuda.TOKENIZATION_QWEN_SHA256:
        raise ContractError("reference tokenizer class drift")
    _validate_attention_inventory(runtime)
    _validate_rotary_inventory(runtime)
    cases = reference.get("cases")
    if not isinstance(cases, list) or [case.get("id") for case in cases if isinstance(case, dict)] != [
        "doc_short", "mixed_long", "multilingual", "query_search"
    ]:
        raise ContractError("reference case identity/order drift")
    dimensions = contract["numerical_acceptance"]["dimensions"]
    norm_error = contract["numerical_acceptance"]["maximum_unit_norm_absolute_error"]
    for case in cases:
        if not isinstance(case.get("token_ids"), list):
            raise ContractError("reference case token evidence is missing")
        validate_vector(case.get("vector"), dimensions, norm_error)
        _validate_attention_backend_evidence(case)
    return cases


def validate_reference_execution(reference_path: Path, probe_sha256: str) -> dict[str, Any]:
    execution_path = reference_path.with_name(reference_path.name + ".execution.json")
    execution = read_json(execution_path)
    reference = read_json(reference_path)
    if not isinstance(execution, dict):
        raise ContractError("reference execution attestation drift")
    output = execution.get("reference_output")
    cleanup = execution.get("cleanup")
    if (
        execution.get("schema_version") != 1
        or execution.get("state") != "reference_passed"
        or execution.get("task_id") != reference_cuda.TASK_ID
        or execution.get("reference_method") != REFERENCE_METHOD
        or execution.get("image_id") != REFERENCE_IMAGE_ID
        or execution.get("probe_sha256") != probe_sha256
        or not isinstance(output, dict)
        or Path(output.get("path", "")).resolve() != reference_path
        or output.get("bytes") != reference_path.stat().st_size
        or output.get("sha256") != sha256_file(reference_path)
        or execution.get("reference_validation") != reference_validation_summary(reference)
        or not isinstance(cleanup, dict)
        or cleanup.get("absent") is not True
        or cleanup.get("failures") != []
    ):
        raise ContractError("reference execution attestation drift")
    return {"path": str(execution_path), "sha256": sha256_file(execution_path), "container_id": execution.get("container_id")}


def main() -> int:
    parser = argparse.ArgumentParser(description="Run bounded TEI parity, full-context, and concurrent GPU validation.")
    parser.add_argument("--contract", type=Path, required=True)
    parser.add_argument("--probes", type=Path, required=True)
    parser.add_argument("--reference", type=Path, required=True)
    args = parser.parse_args()
    started = time.monotonic()
    contract = load_contract(args.contract.resolve())
    probes_value = read_json(args.probes.resolve())
    cases = validate_probes(probes_value, contract)
    reference = read_json(args.reference.resolve())
    probe_sha256 = sha256_file(args.probes.resolve())
    reference_cases = validate_reference_report(reference, probe_sha256, contract)
    reference_execution = validate_reference_execution(args.reference.resolve(), probe_sha256)
    runtime = contract["runtime_contract"]
    thresholds = contract["numerical_acceptance"]
    run_id = uuid.uuid4().hex
    result_root = Path(contract["artifact_root"]) / "validation" / run_id
    result_root.mkdir(parents=True, exist_ok=False)
    boundary_text = " a" * 32767
    full_tokenize_request = {"inputs": boundary_text, "add_special_tokens": True}
    full_request = {"inputs": [boundary_text], "normalize": True, "truncate": False, "prompt_name": None}
    over_request = {"inputs": [boundary_text + " a"], "normalize": True, "truncate": False, "prompt_name": None}
    over_tokenize_request = {"inputs": boundary_text + " a", "add_special_tokens": True}
    atomic_write_json(result_root / "full-32768-request.json", full_request, max_bytes=1048576)
    atomic_write_json(result_root / "full-32768-tokenize.json", full_tokenize_request, max_bytes=1048576)
    atomic_write_json(result_root / "over-32769-request.json", over_request, max_bytes=1048576)
    atomic_write_json(result_root / "over-32769-tokenize.json", over_tokenize_request, max_bytes=1048576)
    report: dict[str, Any] = {
        "schema_version": 1,
        "state": "started",
        "task_id": contract["task_id"],
        "run_id": run_id,
        "controller": {"pid": os.getpid(), "started_utc": datetime.now(timezone.utc).isoformat(), "monotonic_started": started},
        "contract_sha256": sha256_file(args.contract.resolve()),
        "probe_sha256": sha256_file(args.probes.resolve()),
        "reference": {"path": str(args.reference.resolve()), "sha256": sha256_file(args.reference.resolve())},
        "reference_execution": reference_execution,
        "limits": runtime,
        "thresholds": thresholds,
    }
    print(json.dumps({"state": "launched", "run_id": run_id, "controller": report["controller"]}, sort_keys=True), flush=True)
    container_id: str | None = None
    monitor: VramMonitor | None = None
    first_error: BaseException | None = None
    observed_timings: dict[str, Any] = {"singletons": {}}
    query = ["nvidia-smi", "--query-gpu=index,uuid,name,driver_version,memory.total,memory.free,memory.used,compute_cap", "--format=csv,noheader,nounits"]
    try:
        validate_locked_files(Path(contract["artifact_root"]) / "model", contract["model"]["artifacts"], exact=True)
        require_serial_gpu_ownership()
        host_gpu_raw, _ = run(query, 15, 65536)
        report["host_gpu_before"] = parse_gpu_csv(host_gpu_raw, runtime["required_compute_capability"], runtime["minimum_free_vram_mib"])
        name, create_command = build_create_command(contract, run_id)
        create_command = add_validation_mount(create_command, contract["image"]["reference"], result_root)
        report["container_name"] = name
        report["create_argv"] = create_command
        created, _ = run(create_command, 60, 65536)
        candidate = parse_container_id(created)
        inspected_raw, _ = run(["docker", "inspect", candidate], 15, 1048576)
        inspected = authenticate_container(json.loads(inspected_raw), contract, run_id)
        container_id = candidate
        report["container_id"] = container_id
        if inspected.get("HostConfig", {}).get("LogConfig") != {
            "Type": "local", "Config": {"compress": "false", "max-file": "1", "max-size": "8m"}
        }:
            raise ContractError("validation container log contract drift")
        host = inspected.get("HostConfig", {})
        expected_host = {
            "Memory": runtime["host_memory_bytes"], "MemorySwap": runtime["host_memory_swap_bytes"],
            "NanoCpus": runtime["host_cpus"] * 1_000_000_000, "PidsLimit": runtime["pids_limit"],
            "ReadonlyRootfs": True,
        }
        if any(host.get(key) != value for key, value in expected_host.items()):
            raise ContractError("validation container host cap drift")
        if host.get("NetworkMode") != "none" or host.get("CapDrop") != ["ALL"] or "no-new-privileges" not in (host.get("SecurityOpt") or []):
            raise ContractError("validation container isolation drift")
        container_config = inspected.get("Config", {})
        if container_config.get("Image") != contract["image"]["reference"] or inspected.get("Path") != "./entrypoint.sh":
            raise ContractError("validation container image or entrypoint drift")
        mounts = {mount.get("Destination"): mount for mount in inspected.get("Mounts", [])}
        if set(mounts) != {"/model", "/validation"} or any(mount.get("RW") is not False for mount in mounts.values()):
            raise ContractError("validation mounts must be exactly model and evidence read-only binds")
        report["container_caps"] = expected_host
        report["container_mounts"] = inspected.get("Mounts", [])
        monitor_samples = monitor_sample_capacity(runtime)
        report["vram_monitor_max_samples"] = monitor_samples
        monitor = VramMonitor(query, runtime, container_id, max_samples=monitor_samples)
        monitor.start()
        if not monitor.wait_first_sample(10.0):
            raise ContractError("VRAM monitor did not produce an initial sample")
        monitor.check()
        run(["docker", "start", container_id], 30, 65536)
        ready_deadline = time.monotonic() + runtime["startup_timeout_seconds"]
        info = None
        while time.monotonic() < ready_deadline:
            monitor.check()
            state_raw, _ = run(["docker", "inspect", "--format", "{{json .State}}", container_id], 10, 65536)
            state = json.loads(state_raw)
            if state.get("Status") == "exited":
                raise ContractError(f"TEI exited before readiness with code {state.get('ExitCode')}")
            try:
                status, candidate_info = http_json(container_id, "GET", "/info", 2, 1048576)
                if status == 200:
                    info = candidate_info
                    break
            except (ContractError, CommandError):
                pass
            time.sleep(2)
        if info is None:
            raise ContractError("TEI startup deadline expired")
        validate_info(info)
        report["info"] = info
        report["startup_seconds"] = time.monotonic() - started
        report["router_runtime"] = read_router_runtime(container_id)

        candidate_cases = []
        singletons: dict[str, list[float]] = {}
        short_timings: list[float] = []
        for case in cases:
            status, tokens = http_json(
                container_id, "POST", "/tokenize", runtime["short_request_timeout_seconds"], 8 * 1024 * 1024,
                payload={"inputs": case["text"], "add_special_tokens": True},
            )
            if status != 200 or not isinstance(tokens, list) or len(tokens) != 1:
                raise ContractError(f"tokenization failed for {case['id']}")
            token_ids = [token.get("id") for token in tokens[0]]
            if token_ids != case["token_ids"]:
                raise ContractError(f"TEI token IDs differ from frozen IDs for {case['id']}")
            request_started = time.monotonic()
            vector = embed(container_id, [case["text"]], runtime["short_request_timeout_seconds"], thresholds["dimensions"], thresholds["maximum_unit_norm_absolute_error"])[0]
            request_seconds = time.monotonic() - request_started
            short_timings.append(request_seconds)
            observed_timings["singletons"][case["id"]] = request_seconds
            singletons[case["id"]] = vector
            candidate_cases.append({"id": case["id"], "token_ids": token_ids, "vector": vector})
        parity = compare_cases(reference_cases, candidate_cases, thresholds)
        report["parity"] = parity
        report["candidate_cases"] = candidate_cases
        if any(not item["accepted"] for item in parity):
            raise ContractError("TEI/reference numerical compatibility failed")

        request_started = time.monotonic()
        repeated = embed(container_id, [cases[0]["text"]], runtime["short_request_timeout_seconds"], thresholds["dimensions"], thresholds["maximum_unit_norm_absolute_error"])[0]
        repeat_seconds = time.monotonic() - request_started
        short_timings.append(repeat_seconds)
        observed_timings["repeat"] = repeat_seconds
        report["repeat"] = metric_evidence(cases[0]["id"], singletons[cases[0]["id"]], repeated, thresholds)
        if not report["repeat"]["accepted"]:
            raise ContractError("repeat embedding compatibility failed")
        request_started = time.monotonic()
        batch_vectors = embed(container_id, [case["text"] for case in cases], runtime["short_request_timeout_seconds"], thresholds["dimensions"], thresholds["maximum_unit_norm_absolute_error"])
        batch_seconds = time.monotonic() - request_started
        short_timings.append(batch_seconds)
        observed_timings["four_item_batch"] = batch_seconds
        report["batch_order"] = []
        for case, vector in zip(cases, batch_vectors):
            item = metric_evidence(case["id"], singletons[case["id"]], vector, thresholds)
            report["batch_order"].append(item)
            if not item["accepted"]:
                raise ContractError(f"batch ordering/compatibility failed for {case['id']}")

        orders = [cases[:2], list(reversed(cases[-2:]))]
        request_started = time.monotonic()
        with ThreadPoolExecutor(max_workers=2) as pool:
            futures = [pool.submit(embed, container_id, [case["text"] for case in order], runtime["short_request_timeout_seconds"], thresholds["dimensions"], thresholds["maximum_unit_norm_absolute_error"]) for order in orders]
            concurrent_vectors = [future.result(timeout=runtime["short_request_timeout_seconds"] + 10) for future in futures]
        concurrent_seconds = time.monotonic() - request_started
        short_timings.append(concurrent_seconds)
        observed_timings["two_concurrent_mixed_batches"] = concurrent_seconds
        report["concurrent"] = []
        for order, vectors in zip(orders, concurrent_vectors):
            row = []
            for case, vector in zip(order, vectors):
                item = metric_evidence(case["id"], singletons[case["id"]], vector, thresholds)
                row.append(item)
                if not item["accepted"]:
                    report["concurrent"].append(row)
                    raise ContractError(f"concurrent ordering/compatibility failed for {case['id']}")
            report["concurrent"].append(row)

        query_case = next(case for case in cases if case["role"] == "query")
        request_started = time.monotonic()
        wrong_vector = embed(container_id, [query_case["raw_text"]], runtime["short_request_timeout_seconds"], thresholds["dimensions"], thresholds["maximum_unit_norm_absolute_error"])[0]
        wrong_prompt_seconds = time.monotonic() - request_started
        short_timings.append(wrong_prompt_seconds)
        observed_timings["wrong_prompt_negative"] = wrong_prompt_seconds
        reference_query = next(case["vector"] for case in reference["cases"] if case["id"] == query_case["id"])
        wrong_metrics = metrics(reference_query, wrong_vector)
        report["wrong_prompt_negative"] = {**wrong_metrics, "accepted": accepted(wrong_metrics, thresholds), "vector": wrong_vector}
        if report["wrong_prompt_negative"]["accepted"]:
            raise ContractError("wrong-prompt negative did not fail compatibility")

        status, full_tokens = http_json(
            container_id, "POST", "/tokenize", runtime["full_context_request_timeout_seconds"], 16 * 1024 * 1024,
            payload_file="/validation/full-32768-tokenize.json",
        )
        if status != 200 or not isinstance(full_tokens, list) or len(full_tokens) != 1:
            raise ContractError("full-context tokenization failed")
        full_ids = [token.get("id") for token in full_tokens[0]]
        if len(full_ids) != 32768 or full_ids[-1] != contract["model"]["eos_token_id"] or len(set(full_ids[:-1])) != 1:
            raise ContractError("full-context token boundary drift")
        full_started = time.monotonic()
        full_status, full_embedding = http_json(
            container_id, "POST", "/embed", runtime["full_context_request_timeout_seconds"], 64 * 1024 * 1024,
            payload_file="/validation/full-32768-request.json",
        )
        if full_status != 200 or not isinstance(full_embedding, list) or len(full_embedding) != 1:
            raise ContractError(f"full-context embedding failed with status {full_status}")
        full_vector = validate_vector(full_embedding[0], thresholds["dimensions"], thresholds["maximum_unit_norm_absolute_error"])
        report["full_context"] = {
            "status": full_status, "token_count": len(full_ids), "repeated_token_id": full_ids[0],
            "token_ids_sha256": hashlib.sha256(json.dumps(full_ids, separators=(",", ":")).encode()).hexdigest(),
            "vector_sha256": hashlib.sha256(json.dumps(full_vector, separators=(",", ":")).encode()).hexdigest(),
            "vector": full_vector,
            "seconds": time.monotonic() - full_started,
        }
        over_started = time.monotonic()
        over_token_status, over_tokens = http_json(
            container_id, "POST", "/tokenize", runtime["full_context_request_timeout_seconds"], 16 * 1024 * 1024,
            payload_file="/validation/over-32769-tokenize.json",
        )
        if over_token_status != 200 or not isinstance(over_tokens, list) or len(over_tokens) != 1:
            raise ContractError("32769-token boundary authentication failed")
        over_ids = [token.get("id") for token in over_tokens[0]]
        if len(over_ids) != 32769 or over_ids[-1] != contract["model"]["eos_token_id"] or set(over_ids[:-1]) != {full_ids[0]}:
            raise ContractError("32769-token input did not tokenize to the exact intended boundary")
        over_status, over_body = http_json(
            container_id, "POST", "/embed", runtime["short_request_timeout_seconds"], 1048576,
            payload_file="/validation/over-32769-request.json",
        )
        if over_status not in {413, 422}:
            raise ContractError(f"32769-token request was not rejected: status={over_status}")
        over_error_text = json.dumps(over_body, sort_keys=True, ensure_ascii=False)
        expected_error = "`inputs` must have less than 32768 tokens. Given: 32769"
        if expected_error not in over_error_text:
            raise ContractError(f"32769 rejection did not identify a token-length limit: {over_error_text}")
        report["over_limit"] = {
            "token_count": len(over_ids), "repeated_token_id": over_ids[0], "status": over_status,
            "body": over_body,
            "body_sha256": hashlib.sha256(json.dumps(over_body, sort_keys=True, separators=(",", ":")).encode()).hexdigest(),
        }
        observed_timings["over_limit_tokenize_and_reject"] = time.monotonic() - over_started
        observed_timings["full_context_embedding"] = report["full_context"]["seconds"]
        report["observed_timings_seconds"] = observed_timings
        report["request_timings_seconds"] = short_timings
        report["measured_recommendations"] = measured_recommendations(
            report["startup_seconds"], short_timings, report["full_context"]["seconds"]
        )
        monitor.check()
        stats_raw, _ = run(["docker", "stats", "--no-stream", "--format", "{{json .}}", container_id], 15, 1048576)
        report["container_stats_after_validation"] = json.loads(stats_raw)
        logs_raw, logs_err = run(["docker", "logs", container_id], 30, runtime["max_log_bytes"])
        logs = logs_raw + logs_err
        if "Starting FlashQwen2 model on Cuda" not in logs:
            raise ContractError("TEI logs do not prove FlashQwen2 CUDA dispatch")
        (result_root / "tei.log").write_bytes(logs.encode("utf-8"))
        report["log_sha256"] = hashlib.sha256(logs.encode("utf-8")).hexdigest()
        if not monitor.stop(10.0):
            raise ContractError("VRAM monitor did not stop")
        monitor.check()
        report["vram_samples"] = [report["host_gpu_before"], *monitor.samples]
        report["observed_peak_vram_mib"] = max(sample["memory_used_mib"] for sample in report["vram_samples"])
        report["state"] = "foundation_validation_passed"
    except BaseException as exc:
        first_error = exc
        report["state"] = "failed"
        report["error"] = {"type": type(exc).__name__, "message": str(exc)}
    finally:
        cleanup_started = time.monotonic()
        cleanup_deadline = cleanup_started + runtime["cleanup_timeout_seconds"]

        def cleanup_run(command: list[str], maximum_seconds: int, cap: int) -> tuple[str, str]:
            remaining = cleanup_deadline - time.monotonic()
            if remaining < 3.0:
                raise ContractError("cleanup deadline has no time for another Docker operation")
            return run(command, max(3, min(maximum_seconds, int(remaining))), cap)

        cleanup: dict[str, Any] = {"container_id": container_id, "absent": None}
        if monitor is not None:
            cleanup["vram_monitor_stopped"] = monitor.stop(min(10.0, max(0.0, cleanup_deadline - time.monotonic())))
            cleanup["vram_monitor_failure"] = monitor.failure
            cleanup["vram_monitor_trigger"] = monitor.cleanup_trigger
            report.setdefault("vram_samples", [report.get("host_gpu_before"), *monitor.samples])
        if container_id is None:
            try:
                container_id, _ = recover_owned_container(contract, run_id, runner=cleanup_run)
            except BaseException as exc:
                cleanup["recovery"] = f"failed: {type(exc).__name__}: {exc}"
        if container_id is not None:
            cleanup.update(cleanup_authenticated_container(container_id, run_id, result_root, runtime["max_log_bytes"], runner=cleanup_run))
        cleanup["seconds"] = time.monotonic() - cleanup_started
        cleanup["within_deadline"] = cleanup["seconds"] <= runtime["cleanup_timeout_seconds"]
        cleanup["failures"] = cleanup_failures(cleanup)
        report["cleanup"] = cleanup
        report["total_seconds"] = time.monotonic() - started
        if cleanup.get("absent") is not True or cleanup.get("failures") or cleanup.get("vram_monitor_stopped") is False:
            report["state"] = "cleanup_failed"
        atomic_write_json(result_root / "foundation-validation-report-v1.json", report, max_bytes=128 * 1024 * 1024)
    print(json.dumps({"state": report["state"], "report": str(result_root / "foundation-validation-report-v1.json")}, sort_keys=True))
    if first_error is not None:
        print(f"foundation validation failed: {first_error}", file=sys.stderr)
    return 0 if report["state"] == "foundation_validation_passed" else 1


if __name__ == "__main__":
    raise SystemExit(main())
