from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
import math
import os
import platform
import re
import sys
import time
from pathlib import Path
from typing import Any, Callable


TASK_ID = "aa30a81c-2adf-4f54-b1eb-cfeaa39bac77"
BASE_SHA = "ba60c4ecafdac00932fd030876041e347f222948"
KIND = "loaded_weight_rounding_diagnostic_v1"
CASE_ID = "mixed_long"
DIMENSIONS = 1536
COORDINATE_INDEX = 940
EOS_TOKEN_ID = 151643
REFERENCE_METHOD = "original-eager-cuda-fp32-v2"
REFERENCE_DRIVER_PATH = Path("/scripts/reference_cuda.py")
REFERENCE_DRIVER_BYTES = 29060
REFERENCE_DRIVER_SHA256 = "eac9e1235b46425bbad706a188bc83a371f0783c3a0724534e17fdbf6649b6a1"
REFERENCE_MODULE_NAME = f"_hmem_weight_rounding_reference_{REFERENCE_DRIVER_SHA256[:16]}"
RUN_ID_ENV = "HMEM_DIAGNOSTIC_RUN_ID"
MAX_INPUT_BYTES = 8 * 1024 * 1024
MAX_OUTPUT_BYTES = 4 * 1024 * 1024
ROUNDING_CHUNK_ELEMENTS = 4 * 1024 * 1024

EXPECTED_INPUTS = {
    "probes": (Path("/inputs/probes.json"), 68993, "1eded3104b9d3f18ae0a152aa6796d066c637ad1f3ceabea55b299d32b39aff7"),
    "retained_reference": (Path("/inputs/reference-v3.json"), 224438, "b283818f18be1547d2d827d214710e0b2112fdd43bc5e9930460b2b47383b5d6"),
    "failed_tei_report": (Path("/inputs/failed-tei.json"), 185247, "486163fee07d3a6970e51c30da67b5b67318bf4d99782c04055d7ab89b060fd7"),
}
EXPECTED_PROTOCOL = (
    Path("/inputs/diagnostic-protocol.json"),
    16473,
    "4b2eb01016a55f9089d8f2281f07eadb7e0b31dd3e4735d6388981f5fbbf2810",
)
EXPECTED_SOURCE = Path("/model")
EXPECTED_OUTPUT = Path("/output/diagnostic.json")


class DiagnosticError(ValueError):
    pass


def require(condition: bool, message: str) -> None:
    if not condition:
        raise DiagnosticError(message)


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        while chunk := handle.read(4 * 1024 * 1024):
            digest.update(chunk)
    return digest.hexdigest()


def read_json(path: Path) -> Any:
    size = path.stat().st_size
    require(0 < size <= MAX_INPUT_BYTES, f"JSON size is outside bounds: {path}: {size}")
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError) as exc:
        raise DiagnosticError(f"cannot read JSON {path}: {exc}") from exc


def authenticate_artifact(path: Path, expected_path: Path, expected_bytes: int, expected_sha256: str) -> dict[str, Any]:
    resolved = path.resolve()
    require(resolved == expected_path, f"artifact path drift: {resolved} != {expected_path}")
    require(resolved.is_file(), f"artifact is missing: {resolved}")
    observed_bytes = resolved.stat().st_size
    require(observed_bytes == expected_bytes, f"artifact size drift: {resolved}: {observed_bytes} != {expected_bytes}")
    observed_sha256 = sha256_file(resolved)
    require(observed_sha256 == expected_sha256, f"artifact checksum drift: {resolved}")
    return {"path": str(resolved), "bytes": observed_bytes, "sha256": observed_sha256}


def load_reference_driver(path: Path = REFERENCE_DRIVER_PATH) -> tuple[Any, dict[str, Any]]:
    descriptor = authenticate_artifact(path, REFERENCE_DRIVER_PATH, REFERENCE_DRIVER_BYTES, REFERENCE_DRIVER_SHA256)
    require(REFERENCE_MODULE_NAME not in sys.modules, f"owned reference module is already registered: {REFERENCE_MODULE_NAME}")
    spec = importlib.util.spec_from_file_location(REFERENCE_MODULE_NAME, path)
    require(spec is not None and spec.loader is not None, f"cannot construct reference driver module spec: {path}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[REFERENCE_MODULE_NAME] = module
    try:
        spec.loader.exec_module(module)
        require(module.TASK_ID == TASK_ID, "reference driver task identity drift")
        require(module.REFERENCE_METHOD == REFERENCE_METHOD, "reference driver method drift")
        require(module.MODEL_DIMENSIONS == DIMENSIONS, "reference driver dimension drift")
        require(module.EOS_TOKEN_ID == EOS_TOKEN_ID, "reference driver EOS drift")
    except BaseException:
        if sys.modules.get(REFERENCE_MODULE_NAME) is module:
            del sys.modules[REFERENCE_MODULE_NAME]
        raise
    return module, descriptor


def unload_reference_driver(module: Any) -> None:
    require(sys.modules.get(REFERENCE_MODULE_NAME) is module, "owned reference module registration drift")
    del sys.modules[REFERENCE_MODULE_NAME]


def finite_vector(value: Any, label: str) -> list[float]:
    require(isinstance(value, list) and len(value) == DIMENSIONS, f"{label} must contain exactly {DIMENSIONS} coordinates")
    require(all(isinstance(item, (int, float)) and not isinstance(item, bool) and math.isfinite(float(item)) for item in value), f"{label} contains a nonfinite or nonnumeric coordinate")
    return [float(item) for item in value]


def vector_metrics(left: list[float], right: list[float]) -> dict[str, float]:
    require(len(left) == len(right) == DIMENSIONS, "metric vector dimension drift")
    deltas = [candidate - reference for reference, candidate in zip(left, right)]
    left_norm = math.sqrt(math.fsum(value * value for value in left))
    right_norm = math.sqrt(math.fsum(value * value for value in right))
    require(left_norm > 0.0 and right_norm > 0.0, "metric vector has zero norm")
    cosine = math.fsum(a * b for a, b in zip(left, right)) / (left_norm * right_norm)
    return {
        "cosine": cosine,
        "l2_distance": math.sqrt(math.fsum(delta * delta for delta in deltas)),
        "maximum_coordinate_absolute_error": max(abs(delta) for delta in deltas),
        "left_norm": left_norm,
        "right_norm": right_norm,
    }


def select_inputs(probes: Any, retained_reference: Any, failed_tei: Any, validate_probes: Callable[[Any], list[dict[str, Any]]]) -> tuple[dict[str, Any], list[float], list[float]]:
    cases = validate_probes(probes)
    selected = [case for case in cases if case["id"] == CASE_ID]
    require(len(selected) == 1, f"exactly one {CASE_ID} probe is required")
    case = selected[0]
    token_ids = case["token_ids"]
    require(len(token_ids) == 2048, f"{CASE_ID} must contain exactly 2048 token IDs")
    require(token_ids[-1] == EOS_TOKEN_ID and token_ids.count(EOS_TOKEN_ID) == 1, f"{CASE_ID} EOS identity drift")

    require(isinstance(retained_reference, dict) and retained_reference.get("schema_version") == 1, "retained reference schema drift")
    require(retained_reference.get("state") == "passed", "retained reference is not passed")
    require(retained_reference.get("probe_sha256") == EXPECTED_INPUTS["probes"][2], "retained reference probe identity drift")
    runtime = retained_reference.get("runtime")
    require(isinstance(runtime, dict), "retained reference runtime is missing")
    expected_runtime = {
        "reference_method": REFERENCE_METHOD,
        "parameter_dtypes": ["torch.float32"],
        "activation_dtypes": ["torch.float32"],
        "attention_implementation": "eager",
        "is_causal": False,
        "use_cache": False,
        "float32_matmul_precision": "highest",
        "cuda_matmul_allow_tf32": False,
        "cudnn_allow_tf32": False,
        "autocast_enabled": False,
        "torch_compile": False,
    }
    for name, expected in expected_runtime.items():
        require(runtime.get(name) == expected, f"retained reference runtime drift: {name}")
    reference_cases = retained_reference.get("cases")
    require(isinstance(reference_cases, list), "retained reference cases are missing")
    retained = [item for item in reference_cases if isinstance(item, dict) and item.get("id") == CASE_ID]
    require(len(retained) == 1 and retained[0].get("token_ids") == token_ids, "retained reference mixed_long token identity drift")
    retained_vector = finite_vector(retained[0].get("vector"), "retained reference vector")

    require(isinstance(failed_tei, dict) and failed_tei.get("schema_version") == 1, "failed TEI report schema drift")
    require(failed_tei.get("state") == "failed", "TEI report must retain the failed state")
    require(failed_tei.get("probe_sha256") == EXPECTED_INPUTS["probes"][2], "failed TEI probe identity drift")
    candidate_cases = failed_tei.get("candidate_cases")
    require(isinstance(candidate_cases, list), "failed TEI candidate cases are missing")
    candidate = [item for item in candidate_cases if isinstance(item, dict) and item.get("id") == CASE_ID]
    require(len(candidate) == 1 and candidate[0].get("token_ids") == token_ids, "failed TEI mixed_long token identity drift")
    tei_vector = finite_vector(candidate[0].get("vector"), "retained TEI vector")
    return case, retained_vector, tei_vector


def tensor_metadata(name: str, tensor: Any) -> dict[str, Any]:
    return {
        "name": name,
        "shape": [int(value) for value in tensor.shape],
        "numel": int(tensor.numel()),
        "dtype": str(tensor.dtype),
        "device": str(tensor.device),
        "version": int(tensor._version),
    }


def capture_buffers(model: Any) -> tuple[list[dict[str, Any]], list[tuple[str, int, int]]]:
    named = sorted(model.named_buffers(remove_duplicate=True), key=lambda item: item[0])
    names = [name for name, _ in named]
    require(len(names) == len(set(names)), "duplicate buffer names")
    identities = [(name, id(buffer), int(buffer.data_ptr())) for name, buffer in named]
    require(len({identity for _, identity, _ in identities}) == len(identities), "duplicate buffer objects")
    return [tensor_metadata(name, buffer) for name, buffer in named], identities


def require_buffers_unchanged(
    expected_metadata: list[dict[str, Any]],
    expected_identities: list[tuple[str, int, int]],
    observed: tuple[list[dict[str, Any]], list[tuple[str, int, int]]],
) -> None:
    metadata, identities = observed
    require(metadata == expected_metadata, "model buffer metadata or version changed")
    require(identities == expected_identities, "model buffer object or storage changed")


def effective_settings(torch_module: Any) -> dict[str, Any]:
    return {
        "reference_method": REFERENCE_METHOD,
        "float32_matmul_precision": torch_module.get_float32_matmul_precision(),
        "cuda_matmul_allow_tf32": torch_module.backends.cuda.matmul.allow_tf32,
        "cudnn_allow_tf32": torch_module.backends.cudnn.allow_tf32,
        "autocast_enabled": torch_module.is_autocast_enabled("cuda"),
        "torch_compile": False,
    }


def require_settings_unchanged(expected: dict[str, Any], observed: dict[str, Any]) -> None:
    require(observed == expected, f"effective FP32 settings changed: {observed} != {expected}")


def require_cuda_finite_chunks(tensor: Any, torch_module: Any, label: str) -> None:
    flat = tensor.detach().view(-1)
    elements = int(flat.numel())
    require(elements > 0, f"{label} is empty")
    for start in range(0, elements, ROUNDING_CHUNK_ELEMENTS):
        stop = min(elements, start + ROUNDING_CHUNK_ELEMENTS)
        finite_mask = torch_module.isfinite(flat[start:stop])
        finite = bool(finite_mask.all().item())
        del finite_mask
        require(finite, f"{label} is nonfinite")


def require_cuda_fp32_finite(tensor: Any, torch_module: Any, label: str) -> None:
    require(str(tensor.device) == "cuda:0", f"{label} is not on cuda:0: {tensor.device}")
    require(tensor.dtype == torch_module.float32, f"{label} is not FP32: {tensor.dtype}")
    require_cuda_finite_chunks(tensor, torch_module, label)


def require_cuda_f16_finite(tensor: Any, torch_module: Any, label: str) -> None:
    require(str(tensor.device) == "cuda:0", f"{label} is not on cuda:0: {tensor.device}")
    require(tensor.dtype == torch_module.float16, f"{label} is not F16: {tensor.dtype}")
    require_cuda_finite_chunks(tensor, torch_module, label)


def ordered_unique_parameters(model: Any) -> list[tuple[str, Any]]:
    named = sorted(model.named_parameters(remove_duplicate=True), key=lambda item: item[0])
    names = [name for name, _ in named]
    require(len(names) == len(set(names)), "duplicate parameter names")
    require(len({id(parameter) for _, parameter in named}) == len(named), "duplicate parameter objects")
    return named


def round_parameters(model: Any, torch_module: Any, progress: dict[str, Any] | None = None) -> dict[str, Any]:
    named = ordered_unique_parameters(model)
    require(named, "model has no parameters")
    result = {
        "representation": {
            "baseline": "original CUDA FP32 checkpoint values",
            "rounded": "each unique learned CUDA parameter rounded FP32->F16->FP32; inference remains CUDA FP32",
            "buffers_modified": False,
        },
        "parameters": [],
        "summary": {
            "parameter_count": 0,
            "total_elements": 0,
            "total_changed_count": 0,
            "maximum_absolute_delta": 0.0,
            "largest_parameter_name": None,
            "largest_parameter_elements": 0,
            "largest_parameter_round_and_promote_bytes": 0,
            "largest_parameter_finite_check_mask_bytes": 0,
            "largest_parameter_delta_and_finite_check_bytes": 0,
            "largest_parameter_estimated_peak_temporary_bytes": 0,
            "delta_chunk_elements": ROUNDING_CHUNK_ELEMENTS,
            "delta_element_bytes": 4,
            "finite_check_chunk_elements": ROUNDING_CHUNK_ELEMENTS,
            "finite_check_mask_element_bytes": 1,
        },
    }
    if progress is not None:
        progress["parameter_rounding"] = result
    details = result["parameters"]
    total_elements = 0
    total_changed = 0
    maximum_delta = 0.0
    largest_parameter_elements = 0
    largest_parameter_name = None

    with torch_module.no_grad():
        for name, parameter in named:
            require_cuda_fp32_finite(parameter, torch_module, f"parameter {name}")
            elements = int(parameter.numel())
            require(elements > 0, f"parameter is empty: {name}")
            if elements > largest_parameter_elements:
                largest_parameter_elements = elements
                largest_parameter_name = name
            before_version = int(parameter._version)
            rounded_f16 = parameter.detach().to(dtype=torch_module.float16, device="cuda:0", copy=True)
            require_cuda_f16_finite(rounded_f16, torch_module, f"F16 temporary {name}")
            promoted = rounded_f16.to(dtype=torch_module.float32)
            require_cuda_fp32_finite(promoted, torch_module, f"promoted parameter {name}")

            before_flat = parameter.detach().view(-1)
            promoted_flat = promoted.view(-1)
            changed = 0
            item_maximum = 0.0
            for start in range(0, elements, ROUNDING_CHUNK_ELEMENTS):
                stop = min(elements, start + ROUNDING_CHUNK_ELEMENTS)
                delta = promoted_flat[start:stop] - before_flat[start:stop]
                delta.abs_()
                require_cuda_fp32_finite(delta, torch_module, f"rounding delta {name}")
                changed += int(torch_module.count_nonzero(delta).item())
                item_maximum = max(item_maximum, float(delta.max().item()))
                del delta
            parameter.copy_(promoted)
            del before_flat, promoted_flat, promoted, rounded_f16
            require_cuda_fp32_finite(parameter, torch_module, f"rounded parameter {name}")
            details.append({
                **tensor_metadata(name, parameter),
                "version_before": before_version,
                "version_after": int(parameter._version),
                "changed_count": changed,
                "maximum_absolute_delta": item_maximum,
                "f16_temporary_finite": True,
                "promoted_fp32_finite": True,
            })
            total_elements += elements
            total_changed += changed
            maximum_delta = max(maximum_delta, item_maximum)
            result["summary"].update({
                "parameter_count": len(details),
                "total_elements": total_elements,
                "total_changed_count": total_changed,
                "maximum_absolute_delta": maximum_delta,
                "largest_parameter_name": largest_parameter_name,
                "largest_parameter_elements": largest_parameter_elements,
                "largest_parameter_round_and_promote_bytes": largest_parameter_elements * 6,
                "largest_parameter_finite_check_mask_bytes": min(
                    largest_parameter_elements, ROUNDING_CHUNK_ELEMENTS
                ),
                "largest_parameter_delta_and_finite_check_bytes": (
                    min(largest_parameter_elements, ROUNDING_CHUNK_ELEMENTS) * 5
                ),
                "largest_parameter_estimated_peak_temporary_bytes": (
                    largest_parameter_elements * 6
                    + min(largest_parameter_elements, ROUNDING_CHUNK_ELEMENTS) * 5
                ),
            })

    return result


def forward_case(model: Any, case: dict[str, Any], torch_module: Any) -> tuple[dict[str, Any], dict[str, str]]:
    started = time.monotonic()
    token_ids = case["token_ids"]
    with torch_module.inference_mode(), torch_module.autocast(device_type="cuda", enabled=False):
        input_ids = torch_module.tensor([token_ids], dtype=torch_module.long, device="cuda:0")
        attention_mask = torch_module.ones_like(input_ids, dtype=torch_module.long, device="cuda:0")
        position_ids = attention_mask.cumsum(dim=-1) - 1
        position_ids.masked_fill_(attention_mask == 0, 0)
        output = model(
            input_ids=input_ids,
            attention_mask=attention_mask,
            position_ids=position_ids,
            is_causal=False,
            use_cache=False,
            return_dict=True,
        ).last_hidden_state
        require_cuda_fp32_finite(output, torch_module, "last hidden activation")
        pooled = output[:, -1, :]
        require_cuda_fp32_finite(pooled, torch_module, "last-valid pooled activation")
        pooled_norm = torch_module.linalg.vector_norm(pooled, ord=2, dim=1, keepdim=True)
        require_cuda_fp32_finite(pooled_norm, torch_module, "pooled norm")
        require(float(pooled_norm.item()) > 0.0, "pooled norm is zero")
        normalized = pooled / pooled_norm
        require_cuda_fp32_finite(normalized, torch_module, "normalized vector")
        require(tuple(normalized.shape) == (1, DIMENSIONS), f"normalized vector shape drift: {tuple(normalized.shape)}")
        torch_module.cuda.synchronize()
        vector = [float(value) for value in normalized[0].cpu().tolist()]
    norm = math.sqrt(math.fsum(value * value for value in vector))
    require(len(vector) == DIMENSIONS and all(math.isfinite(value) for value in vector), "serialized vector is invalid")
    require(math.isfinite(norm), "serialized norm is nonfinite")
    return ({
        "token_ids": list(token_ids),
        "pooled_norm": float(pooled_norm.item()),
        "vector": vector,
        "norm": norm,
        "elapsed_seconds": time.monotonic() - started,
    }, {"device": str(output.device), "dtype": str(output.dtype)})


def build_comparisons(
    baseline: list[float],
    rounded: list[float],
    retained_reference: list[float],
    retained_tei: list[float],
) -> dict[str, Any]:
    signed = [after - before for before, after in zip(baseline, rounded)]
    index = COORDINATE_INDEX
    before_error = abs(baseline[index] - retained_tei[index])
    after_error = abs(rounded[index] - retained_tei[index])
    return {
        "baseline_vs_rounded": vector_metrics(baseline, rounded),
        "rounded_vs_retained_reference": vector_metrics(retained_reference, rounded),
        "rounded_vs_retained_tei": vector_metrics(retained_tei, rounded),
        "baseline_vs_retained_tei": vector_metrics(retained_tei, baseline),
        "signed_coordinate_deltas": {
            "meaning": "rounded_minus_baseline",
            "values": signed,
        },
        "coordinate_940": {
            "zero_based_index": index,
            "baseline": baseline[index],
            "rounded": rounded[index],
            "retained_reference": retained_reference[index],
            "retained_tei": retained_tei[index],
            "rounded_minus_baseline": signed[index],
            "rounded_minus_retained_reference": rounded[index] - retained_reference[index],
            "rounded_minus_retained_tei": rounded[index] - retained_tei[index],
            "baseline_minus_retained_tei": baseline[index] - retained_tei[index],
            "absolute_tei_error_before": before_error,
            "absolute_tei_error_after": after_error,
            "absolute_tei_error_reduction": before_error - after_error,
        },
    }


def execute_experiment(
    model: Any,
    case: dict[str, Any],
    retained_reference: list[float],
    retained_tei: list[float],
    *,
    forward_fn: Callable[[Any, dict[str, Any]], tuple[dict[str, Any], dict[str, str]]],
    round_fn: Callable[[Any], dict[str, Any]],
    buffer_snapshot_fn: Callable[[Any], tuple[list[dict[str, Any]], list[tuple[str, int, int]]]],
    settings_snapshot_fn: Callable[[], dict[str, Any]],
    expected_settings: dict[str, Any],
    progress: dict[str, Any],
) -> dict[str, Any]:
    buffers_before, buffer_identities = buffer_snapshot_fn(model)
    progress["stage"] = "baseline_forward"
    baseline, baseline_activation = forward_fn(model, case)
    progress["forwards_completed"] += 1
    progress["baseline"] = baseline
    baseline_vector = finite_vector(baseline.get("vector"), "within-run baseline vector")
    require(baseline.get("token_ids") == case["token_ids"], "within-run baseline token identity drift")
    require_buffers_unchanged(buffers_before, buffer_identities, buffer_snapshot_fn(model))
    require_settings_unchanged(expected_settings, settings_snapshot_fn())
    baseline["exact_match_retained_reference"] = baseline_vector == retained_reference
    baseline["metrics_to_retained_reference"] = vector_metrics(retained_reference, baseline_vector)
    if not baseline["exact_match_retained_reference"]:
        return {
            "state": "inconclusive_baseline_drift",
            "baseline": baseline,
            "parameter_rounding": None,
            "rounded": None,
            "comparisons": None,
            "buffers_before": buffers_before,
            "buffers_after": buffers_before,
            "activation_devices": [baseline_activation["device"]],
            "activation_dtypes": [baseline_activation["dtype"]],
        }

    progress["stage"] = "parameter_rounding"
    rounding = round_fn(model)
    require_buffers_unchanged(buffers_before, buffer_identities, buffer_snapshot_fn(model))
    require_settings_unchanged(expected_settings, settings_snapshot_fn())
    progress["stage"] = "rounded_forward"
    rounded, rounded_activation = forward_fn(model, case)
    progress["forwards_completed"] += 1
    progress["rounded"] = rounded
    rounded_vector = finite_vector(rounded.get("vector"), "rounded diagnostic vector")
    require(rounded.get("token_ids") == case["token_ids"], "rounded forward token identity drift")
    buffers_after, after_identities = buffer_snapshot_fn(model)
    require_buffers_unchanged(buffers_before, buffer_identities, (buffers_after, after_identities))
    require_settings_unchanged(expected_settings, settings_snapshot_fn())
    rounding["buffers_before"] = buffers_before
    rounding["buffers_after"] = buffers_after
    rounding["buffers_unchanged"] = True
    comparisons = build_comparisons(baseline_vector, rounded_vector, retained_reference, retained_tei)
    progress["comparisons"] = comparisons
    return {
        "state": "completed_diagnostic",
        "baseline": baseline,
        "parameter_rounding": rounding,
        "rounded": rounded,
        "comparisons": comparisons,
        "buffers_before": buffers_before,
        "buffers_after": buffers_after,
        "activation_devices": sorted({baseline_activation["device"], rounded_activation["device"]}),
        "activation_dtypes": sorted({baseline_activation["dtype"], rounded_activation["dtype"]}),
    }


def empty_report(run_id: str) -> dict[str, Any]:
    return {
        "schema_version": 1,
        "kind": KIND,
        "state": "failed",
        "task_id": TASK_ID,
        "base_sha": BASE_SHA,
        "run_id": run_id,
        "protocol_artifact": None,
        "input_artifacts": None,
        "runtime": None,
        "case": None,
        "baseline": None,
        "parameter_rounding": None,
        "rounded": None,
        "comparisons": None,
        "elapsed_seconds": None,
        "error": None,
    }


def create_new_json(path: Path, value: Any) -> None:
    payload = (json.dumps(value, ensure_ascii=False, indent=2, sort_keys=True) + "\n").encode("utf-8")
    require(len(payload) <= MAX_OUTPUT_BYTES, f"diagnostic output exceeds {MAX_OUTPUT_BYTES} bytes")
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("xb") as handle:
        handle.write(payload)
        handle.flush()
        os.fsync(handle.fileno())


def run_diagnostic(args: argparse.Namespace, report: dict[str, Any], progress: dict[str, Any]) -> None:
    started = time.monotonic()
    progress["stage"] = "validate_paths"
    source = args.source.resolve()
    output = args.output.resolve()
    require(source == EXPECTED_SOURCE, f"model source path drift: {source}")
    require(output == EXPECTED_OUTPUT, f"output path drift: {output}")
    require(not output.exists(), f"output already exists: {output}")

    progress["stage"] = "authenticate_protocol"
    protocol_descriptor = authenticate_artifact(args.protocol, *EXPECTED_PROTOCOL)
    protocol = read_json(args.protocol)
    require(protocol.get("schema_version") == 1 and protocol.get("kind") == "gpu_loaded_weight_rounding_diagnostic_protocol", "protocol schema or kind drift")
    require(protocol.get("identity", {}).get("task_id") == TASK_ID and protocol.get("identity", {}).get("base_sha") == BASE_SHA, "protocol task identity drift")
    report["protocol_artifact"] = protocol_descriptor

    progress["stage"] = "authenticate_inputs"
    descriptors = {}
    values = {}
    for name, path in (
        ("probes", args.probes),
        ("retained_reference", args.reference),
        ("failed_tei_report", args.tei_report),
    ):
        expected_path, expected_bytes, expected_sha = EXPECTED_INPUTS[name]
        descriptors[name] = authenticate_artifact(path, expected_path, expected_bytes, expected_sha)
        values[name] = read_json(path)
    report["input_artifacts"] = descriptors

    reference_driver = None
    original_module = None
    model = None
    try:
        progress["stage"] = "load_reference_driver"
        reference_driver, driver_descriptor = load_reference_driver()
        progress["stage"] = "validate_retained_inputs"
        case, retained_reference, retained_tei = select_inputs(
            values["probes"], values["retained_reference"], values["failed_tei_report"],
            reference_driver.validate_probes,
        )
        report["case"] = {
            "id": case["id"],
            "role": case["role"],
            "utf8_sha256": case["utf8_sha256"],
            "token_ids": list(case["token_ids"]),
        }

        progress["stage"] = "load_pinned_runtime"
        import torch
        import transformers

        require(platform.python_version() == "3.11.9", f"Python version drift: {platform.python_version()}")
        require(torch.__version__ == "2.7.1+cu128", f"Torch version drift: {torch.__version__}")
        require(torch.version.cuda == "12.8", f"Torch CUDA version drift: {torch.version.cuda}")
        require(transformers.__version__ == "4.41.2", f"Transformers version drift: {transformers.__version__}")
        packages = reference_driver.package_inventory()
        model_inventory = reference_driver.validate_model_source(source)
        require(torch.cuda.is_available(), "CUDA is unavailable; CPU diagnostic is forbidden")
        require(torch.cuda.device_count() == 1, f"expected exactly one visible CUDA device, observed {torch.cuda.device_count()}")
        torch.cuda.set_device(0)
        properties = torch.cuda.get_device_properties(0)
        require((properties.major, properties.minor) == (12, 0), "unexpected CUDA compute capability")
        supported_arches = torch.cuda.get_arch_list()
        require("sm_120" in supported_arches, f"Torch binary lacks sm_120 support: {supported_arches}")

        with torch.autocast(device_type="cuda", enabled=False):
            settings = reference_driver.configure_float32_runtime(torch)
            expected_settings = {
                key: settings[key]
                for key in (
                    "reference_method", "float32_matmul_precision", "cuda_matmul_allow_tf32",
                    "cudnn_allow_tf32", "autocast_enabled", "torch_compile",
                )
            }
            require_settings_unchanged(expected_settings, effective_settings(torch))
            smoke = torch.tensor([1.0, 2.0], dtype=torch.float32, device="cuda:0")
            require_cuda_fp32_finite(smoke, torch, "CUDA FP32 smoke tensor")
            require(float((smoke * smoke).sum().item()) == 5.0, "CUDA FP32 smoke operation failed")
            del smoke
            torch.cuda.synchronize()

            progress["stage"] = "load_original_model"
            original_module, model_class, authenticated_identity = reference_driver.load_original_model_class(source)
            model = model_class.from_pretrained(
                str(source),
                local_files_only=True,
                use_safetensors=True,
                torch_dtype=torch.float32,
                attn_implementation="eager",
            ).eval().to("cuda:0")
            progress["model_loads"] = 1
            model_identity = reference_driver.loaded_source_identity(model, reference_driver.MODELING_QWEN_SHA256, "model")
            require(model.__class__ is model_class and model_identity == authenticated_identity, "authenticated original model class identity drift")
            require(model.config.auto_map.get("AutoModel") == "modeling_qwen.Qwen2Model", "model AutoModel mapping drift")
            require(model.config.is_causal is False and model.config._attn_implementation == "eager", "model attention semantics drift")
            parameter_devices = sorted({str(parameter.device) for parameter in model.parameters()})
            parameter_dtypes = sorted({str(parameter.dtype) for parameter in model.parameters()})
            require(parameter_devices == ["cuda:0"] and parameter_dtypes == ["torch.float32"], "model parameters are not exclusively CUDA FP32")
            cuda_inventory = reference_driver.nvidia_inventory()
            require(cuda_inventory["index"] == 0 and cuda_inventory["compute_capability"] == "12.0", "nvidia-smi device identity drift")

            report["runtime"] = {
                "python": platform.python_version(),
                "torch": torch.__version__,
                "torch_cuda": torch.version.cuda,
                "transformers": transformers.__version__,
                "packages": packages,
                "reference_driver": driver_descriptor,
                "model_artifacts": model_inventory,
                "model_class": model_identity,
                "device": cuda_inventory,
                "torch_device_name": properties.name,
                "torch_total_memory_bytes": properties.total_memory,
                "torch_supported_architectures": supported_arches,
                "parameter_devices": parameter_devices,
                "parameter_dtypes": parameter_dtypes,
                "activation_devices": [],
                "activation_dtypes": [],
                "reference_method": settings["reference_method"],
                "smoke_dtype": settings["smoke_dtype"],
                "float32_matmul_precision": settings["float32_matmul_precision"],
                "cuda_matmul_allow_tf32": settings["cuda_matmul_allow_tf32"],
                "cudnn_allow_tf32": settings["cudnn_allow_tf32"],
                "autocast_enabled": settings["autocast_enabled"],
                "torch_compile": settings["torch_compile"],
                "attention_implementation": model.config._attn_implementation,
                "is_causal": False,
                "use_cache": False,
                "model_loads": progress["model_loads"],
                "forwards_completed": progress["forwards_completed"],
            }

            result = execute_experiment(
                model,
                case,
                retained_reference,
                retained_tei,
                forward_fn=lambda current_model, current_case: forward_case(current_model, current_case, torch),
                round_fn=lambda current_model: round_parameters(current_model, torch, progress),
                buffer_snapshot_fn=capture_buffers,
                settings_snapshot_fn=lambda: effective_settings(torch),
                expected_settings=expected_settings,
                progress=progress,
            )
            report["state"] = result["state"]
            report["baseline"] = result["baseline"]
            report["parameter_rounding"] = result["parameter_rounding"]
            report["rounded"] = result["rounded"]
            report["comparisons"] = result["comparisons"]
            report["runtime"]["activation_devices"] = result["activation_devices"]
            report["runtime"]["activation_dtypes"] = result["activation_dtypes"]
            report["runtime"]["model_loads"] = progress["model_loads"]
            report["runtime"]["forwards_completed"] = progress["forwards_completed"]
            report["elapsed_seconds"] = time.monotonic() - started
            progress["stage"] = "completed"
    finally:
        if model is not None:
            del model
        if original_module is not None:
            reference_driver.unload_original_model_module(original_module)
        if "torch" in locals() and torch.cuda.is_available():
            torch.cuda.empty_cache()
        if reference_driver is not None:
            unload_reference_driver(reference_driver)


def main() -> int:
    parser = argparse.ArgumentParser(description="Run the bounded loaded-weight-rounding diagnostic.")
    parser.add_argument("--source", type=Path, required=True)
    parser.add_argument("--probes", type=Path, required=True)
    parser.add_argument("--reference", type=Path, required=True)
    parser.add_argument("--tei-report", type=Path, required=True)
    parser.add_argument("--protocol", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    run_id = os.environ.get(RUN_ID_ENV, "")
    if re.fullmatch(r"[0-9a-f]{32}", run_id) is None:
        print(f"diagnostic failed: invalid or missing {RUN_ID_ENV}", file=sys.stderr)
        return 1
    report = empty_report(run_id)
    progress: dict[str, Any] = {"stage": "startup", "model_loads": 0, "forwards_completed": 0}
    started = time.monotonic()
    try:
        run_diagnostic(args, report, progress)
        create_new_json(args.output.resolve(), report)
        print(json.dumps({"kind": KIND, "state": report["state"], "output": str(args.output), "sha256": sha256_file(args.output)}, sort_keys=True))
        return 0
    except Exception as exc:
        report["state"] = "failed"
        report["elapsed_seconds"] = time.monotonic() - started
        if isinstance(report.get("runtime"), dict):
            report["runtime"]["model_loads"] = progress["model_loads"]
            report["runtime"]["forwards_completed"] = progress["forwards_completed"]
        for name in ("baseline", "parameter_rounding", "rounded", "comparisons"):
            if report.get(name) is None and progress.get(name) is not None:
                report[name] = progress[name]
        report["error"] = {"stage": progress["stage"], "type": type(exc).__name__, "message": str(exc)}
        try:
            output = args.output.resolve()
            if not output.exists():
                create_new_json(output, report)
        except Exception as output_exc:
            print(f"diagnostic failure report could not be preserved: {type(output_exc).__name__}: {output_exc}", file=sys.stderr)
        print(f"diagnostic failed: {type(exc).__name__}: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
