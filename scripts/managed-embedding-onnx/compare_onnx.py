from __future__ import annotations

import argparse
import json
import time
from collections import Counter
from pathlib import Path
from typing import Any

import numpy as np
import onnxruntime as ort

from common import load_json, sha256_file, write_json


THRESHOLDS_SHA256 = "3ee04181139c7a72623bd7e42f011555321e61f05b96ba52ce942f2b0a12319e"


def metrics(reference: np.ndarray, candidate: np.ndarray, limits: dict[str, float]) -> dict[str, Any]:
    difference = np.abs(candidate.astype(np.float64) - reference.astype(np.float64))
    allowed = limits["absolute_tolerance"] + limits["relative_tolerance"] * np.abs(reference)
    relative = difference / np.maximum(np.abs(reference), 1e-6)
    result: dict[str, Any] = {
        "allclose": bool(np.all(difference <= allowed)),
        "max_absolute_error": float(difference.max(initial=0.0)),
        "max_relative_error_diagnostic": float(relative.max(initial=0.0)),
    }
    if "max_cosine_distance" in limits:
        distances = []
        for left, right in zip(reference.reshape((-1, reference.shape[-1])), candidate.reshape((-1, candidate.shape[-1]))):
            denominator = np.linalg.norm(left) * np.linalg.norm(right)
            distances.append(1.0 - np.dot(left, right) / denominator)
        result["max_cosine_distance"] = float(max(distances, default=0.0))
        result["cosine_pass"] = result["max_cosine_distance"] <= limits["max_cosine_distance"]
    result["pass"] = result["allclose"] and result.get("cosine_pass", True)
    return result


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--graph", type=Path, required=True)
    parser.add_argument("--oracle", type=Path, required=True)
    parser.add_argument("--thresholds", type=Path, required=True)
    parser.add_argument("--report", type=Path, required=True)
    parser.add_argument("--threads", type=int, default=8)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if sha256_file(args.thresholds) != THRESHOLDS_SHA256:
        raise ValueError("threshold file differs from the pre-acceptance frozen hash")
    thresholds = load_json(args.thresholds)["comparisons"]
    oracle = np.load(args.oracle, allow_pickle=False)
    options = ort.SessionOptions()
    options.intra_op_num_threads = args.threads
    options.inter_op_num_threads = 1
    options.graph_optimization_level = ort.GraphOptimizationLevel.ORT_ENABLE_ALL
    options.enable_profiling = True
    options.profile_file_prefix = str(args.report.parent / "ort-profile")
    started = time.monotonic()
    session = ort.InferenceSession(args.graph, sess_options=options, providers=["CPUExecutionProvider"])
    session_load_seconds = time.monotonic() - started
    inference_started = time.monotonic()
    output = session.run(
        ["last_hidden_state"],
        {"input_ids": oracle["input_ids"], "attention_mask": oracle["attention_mask"]},
    )[0]
    left_inference_seconds = time.monotonic() - inference_started
    mask = oracle["attention_mask"]
    left_padding = bool(mask[:, -1].sum() == mask.shape[0])
    if left_padding:
        pooled = output[:, -1]
    else:
        sequence_lengths = mask.sum(axis=1) - 1
        pooled = output[np.arange(output.shape[0]), sequence_lengths]
    normalized = pooled / np.linalg.norm(pooled, axis=1, keepdims=True)
    comparisons = {
        "hidden_state": metrics(oracle["last_hidden_state"], output, thresholds["hidden_state"]),
        "pooled_raw": metrics(oracle["pooled_raw"], pooled, thresholds["pooled_raw"]),
        "normalized": metrics(oracle["normalized"], normalized, thresholds["normalized"]),
    }
    right_ids = np.full_like(oracle["input_ids"], 151643)
    right_mask = np.zeros_like(mask)
    right_positions = []
    for index in range(mask.shape[0]):
        real_ids = oracle["input_ids"][index][mask[index].astype(bool)]
        length = len(real_ids)
        right_ids[index, :length] = real_ids
        right_mask[index, :length] = 1
        right_positions.append(
            {
                "id": str(oracle["item_ids"][index]),
                "real_token_count": length,
                "implicit_real_position_start": 0,
                "implicit_real_position_end": length - 1,
                "padding_position_start": length if length < mask.shape[1] else None,
            }
        )
    right_started = time.monotonic()
    right_output = session.run(
        ["last_hidden_state"],
        {"input_ids": right_ids, "attention_mask": right_mask},
    )[0]
    right_inference_seconds = time.monotonic() - right_started
    right_lengths = right_mask.sum(axis=1) - 1
    right_pooled = right_output[np.arange(right_output.shape[0]), right_lengths]
    right_normalized = right_pooled / np.linalg.norm(right_pooled, axis=1, keepdims=True)
    right_comparisons = {
        "pooled_raw": metrics(oracle["pooled_raw"], right_pooled, thresholds["pooled_raw"]),
        "normalized": metrics(oracle["normalized"], right_normalized, thresholds["normalized"]),
    }
    profile_path = Path(session.end_profiling())
    profile = json.loads(profile_path.read_text(encoding="utf-8"))
    kernel_events = [
        event
        for event in profile
        if event.get("cat") == "Node" and event.get("args", {}).get("provider")
    ]
    kernel_operators = Counter(
        str(event.get("args", {}).get("op_name", "unknown")) for event in kernel_events
    )
    kernel_providers = Counter(
        str(event.get("args", {}).get("provider", "unreported")) for event in kernel_events
    )
    profile_path.unlink()
    active_providers = session.get_providers()
    provider_pass = active_providers == ["CPUExecutionProvider"] and set(kernel_providers) == {
        "CPUExecutionProvider"
    }
    write_json(
        args.report,
        {
            "schema_version": 1,
            "thresholds_sha256": THRESHOLDS_SHA256,
            "providers_requested": ["CPUExecutionProvider"],
            "providers_active": active_providers,
            "provider_options": session.get_provider_options(),
            "provider_pass": provider_pass,
            "graph_optimization": "ORT_ENABLE_ALL",
            "profiled_kernel_events": len(kernel_events),
            "profiled_kernel_operator_counts": dict(sorted(kernel_operators.items())),
            "profiled_kernel_provider_counts": dict(sorted(kernel_providers.items())),
            "threads": args.threads,
            "oracle_padding_side_detected": "left" if left_padding else "right",
            "output_shape": list(output.shape),
            "all_finite": bool(np.isfinite(output).all()),
            "comparisons": comparisons,
            "right_padded_tei_ort_path": {
                "source_semantics": "Pinned TEI ORT uses right padding when tokenizer_config.json has no padding_side. This graph has no position_ids input and derives row-wise arange positions internally.",
                "positions": right_positions,
                "comparisons": right_comparisons,
                "all_finite": bool(np.isfinite(right_output).all()),
                "inference_seconds": right_inference_seconds,
                "pass": bool(
                    np.isfinite(right_output).all()
                    and all(value["pass"] for value in right_comparisons.values())
                ),
            },
            "pass": bool(
                provider_pass
                and np.isfinite(output).all()
                and all(value["pass"] for value in comparisons.values())
                and np.isfinite(right_output).all()
                and all(value["pass"] for value in right_comparisons.values())
            ),
            "session_load_seconds": session_load_seconds,
            "left_padded_inference_seconds": left_inference_seconds,
            "elapsed_seconds": time.monotonic() - started,
        },
    )
    if (
        not provider_pass
        or not all(value["pass"] for value in comparisons.values())
        or not all(value["pass"] for value in right_comparisons.values())
    ):
        raise ValueError("ONNX output failed the frozen numerical thresholds")


if __name__ == "__main__":
    main()
