from __future__ import annotations

import argparse
import concurrent.futures
import json
import math
import time
import urllib.error
import urllib.request
from pathlib import Path
from typing import Any

import numpy as np
from transformers import AutoTokenizer

from common import clean_offline_environment, load_json, prepared_text, sha256_file, write_json


THRESHOLDS_SHA256 = "3ee04181139c7a72623bd7e42f011555321e61f05b96ba52ce942f2b0a12319e"
MODEL_REVISION = "1cad2ab3ff41c2671f34e135d29831368ee26b68"
SERVED_MODEL_NAME = "Alibaba-NLP/gte-Qwen2-1.5B-instruct"
OVER_LIMIT_ERROR = "`inputs` must have less than 32768 tokens. Given: 32769"
MAX_CLIENT_BATCH_SIZE = 8


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--url", default="http://127.0.0.1:80")
    parser.add_argument("--metrics-url", default="http://127.0.0.1:9000")
    parser.add_argument("--expected-model-id", default="/model")
    parser.add_argument("--expected-served-model-name", default=SERVED_MODEL_NAME)
    parser.add_argument("--expected-model-sha", default=MODEL_REVISION)
    parser.add_argument("--source", type=Path, required=True)
    parser.add_argument("--corpus", type=Path, required=True)
    parser.add_argument("--thresholds", type=Path, required=True)
    parser.add_argument("--oracle", type=Path, required=True)
    parser.add_argument("--report", type=Path, required=True)
    parser.add_argument("--startup-timeout-seconds", type=float, default=1800)
    parser.add_argument("--request-timeout-seconds", type=float, default=1800)
    return parser.parse_args()


def request(url: str, path: str, body: dict[str, Any] | None, timeout: float) -> tuple[int, Any, float]:
    data = None if body is None else json.dumps(body, ensure_ascii=False).encode("utf-8")
    headers = {} if body is None else {"Content-Type": "application/json"}
    started = time.monotonic()
    try:
        with urllib.request.urlopen(
            urllib.request.Request(url + path, data=data, headers=headers), timeout=timeout
        ) as response:
            payload = response.read()
            return response.status, json.loads(payload), time.monotonic() - started
    except urllib.error.HTTPError as error:
        payload = error.read().decode("utf-8", errors="replace")[:2048]
        try:
            decoded: Any = json.loads(payload)
        except json.JSONDecodeError:
            decoded = {"error": payload}
        return error.code, decoded, time.monotonic() - started


def compare(reference: np.ndarray, candidate: np.ndarray, limits: dict[str, float]) -> dict[str, Any]:
    difference = np.abs(candidate.astype(np.float64) - reference.astype(np.float64))
    allowed = limits["absolute_tolerance"] + limits["relative_tolerance"] * np.abs(reference)
    denominator = np.linalg.norm(reference) * np.linalg.norm(candidate)
    cosine = float(1.0 - np.dot(reference, candidate) / denominator)
    cosine_limit = limits.get("max_cosine_distance")
    cosine_pass = cosine_limit is None or cosine <= cosine_limit
    return {
        "allclose": bool(np.all(difference <= allowed)),
        "max_absolute_error": float(difference.max(initial=0.0)),
        "max_relative_error_diagnostic": float((difference / np.maximum(np.abs(reference), 1e-6)).max(initial=0.0)),
        "cosine_distance": cosine,
        "pass": bool(np.all(difference <= allowed) and cosine_pass),
    }


def exact_repeat_text(tokenizer: Any, count: int) -> str:
    for unit in [" a", " x", " !", "\n"]:
        probe = unit * 32
        if len(tokenizer.encode(probe, add_special_tokens=False)) == 32:
            value = unit * count
            if len(tokenizer.encode(value, add_special_tokens=False)) == count:
                return value
    raise ValueError("no stable one-token repeat unit found in pinned tokenizer")


def request_embedding_batches(
    url: str,
    texts: list[str],
    normalize: bool,
    timeout: float,
) -> tuple[list[Any], list[float]]:
    vectors: list[Any] = []
    latencies: list[float] = []
    for start in range(0, len(texts), MAX_CLIENT_BATCH_SIZE):
        batch = texts[start : start + MAX_CLIENT_BATCH_SIZE]
        status, body, latency = request(
            url,
            "/embed",
            {"inputs": batch, "truncate": False, "normalize": normalize},
            timeout,
        )
        if status != 200 or not isinstance(body, list) or len(body) != len(batch):
            raise ValueError(
                f"short /embed batch failed: normalize={normalize}, "
                f"start={start}, size={len(batch)}, status={status}"
            )
        vectors.extend(body)
        latencies.append(latency)
    return vectors, latencies


def tokenize_ids(body: Any, expected_count: int) -> list[int]:
    if not isinstance(body, list) or len(body) != 1 or not isinstance(body[0], list):
        raise ValueError("TEI /tokenize response did not contain exactly one token list")
    token_ids = [token.get("id") for token in body[0] if isinstance(token, dict)]
    if len(token_ids) != expected_count or token_ids[-1] != 151643:
        raise ValueError(
            f"TEI /tokenize did not prove {expected_count} tokens with final EOS 151643"
        )
    return token_ids


def is_over_limit_embed_rejection(status: int, body: Any) -> bool:
    return bool(
        status == 422
        and isinstance(body, dict)
        and body.get("error") == OVER_LIMIT_ERROR
        and body.get("error_type") in {"tokenizer", "validation"}
    )


def main() -> None:
    args = parse_args()
    clean_offline_environment()
    if sha256_file(args.thresholds) != THRESHOLDS_SHA256:
        raise ValueError("threshold file differs from the pre-acceptance frozen hash")
    limits = load_json(args.thresholds)["comparisons"]
    corpus = load_json(args.corpus)
    items = corpus["calibration"] + corpus["acceptance"]
    texts = [prepared_text(item, corpus["query_prompt"]) for item in items]
    oracle = np.load(args.oracle, allow_pickle=False)

    deadline = time.monotonic() + args.startup_timeout_seconds
    info = None
    attempts = 0
    while time.monotonic() < deadline:
        attempts += 1
        try:
            status, candidate, _ = request(args.url, "/info", None, 2)
            if status == 200:
                info = candidate
                break
        except (OSError, ValueError):
            pass
        time.sleep(1)
    if info is None:
        raise TimeoutError("TEI did not become ready before the bounded startup deadline")
    info_checks = {
        "model_id": info.get("model_id") == args.expected_model_id,
        "served_model_name": info.get("served_model_name") == args.expected_served_model_name,
        "model_sha": info.get("model_sha") == args.expected_model_sha,
        "dtype": info.get("model_dtype") == "float32",
        "max_input_length": info.get("max_input_length") == 32768,
        "max_batch_tokens": int(info.get("max_batch_tokens", 0)) >= 32768,
        "auto_truncate": info.get("auto_truncate") is False,
        "last_token_pooling": info.get("model_type") == {"embedding": {"pooling": "last_token"}},
    }

    raw_body, raw_latencies = request_embedding_batches(
        args.url, texts, False, args.request_timeout_seconds
    )
    normalized_body, normalized_latencies = request_embedding_batches(
        args.url, texts, True, args.request_timeout_seconds
    )
    raw = np.asarray(raw_body, dtype=np.float32)
    normalized = np.asarray(normalized_body, dtype=np.float32)
    if raw.shape != (len(items), 1536) or normalized.shape != (len(items), 1536):
        raise ValueError(f"invalid TEI short output shapes: raw={raw.shape}, normalized={normalized.shape}")
    comparisons = {
        item["id"]: {
            "pooled_raw": compare(oracle["pooled_raw"][index], raw[index], limits["pooled_raw"]),
            "normalized": compare(oracle["normalized"][index], normalized[index], limits["normalized"]),
        }
        for index, item in enumerate(items)
    }

    id_to_index = {item["id"]: index for index, item in enumerate(items)}
    scheduled = [item_id for batch in corpus["scheduled_batches"] for item_id in batch]
    concurrent_started = time.monotonic()

    def embed_one(item_id: str) -> dict[str, Any]:
        index = id_to_index[item_id]
        status, body, latency = request(
            args.url,
            "/embed",
            {"inputs": texts[index], "truncate": False, "normalize": True},
            args.request_timeout_seconds,
        )
        if status != 200:
            return {"id": item_id, "status": status, "latency_seconds": latency, "pass": False}
        vector = np.asarray(body[0], dtype=np.float32)
        result = compare(oracle["normalized"][index], vector, limits["batch_invariance"])
        return {"id": item_id, "status": status, "latency_seconds": latency, **result}

    with concurrent.futures.ThreadPoolExecutor(max_workers=12) as executor:
        concurrent_results = list(executor.map(embed_one, scheduled))
    concurrent_elapsed = time.monotonic() - concurrent_started

    tokenizer = AutoTokenizer.from_pretrained(args.source, trust_remote_code=True, local_files_only=True)
    maximum_text = exact_repeat_text(tokenizer, 32767)
    overlimit_text = exact_repeat_text(tokenizer, 32768)
    if len(tokenizer.encode(maximum_text, add_special_tokens=True)) != 32768:
        raise ValueError("full-context synthetic input did not produce exactly 32768 tokens including EOS")
    if len(tokenizer.encode(overlimit_text, add_special_tokens=True)) != 32769:
        raise ValueError("over-limit synthetic input did not produce exactly 32769 tokens including EOS")
    full_tokenize_status, full_tokens_body, full_tokenize_latency = request(
        args.url,
        "/tokenize",
        {"inputs": maximum_text, "add_special_tokens": True},
        args.request_timeout_seconds,
    )
    if full_tokenize_status != 200:
        raise ValueError(f"TEI full-context /tokenize failed: status={full_tokenize_status}")
    full_tei_token_ids = tokenize_ids(full_tokens_body, 32768)
    full_status, full_body, full_latency = request(
        args.url,
        "/embed",
        {"inputs": maximum_text, "truncate": False, "normalize": True},
        args.request_timeout_seconds,
    )
    full_vector = np.asarray(full_body[0], dtype=np.float32) if full_status == 200 else np.asarray([])
    over_status, over_body, over_latency = request(
        args.url,
        "/embed",
        {"inputs": overlimit_text, "truncate": False, "normalize": True},
        min(args.request_timeout_seconds, 60),
    )
    over_tokenize_status, over_tokenize_body, over_tokenize_latency = request(
        args.url,
        "/tokenize",
        {"inputs": overlimit_text, "add_special_tokens": True},
        min(args.request_timeout_seconds, 60),
    )
    if over_tokenize_status != 200:
        raise ValueError(f"TEI over-limit /tokenize failed: status={over_tokenize_status}")
    over_tei_token_ids = tokenize_ids(over_tokenize_body, 32769)

    metrics_lines: list[str] = []
    try:
        with urllib.request.urlopen(args.metrics_url + "/metrics", timeout=5) as response:
            for line in response.read().decode("utf-8", errors="replace").splitlines():
                if line.startswith(("te_batch", "te_request", "te_queue", "te_inference")):
                    metrics_lines.append(line)
    except OSError:
        pass

    all_short_pass = all(
        result["pooled_raw"]["pass"] and result["normalized"]["pass"] for result in comparisons.values()
    )
    full_pass = full_status == 200 and full_vector.shape == (1536,) and all(math.isfinite(float(v)) for v in full_vector)
    overlimit_pass = is_over_limit_embed_rejection(over_status, over_body)
    write_json(
        args.report,
        {
            "schema_version": 1,
            "thresholds_sha256": THRESHOLDS_SHA256,
            "readiness": {"attempts": attempts, "info": info, "checks": info_checks},
            "model_revision_reporting": {
                "observed_model_sha": info.get("model_sha"),
                "expected_revision": args.expected_model_sha,
                "local_path_reports_expected_revision": info.get("model_sha") == args.expected_model_sha,
            },
            "short": {
                "client_batch_ceiling": MAX_CLIENT_BATCH_SIZE,
                "raw_batch_latency_seconds": raw_latencies,
                "normalized_batch_latency_seconds": normalized_latencies,
                "raw_latency_seconds": sum(raw_latencies),
                "normalized_latency_seconds": sum(normalized_latencies),
                "shape": list(normalized.shape),
                "finite": bool(np.isfinite(raw).all() and np.isfinite(normalized).all()),
                "comparisons": comparisons,
                "pass": all_short_pass,
            },
            "concurrent_scheduler": {
                "request_count": len(scheduled),
                "client_workers": 12,
                "backend_source_batch_ceiling": 8,
                "elapsed_seconds": concurrent_elapsed,
                "requests_per_second": len(scheduled) / concurrent_elapsed,
                "results": concurrent_results,
                "metrics": metrics_lines,
                "pass": all(result["pass"] for result in concurrent_results),
            },
            "full_context": {
                "input_tokens_including_eos": 32768,
                "tei_tokenize": {
                    "status": full_tokenize_status,
                    "token_count_including_eos": len(full_tei_token_ids),
                    "final_token_id": full_tei_token_ids[-1],
                    "latency_seconds": full_tokenize_latency,
                },
                "status": full_status,
                "latency_seconds": full_latency,
                "output_shape": list(full_vector.shape),
                "finite": bool(full_vector.size and np.isfinite(full_vector).all()),
                "pass": full_pass,
            },
            "over_limit": {
                "input_tokens_including_eos": 32769,
                "status": over_status,
                "latency_seconds": over_latency,
                "sanitized_response": over_body,
                "tei_tokenize": {
                    "status": over_tokenize_status,
                    "latency_seconds": over_tokenize_latency,
                    "token_count_including_eos": len(over_tei_token_ids),
                    "final_token_id": over_tei_token_ids[-1],
                },
                "pass": overlimit_pass,
            },
            "hmem_admission_contract": {
                "maximum_formatted_utf8_bytes": 32767,
                "first_rejected_formatted_utf8_bytes": 32768,
                "local_truncation": False,
                "distinct_from_direct_tei_token_limit": True,
            },
            "pass": bool(all(info_checks.values()) and all_short_pass and all(result["pass"] for result in concurrent_results) and full_pass and overlimit_pass),
        },
    )


if __name__ == "__main__":
    main()
