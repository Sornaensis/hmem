from __future__ import annotations

import argparse
import inspect
import platform
import time
from pathlib import Path
from typing import Any

import numpy as np
import torch
import transformers
from transformers import AutoModel, AutoTokenizer

from common import (
    clean_offline_environment,
    last_token_pool,
    load_json,
    permit_guarded_flash_attention_import,
    PINNED_MODELING_SHA256,
    prepared_text,
    sha256_file,
    verify_source_snapshot,
    write_json,
)


THRESHOLDS_SHA256 = "3ee04181139c7a72623bd7e42f011555321e61f05b96ba52ce942f2b0a12319e"
SPACE_FINGERPRINT = "Alibaba-NLP/gte-Qwen2-1.5B-instruct@1cad2ab3ff41c2671f34e135d29831368ee26b68:1536"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", type=Path, required=True)
    parser.add_argument("--corpus", type=Path, required=True)
    parser.add_argument("--thresholds", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--threads", type=int, default=8)
    return parser.parse_args()


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
        left = reference.reshape(-1).astype(np.float64)
        right = candidate.reshape(-1).astype(np.float64)
        denominator = np.linalg.norm(left) * np.linalg.norm(right)
        result["cosine_distance"] = float(1.0 - np.dot(left, right) / denominator)
        result["cosine_pass"] = result["cosine_distance"] <= limits["max_cosine_distance"]
    result["pass"] = result["allclose"] and result.get("cosine_pass", True)
    return result


def attention_delta(
    causal_hidden: np.ndarray,
    noncausal_hidden: np.ndarray,
    causal_normalized: np.ndarray,
    noncausal_normalized: np.ndarray,
    items: list[dict[str, str]],
) -> dict[str, Any]:
    per_item: dict[str, Any] = {}
    for index, item in enumerate(items):
        left = causal_normalized[index].astype(np.float64)
        right = noncausal_normalized[index].astype(np.float64)
        denominator = np.linalg.norm(left) * np.linalg.norm(right)
        per_item[item["id"]] = {
            "hidden_state_max_absolute_difference": float(
                np.max(np.abs(causal_hidden[index].astype(np.float64) - noncausal_hidden[index].astype(np.float64)))
            ),
            "normalized_vector_max_absolute_difference": float(np.max(np.abs(left - right))),
            "normalized_vector_cosine_distance": float(1.0 - np.dot(left, right) / denominator),
        }
    return {
        "purpose": "diagnostic only; this does not select, relabel, or establish parity with the native TEI GPU space",
        "same_input_ids_attention_mask_and_positions": True,
        "per_item": per_item,
    }


def main() -> None:
    args = parse_args()
    clean_offline_environment()
    if sha256_file(args.thresholds) != THRESHOLDS_SHA256:
        raise ValueError("threshold file differs from the pre-acceptance frozen hash")
    thresholds = load_json(args.thresholds)["comparisons"]
    corpus = load_json(args.corpus)
    items = corpus["calibration"] + corpus["acceptance"]
    texts = [prepared_text(item, corpus["query_prompt"]) for item in items]
    source_evidence = verify_source_snapshot(args.source)
    output = args.output.resolve(strict=True)
    if any(output.iterdir()):
        raise ValueError("output directory must be empty")
    torch.set_num_threads(args.threads)
    torch.set_num_interop_threads(1)
    torch.manual_seed(1729)
    torch.use_deterministic_algorithms(True)

    started = time.monotonic()
    tokenizer = AutoTokenizer.from_pretrained(
        args.source,
        trust_remote_code=True,
        local_files_only=True,
    )
    with permit_guarded_flash_attention_import():
        model = AutoModel.from_pretrained(
            args.source,
            trust_remote_code=True,
            local_files_only=True,
            torch_dtype=torch.float32,
        ).eval()
    if sha256_file(Path(inspect.getfile(model.__class__))) != PINNED_MODELING_SHA256:
        raise ValueError("executed remote modeling code does not match the pinned source hash")
    causal_default = inspect.signature(model.forward).parameters["is_causal"].default
    if causal_default is not True:
        raise ValueError(f"unexpected AutoModel is_causal default: {causal_default!r}")
    encoded = tokenizer(
        texts,
        padding=True,
        truncation=False,
        add_special_tokens=True,
        return_tensors="pt",
    )
    token_evidence = []
    for index, item in enumerate(items):
        mask = encoded["attention_mask"][index].bool()
        real_ids = encoded["input_ids"][index][mask].tolist()
        if not real_ids or real_ids[-1] != 151643 or real_ids.count(151643) != 1:
            raise ValueError(f"EOS 151643 was not appended exactly once for {item['id']}")
        token_evidence.append(
            {
                "id": item["id"],
                "kind": item["kind"],
                "formatted_utf8_bytes": len(texts[index].encode("utf-8")),
                "token_count_including_eos": len(real_ids),
                "token_ids": real_ids,
                "padded_sequence_length": int(encoded["input_ids"].shape[1]),
                "implicit_real_position_start": int(torch.nonzero(mask, as_tuple=False)[0].item()),
                "implicit_real_position_end": int(torch.nonzero(mask, as_tuple=False)[-1].item()),
            }
        )

    with torch.inference_mode():
        default_cache = model(**encoded).last_hidden_state
        first = model(**encoded, use_cache=False, is_causal=True).last_hidden_state
        second = model(**encoded, use_cache=False, is_causal=True).last_hidden_state
        noncausal = model(**encoded, use_cache=False, is_causal=False).last_hidden_state
        pooled = last_token_pool(first, encoded["attention_mask"])
        normalized = torch.nn.functional.normalize(pooled, p=2, dim=1)
        noncausal_pooled = last_token_pool(noncausal, encoded["attention_mask"])
        noncausal_normalized = torch.nn.functional.normalize(noncausal_pooled, p=2, dim=1)
        singles: dict[str, np.ndarray] = {}
        for item, text in zip(items, texts):
            single_encoded = tokenizer(
                text,
                padding=False,
                truncation=False,
                add_special_tokens=True,
                return_tensors="pt",
            )
            single_hidden = model(**single_encoded, use_cache=False).last_hidden_state
            single_pooled = last_token_pool(single_hidden, single_encoded["attention_mask"])
            singles[item["id"]] = torch.nn.functional.normalize(single_pooled, p=2, dim=1)[0].numpy()

    first_np = first.numpy()
    second_np = second.numpy()
    default_np = default_cache.numpy()
    pooled_np = pooled.numpy()
    normalized_np = normalized.numpy()
    noncausal_np = noncausal.numpy()
    noncausal_normalized_np = noncausal_normalized.numpy()
    repeat_metrics = metrics(first_np, second_np, thresholds["reference_repeat"])
    cache_metrics = metrics(first_np, default_np, thresholds["reference_repeat"])
    invariance = {
        item["id"]: metrics(normalized_np[index], singles[item["id"]], thresholds["batch_invariance"])
        for index, item in enumerate(items)
    }
    if not repeat_metrics["pass"] or not cache_metrics["pass"] or not all(value["pass"] for value in invariance.values()):
        raise ValueError("independent reference determinism or batch invariance failed frozen thresholds")
    if normalized_np.shape != (len(items), 1536) or not np.isfinite(normalized_np).all():
        raise ValueError(f"invalid oracle vector shape/values: {normalized_np.shape}")

    np.savez_compressed(
        output / "oracle.npz",
        input_ids=encoded["input_ids"].numpy(),
        attention_mask=encoded["attention_mask"].numpy(),
        last_hidden_state=first_np,
        pooled_raw=pooled_np,
        normalized=normalized_np,
        item_ids=np.asarray([item["id"] for item in items]),
    )
    write_json(
        output / "oracle-report.json",
        {
            "schema_version": 1,
            "source": source_evidence,
            "space_fingerprint": SPACE_FINGERPRINT,
            "space_fingerprint_status": "legacy attention-unqualified identity; candidate is not promoted",
            "thresholds_sha256": THRESHOLDS_SHA256,
            "transformers": transformers.__version__,
            "torch": torch.__version__,
            "python": platform.python_version(),
            "attention_implementation": getattr(model, "_attn_implementation", None),
            "config_is_causal_metadata": bool(getattr(model.config, "is_causal", False)),
            "forward_is_causal_default": causal_default,
            "published_call_effective_is_causal": True,
            "oracle_acceptance_call": "model(**encoded, use_cache=False, is_causal=True)",
            "causal_true_vs_false": attention_delta(
                first_np,
                noncausal_np,
                normalized_np,
                noncausal_normalized_np,
                items,
            ),
            "padding_side": tokenizer.padding_side,
            "position_semantics": "No position_ids were supplied. The pinned custom model derives row-wise arange(0,padded_sequence_length); token evidence records the real-token positions selected by each left-padded attention mask.",
            "eos_token_id": tokenizer.eos_token_id,
            "pad_token_id": tokenizer.pad_token_id,
            "tokens": token_evidence,
            "reference_repeat": repeat_metrics,
            "default_cache_vs_cache_false": cache_metrics,
            "batch_invariance": invariance,
            "vector_shape": list(normalized_np.shape),
            "all_vectors_finite": bool(np.isfinite(normalized_np).all()),
            "unit_norm_max_error": float(np.max(np.abs(np.linalg.norm(normalized_np, axis=1) - 1.0))),
            "elapsed_seconds": time.monotonic() - started,
            "artifacts": [
                {
                    "path": "oracle.npz",
                    "size": (output / "oracle.npz").stat().st_size,
                    "sha256": sha256_file(output / "oracle.npz"),
                }
            ],
        },
    )


if __name__ == "__main__":
    main()
