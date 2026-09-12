from __future__ import annotations

import math
from typing import Any

from gpu_contract import ContractError


def validate_vector(vector: Any, dimensions: int, norm_error: float) -> list[float]:
    if not isinstance(vector, list) or len(vector) != dimensions:
        raise ContractError(f"vector must contain exactly {dimensions} coordinates")
    values = [float(value) for value in vector]
    if any(isinstance(value, bool) or not math.isfinite(number) for value, number in zip(vector, values)):
        raise ContractError("vector coordinates must be finite numbers")
    norm = math.sqrt(math.fsum(value * value for value in values))
    if abs(norm - 1.0) > norm_error:
        raise ContractError(f"vector unit-norm drift: {norm}")
    return values


def metrics(reference: list[float], candidate: list[float]) -> dict[str, float]:
    reference_norm = math.sqrt(math.fsum(value * value for value in reference))
    candidate_norm = math.sqrt(math.fsum(value * value for value in candidate))
    if reference_norm == 0.0 or candidate_norm == 0.0:
        raise ContractError("cosine is undefined for a zero-norm vector")
    cosine = math.fsum(left * right for left, right in zip(reference, candidate)) / (reference_norm * candidate_norm)
    deltas = [left - right for left, right in zip(reference, candidate)]
    return {
        "cosine": cosine,
        "l2_distance": math.sqrt(math.fsum(delta * delta for delta in deltas)),
        "maximum_coordinate_absolute_error": max(abs(delta) for delta in deltas),
    }


def accepted(observed: dict[str, float], thresholds: dict[str, Any]) -> bool:
    return (
        observed["cosine"] >= thresholds["minimum_cosine"]
        and observed["l2_distance"] <= thresholds["maximum_l2_distance"]
        and observed["maximum_coordinate_absolute_error"] <= thresholds["maximum_coordinate_absolute_error"]
    )


def compare_cases(
    reference_cases: list[dict[str, Any]],
    candidate_cases: list[dict[str, Any]],
    thresholds: dict[str, Any],
) -> list[dict[str, Any]]:
    dimensions = thresholds["dimensions"]
    norm_error = thresholds["maximum_unit_norm_absolute_error"]
    references = {case.get("id"): case for case in reference_cases}
    candidates = {case.get("id"): case for case in candidate_cases}
    if len(references) != len(reference_cases) or len(candidates) != len(candidate_cases):
        raise ContractError("case IDs must be unique")
    if references.keys() != candidates.keys():
        raise ContractError("reference and candidate case IDs differ")
    results = []
    for case_id in references:
        reference = references[case_id]
        candidate = candidates[case_id]
        if reference.get("token_ids") != candidate.get("token_ids"):
            raise ContractError(f"token IDs differ for {case_id}")
        reference_vector = validate_vector(reference.get("vector"), dimensions, norm_error)
        candidate_vector = validate_vector(candidate.get("vector"), dimensions, norm_error)
        observed = metrics(reference_vector, candidate_vector)
        results.append({"id": case_id, **observed, "accepted": accepted(observed, thresholds)})
    return results
