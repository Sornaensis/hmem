from __future__ import annotations

import math
import unittest

from compare_gpu_vectors import accepted, compare_cases, metrics, validate_vector
from gpu_contract import ContractError


THRESHOLDS = {
    "dimensions": 3,
    "minimum_cosine": 0.9995,
    "maximum_l2_distance": 0.032,
    "maximum_coordinate_absolute_error": 0.005,
    "maximum_unit_norm_absolute_error": 0.0001,
}


class VectorComparisonTests(unittest.TestCase):
    def test_metrics_and_thresholds_are_independent(self) -> None:
        reference = [1.0, 0.0, 0.0]
        candidate = [math.sqrt(1.0 - 0.004**2), 0.004, 0.0]
        observed = metrics(reference, candidate)
        self.assertTrue(accepted(observed, THRESHOLDS))
        concentrated = metrics(reference, [math.sqrt(1.0 - 0.006**2), 0.006, 0.0])
        self.assertFalse(accepted(concentrated, THRESHOLDS))

    def test_cosine_divides_by_both_norms_within_allowed_norm_drift(self) -> None:
        observed = metrics([1.00005, 0.0, 0.0], [0.99995, 0.0, 0.0])
        self.assertAlmostEqual(observed["cosine"], 1.0, places=15)
        self.assertAlmostEqual(observed["l2_distance"], 0.0001, places=15)

    def test_case_comparison_requires_identical_token_ids(self) -> None:
        reference = [{"id": "case", "token_ids": [1, 151643], "vector": [1.0, 0.0, 0.0]}]
        candidate = [{"id": "case", "token_ids": [2, 151643], "vector": [1.0, 0.0, 0.0]}]
        with self.assertRaisesRegex(ContractError, "token IDs differ"):
            compare_cases(reference, candidate, THRESHOLDS)

    def test_vector_validation_rejects_nonfinite_and_norm_drift(self) -> None:
        with self.assertRaisesRegex(ContractError, "finite"):
            validate_vector([math.nan, 0.0, 0.0], 3, 0.0001)
        with self.assertRaisesRegex(ContractError, "unit-norm"):
            validate_vector([0.5, 0.0, 0.0], 3, 0.0001)


if __name__ == "__main__":
    unittest.main()
