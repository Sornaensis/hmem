from __future__ import annotations

import copy
import contextlib
import json
import math
import sys
import types
import unittest
from pathlib import Path
from unittest.mock import MagicMock, mock_open, patch

import diagnose_weight_rounding as diagnostic


def unit_vector(index: int = 0) -> list[float]:
    value = [0.0] * diagnostic.DIMENSIONS
    value[index] = 1.0
    return value


def probe_case() -> dict:
    return {
        "id": diagnostic.CASE_ID,
        "role": "document",
        "text": "x",
        "utf8_sha256": "unused-by-injected-validator",
        "token_ids": [7] * 2047 + [diagnostic.EOS_TOKEN_ID],
    }


def retained_inputs(vector: list[float] | None = None) -> tuple[dict, dict, dict]:
    case = probe_case()
    vector = unit_vector() if vector is None else vector
    probes = {"cases": [case]}
    reference = {
        "schema_version": 1,
        "state": "passed",
        "probe_sha256": diagnostic.EXPECTED_INPUTS["probes"][2],
        "runtime": {
            "reference_method": diagnostic.REFERENCE_METHOD,
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
        },
        "cases": [{"id": diagnostic.CASE_ID, "token_ids": case["token_ids"], "vector": vector}],
    }
    tei = {
        "schema_version": 1,
        "state": "failed",
        "probe_sha256": diagnostic.EXPECTED_INPUTS["probes"][2],
        "candidate_cases": [{"id": diagnostic.CASE_ID, "token_ids": case["token_ids"], "vector": vector}],
    }
    return probes, reference, tei


class FakeBuffer:
    def __init__(self, *, version: int = 0):
        self.shape = (2,)
        self.dtype = "torch.float32"
        self.device = "cuda:0"
        self._version = version

    def numel(self):
        return 2

    def data_ptr(self):
        return 1234


class FakeModel:
    def __init__(self):
        self.buffer = FakeBuffer()

    def named_buffers(self, *, remove_duplicate):
        if not remove_duplicate:
            raise AssertionError("buffer deduplication must be explicit")
        return [("rotary", self.buffer)]


class DiagnosticTests(unittest.TestCase):
    def test_fixed_schema_is_diagnostic_only_and_partial(self) -> None:
        report = diagnostic.empty_report("a" * 32)
        self.assertEqual(report["schema_version"], 1)
        self.assertEqual(report["kind"], "loaded_weight_rounding_diagnostic_v1")
        self.assertEqual(report["state"], "failed")
        self.assertNotIn("accepted", report)
        self.assertIsNone(report["baseline"])
        self.assertIsNone(report["parameter_rounding"])
        self.assertIsNone(report["rounded"])
        self.assertIsNone(report["comparisons"])

    def test_artifact_authentication_rejects_before_json_read(self) -> None:
        path = MagicMock(spec=Path)
        path.resolve.return_value = diagnostic.EXPECTED_PROTOCOL[0]
        with (
            patch.object(Path, "is_file", return_value=True),
            patch.object(Path, "stat") as stat,
            patch.object(diagnostic, "sha256_file") as sha,
        ):
            stat.return_value.st_size = diagnostic.EXPECTED_PROTOCOL[1] - 1
            with self.assertRaisesRegex(diagnostic.DiagnosticError, "size drift"):
                diagnostic.authenticate_artifact(path, *diagnostic.EXPECTED_PROTOCOL)
        sha.assert_not_called()

    def test_reference_driver_hash_is_checked_before_import(self) -> None:
        with (
            patch.object(Path, "resolve", return_value=diagnostic.REFERENCE_DRIVER_PATH),
            patch.object(Path, "is_file", return_value=True),
            patch.object(Path, "stat") as stat,
            patch.object(diagnostic, "sha256_file", return_value="0" * 64),
            patch.object(diagnostic.importlib.util, "spec_from_file_location") as make_spec,
        ):
            stat.return_value.st_size = diagnostic.REFERENCE_DRIVER_BYTES
            with self.assertRaisesRegex(diagnostic.DiagnosticError, "checksum drift"):
                diagnostic.load_reference_driver()
        make_spec.assert_not_called()

    def test_selects_only_exact_mixed_long_and_authenticates_retained_vectors(self) -> None:
        probes, reference, tei = retained_inputs()
        selected, retained, candidate = diagnostic.select_inputs(
            probes, reference, tei, lambda _: [probe_case()]
        )
        self.assertEqual(selected["id"], "mixed_long")
        self.assertEqual(len(selected["token_ids"]), 2048)
        self.assertEqual(retained, unit_vector())
        self.assertEqual(candidate, unit_vector())

        bad = copy.deepcopy(reference)
        bad["cases"][0]["token_ids"][-1] = 1
        with self.assertRaisesRegex(diagnostic.DiagnosticError, "token identity drift"):
            diagnostic.select_inputs(probes, bad, tei, lambda _: [probe_case()])
        bad_case = probe_case()
        bad_case["token_ids"][-2] = diagnostic.EOS_TOKEN_ID
        with self.assertRaisesRegex(diagnostic.DiagnosticError, "EOS identity drift"):
            diagnostic.select_inputs(probes, reference, tei, lambda _: [bad_case])

    def test_two_forward_sequence_and_continuous_coordinate_metrics(self) -> None:
        model = FakeModel()
        baseline_vector = unit_vector()
        rounded_vector = unit_vector()
        rounded_vector[0] = math.sqrt(1.0 - 0.01**2)
        rounded_vector[diagnostic.COORDINATE_INDEX] = 0.01
        events = []
        vectors = iter([baseline_vector, rounded_vector])
        progress = {"model_loads": 1, "forwards_completed": 0}
        settings = {"float32_matmul_precision": "highest"}

        def forward(_, case):
            events.append("forward")
            vector = next(vectors)
            return ({
                "token_ids": case["token_ids"], "pooled_norm": 2.0,
                "vector": vector, "norm": 1.0, "elapsed_seconds": 0.1,
            }, {"device": "cuda:0", "dtype": "torch.float32"})

        def rounder(_):
            events.append("round")
            return {"parameters": [], "summary": {}}

        result = diagnostic.execute_experiment(
            model, probe_case(), baseline_vector, unit_vector(diagnostic.COORDINATE_INDEX),
            forward_fn=forward,
            round_fn=rounder,
            buffer_snapshot_fn=diagnostic.capture_buffers,
            settings_snapshot_fn=lambda: settings.copy(),
            expected_settings=settings,
            progress=progress,
        )
        self.assertEqual(events, ["forward", "round", "forward"])
        self.assertEqual(progress["forwards_completed"], 2)
        self.assertEqual(result["state"], "completed_diagnostic")
        self.assertTrue(result["baseline"]["exact_match_retained_reference"])
        self.assertEqual(len(result["baseline"]["vector"]), diagnostic.DIMENSIONS)
        self.assertEqual(len(result["rounded"]["vector"]), diagnostic.DIMENSIONS)
        self.assertEqual(len(result["comparisons"]["signed_coordinate_deltas"]["values"]), diagnostic.DIMENSIONS)
        coordinate = result["comparisons"]["coordinate_940"]
        self.assertEqual(coordinate["rounded_minus_baseline"], 0.01)
        self.assertIsInstance(coordinate["absolute_tei_error_reduction"], float)

    def test_baseline_mismatch_stops_before_rounding_and_second_forward(self) -> None:
        events = []
        progress = {"model_loads": 1, "forwards_completed": 0}
        baseline = unit_vector(1)

        def forward(_, case):
            events.append("forward")
            return ({
                "token_ids": case["token_ids"], "pooled_norm": 1.0,
                "vector": baseline, "norm": 1.0, "elapsed_seconds": 0.1,
            }, {"device": "cuda:0", "dtype": "torch.float32"})

        result = diagnostic.execute_experiment(
            FakeModel(), probe_case(), unit_vector(), unit_vector(),
            forward_fn=forward,
            round_fn=lambda _: events.append("round"),
            buffer_snapshot_fn=diagnostic.capture_buffers,
            settings_snapshot_fn=lambda: {"ok": True},
            expected_settings={"ok": True},
            progress=progress,
        )
        self.assertEqual(events, ["forward"])
        self.assertEqual(progress["forwards_completed"], 1)
        self.assertEqual(result["state"], "inconclusive_baseline_drift")
        self.assertIsNone(result["parameter_rounding"])
        self.assertIsNone(result["rounded"])
        self.assertIsNone(result["comparisons"])

    def test_buffer_or_setting_mutation_rejects_without_third_forward(self) -> None:
        progress = {"model_loads": 1, "forwards_completed": 0}
        model = FakeModel()
        calls = 0

        def forward(_, case):
            nonlocal calls
            calls += 1
            if calls == 2:
                model.buffer._version += 1
            return ({
                "token_ids": case["token_ids"], "pooled_norm": 1.0,
                "vector": unit_vector(), "norm": 1.0, "elapsed_seconds": 0.1,
            }, {"device": "cuda:0", "dtype": "torch.float32"})

        with self.assertRaisesRegex(diagnostic.DiagnosticError, "buffer metadata"):
            diagnostic.execute_experiment(
                model, probe_case(), unit_vector(), unit_vector(),
                forward_fn=forward,
                round_fn=lambda _: {"parameters": [], "summary": {}},
                buffer_snapshot_fn=diagnostic.capture_buffers,
                settings_snapshot_fn=lambda: {"ok": True},
                expected_settings={"ok": True},
                progress=progress,
            )
        self.assertEqual(calls, 2)
        self.assertEqual(progress["stage"], "rounded_forward")
        self.assertIn("baseline", progress)
        self.assertIn("rounded", progress)

        observations = iter([{"ok": True}, {"ok": False}])
        with self.assertRaisesRegex(diagnostic.DiagnosticError, "settings changed"):
            diagnostic.execute_experiment(
                FakeModel(), probe_case(), unit_vector(), unit_vector(),
                forward_fn=lambda _, case: ({
                    "token_ids": case["token_ids"], "pooled_norm": 1.0,
                    "vector": unit_vector(), "norm": 1.0, "elapsed_seconds": 0.1,
                }, {"device": "cuda:0", "dtype": "torch.float32"}),
                round_fn=lambda _: {"parameters": [], "summary": {}},
                buffer_snapshot_fn=diagnostic.capture_buffers,
                settings_snapshot_fn=lambda: next(observations),
                expected_settings={"ok": True},
                progress={"model_loads": 1, "forwards_completed": 0},
            )

    def test_parameter_order_uniqueness_and_cuda_fp32_finite_rejections(self) -> None:
        first = object()
        second = object()
        model = MagicMock()
        model.named_parameters.return_value = [("z", second), ("a", first)]
        self.assertEqual(diagnostic.ordered_unique_parameters(model), [("a", first), ("z", second)])
        model.named_parameters.assert_called_once_with(remove_duplicate=True)
        model.named_parameters.return_value = [("a", first), ("b", first)]
        with self.assertRaisesRegex(diagnostic.DiagnosticError, "duplicate parameter objects"):
            diagnostic.ordered_unique_parameters(model)

        class Scalar:
            def __init__(self, value):
                self.value = value

            def item(self):
                return self.value

        class Check:
            def __init__(self, value):
                self.value = value

            def all(self):
                return Scalar(self.value)

        torch_module = types.SimpleNamespace(
            float32="torch.float32", float16="torch.float16",
            isfinite=lambda tensor: Check(tensor.finite),
        )
        class FiniteTensor:
            def __init__(self, *, device="cuda:0", dtype="torch.float32", finite=True):
                self.device = device
                self.dtype = dtype
                self.finite = finite

            def detach(self):
                return self

            def view(self, _shape):
                return self

            def numel(self):
                return 2

            def __getitem__(self, _key):
                return self

        tensor = FiniteTensor()
        diagnostic.require_cuda_fp32_finite(tensor, torch_module, "parameter")
        for changed, message in (
            ({"device": "cpu"}, "not on cuda:0"),
            ({"dtype": "torch.float16"}, "not FP32"),
            ({"finite": False}, "nonfinite"),
        ):
            bad = FiniteTensor()
            for name, value in changed.items():
                setattr(bad, name, value)
            with self.assertRaisesRegex(diagnostic.DiagnosticError, message):
                diagnostic.require_cuda_fp32_finite(bad, torch_module, "parameter")

        cast = FiniteTensor(dtype="torch.float16")
        diagnostic.require_cuda_f16_finite(cast, torch_module, "cast")
        cast.finite = False
        with self.assertRaisesRegex(diagnostic.DiagnosticError, "nonfinite"):
            diagnostic.require_cuda_f16_finite(cast, torch_module, "cast")

    def test_round_parameters_bounds_masks_and_releases_aliases_between_parameters(self) -> None:
        class Scalar:
            def __init__(self, value):
                self.value = value

            def item(self):
                return self.value

        class Mask:
            def __init__(self, finite):
                self.finite = finite

            def all(self):
                return Scalar(self.finite)

        class Storage:
            def __init__(self, tracker, kind):
                self.tracker = tracker
                self.kind = kind
                self.references = 0
                if kind != "parameter":
                    tracker.temporary_storages.append(self)

        class Tracker:
            def __init__(self):
                self.temporary_storages = []
                self.maximum_finite_check_elements = 0
                self.second_parameter_started_clean = False

            def allocate(self, kind, name):
                if kind == "f16" and name == "b_small":
                    self.second_parameter_started_clean = all(
                        storage.references == 0 for storage in self.temporary_storages
                    )
                    if not self.second_parameter_started_clean:
                        raise AssertionError("prior parameter temporary storage is still referenced")
                return Storage(self, kind)

        class Tensor:
            def __init__(self, tracker, name, elements, dtype, *, storage=None, finite=True):
                self.tracker = tracker
                self.name = name
                self.elements = elements
                self.dtype = dtype
                self.device = "cuda:0"
                self.shape = (elements,)
                self.finite = finite
                self._version = 0
                self.storage = storage or Storage(tracker, "parameter")
                self.storage.references += 1

            def __del__(self):
                self.storage.references -= 1

            def detach(self):
                return self

            def view(self, _shape):
                return Tensor(
                    self.tracker, self.name, self.elements, self.dtype,
                    storage=self.storage, finite=self.finite,
                )

            def numel(self):
                return self.elements

            def __getitem__(self, key):
                start = 0 if key.start is None else key.start
                stop = self.elements if key.stop is None else min(key.stop, self.elements)
                return Tensor(
                    self.tracker, self.name, max(0, stop - start), self.dtype,
                    storage=self.storage, finite=self.finite,
                )

            def to(self, *, dtype, device="cuda:0", copy=False):
                kind = "f16" if dtype == "torch.float16" else "promoted"
                self.assert_conversion(device, copy, kind)
                return Tensor(
                    self.tracker, self.name, self.elements, dtype,
                    storage=self.tracker.allocate(kind, self.name), finite=self.finite,
                )

            @staticmethod
            def assert_conversion(device, copy, kind):
                if device != "cuda:0":
                    raise AssertionError("conversion left cuda:0")
                if kind == "f16" and not copy:
                    raise AssertionError("conversion must create an owned temporary")

            def __sub__(self, other):
                if self.elements != other.elements:
                    raise AssertionError("chunk shape mismatch")
                return Tensor(
                    self.tracker, self.name, self.elements, self.dtype,
                    storage=self.tracker.allocate("delta", self.name),
                    finite=self.finite and other.finite,
                )

            def abs_(self):
                return self

            def max(self):
                return Scalar(0.25)

            def copy_(self, other):
                if self.elements != other.elements:
                    raise AssertionError("copy shape mismatch")
                self._version += 1
                return self

        tracker = Tracker()
        large_elements = diagnostic.ROUNDING_CHUNK_ELEMENTS + 3
        parameters = [
            ("b_small", Tensor(tracker, "b_small", 2, "torch.float32")),
            ("a_large", Tensor(tracker, "a_large", large_elements, "torch.float32")),
        ]
        model = types.SimpleNamespace(
            named_parameters=lambda *, remove_duplicate: parameters if remove_duplicate else None
        )

        def isfinite(tensor):
            tracker.maximum_finite_check_elements = max(
                tracker.maximum_finite_check_elements, tensor.numel()
            )
            return Mask(tensor.finite)

        torch_module = types.SimpleNamespace(
            float32="torch.float32",
            float16="torch.float16",
            no_grad=contextlib.nullcontext,
            isfinite=isfinite,
            count_nonzero=lambda tensor: Scalar(tensor.numel()),
        )
        result = diagnostic.round_parameters(model, torch_module)
        summary = result["summary"]

        self.assertTrue(tracker.second_parameter_started_clean)
        self.assertTrue(all(storage.references == 0 for storage in tracker.temporary_storages))
        self.assertLessEqual(
            tracker.maximum_finite_check_elements, diagnostic.ROUNDING_CHUNK_ELEMENTS
        )
        self.assertEqual(summary["largest_parameter_elements"], large_elements)
        self.assertEqual(
            summary["largest_parameter_finite_check_mask_bytes"],
            diagnostic.ROUNDING_CHUNK_ELEMENTS,
        )
        self.assertEqual(
            summary["largest_parameter_delta_and_finite_check_bytes"],
            diagnostic.ROUNDING_CHUNK_ELEMENTS * 5,
        )
        self.assertEqual(
            summary["largest_parameter_estimated_peak_temporary_bytes"],
            large_elements * 6 + diagnostic.ROUNDING_CHUNK_ELEMENTS * 5,
        )

    def test_serialization_is_create_new_and_preserves_all_coordinates(self) -> None:
        report = diagnostic.empty_report("b" * 32)
        report["state"] = "completed_diagnostic"
        report["baseline"] = {"vector": unit_vector()}
        report["rounded"] = {"vector": unit_vector(940)}
        path = MagicMock(spec=Path)
        opened = mock_open()
        path.open = opened
        with patch.object(diagnostic.os, "fsync") as fsync:
            diagnostic.create_new_json(path, report)
        path.parent.mkdir.assert_called_once_with(parents=True, exist_ok=True)
        opened.assert_called_once_with("xb")
        payload = b"".join(call.args[0] for call in opened().write.call_args_list)
        decoded = json.loads(payload.decode("utf-8"))
        self.assertEqual(len(decoded["baseline"]["vector"]), diagnostic.DIMENSIONS)
        self.assertEqual(len(decoded["rounded"]["vector"]), diagnostic.DIMENSIONS)
        fsync.assert_called_once()

    def test_module_imports_without_torch_or_model(self) -> None:
        self.assertNotIn("torch", diagnostic.__dict__)
        self.assertNotIn(diagnostic.REFERENCE_MODULE_NAME, sys.modules)


if __name__ == "__main__":
    unittest.main()
