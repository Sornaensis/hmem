from __future__ import annotations

import copy
import hashlib
import json
import unittest
from pathlib import Path
from unittest.mock import patch

from gpu_contract import ContractError, load_contract
import reference_cuda
from run_foundation_validation import (
    EXPECTED_ATTENTION_NAMES,
    EXPECTED_ROTARY_NAMES,
    EXPECTED_SDPA_FLAGS,
    REFERENCE_IMAGE_ID,
    add_validation_mount,
    cleanup_failures,
    http_json,
    measured_recommendations,
    metric_evidence,
    monitor_sample_capacity,
    require_serial_gpu_ownership,
    validate_probes,
    validate_reference_execution,
    validate_reference_report,
)
from run_reference_validation import IMAGE_ID
from run_tei_startup import build_create_command


ROOT = Path(__file__).resolve().parents[2]
CONTRACT_PATH = ROOT / "hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json"
PROBES_PATH = ROOT / "hmem-server/test/fixtures/embedding-gpu-viability/parity-probes-v1.json"


def sdpa_backend_evidence() -> dict:
    return {
        "profiler": {"activities": ["CPU"], "record_shapes": False, "profile_memory": False, "with_stack": False},
        "effective_flags_before": copy.deepcopy(EXPECTED_SDPA_FLAGS),
        "effective_flags_after": copy.deepcopy(EXPECTED_SDPA_FLAGS),
        "scaled_dot_product_attention_calls": 28,
        "implementation_operator_counts": {"aten::_scaled_dot_product_attention_math": 28},
    }


def rotary_inventory() -> list[dict]:
    buffers = [
        ("inv_freq", [64], 64),
        ("cos_cached", [131072, 128], 16_777_216),
        ("sin_cached", [131072, 128], 16_777_216),
    ]
    return [
        {
            "name": name, "class": "Qwen2RotaryEmbedding", "module": reference_cuda.ORIGINAL_MODEL_MODULE_NAME,
            "max_seq_len_cached": 131072,
            "buffers": [
                {"name": f"{name}.{buffer_name}", "shape": shape, "numel": numel,
                 "dtype": "torch.float16", "device": "cuda:0", "version": 1}
                for buffer_name, shape, numel in buffers
            ],
        }
        for name in EXPECTED_ROTARY_NAMES
    ]


def passing_reference(contract: dict, probe_sha256: str = "a" * 64) -> dict:
    unit = [1.0, *([0.0] * 1535)]
    runtime = {
        "python": "3.11.9", "torch": "2.7.1+cu128", "torch_cuda": "12.8", "transformers": "4.41.2",
        "packages": reference_cuda.PACKAGE_VERSIONS,
        "device": {"index": 0, "uuid": "GPU-test", "compute_capability": "12.0"},
        "torch_supported_architectures": ["sm_120"], "parameter_devices": ["cuda:0"],
        "reference_method": "original-sdpa-math-cuda-f16-v3", "parameter_dtypes": ["torch.float16"],
        "activation_dtypes": ["torch.float16"], "pooling_input_dtype": "torch.float16",
        "pooling_compute_dtype": "torch.float32", "normalized_vector_dtype": "torch.float32",
        "smoke_dtype": "torch.float16", "float32_matmul_precision": "highest",
        "cuda_matmul_allow_tf32": False, "cudnn_allow_tf32": False,
        "allow_fp16_reduced_precision_reduction": False, "fp16_bf16_reduction_math_sdp_allowed": False,
        "autocast_enabled": False, "torch_compile": False, "attention_implementation": "sdpa",
        "output_attentions": False, "is_causal": False, "use_cache": False, "model_loads": 1,
        "forwards_completed": 4,
        "script_sha256": hashlib.sha256((Path(__file__).parent / "reference_cuda.py").read_bytes()).hexdigest(),
        "model_class": {"class": "Qwen2Model", "module": reference_cuda.ORIGINAL_MODEL_MODULE_NAME,
                        "path": "/model/modeling_qwen.py", "sha256": reference_cuda.MODELING_QWEN_SHA256},
        "tokenizer_class": {"class": "Qwen2TokenizerFast", "sha256": reference_cuda.TOKENIZATION_QWEN_SHA256},
        "attention_modules": {"count": 28, "class": "Qwen2SdpaAttention",
                              "module": reference_cuda.ORIGINAL_MODEL_MODULE_NAME, "names": EXPECTED_ATTENTION_NAMES},
        "rotary_modules": rotary_inventory(),
        **copy.deepcopy(EXPECTED_SDPA_FLAGS),
    }
    return {
        "schema_version": 1, "state": "passed", "probe_sha256": probe_sha256,
        "model": {"id": contract["model"]["id"], "revision": contract["model"]["revision"],
                  "source": "/model", "config_sha256": reference_cuda.CONFIG_SHA256,
                  "artifacts": contract["model"]["artifacts"]},
        "runtime": runtime,
        "cases": [
            {"id": case_id, "token_ids": [index, 151643], "vector": list(unit),
             "attention_backend_evidence": sdpa_backend_evidence()}
            for index, case_id in enumerate(["doc_short", "mixed_long", "multilingual", "query_search"])
        ],
    }


class FoundationValidationTests(unittest.TestCase):
    def setUp(self) -> None:
        self.contract = load_contract(CONTRACT_PATH)
        text = self.contract["model"]["query_prefix"] + "example"
        self.probes = {
            "schema_version": 1,
            "task_id": self.contract["task_id"],
            "model": {"id": self.contract["model"]["id"], "revision": self.contract["model"]["revision"]},
            "cases": [
                {
                    "id": f"case-{index}", "role": "query" if index == 0 else "document",
                    "raw_text": "example" if index == 0 else text,
                    "text": text, "utf8_sha256": hashlib.sha256(text.encode()).hexdigest(),
                    "token_ids": [index, 151643],
                }
                for index in range(4)
            ],
        }

    def test_probes_bind_model_bytes_prompt_eos_and_token_cap(self) -> None:
        validate_probes(self.probes, self.contract)
        changed = copy.deepcopy(self.probes)
        changed["cases"][0]["text"] = "example"
        changed["cases"][0]["utf8_sha256"] = hashlib.sha256(b"example").hexdigest()
        with self.assertRaisesRegex(ContractError, "query prefix"):
            validate_probes(changed, self.contract)
        changed = copy.deepcopy(self.probes)
        changed["cases"][1]["token_ids"] = [1] * 2048 + [151643]
        with self.assertRaisesRegex(ContractError, "token cap"):
            validate_probes(changed, self.contract)

    def test_frozen_parity_fixture_has_authenticated_case_boundaries(self) -> None:
        value = json.loads(PROBES_PATH.read_text(encoding="utf-8"))
        cases = validate_probes(value, self.contract)
        self.assertEqual([case["id"] for case in cases], ["doc_short", "mixed_long", "multilingual", "query_search"])
        self.assertEqual([len(case["token_ids"]) for case in cases], [10, 2048, 21, 26])
        self.assertTrue(all(case["token_ids"].count(self.contract["model"]["eos_token_id"]) == 1 for case in cases))

    def test_validation_mount_precedes_pinned_image_and_is_read_only(self) -> None:
        _name, command = build_create_command(self.contract, "a" * 32)
        mounted = add_validation_mount(command, self.contract["image"]["reference"], Path("D:/evidence/run"))
        image_index = mounted.index(self.contract["image"]["reference"])
        self.assertEqual(mounted[image_index - 2], "--mount")
        self.assertEqual(mounted[image_index - 1], "type=bind,src=D:\\evidence\\run,dst=/validation,readonly")

    def test_measured_recommendation_policy_is_frozen_and_rounded_up(self) -> None:
        self.assertEqual(
            measured_recommendations(100.1, [0.2, 15.1], 201.0),
            {
                "startup_timeout_seconds": 151,
                "short_request_timeout_seconds": 31,
                "full_context_request_timeout_seconds": 302,
                "validated_concurrent_requests": 2,
                "validated_compact_batch_items": 4,
                "policy": "ceil startup/full x1.5, short maximum x2; retain frozen minima 120/30/300",
            },
        )

    def test_large_request_uses_container_file_and_preserves_http_status(self) -> None:
        with patch("run_foundation_validation.run", return_value=('{"error":"too long"}\n413', "")) as bounded:
            status, body = http_json(
                "a" * 64, "POST", "/embed", 120, 1048576, payload_file="/validation/over-32769-request.json"
            )
        self.assertEqual(status, 413)
        self.assertEqual(body, {"error": "too long"})
        argv = bounded.call_args.args[0]
        self.assertIn("@/validation/over-32769-request.json", argv)
        self.assertLess(max(len(argument) for argument in argv), 1024)

    def test_cleanup_retention_and_stop_failures_prevent_success(self) -> None:
        self.assertEqual(
            cleanup_failures({"logs": "ok", "log_write": "failed: disk", "stop": "failed: timeout", "remove": "ok", "absent": True, "within_deadline": True}),
            ["log_write", "stop"],
        )

    def test_reference_report_binds_runtime_model_and_noncausal_cuda(self) -> None:
        report = passing_reference(self.contract)
        self.assertEqual(len(validate_reference_report(report, "a" * 64, self.contract)), 4)
        report["runtime"]["cuda_matmul_allow_tf32"] = True
        with self.assertRaisesRegex(ContractError, "CUDA/F16/SDPA/noncausal"):
            validate_reference_report(report, "a" * 64, self.contract)
        report["runtime"]["cuda_matmul_allow_tf32"] = False
        report["runtime"]["is_causal"] = True
        with self.assertRaisesRegex(ContractError, "CUDA/F16/SDPA/noncausal"):
            validate_reference_report(report, "a" * 64, self.contract)

    def test_reference_report_rejects_old_fp32_and_ambiguous_sdpa_dispatch(self) -> None:
        report = passing_reference(self.contract)
        report["runtime"]["reference_method"] = "original-eager-cuda-fp32-v2"
        report["runtime"]["parameter_dtypes"] = ["torch.float32"]
        with self.assertRaisesRegex(ContractError, "CUDA/F16/SDPA/noncausal"):
            validate_reference_report(report, "a" * 64, self.contract)
        report = passing_reference(self.contract)
        report["cases"][0]["attention_backend_evidence"]["implementation_operator_counts"] = {
            "aten::_scaled_dot_product_attention_math": 27,
            "aten::_scaled_dot_product_flash_attention": 1,
        }
        with self.assertRaisesRegex(ContractError, "MATH dispatch"):
            validate_reference_report(report, "a" * 64, self.contract)
        report = passing_reference(self.contract)
        report["cases"][0]["attention_backend_evidence"]["effective_flags_after"]["flash_sdp_enabled"] = True
        with self.assertRaisesRegex(ContractError, "MATH dispatch"):
            validate_reference_report(report, "a" * 64, self.contract)

    def test_reference_report_rejects_rotary_cache_or_pooling_drift(self) -> None:
        report = passing_reference(self.contract)
        report["runtime"]["rotary_modules"][3]["buffers"][1]["dtype"] = "torch.float32"
        with self.assertRaisesRegex(ContractError, "rotary buffer"):
            validate_reference_report(report, "a" * 64, self.contract)
        report = passing_reference(self.contract)
        report["runtime"]["pooling_compute_dtype"] = "torch.float16"
        with self.assertRaisesRegex(ContractError, "CUDA/F16/SDPA/noncausal"):
            validate_reference_report(report, "a" * 64, self.contract)

    def test_reference_execution_requires_authenticated_cleanup_and_output_hash(self) -> None:
        reference = Path("D:/evidence/reference.json").resolve()
        attestation = {
            "schema_version": 1, "state": "reference_passed", "task_id": self.contract["task_id"],
            "reference_method": "original-sdpa-math-cuda-f16-v3",
            "image_id": REFERENCE_IMAGE_ID,
            "probe_sha256": "a" * 64, "container_id": "b" * 64,
            "reference_output": {"path": str(reference), "bytes": 123, "sha256": "c" * 64},
            "reference_validation": {},
            "cleanup": {"absent": True, "failures": []},
        }
        report = passing_reference(self.contract)
        attestation["reference_validation"] = __import__("run_foundation_validation").reference_validation_summary(report)
        with patch("run_foundation_validation.read_json", side_effect=[attestation, report]), patch(
            "run_foundation_validation.sha256_file", side_effect=["c" * 64, "d" * 64]
        ), patch.object(Path, "stat", return_value=type("S", (), {"st_size": 123})()):
            observed = validate_reference_execution(reference, "a" * 64)
        self.assertEqual(observed["container_id"], "b" * 64)
        attestation["reference_method"] = "original-eager-cuda-fp32-v2"
        with patch("run_foundation_validation.read_json", side_effect=[attestation, report]), patch(
            "run_foundation_validation.sha256_file", return_value="c" * 64
        ), patch.object(Path, "stat", return_value=type("S", (), {"st_size": 123})()), self.assertRaisesRegex(ContractError, "attestation"):
            validate_reference_execution(reference, "a" * 64)
        attestation["reference_method"] = "original-sdpa-math-cuda-f16-v3"
        attestation["cleanup"] = {"absent": True, "failures": ["stop"]}
        with patch("run_foundation_validation.read_json", side_effect=[attestation, report]), patch(
            "run_foundation_validation.sha256_file", return_value="c" * 64
        ), patch.object(Path, "stat", return_value=type("S", (), {"st_size": 123})()), self.assertRaisesRegex(ContractError, "attestation"):
            validate_reference_execution(reference, "a" * 64)

    def test_monitor_capacity_covers_declared_envelope_and_failed_metric_is_retained(self) -> None:
        runtime = self.contract["runtime_contract"]
        self.assertGreater(monitor_sample_capacity(runtime), runtime["startup_timeout_seconds"] + 3 * runtime["full_context_request_timeout_seconds"])
        evidence = metric_evidence("case", [1.0, 0.0, 0.0], [0.999982, 0.006, 0.0], {
            "minimum_cosine": 0.9995, "maximum_l2_distance": 0.032, "maximum_coordinate_absolute_error": 0.005,
        })
        self.assertFalse(evidence["accepted"])
        self.assertEqual(evidence["vector"], [0.999982, 0.006, 0.0])

    def test_serial_guard_queries_reference_and_tei_labels(self) -> None:
        commands = []

        def runner(command, timeout, cap):
            commands.append(command)
            return "", ""

        require_serial_gpu_ownership(runner=runner)
        self.assertEqual(len(commands), 2)
        self.assertTrue(any("label=hmem.task=" in argument for argument in commands[0]))
        self.assertTrue(any("label=io.hmem.task=" in argument for argument in commands[1]))


if __name__ == "__main__":
    unittest.main()
