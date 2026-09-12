from __future__ import annotations

import copy
import io
import json
import math
import struct
import sys
import unittest
from pathlib import Path
from unittest.mock import patch

import reference_cuda
import run_weight_rounding_diagnostic as diagnostic_controller
from gpu_contract import ContractError, load_contract
from run_foundation_validation import validate_reference_report
from run_weight_rounding_diagnostic import (
    BASE_SHA,
    CPUS,
    DIAGNOSTIC_KIND,
    DIAGNOSTIC_LABEL_VALUE,
    IMAGE_ID,
    MEMORY_BYTES,
    MEMORY_SWAP_BYTES,
    PIDS,
    RESERVED_VRAM_MIB,
    TASK_ID,
    TMPFS_BYTES,
    authenticate_container,
    build_create_command,
    cleanup_failures,
    largest_rounding_temporary,
    recover_owned_container,
    validate_diagnostic_output,
    validate_output_root,
    validate_protocol_and_inputs,
    validate_temporary_headroom,
)


ROOT = Path(__file__).resolve().parents[2]
CONTRACT = load_contract(ROOT / "hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json")
PROBES = ROOT / "hmem-server/test/fixtures/embedding-gpu-viability/parity-probes-v1.json"
REFERENCE = Path(CONTRACT["artifact_root"]) / "reference-output/reference-v3.json"
TEI_REPORT = (
    Path(CONTRACT["artifact_root"])
    / "validation/58e6b864aa984e489d406132c428785f/foundation-validation-report-v1.json"
)
PROTOCOL = Path(r"C:\Users\Sornaensis\AppData\Local\Temp\hmem-gpu-weight-rounding-diagnostic-protocol-ba60c4e-r1.json")
SCRIPT = Path(__file__).with_name("diagnose_weight_rounding.py")
OUTPUT_PARENT = Path(CONTRACT["artifact_root"]) / "diagnostics/weight-rounding"
RETAINED_DIAGNOSTIC = OUTPUT_PARENT / "507ce9b89c5b4f76b1b8cc653983b273/diagnostic.json"


def inspected(run_id: str, name: str, output: Path) -> list[dict]:
    cmd = [
        "-I", "-B", "/diagnostic/diagnose_weight_rounding.py",
        "--source", "/model", "--probes", "/inputs/probes.json",
        "--reference", "/inputs/reference-v3.json", "--tei-report", "/inputs/failed-tei.json",
        "--protocol", "/inputs/diagnostic-protocol.json", "--output", "/output/diagnostic.json",
    ]
    env = [
        "HF_HUB_OFFLINE=1", "TRANSFORMERS_OFFLINE=1", "HF_DATASETS_OFFLINE=1",
        "HF_HOME=/tmp/hf", "HF_MODULES_CACHE=/tmp/hf/modules", "TOKENIZERS_PARALLELISM=false",
        "NVIDIA_DRIVER_CAPABILITIES=compute,utility", f"HMEM_DIAGNOSTIC_RUN_ID={run_id}",
    ]
    mounts = [
        ("/model", Path(CONTRACT["model"]["source_root"]), False),
        ("/inputs/probes.json", PROBES, False),
        ("/inputs/reference-v3.json", REFERENCE, False),
        ("/inputs/failed-tei.json", TEI_REPORT, False),
        ("/inputs/diagnostic-protocol.json", PROTOCOL, False),
        ("/diagnostic/diagnose_weight_rounding.py", SCRIPT, False),
        ("/output", output, True),
    ]
    return [{
        "Name": "/" + name,
        "Config": {
            "Image": IMAGE_ID,
            "User": "65532:65532",
            "Entrypoint": ["/usr/local/bin/python3.11"],
            "Cmd": cmd,
            "Env": env,
            "Labels": {
                "io.hmem.task": TASK_ID,
                "io.hmem.run": run_id,
                "io.hmem.diagnostic": DIAGNOSTIC_LABEL_VALUE,
            },
        },
        "HostConfig": {
            "NetworkMode": "none",
            "Memory": MEMORY_BYTES,
            "MemorySwap": MEMORY_SWAP_BYTES,
            "NanoCpus": CPUS * 1_000_000_000,
            "PidsLimit": PIDS,
            "ReadonlyRootfs": True,
            "LogConfig": {"Type": "local", "Config": {"compress": "false", "max-file": "1", "max-size": "8m"}},
            "CapDrop": ["ALL"],
            "SecurityOpt": ["no-new-privileges"],
            "Tmpfs": {"/tmp": f"rw,nosuid,nodev,size={TMPFS_BYTES}"},
            "DeviceRequests": [{"DeviceIDs": ["0"], "Capabilities": [["gpu"]]}],
        },
        "Mounts": [
            {"Type": "bind", "Destination": destination, "Source": str(source), "RW": writable}
            for destination, source, writable in mounts
        ],
    }]


def output_value(run_id: str, protocol: dict, inputs: dict) -> dict:
    driver = Path(__file__).with_name("reference_cuda.py")
    runtime = {
        "python": "3.11.9",
        "torch": "2.7.1+cu128",
        "torch_cuda": "12.8",
        "transformers": "4.41.2",
        "packages": reference_cuda.PACKAGE_VERSIONS,
        "reference_driver": {
            "path": "/scripts/reference_cuda.py",
            "bytes": driver.stat().st_size,
            "sha256": __import__("hashlib").sha256(driver.read_bytes()).hexdigest(),
        },
        "model_class": {
            "class": "Qwen2Model",
            "module": reference_cuda.ORIGINAL_MODEL_MODULE_NAME,
            "path": "/model/modeling_qwen.py",
            "sha256": reference_cuda.MODELING_QWEN_SHA256,
        },
        "device": {"index": 0, "uuid": "GPU-test", "compute_capability": "12.0"},
        "torch_supported_architectures": ["sm_120"],
        "parameter_devices": ["cuda:0"],
        "parameter_dtypes": ["torch.float32"],
        "activation_devices": ["cuda:0"],
        "activation_dtypes": ["torch.float32"],
        "reference_method": "original-eager-cuda-fp32-v2",
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
        "forwards_completed": 2,
    }
    baseline = [1.0, *([0.0] * 1535)]
    rounded = [0.0, 1.0, *([0.0] * 1534)]
    retained_tei = [0.0, 0.0, 1.0, *([0.0] * 1533)]
    metric_same = {
        "cosine": 1.0, "l2_distance": 0.0, "maximum_coordinate_absolute_error": 0.0,
        "left_norm": 1.0, "right_norm": 1.0,
    }
    metric_orthogonal = {
        "cosine": 0.0, "l2_distance": math.sqrt(2.0), "maximum_coordinate_absolute_error": 1.0,
        "left_norm": 1.0, "right_norm": 1.0,
    }
    case = {
        "id": "mixed_long", "role": "document", "utf8_sha256": "a" * 64,
        "token_ids": [7] * 2047 + [151643],
    }
    rounding = {
        "representation": {
            "baseline": "original CUDA FP32 checkpoint values",
            "rounded": "each unique learned CUDA parameter rounded FP32->F16->FP32; inference remains CUDA FP32",
            "buffers_modified": False,
        },
        "parameters": [{
            "name": "weight", "shape": [1536], "numel": 1536, "dtype": "torch.float32",
            "device": "cuda:0", "version": 1, "version_before": 0, "version_after": 1,
            "changed_count": 1, "maximum_absolute_delta": 0.1,
            "f16_temporary_finite": True, "promoted_fp32_finite": True,
        }],
        "summary": {
            "parameter_count": 1, "total_elements": 1536, "total_changed_count": 1,
            "maximum_absolute_delta": 0.1, "largest_parameter_name": "weight",
            "largest_parameter_elements": 1536, "largest_parameter_round_and_promote_bytes": 9216,
            "largest_parameter_finite_check_mask_bytes": 1536,
            "largest_parameter_delta_and_finite_check_bytes": 7680,
            "largest_parameter_estimated_peak_temporary_bytes": 16896,
            "delta_chunk_elements": 4194304, "finite_check_chunk_elements": 4194304,
            "finite_check_mask_element_bytes": 1, "delta_element_bytes": 4,
        },
        "buffers_before": [], "buffers_after": [], "buffers_unchanged": True,
    }
    signed = [after - before for before, after in zip(baseline, rounded)]
    comparisons = {
        "baseline_vs_rounded": metric_orthogonal,
        "rounded_vs_retained_reference": metric_orthogonal,
        "rounded_vs_retained_tei": metric_orthogonal,
        "baseline_vs_retained_tei": metric_orthogonal,
        "signed_coordinate_deltas": {"meaning": "rounded_minus_baseline", "values": signed},
        "coordinate_940": {
            "zero_based_index": 940, "baseline": 0.0, "rounded": 0.0,
            "retained_reference": 0.0, "retained_tei": 0.0,
            "rounded_minus_baseline": 0.0, "rounded_minus_retained_reference": 0.0,
            "rounded_minus_retained_tei": 0.0, "baseline_minus_retained_tei": 0.0,
            "absolute_tei_error_before": 0.0, "absolute_tei_error_after": 0.0,
            "absolute_tei_error_reduction": 0.0,
        },
    }
    return {
        "schema_version": 1,
        "kind": DIAGNOSTIC_KIND,
        "state": "completed_diagnostic",
        "task_id": TASK_ID,
        "base_sha": BASE_SHA,
        "run_id": run_id,
        "protocol_artifact": {
            "path": "/inputs/diagnostic-protocol.json",
            "bytes": protocol["bytes"],
            "sha256": protocol["sha256"],
        },
        "input_artifacts": {
            "probes": {"path": "/inputs/probes.json", **{k: inputs["probe"][k] for k in ("bytes", "sha256")}},
            "retained_reference": {
                "path": "/inputs/reference-v3.json",
                **{k: inputs["reference"][k] for k in ("bytes", "sha256")},
            },
            "failed_tei_report": {
                "path": "/inputs/failed-tei.json",
                **{k: inputs["failed_tei"][k] for k in ("bytes", "sha256")},
            },
        },
        "runtime": runtime,
        "case": case,
        "baseline": {
            "token_ids": case["token_ids"], "pooled_norm": 1.0, "vector": baseline,
            "norm": 1.0, "elapsed_seconds": 0.1, "exact_match_retained_reference": True,
            "metrics_to_retained_reference": metric_same,
        },
        "parameter_rounding": rounding,
        "rounded": {
            "token_ids": case["token_ids"], "pooled_norm": 1.0, "vector": rounded,
            "norm": 1.0, "elapsed_seconds": 0.1,
        },
        "comparisons": comparisons,
    }


def expected_evidence(value: dict) -> dict:
    return {
        "case": value["case"],
        "reference_vector": value["baseline"]["vector"],
        "tei_vector": [0.0, 0.0, 1.0, *([0.0] * 1533)],
        "reference_driver": copy.deepcopy(value["runtime"]["reference_driver"]),
    }


class WeightRoundingControllerTests(unittest.TestCase):
    def test_create_command_freezes_exact_diagnostic_boundary(self) -> None:
        run_id = "a" * 32
        output = OUTPUT_PARENT / run_id
        name, command = build_create_command(
            CONTRACT, PROBES, REFERENCE, TEI_REPORT, PROTOCOL, SCRIPT, output, run_id
        )
        joined = "\n".join(command)
        self.assertEqual(name, "hmem-aa30-weight-rounding-" + "a" * 12)
        for value in (
            IMAGE_ID,
            "io.hmem.diagnostic=loaded-weight-rounding-v1",
            f"HMEM_DIAGNOSTIC_RUN_ID={run_id}",
            "/usr/local/bin/python3.11",
            "-I",
            "-B",
            "dst=/diagnostic/diagnose_weight_rounding.py,readonly",
            "dst=/inputs/reference-v3.json,readonly",
            "dst=/inputs/failed-tei.json,readonly",
            "dst=/output",
            str(MEMORY_BYTES),
            str(MEMORY_SWAP_BYTES),
            str(CPUS),
            str(PIDS),
        ):
            self.assertIn(value, joined)
        self.assertEqual(RESERVED_VRAM_MIB, 2048)

    def test_inspect_authenticates_argv_env_caps_and_all_mounts(self) -> None:
        run_id = "b" * 32
        output = OUTPUT_PARENT / run_id
        name, _ = build_create_command(CONTRACT, PROBES, REFERENCE, TEI_REPORT, PROTOCOL, SCRIPT, output, run_id)
        value = inspected(run_id, name, output)
        authenticate_container(value, CONTRACT, PROBES, REFERENCE, TEI_REPORT, PROTOCOL, SCRIPT, output, run_id, name)
        changed = copy.deepcopy(value)
        changed[0]["Config"]["Env"][-1] = "HMEM_DIAGNOSTIC_RUN_ID=" + "c" * 32
        with self.assertRaisesRegex(ContractError, "environment drift"):
            authenticate_container(changed, CONTRACT, PROBES, REFERENCE, TEI_REPORT, PROTOCOL, SCRIPT, output, run_id, name)

    def test_recovery_requires_all_diagnostic_ownership_labels(self) -> None:
        run_id = "c" * 32
        output = OUTPUT_PARENT / run_id
        name, _ = build_create_command(CONTRACT, PROBES, REFERENCE, TEI_REPORT, PROTOCOL, SCRIPT, output, run_id)
        container_id = "d" * 64
        value = inspected(run_id, name, output)
        calls = []

        def runner(command, timeout, cap):
            calls.append((command, timeout, cap))
            return (container_id + "\n", "") if command[1:3] == ["ps", "-aq"] else (json.dumps(value), "")

        self.assertEqual(recover_owned_container(name, run_id, runner=runner), container_id)
        self.assertIn(f"label=io.hmem.diagnostic={DIAGNOSTIC_LABEL_VALUE}", calls[0][0])
        value[0]["Config"]["Labels"]["io.hmem.diagnostic"] = "other"
        with self.assertRaisesRegex(ContractError, "ownership labels"):
            recover_owned_container(name, run_id, runner=runner)

    def test_output_root_requires_fresh_uuid_child(self) -> None:
        good = OUTPUT_PARENT / ("e" * 32)
        with patch.object(Path, "exists", return_value=False):
            self.assertEqual(validate_output_root(good, Path(CONTRACT["artifact_root"])), "e" * 32)
        with patch.object(Path, "exists", return_value=True), self.assertRaisesRegex(ContractError, "create-new"):
            validate_output_root(good, Path(CONTRACT["artifact_root"]))
        with self.assertRaisesRegex(ContractError, "UUID hex"):
            validate_output_root(OUTPUT_PARENT / "not-a-run", Path(CONTRACT["artifact_root"]))

    def test_largest_rounding_temporary_parses_only_bounded_fp32_header(self) -> None:
        header = json.dumps({
            "small": {"dtype": "F32", "shape": [2], "data_offsets": [0, 8]},
            "large": {"dtype": "F32", "shape": [3, 4], "data_offsets": [8, 56]},
        }).encode()
        data = struct.pack("<Q", len(header)) + header
        with patch.object(Path, "open", return_value=io.BytesIO(data)):
            result = largest_rounding_temporary(Path("model"), [{"path": "weights.safetensors"}])
        self.assertEqual(result["tensor"], "large")
        self.assertEqual(result["source_fp32_bytes"], 48)
        self.assertEqual(result["temporary_fp16_bytes"], 24)
        self.assertEqual(result["temporary_promoted_fp32_bytes"], 48)
        self.assertEqual(result["round_and_promote_bytes"], 72)
        self.assertEqual(result["chunked_delta_bytes"], 48)
        self.assertEqual(result["chunked_finite_check_mask_bytes"], 12)
        self.assertEqual(result["estimated_peak_temporary_bytes"], 132)

    def test_temporary_estimate_must_fit_after_vram_reserve(self) -> None:
        available = validate_temporary_headroom(
            {"memory_free_mib": RESERVED_VRAM_MIB + 2},
            {"estimated_peak_temporary_bytes": 2 * 1024 * 1024},
        )
        self.assertEqual(available, 2 * 1024 * 1024)
        with self.assertRaisesRegex(ContractError, "fresh VRAM headroom"):
            validate_temporary_headroom(
                {"memory_free_mib": RESERVED_VRAM_MIB + 1},
                {"estimated_peak_temporary_bytes": 2 * 1024 * 1024},
            )

    def test_output_validation_rejects_reference_kind_and_runtime_drift(self) -> None:
        run_id = "f" * 32
        protocol = {"bytes": 100, "sha256": "1" * 64}
        inputs = {
            "probe": {"bytes": 101, "sha256": "2" * 64},
            "reference": {"bytes": 102, "sha256": "3" * 64},
            "failed_tei": {"bytes": 103, "sha256": "4" * 64},
        }
        value = output_value(run_id, protocol, inputs)
        expected = expected_evidence(value)
        self.assertEqual(validate_diagnostic_output(value, run_id, protocol, inputs, expected), "completed_diagnostic")
        changed = copy.deepcopy(value)
        changed["kind"] = "reference_execution"
        with self.assertRaisesRegex(ContractError, "identity drift"):
            validate_diagnostic_output(changed, run_id, protocol, inputs, expected)
        with self.assertRaisesRegex(ContractError, "passing schema-v1 result"):
            validate_reference_report(value, inputs["probe"]["sha256"], CONTRACT)
        changed = copy.deepcopy(value)
        changed["runtime"]["cuda_matmul_allow_tf32"] = True
        with self.assertRaisesRegex(ContractError, "runtime drift"):
            validate_diagnostic_output(changed, run_id, protocol, inputs, expected)
        changed = copy.deepcopy(value)
        changed["runtime"]["reference_driver"]["sha256"] = "f" * 64
        with self.assertRaisesRegex(ContractError, "reference driver"):
            validate_diagnostic_output(changed, run_id, protocol, inputs, expected)
        with self.assertRaisesRegex(ContractError, "host/container GPU identity"):
            validate_diagnostic_output(
                value,
                run_id,
                protocol,
                inputs,
                expected,
                {"index": 0, "uuid": "GPU-other", "compute_capability": "12.0"},
            )

    def test_retained_diagnostic_uses_immutable_fp32_reference_driver_identity(self) -> None:
        protocol, inputs, _estimate, expected = validate_protocol_and_inputs(
            CONTRACT, PROBES, REFERENCE, TEI_REPORT, PROTOCOL, SCRIPT
        )
        retained = json.loads(RETAINED_DIAGNOSTIC.read_text(encoding="utf-8"))
        self.assertEqual(
            validate_diagnostic_output(
                retained,
                "507ce9b89c5b4f76b1b8cc653983b273",
                protocol,
                inputs,
                expected,
            ),
            "completed_diagnostic",
        )
        self.assertEqual(
            expected["reference_driver"],
            {
                "path": "/scripts/reference_cuda.py",
                "bytes": 29060,
                "sha256": "eac9e1235b46425bbad706a188bc83a371f0783c3a0724534e17fdbf6649b6a1",
            },
        )

    def test_completed_output_requires_baseline_rounding_and_consistent_comparisons(self) -> None:
        run_id = "1" * 32
        protocol = {"bytes": 100, "sha256": "1" * 64}
        inputs = {
            "probe": {"bytes": 101, "sha256": "2" * 64},
            "reference": {"bytes": 102, "sha256": "3" * 64},
            "failed_tei": {"bytes": 103, "sha256": "4" * 64},
        }
        value = output_value(run_id, protocol, inputs)
        expected = expected_evidence(value)
        changed = copy.deepcopy(value)
        changed["baseline"]["vector"] = [0.0] * 1536
        with self.assertRaisesRegex(ContractError, "unit-norm drift"):
            validate_diagnostic_output(changed, run_id, protocol, inputs, expected)
        changed = copy.deepcopy(value)
        changed["comparisons"] = {}
        with self.assertRaisesRegex(ContractError, "comparison evidence is incomplete"):
            validate_diagnostic_output(changed, run_id, protocol, inputs, expected)
        changed = copy.deepcopy(value)
        changed["parameter_rounding"]["summary"]["total_changed_count"] = 0
        with self.assertRaisesRegex(ContractError, "summary drift"):
            validate_diagnostic_output(changed, run_id, protocol, inputs, expected)
        changed = copy.deepcopy(value)
        changed["baseline"]["exact_match_retained_reference"] = False
        with self.assertRaisesRegex(ContractError, "exact-match guard drift"):
            validate_diagnostic_output(changed, run_id, protocol, inputs, expected)

    def test_cleanup_failures_retain_stop_failure_after_remove(self) -> None:
        self.assertEqual(
            cleanup_failures({
                "logs": "ok", "log_write": "ok", "authentication": "ok",
                "stop": "failed: timeout", "remove": "ok", "absent": True, "within_deadline": True,
            }),
            ["stop"],
        )

    def test_main_descriptor_failure_still_stops_monitor_and_exact_owned_container(self) -> None:
        run_id = "2" * 32
        output_root = OUTPUT_PARENT / run_id
        container_id = "3" * 64
        container_name = "hmem-aa30-weight-rounding-" + run_id[:12]
        calls: list[list[str]] = []
        reports: list[dict] = []

        class FakeMonitor:
            def __init__(self, *args, **kwargs):
                self.failure = None
                self.samples = []
                self.stopped = False

            def start(self):
                return None

            def wait_first_sample(self, timeout):
                return True

            def check(self):
                return None

            def stop(self, timeout):
                self.stopped = True
                return True

        monitor = FakeMonitor()

        def runner(command, timeout, cap):
            calls.append(command)
            if command[0] == "nvidia-smi":
                return "gpu", ""
            if command[:2] == ["docker", "create"]:
                return container_id + "\n", ""
            if command[:2] == ["docker", "inspect"]:
                return json.dumps(inspected(run_id, container_name, output_root)), ""
            if command[:2] == ["docker", "logs"]:
                raise OSError("injected log capture failure")
            return "", ""

        def fake_descriptor(path):
            if Path(path).name == "diagnostic.json":
                raise OSError("injected descriptor failure")
            return {"path": str(path), "bytes": 1, "sha256": "0" * 64}

        protocol_descriptor = {"path": str(PROTOCOL), "bytes": 1, "sha256": "1" * 64}
        inputs = {
            "probe": {"path": str(PROBES), "bytes": 1, "sha256": "2" * 64},
            "reference": {"path": str(REFERENCE), "bytes": 1, "sha256": "3" * 64},
            "reference_execution": {"path": str(REFERENCE) + ".execution.json", "bytes": 1, "sha256": "4" * 64},
            "failed_tei": {"path": str(TEI_REPORT), "bytes": 1, "sha256": "5" * 64},
            "diagnostic_script": {"path": str(SCRIPT), "bytes": 1, "sha256": "6" * 64},
        }
        expected = {"case": {}, "reference_vector": [], "tei_vector": []}
        argv = [
            "run_weight_rounding_diagnostic.py", "--contract", str(ROOT / "hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json"),
            "--probes", str(PROBES), "--reference", str(REFERENCE), "--tei-report", str(TEI_REPORT),
            "--protocol", str(PROTOCOL), "--diagnostic-script", str(SCRIPT), "--output-root", str(output_root),
        ]
        with (
            patch.object(sys, "argv", argv),
            patch.object(diagnostic_controller, "load_contract", return_value=CONTRACT),
            patch.object(diagnostic_controller, "validate_output_root", return_value=run_id),
            patch.object(
                diagnostic_controller,
                "validate_protocol_and_inputs",
                return_value=(protocol_descriptor, inputs, {"estimated_peak_temporary_bytes": 1}, expected),
            ),
            patch.object(diagnostic_controller, "build_create_command", return_value=(container_name, ["docker", "create"])),
            patch.object(diagnostic_controller, "make_deadline_runner", side_effect=[runner, runner]),
            patch.object(diagnostic_controller, "parse_gpu_csv", return_value={"index": 0, "uuid": "GPU-test", "compute_capability": "12.0", "memory_free_mib": 30000}),
            patch.object(diagnostic_controller, "validate_temporary_headroom", return_value=1),
            patch.object(diagnostic_controller, "parse_container_id", return_value=container_id),
            patch.object(diagnostic_controller, "authenticate_ownership"),
            patch.object(diagnostic_controller, "authenticate_container"),
            patch.object(diagnostic_controller, "VramMonitor", return_value=monitor),
            patch.object(diagnostic_controller, "read_json", side_effect=OSError("injected output read failure")),
            patch.object(diagnostic_controller, "descriptor", side_effect=fake_descriptor),
            patch.object(diagnostic_controller, "atomic_write_json", side_effect=lambda path, value, **kwargs: reports.append(copy.deepcopy(value))),
            patch.object(Path, "mkdir"),
            patch.object(Path, "is_file", return_value=True),
            patch("builtins.print"),
        ):
            self.assertEqual(diagnostic_controller.main(), 1)
        self.assertTrue(monitor.stopped)
        self.assertIn(["docker", "stop", "--time", "10", container_id], calls)
        self.assertIn(["docker", "rm", "--force", container_id], calls)
        self.assertTrue(reports)
        report = reports[-1]
        self.assertIn("OSError: injected descriptor failure", report["diagnostic_output_error"])
        self.assertEqual(report["cleanup"]["container_id"], container_id)
        self.assertTrue(report["cleanup"]["absent"])


if __name__ == "__main__":
    unittest.main()
