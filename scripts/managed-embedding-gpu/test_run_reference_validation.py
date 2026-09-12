from __future__ import annotations

import copy
import unittest
from pathlib import Path
from unittest.mock import patch

from gpu_contract import ContractError, load_contract
from run_reference_validation import (
    CPUS,
    IMAGE_ID,
    MEMORY_BYTES,
    MEMORY_SWAP_BYTES,
    PIDS,
    REFERENCE_METHOD,
    authenticate_container,
    authenticate_ownership,
    build_create_command,
    cleanup_failures,
    make_deadline_runner,
    recover_owned_container,
    validate_reference_output,
)


ROOT = Path(__file__).resolve().parents[2]
CONTRACT = load_contract(ROOT / "hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json")
PROBES = ROOT / "hmem-server/test/fixtures/embedding-gpu-viability/parity-probes-v1.json"
OUTPUT = Path(CONTRACT["artifact_root"]) / "reference-output"


def inspected(run_id: str, name: str) -> list[dict]:
    return [{
        "Name": "/" + name,
        "Config": {"Image": IMAGE_ID, "Labels": {"io.hmem.task": CONTRACT["task_id"], "io.hmem.run": run_id}},
        "HostConfig": {
            "NetworkMode": "none", "Memory": MEMORY_BYTES, "MemorySwap": MEMORY_SWAP_BYTES,
            "NanoCpus": CPUS * 1_000_000_000, "PidsLimit": PIDS, "ReadonlyRootfs": True,
            "LogConfig": {"Type": "local", "Config": {"compress": "false", "max-file": "1", "max-size": "8m"}},
            "CapDrop": ["ALL"], "SecurityOpt": ["no-new-privileges"],
            "DeviceRequests": [{"DeviceIDs": ["0"], "Capabilities": [["gpu"]]}],
        },
        "Mounts": [
            {"Destination": "/model", "Source": CONTRACT["model"]["source_root"], "RW": False},
            {"Destination": "/inputs/probes.json", "Source": str(PROBES), "RW": False},
            {"Destination": "/output", "Source": str(OUTPUT), "RW": True},
        ],
    }]


class ReferenceSupervisorTests(unittest.TestCase):
    def test_deadline_runner_clips_near_deadline(self) -> None:
        calls = []

        def runner(command, timeout, cap):
            calls.append((command, timeout, cap))
            return "", ""

        bounded = make_deadline_runner(10.0, "execution", clock=lambda: 9.75, runner=runner)
        bounded(["docker", "inspect", "x"], 15, 1024)
        self.assertEqual(calls, [(["docker", "inspect", "x"], 0.25, 1024)])

    def test_deadline_runner_refuses_launch_when_expired(self) -> None:
        called = False

        def runner(command, timeout, cap):
            nonlocal called
            called = True
            return "", ""

        bounded = make_deadline_runner(10.0, "cleanup", clock=lambda: 10.0, runner=runner)
        with self.assertRaisesRegex(ContractError, "deadline exhausted before subprocess launch"):
            bounded(["docker", "ps"], 5, 1024)
        self.assertFalse(called)

    def test_create_command_freezes_offline_caps_and_mounts(self) -> None:
        name, command = build_create_command(CONTRACT, PROBES, OUTPUT, "reference.json", "a" * 32)
        joined = "\n".join(command)
        for expected in (IMAGE_ID, "--network\nnone", str(MEMORY_BYTES), str(MEMORY_SWAP_BYTES), str(CPUS), str(PIDS),
                         "HF_HUB_OFFLINE=1", "NVIDIA_DRIVER_CAPABILITIES=compute,utility", "dst=/model,readonly",
                         "dst=/inputs/probes.json,readonly", "dst=/output"):
            self.assertIn(expected, joined)
        self.assertTrue(name.endswith("a" * 12))
        self.assertEqual(REFERENCE_METHOD, "original-sdpa-math-cuda-f16-v3")
        self.assertEqual(MEMORY_BYTES, 21_474_836_480)
        self.assertEqual(MEMORY_SWAP_BYTES, 25_769_803_776)

    def test_inspect_authenticates_identity_caps_gpu_and_mounts(self) -> None:
        run_id = "b" * 32
        name, _ = build_create_command(CONTRACT, PROBES, OUTPUT, "reference.json", run_id)
        authenticate_container(inspected(run_id, name), CONTRACT, PROBES, OUTPUT, run_id, name)
        changed = copy.deepcopy(inspected(run_id, name))
        changed[0]["HostConfig"]["DeviceRequests"][0]["DeviceIDs"] = ["1"]
        with self.assertRaisesRegex(ContractError, "GPU selection"):
            authenticate_container(changed, CONTRACT, PROBES, OUTPUT, run_id, name)

    def test_cleanup_failure_aggregation_rejects_stop_even_after_remove(self) -> None:
        failures = cleanup_failures({"logs": "ok", "log_write": "ok", "authentication": "ok", "stop": "failed: timeout", "remove": "ok", "absent": True, "within_deadline": True})
        self.assertEqual(failures, ["stop"])

    def test_owned_recovery_survives_full_contract_failure(self) -> None:
        run_id = "c" * 32
        name, _ = build_create_command(CONTRACT, PROBES, OUTPUT, "reference.json", run_id)
        value = inspected(run_id, name)
        value[0]["HostConfig"]["Memory"] = 1
        container_id = "d" * 64
        calls = []

        def runner(command, timeout, cap):
            calls.append(command)
            return (container_id + "\n", "") if command[1:3] == ["ps", "-aq"] else (__import__("json").dumps(value), "")

        self.assertEqual(recover_owned_container(name, run_id, runner=runner), container_id)
        authenticate_ownership(value, run_id, name)
        with self.assertRaisesRegex(ContractError, "host contract"):
            authenticate_container(value, CONTRACT, PROBES, OUTPUT, run_id, name)
        self.assertEqual(calls[0][0:3], ["docker", "ps", "-aq"])

    def test_supervisor_uses_strict_shared_reference_consumer(self) -> None:
        value = {"runtime": {"reference_method": REFERENCE_METHOD}}
        summary = {"reference_method": REFERENCE_METHOD, "case_ids": ["doc_short"]}
        with patch("run_foundation_validation.validate_reference_report") as validate, patch(
            "run_foundation_validation.reference_validation_summary", return_value=summary
        ) as summarize:
            self.assertEqual(validate_reference_output(value, "a" * 64, CONTRACT), summary)
        validate.assert_called_once_with(value, "a" * 64, CONTRACT)
        summarize.assert_called_once_with(value)


if __name__ == "__main__":
    unittest.main()
