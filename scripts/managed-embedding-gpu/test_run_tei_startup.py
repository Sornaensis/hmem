from __future__ import annotations

import copy
import hashlib
import json
import math
import unittest
from pathlib import Path

from gpu_contract import ContractError, load_contract
from run_tei_startup import (
    VramMonitor,
    absence_proven,
    authenticate_container,
    build_create_command,
    cleanup_authenticated_container,
    parse_container_id,
    parse_gpu_csv,
    validate_embedding,
    validate_info,
    validate_probe,
)


ROOT = Path(__file__).resolve().parents[2]
CONTRACT_PATH = ROOT / "hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json"
PROBE_PATH = ROOT / "hmem-server/test/fixtures/embedding-gpu-viability/startup-probe-v1.json"


class StartupTests(unittest.TestCase):
    def setUp(self) -> None:
        self.contract = load_contract(CONTRACT_PATH)
        self.probe = json.loads(PROBE_PATH.read_text(encoding="utf-8"))

    def test_probe_bytes_are_frozen(self) -> None:
        validate_probe(self.probe, self.contract)
        changed = copy.deepcopy(self.probe)
        changed["text"] += "x"
        with self.assertRaisesRegex(ContractError, "checksum drift"):
            validate_probe(changed, self.contract)

    def test_create_command_has_exact_gpu_runtime_and_caps(self) -> None:
        name, command = build_create_command(self.contract, "a" * 32)
        self.assertEqual(name, "hmem-aa30-gpu-startup-aaaaaaaaaaaa")
        joined = "\n".join(command)
        for required in (
            self.contract["image"]["reference"], "device=0", "65532:65532", "USE_FLASH_ATTENTION=True",
            "HF_HUB_OFFLINE=1", "TRANSFORMERS_OFFLINE=1", "--read-only", "no-new-privileges",
            "--dtype\nfloat16", "--auto-truncate\nfalse", "--max-batch-tokens\n32768",
            "--network\nnone", "--hostname\n127.0.0.1", "--port\n8080", "dst=/model,readonly",
            "--log-driver\nlocal", "--log-opt\nmax-size=8m", "--log-opt\nmax-file=1",
            "--log-opt\ncompress=false",
        ):
            self.assertIn(required, joined)
        self.assertNotIn("LD_PRELOAD", joined)
        self.assertNotIn("default-prompt", joined)

    def test_info_requires_runtime_contract(self) -> None:
        info = {
            "model_id": "/model", "model_dtype": "float16", "max_input_length": 32768,
            "max_batch_tokens": 32768, "max_concurrent_requests": 8, "max_client_batch_size": 8,
            "auto_truncate": False, "tokenization_workers": 2, "version": "1.9.3",
            "model_type": {"embedding": {"pooling": "last_token"}},
        }
        validate_info(info)
        info["auto_truncate"] = True
        with self.assertRaisesRegex(ContractError, "auto_truncate"):
            validate_info(info)

    def test_embedding_shape_finiteness_and_norm(self) -> None:
        vector = [0.0] * 1536
        vector[0] = 1.0
        self.assertEqual(validate_embedding([vector], 1536, 0.0001)["norm"], 1.0)
        vector[1] = math.nan
        with self.assertRaisesRegex(ContractError, "finite"):
            validate_embedding([vector], 1536, 0.0001)

    def test_gpu_inventory_enforces_sm_and_free_vram(self) -> None:
        value = parse_gpu_csv("0, GPU-123, NVIDIA GeForce RTX 5090, 596.49, 32607, 29492, 2696, 12.0\n", "12.0", 18432)
        self.assertEqual(value["uuid"], "GPU-123")
        with self.assertRaisesRegex(ContractError, "insufficient free VRAM"):
            parse_gpu_csv("0, GPU-123, RTX 5090, 596.49, 32607, 100, 32507, 12.0", "12.0", 18432)

    def test_cleanup_absence_requires_both_successful_empty_queries(self) -> None:
        self.assertTrue(absence_proven("\n", ""))
        self.assertFalse(absence_proven("abc\n", ""))
        self.assertFalse(absence_proven("", "abc\n"))

    def test_cleanup_identity_requires_hex_and_exact_ownership_labels(self) -> None:
        valid = "a" * 64
        self.assertEqual(parse_container_id(valid + "\n"), valid)
        with self.assertRaisesRegex(ContractError, "invalid container ID"):
            parse_container_id("hmem-aa30-gpu-startup-name")
        inspected = [{"Config": {"Labels": {"hmem.task": self.contract["task_id"], "hmem.run": "run-1"}}}]
        authenticate_container(inspected, self.contract, "run-1")
        inspected[0]["Config"]["Labels"]["hmem.run"] = "someone-else"
        with self.assertRaisesRegex(ContractError, "ownership labels"):
            authenticate_container(inspected, self.contract, "run-1")

    def test_vram_monitor_stops_owned_container_on_reserve_breach(self) -> None:
        calls: list[list[str]] = []

        def fake_run(command, _timeout, _cap):
            calls.append(command)
            if command[0] == "nvidia-smi":
                return "0, GPU-123, RTX 5090, 596.49, 32607, 1000, 32000, 12.0\n", ""
            return "", ""

        monitor = VramMonitor(
            ["nvidia-smi"], self.contract["runtime_contract"], "a" * 64, runner=fake_run, interval=0.001
        )
        monitor.start()
        self.assertTrue(monitor.wait_first_sample(timeout=1))
        self.assertTrue(monitor.stop(timeout=1))
        with self.assertRaisesRegex(ContractError, "VRAM monitoring failed"):
            monitor.check()
        self.assertIn(["docker", "stop", "--time", "2", "a" * 64], calls)

    def test_cleanup_continues_after_log_write_failure(self) -> None:
        calls: list[list[str]] = []

        def fake_run(command, _timeout, _cap):
            calls.append(command)
            return ("router log", "") if command[1] == "logs" else ("", "")

        def fail_write(_value):
            raise OSError("synthetic write failure")

        container_id = "b" * 64
        cleanup = cleanup_authenticated_container(
            container_id, "run-2", Path("unused"), 1024, runner=fake_run, log_writer=fail_write
        )
        self.assertIn("synthetic write failure", cleanup["log_write"])
        self.assertEqual(cleanup["stop"], "ok")
        self.assertEqual(cleanup["remove"], "ok")
        self.assertTrue(cleanup["absent"])
        self.assertIn(["docker", "rm", "--force", container_id], calls)


if __name__ == "__main__":
    unittest.main()
