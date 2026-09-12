from __future__ import annotations

import copy
import hashlib
import json
import math
import sys
import time
import unittest
from pathlib import Path

from gpu_contract import (
    ContractError,
    atomic_write_json,
    load_contract,
    validate_contract,
    validate_index_manifest,
    validate_platform_manifest,
)
from prepare_image import CommandError, run


ROOT = Path(__file__).resolve().parents[2]
CONTRACT_PATH = ROOT / "hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json"
PROBES_PATH = ROOT / "hmem-server/test/fixtures/embedding-gpu-viability/parity-probes-v1.json"
GOLDEN_PATH = ROOT / "hmem-server/test/fixtures/embedding-gpu-viability/reference-golden-v1.json"
QUALIFICATION_PATH = ROOT / "hmem-server/test/fixtures/embedding-gpu-viability/gpu-foundation-qualification-v1.json"
TEST_TEMP_ROOT = Path(__file__).resolve().parent
SEMANTIC_SPACE_FINGERPRINT = "Alibaba-NLP/gte-Qwen2-1.5B-instruct@1cad2ab3ff41c2671f34e135d29831368ee26b68:1536:attention=noncausal:v1"


def validate_qualification_acceptance(qualification: dict) -> None:
    compact = qualification["compact_numerical"]
    expected_cases = ["doc_short", "mixed_long", "multilingual", "query_search"]
    if [item["id"] for item in compact["parity"]] != expected_cases:
        raise AssertionError("parity case identity/order drift")
    if [item["id"] for item in compact["batch_order"]] != expected_cases:
        raise AssertionError("batch case identity/order drift")
    for section in ("parity", "batch_order"):
        for item in compact[section]:
            if item["accepted"] is not True:
                raise AssertionError(f"{section} contains a rejected item")

    expected_rows = [["doc_short", "mixed_long"], ["query_search", "multilingual"]]
    rows = compact["concurrent"]
    if len(rows) != 2:
        raise AssertionError("concurrent result must contain two rows")
    for row, expected_ids in zip(rows, expected_rows, strict=True):
        if row["id"] != expected_ids:
            raise AssertionError("concurrent row identity/order drift")
        for key in ("accepted", "cosine", "l2_distance", "maximum_coordinate_absolute_error"):
            if not isinstance(row[key], list) or len(row[key]) != 2:
                raise AssertionError(f"concurrent {key} must contain two items")
        if any(value is not True for value in row["accepted"]):
            raise AssertionError("concurrent result contains a rejected item")


class ContractTests(unittest.TestCase):
    def setUp(self) -> None:
        self.contract = load_contract(CONTRACT_PATH)

    def test_reference_golden_binds_exact_probes_and_finite_unit_vectors(self) -> None:
        probes = json.loads(PROBES_PATH.read_text(encoding="utf-8"))
        golden = json.loads(GOLDEN_PATH.read_text(encoding="utf-8"))
        self.assertEqual(golden["schema_version"], 1)
        self.assertEqual(golden["kind"], "hmem-native-tei-gpu-reference-golden")
        self.assertEqual(golden["semantic_space_fingerprint"], SEMANTIC_SPACE_FINGERPRINT)
        self.assertEqual(golden["reference_method"], "original-sdpa-math-cuda-f16-v3")
        self.assertEqual(golden["model"]["revision"], self.contract["model"]["revision"])
        self.assertEqual(golden["model"]["dimensions"], 1536)
        self.assertEqual(golden["inputs"]["probes"]["sha256"], hashlib.sha256(PROBES_PATH.read_bytes()).hexdigest())
        self.assertEqual(golden["inputs"]["reference_output"], {
            "path": r"D:\hmem-artifacts\aa30a81c-2adf-4f54-b1eb-cfeaa39bac77\gpu-foundation-v1\reference-output\reference-v4.json",
            "bytes": 262019,
            "sha256": "e87b306fd2f4992094198ed8d3026d7732643b2696f6960035e8e262c46c719a",
        })
        self.assertEqual(golden["inputs"]["reference_execution"]["sha256"], "ec2be10cdf4bc8241365a6fda20c36a83ee6cb51cc2e70583a87b12cc1bf3993")
        self.assertEqual(golden["runtime"]["script_sha256"], "752c65c6242d092f657b427b20c80c81b23bbab7abea79fb3d798ea465091982")
        self.assertEqual(golden["runtime"]["attention_module_count"], 28)
        self.assertEqual(golden["runtime"]["rotary_module_count"], 28)
        self.assertEqual(golden["runtime"]["model_loads"], 1)
        self.assertEqual(golden["runtime"]["forwards_completed"], 4)

        expected = {case["id"]: case for case in probes["cases"]}
        self.assertEqual([case["id"] for case in golden["cases"]], ["doc_short", "mixed_long", "multilingual", "query_search"])
        for case in golden["cases"]:
            probe = expected[case["id"]]
            self.assertEqual(case["role"], probe["role"])
            self.assertEqual(case["formatted_utf8_sha256"], probe["utf8_sha256"])
            self.assertEqual(case["token_ids"], probe["token_ids"])
            self.assertEqual(case["token_count"], len(probe["token_ids"]))
            self.assertEqual(case["vector_dimensions"], 1536)
            self.assertTrue(all(math.isfinite(value) for value in case["vector"]))
            self.assertLessEqual(abs(math.sqrt(math.fsum(value * value for value in case["vector"])) - 1.0), 1e-6)
            evidence = case["attention_backend_evidence"]
            self.assertEqual(evidence["scaled_dot_product_attention_calls"], 28)
            self.assertEqual(evidence["implementation_operator_counts"], {"aten::_scaled_dot_product_attention_math": 28})

    def test_qualification_binds_actual_runtime_results_and_scope(self) -> None:
        qualification = json.loads(QUALIFICATION_PATH.read_text(encoding="utf-8"))
        self.assertEqual(qualification["schema_version"], 1)
        self.assertEqual(qualification["kind"], "hmem-native-tei-gpu-foundation-qualification")
        self.assertEqual(qualification["state"], "qualified")
        self.assertEqual(qualification["base_sha"], "ba60c4ecafdac00932fd030876041e347f222948")
        self.assertEqual(qualification["semantic_space_fingerprint"], SEMANTIC_SPACE_FINGERPRINT)
        self.assertEqual(qualification["semantic_space_fingerprint"], json.loads(GOLDEN_PATH.read_text(encoding="utf-8"))["semantic_space_fingerprint"])
        self.assertEqual(qualification["artifacts"]["golden"]["sha256"], hashlib.sha256(GOLDEN_PATH.read_bytes()).hexdigest())
        self.assertEqual(qualification["artifacts"]["reference_output"]["sha256"], "e87b306fd2f4992094198ed8d3026d7732643b2696f6960035e8e262c46c719a")
        self.assertEqual(qualification["artifacts"]["reference_execution"]["sha256"], "ec2be10cdf4bc8241365a6fda20c36a83ee6cb51cc2e70583a87b12cc1bf3993")
        self.assertEqual(qualification["artifacts"]["tei_report"], {
            "path": r"D:\hmem-artifacts\aa30a81c-2adf-4f54-b1eb-cfeaa39bac77\gpu-foundation-v1\validation\6ed32e069c3a41038f1a7e3c95da5693\foundation-validation-report-v1.json",
            "bytes": 562758,
            "sha256": "012eaaa5fbb98328e71210231e49c179d57a3fec8fa177e085a6c86cc02cc630",
        })
        self.assertEqual(qualification["reference"]["method"], "original-sdpa-math-cuda-f16-v3")
        self.assertEqual(qualification["reference"]["image"]["id"], "sha256:24ece9d9b0ff4710e0c9b955e3c67c520b18b9504256043af1f7a324a8a51582")
        self.assertEqual(qualification["native_tei"]["info"]["version"], "1.9.3")
        self.assertEqual(qualification["native_tei"]["info"]["model_dtype"], "float16")
        self.assertFalse(qualification["native_tei"]["info"]["auto_truncate"])
        self.assertEqual(qualification["compact_numerical"]["maximum_probe_tokens"], 2048)
        validate_qualification_acceptance(qualification)
        self.assertFalse(qualification["compact_numerical"]["wrong_prompt_negative"]["accepted"])
        self.assertEqual(qualification["full_context"]["exact_32768"]["status"], 200)
        self.assertEqual(qualification["full_context"]["exact_32768"]["token_count"], 32768)
        self.assertEqual(qualification["full_context"]["exact_32769"]["status"], 422)
        self.assertEqual(qualification["full_context"]["exact_32769"]["token_count"], 32769)
        self.assertEqual(qualification["measurements"]["observed_peak_vram_mib"], 8724)
        self.assertEqual(qualification["measurements"]["recommendations"], {
            "full_context_request_timeout_seconds": 300,
            "policy": "ceil startup/full x1.5, short maximum x2; retain frozen minima 120/30/300",
            "short_request_timeout_seconds": 30,
            "startup_timeout_seconds": 120,
            "validated_compact_batch_items": 4,
            "validated_concurrent_requests": 2,
        })
        self.assertTrue(qualification["cleanup"]["reference"]["absent"])
        self.assertTrue(qualification["cleanup"]["native_tei"]["absent"])
        self.assertIn("No independent full-32768 reference comparison was performed.", qualification["scope"]["exclusions"])
        self.assertEqual(len(qualification["historical_evidence"]["failed_reference_launches"]), 4)
        self.assertEqual(len(qualification["historical_evidence"]["fp32_reference_and_tei_failure"]), 5)

    def test_qualification_rejects_one_false_concurrent_item(self) -> None:
        qualification = json.loads(QUALIFICATION_PATH.read_text(encoding="utf-8"))
        qualification["compact_numerical"]["concurrent"][1]["accepted"][0] = False
        with self.assertRaisesRegex(AssertionError, "rejected item"):
            validate_qualification_acceptance(qualification)

    def test_contract_is_locked_and_bounded(self) -> None:
        self.assertEqual(len(self.contract["model"]["artifacts"]), 20)
        self.assertEqual(self.contract["preparation_limits"]["attempts"], 1)
        self.assertLessEqual(
            sum(layer["bytes"] for layer in self.contract["image"]["layers"]),
            self.contract["preparation_limits"]["max_compressed_image_bytes"],
        )

    def test_index_requires_exact_single_platform(self) -> None:
        value = {
            "mediaType": "application/vnd.oci.image.index.v1+json",
            "manifests": [{"digest": self.contract["image"]["platform_manifest_digest"], "platform": self.contract["image"]["platform"]}],
        }
        validate_index_manifest(self.contract, value)
        value["manifests"][0]["digest"] = "sha256:" + "0" * 64
        with self.assertRaisesRegex(ContractError, "digest drift"):
            validate_index_manifest(self.contract, value)

    def test_platform_manifest_rejects_layer_drift(self) -> None:
        value = {
            "mediaType": "application/vnd.oci.image.manifest.v1+json",
            "config": {"digest": self.contract["image"]["config_digest"]},
            "layers": [{"digest": item["digest"], "size": item["bytes"]} for item in self.contract["image"]["layers"]],
        }
        validate_platform_manifest(self.contract, value)
        value["layers"][0]["size"] += 1
        with self.assertRaisesRegex(ContractError, "layer order"):
            validate_platform_manifest(self.contract, value)

    def test_contract_rejects_floating_image_reference(self) -> None:
        changed = copy.deepcopy(self.contract)
        changed["image"]["reference"] = "ghcr.io/huggingface/text-embeddings-inference:120-1.9.3"
        with self.assertRaisesRegex(ContractError, "digest-pinned"):
            validate_contract(changed)

    def test_atomic_json_cap_fails_before_replacing_destination(self) -> None:
        path = TEST_TEMP_ROOT / "must-not-exist-report.json"
        self.assertFalse(path.exists())
        with self.assertRaisesRegex(ContractError, "exceeds cap"):
            atomic_write_json(path, {"large": "x" * 100}, max_bytes=10)
        self.assertFalse(path.exists())

    def test_command_output_cap_catches_fast_exit(self) -> None:
        with self.assertRaises(CommandError) as raised:
            run([sys.executable, "-c", "import sys; sys.stdout.write('x' * 4097)"], 10, 4096)
        self.assertEqual(raised.exception.reason, "output_cap")
        self.assertEqual(len(raised.exception.stdout) + len(raised.exception.stderr), 4096)

    @unittest.skipUnless(sys.platform == "win32", "Windows process-tree regression")
    def test_command_timeout_terminates_descendant_that_retains_pipe(self) -> None:
        child = "import time; time.sleep(60)"
        parent = (
            "import subprocess,sys,time; "
            f"subprocess.Popen([sys.executable,'-c',{child!r}], stdout=sys.stdout, stderr=sys.stderr); "
            "print('spawned', flush=True); time.sleep(60)"
        )
        started = time.monotonic()
        with self.assertRaises(CommandError) as raised:
            run([sys.executable, "-c", parent], 5, 4096)
        self.assertEqual(raised.exception.reason, "timeout")
        self.assertLessEqual(time.monotonic() - started, 5.5)
        self.assertNotEqual(raised.exception.reason, "unresolved_streams")


if __name__ == "__main__":
    unittest.main()
