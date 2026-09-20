#!/usr/bin/env python3
"""Focused bundle and optional built-image container contract checks.

Run without --image for offline preparation checks. With --image, each case
owns one uniquely named disposable container with a bounded private HOME tmpfs.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
import types
import unittest
from unittest.mock import Mock, patch
import uuid

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "docker" / "prepare-managed-gpu.py"
IMAGE: str | None = None

spec = importlib.util.spec_from_file_location("prepare_managed_gpu", SCRIPT)
assert spec is not None and spec.loader is not None
prep = importlib.util.module_from_spec(spec)
spec.loader.exec_module(prep)


def docker(*args: str, timeout: int = 30) -> str:
    result = subprocess.run(
        ["docker", *args], capture_output=True, timeout=timeout, check=False,
        text=True, encoding="utf-8", errors="replace",
    )
    if len(result.stdout) + len(result.stderr) > 65536:
        raise AssertionError("Docker diagnostic exceeded 64 KiB")
    if result.returncode:
        raise AssertionError(f"Docker command failed ({result.returncode}): {args[0]} {result.stderr[-4000:]}")
    # Docker logs normally writes container stderr to its own stderr. Keep the
    # command status strict while inspecting both bounded output streams.
    return (result.stdout + result.stderr if args[0] == "logs" else result.stdout).strip()


class BundlePreparationTest(unittest.TestCase):
    def test_checker_uses_pinned_stack_snapshot_and_packages(self) -> None:
        runner = Mock(return_value=("Managed embedding GPU provenance check passed.", ""))
        module = types.ModuleType("prepare_image")
        module.run = runner
        module.CommandError = type("CommandError", (Exception,), {})
        with tempfile.TemporaryDirectory(prefix="hmem-checker-argv-") as private:
            root = Path(private)
            with patch.dict(sys.modules, {"prepare_image": module}):
                prep.run_checker(root, root / "model", root / "runtime")
        command = runner.call_args.args[0]
        self.assertEqual(command[:4], ["stack", "script", "--resolver", "lts-24.2"])
        self.assertEqual(command[4:-8], [
            "--package", "aeson", "--package", "yaml", "--package", "crypton",
            "--package", "directory", "--package", "filepath",
            "--package", "bytestring", "--package", "text",
            "--package", "containers", "--package", "scientific",
            "--package", "temporary", "--package", "process",
            "--package", "vector",
        ])
        self.assertEqual(command[-8:], [
            str(root / "scripts" / "check-managed-embedding-provenance.hs"),
            "--", "--root", str(root), "--model-root", str(root / "model"),
            "--runtime-root", str(root / "runtime"),
        ])

    def test_checker_failure_keeps_only_bounded_safe_finding(self) -> None:
        class CheckerCommandError(Exception):
            reason = "exit_code"
            stdout = (b"Managed embedding GPU provenance check failed:\n"
                      b"  - size drift for numerical authority: "
                      b"hmem-server/test/fixtures/embedding-gpu-viability/parity-probes-v1.json\n"
                      b"  - token=private-value: secret.txt\n")
            stderr = b"token=private-value [S-5027]"

        module = types.ModuleType("prepare_image")
        module.CommandError = CheckerCommandError
        module.run = Mock(side_effect=CheckerCommandError())
        with tempfile.TemporaryDirectory(prefix="hmem-checker-diagnostic-") as private:
            root = Path(private)
            with patch.dict(sys.modules, {"prepare_image": module}):
                with self.assertRaises(prep.PreparationError) as caught:
                    prep.run_checker(root, root / "model", root / "runtime")
        message = str(caught.exception)
        self.assertIn("size drift for numerical authority: hmem-server/test/fixtures/", message)
        self.assertNotIn("private-value", message)
        self.assertNotIn("secret.txt", message)
        self.assertLess(len(message), 300)

    def test_exact_copy_and_both_checker_passes(self) -> None:
        with tempfile.TemporaryDirectory(prefix="hmem-bundle-test-") as private:
            base = Path(private)
            root = base / "repo"
            model = base / "model"
            runtime = base / "runtime"
            (root / "config").mkdir(parents=True)
            (root / "config" / "managed-embedding-provenance.yaml").write_text("locked\n", encoding="utf-8")
            (model / "nested").mkdir(parents=True)
            (model / "nested" / "tensor").write_bytes(b"\x00\x01")
            runtime.mkdir()
            (runtime / "router").write_bytes(b"router")
            output = base / "bundle"
            report = base / "evidence.json"
            with patch.object(prep, "run_checker") as checker:
                evidence = prep.prepare(root, model, runtime, output, report)
            self.assertEqual(checker.call_count, 2)
            self.assertEqual((output / "model/nested/tensor").read_bytes(), b"\x00\x01")
            self.assertEqual((output / "tei-runtime/router").read_bytes(), b"router")
            self.assertEqual(evidence["model_file_count"], 1)
            self.assertTrue(report.is_file())
            with self.assertRaises(prep.PreparationError):
                prep.prepare(root, model, runtime, output, base / "other.json")

    def test_rejects_unexpected_node_before_copy(self) -> None:
        with tempfile.TemporaryDirectory(prefix="hmem-bundle-test-") as private:
            base = Path(private)
            root = base / "repo"
            model = base / "model"
            runtime = base / "runtime"
            (root / "config").mkdir(parents=True)
            (root / "config" / "managed-embedding-provenance.yaml").write_text("locked", encoding="utf-8")
            model.mkdir()
            runtime.mkdir()
            (model / "good").write_bytes(b"data")
            try:
                (model / "link").symlink_to(model / "good")
            except (OSError, NotImplementedError):
                self.skipTest("this filesystem cannot create symlinks")
            with patch.object(prep, "run_checker"):
                with self.assertRaises(prep.PreparationError):
                    prep.prepare(root, model, runtime, base / "bundle", base / "evidence.json")
            self.assertFalse((base / "bundle").exists())

    def test_checker_failure_leaves_no_accepted_context(self) -> None:
        with tempfile.TemporaryDirectory(prefix="hmem-bundle-test-") as private:
            base = Path(private)
            root = base / "repo"
            model = base / "model"
            runtime = base / "runtime"
            (root / "config").mkdir(parents=True)
            (root / "config" / "managed-embedding-provenance.yaml").write_text("locked", encoding="utf-8")
            model.mkdir()
            runtime.mkdir()
            (model / "weight").write_bytes(b"weight")
            (runtime / "router").write_bytes(b"router")
            with patch.object(prep, "run_checker", side_effect=[None, prep.PreparationError("drift")]):
                with self.assertRaises(prep.PreparationError):
                    prep.prepare(root, model, runtime, base / "bundle", base / "evidence.json")
            self.assertFalse((base / "bundle").exists())
            self.assertFalse((base / "evidence.json").exists())
            self.assertEqual(list(base.glob("bundle.tmp-*")), [])

    def test_destination_traversal_and_source_alias_are_rejected(self) -> None:
        with tempfile.TemporaryDirectory(prefix="hmem-bundle-test-") as private:
            base = Path(private)
            root, model, runtime = base / "repo", base / "model", base / "runtime"
            (root / "config").mkdir(parents=True)
            (root / "config" / "managed-embedding-provenance.yaml").write_text("locked", encoding="utf-8")
            model.mkdir(); runtime.mkdir()
            destination = base / "private"
            destination.mkdir()
            with patch.object(prep, "run_checker") as checker:
                with self.assertRaises(prep.PreparationError):
                    prep.prepare(root, model, runtime,
                                 destination / ".." / "bundle", destination / ".." / "receipt")
                with self.assertRaises(prep.PreparationError):
                    prep.prepare(root, model, runtime, root / "bundle", root / "receipt")
            checker.assert_not_called()
            self.assertFalse((base / "bundle").exists())
            self.assertFalse((root / "bundle").exists())

    def test_symlinked_destination_parent_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory(prefix="hmem-bundle-test-") as private:
            base = Path(private)
            root, model, runtime = base / "repo", base / "model", base / "runtime"
            (root / "config").mkdir(parents=True)
            (root / "config" / "managed-embedding-provenance.yaml").write_text("locked", encoding="utf-8")
            model.mkdir(); runtime.mkdir()
            alias = base / "private-alias"
            try:
                alias.symlink_to(root, target_is_directory=True)
            except (OSError, NotImplementedError):
                self.skipTest("this filesystem cannot create directory symlinks")
            with patch.object(prep, "run_checker") as checker:
                with self.assertRaises(prep.PreparationError):
                    prep.prepare(root, model, runtime, alias / "bundle", alias / "receipt")
            checker.assert_not_called()
            self.assertFalse((root / "bundle").exists())


class ComposeContractTest(unittest.TestCase):
    def render(self, gpu: bool) -> dict:
        command = ["docker", "compose", "--profile", "admin",
                   "-f", str(ROOT / "compose.yaml")]
        if gpu:
            command += ["-f", str(ROOT / "compose.gpu.yaml")]
        command += ["config", "--format", "json"]
        env = os.environ.copy()
        env["HMEM_MANAGED_BUNDLE_CONTEXT"] = "D:/private/verified-managed-bundle"
        result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True,
                                timeout=30, text=True, encoding="utf-8", errors="replace")
        self.assertEqual(result.returncode, 0, result.stderr[-4000:])
        self.assertLess(len(result.stdout), 256 * 1024)
        return json.loads(result.stdout)

    def test_default_and_gpu_override_are_one_service(self) -> None:
        ordinary = self.render(False)
        gpu = self.render(True)
        self.assertEqual(ordinary["services"]["hmem"]["build"]["target"], "runtime")
        self.assertEqual(gpu["services"]["hmem"]["build"]["target"], "gpu-runtime")
        self.assertEqual(gpu["services"]["hmem"]["environment"]["HMEM_EMBEDDING_PROVIDER"], "managed-tei")
        self.assertEqual(set(ordinary["services"]), set(gpu["services"]))
        for service in ("hmem", "migrate"):
            configured = gpu["services"][service]
            self.assertTrue(any(item.startswith("/var/lib/hmem:size=128m,") and
                                all(flag in item for flag in
                                    ("uid=10001", "gid=10001", "mode=0700",
                                     "noexec", "nosuid", "nodev"))
                                for item in configured["tmpfs"]))
            self.assertFalse(any(mount.get("target") == "/var/lib/hmem"
                                 for mount in configured.get("volumes", [])))
        self.assertRegex((ROOT / "compose.yaml").read_text(encoding="utf-8"),
                         r"(?m)^  hmem-data:\s*$")
        self.assertNotIn("hmem-data", gpu.get("volumes", {}))
        self.assertTrue(gpu["services"]["hmem"]["init"])
        self.assertTrue(gpu["services"]["hmem"]["read_only"])
        self.assertEqual(gpu["services"]["hmem"]["stop_signal"], "SIGINT")
        self.assertIn(gpu["services"]["hmem"]["stop_grace_period"], ["3m0s", "180s"])
        reservation = gpu["services"]["hmem"]["deploy"]["resources"]["reservations"]["devices"]
        self.assertEqual(len(reservation), 1)
        self.assertEqual(reservation[0]["driver"], "nvidia")
        self.assertEqual(reservation[0]["capabilities"], ["gpu"])
        self.assertEqual(len(gpu["services"]["hmem"]["ports"]), 1)


class BuiltImageTest(unittest.TestCase):
    def setUp(self) -> None:
        if not IMAGE:
            self.skipTest("pass --image to run disposable built-image checks")
        token = uuid.uuid4().hex[:16]
        self.name = f"hmem-packaging-test-{token}"
        self.cid: str | None = None
        self.create_intent = False
        self.assertEqual(self.exact_id(), "", "one-use test name already exists")

    def exact_id(self) -> str:
        ids = docker("ps", "-aq", "--no-trunc", "--filter", f"name=^/{self.name}$")
        if ids and (len(ids.splitlines()) != 1 or len(ids) != 64):
            raise AssertionError("exact owned container listing malformed")
        return ids

    def tearDown(self) -> None:
        identity = self.exact_id()
        if identity:
            if not self.create_intent:
                raise AssertionError("unowned container appeared under reserved test name")
            records = json.loads(docker("inspect", identity))
            if (len(records) != 1 or records[0].get("Id") != identity or
                records[0].get("Name") != "/" + self.name or
                (records[0].get("Config") or {}).get("Image") != IMAGE or
                ((records[0].get("Config") or {}).get("Labels") or {}).get(
                    "hmem.packaging.test") != self.name):
                raise AssertionError("test container ownership authentication failed")
            docker("rm", "-f", identity)
        self.assertEqual(self.exact_id(), "", "owned test container remained after cleanup")

    def create(self, overrides: dict[str, str], command: str = "hmem-server") -> None:
        assert IMAGE is not None
        args = [
            "create", "--name", self.name, "--label", f"hmem.packaging.test={self.name}",
            "--network", "none", "--read-only", "--init",
            "--tmpfs", "/var/lib/hmem:uid=10001,gid=10001,mode=0700,size=128m,noexec,nosuid,nodev",
            "--tmpfs", "/tmp:uid=10001,gid=10001,mode=1770,size=64m,noexec,nosuid,nodev",
            "--tmpfs", "/run/hmem:uid=10001,gid=10001,mode=0700,size=64m,noexec,nosuid,nodev",
            "--env", "HMEM_DB_HOST=127.0.0.1", "--env", "HMEM_DB_CONNECT_RETRIES=3600",
            "--env", "HMEM_DB_CONNECT_RETRY_DELAY_SECONDS=2",
        ]
        for key, value in sorted(overrides.items()):
            args += ["--env", f"{key}={value}"]
        args += [IMAGE, command]
        # Register intent before create: a daemon-side create can succeed even
        # if the client loses the returned ID. Cleanup recovers by unique name
        # and authenticates label + image before removing anything.
        self.create_intent = True
        self.cid = docker(*args)
        self.assertEqual(len(self.cid), 64)
        docker("start", self.cid)

    def await_config(self) -> str:
        assert self.cid is not None
        end = time.monotonic() + 15
        while time.monotonic() < end:
            result = subprocess.run(
                ["docker", "exec", self.cid, "cat", "/var/lib/hmem/.hmem/config.yaml"],
                capture_output=True, timeout=10, text=True, encoding="utf-8", errors="replace",
            )
            if result.returncode == 0:
                return result.stdout
            time.sleep(0.2)
        self.fail("entrypoint did not generate config within 15 seconds")

    def test_default_config_and_read_only_frontend(self) -> None:
        secret = "packaging-secret-never-in-files"
        self.create({"HMEM_API_KEY": secret})
        config = self.await_config()
        self.assertIn("embedding:\n  mode: 'disabled'", config)
        self.assertIn("  batch_size: 1\n  timeout_ms: 300000\n  retry_attempts: 0", config)
        self.assertNotIn("gpu_profile:", config)
        self.assertNotIn(secret, config)
        assert self.cid is not None
        js = docker("exec", self.cid, "cat", "/run/hmem/static/hmem-runtime-config.js")
        self.assertIn("window.HMEM_CONFIG", js)
        self.assertNotIn(secret, js)
        self.assertEqual(docker("exec", self.cid, "id", "-u"), "10001")
        self.assertEqual(docker("exec", self.cid, "stat", "-c", "%a", "/var/lib/hmem/.hmem/logs"), "700")
        record = json.loads(docker("inspect", self.cid))[0]
        host = record["HostConfig"]
        self.assertTrue(host["ReadonlyRootfs"])
        home_mount = host["Tmpfs"]["/var/lib/hmem"]
        self.assertTrue(all(value in home_mount for value in
                            ("size=128m", "uid=10001", "gid=10001", "mode=0700",
                             "noexec", "nosuid", "nodev")))
        self.assertEqual(docker("exec", self.cid, "stat", "-f", "-c", "%T", "/var/lib/hmem"), "tmpfs")
        docker("exec", self.cid, "test", "-x", "/usr/local/bin/hmem-embedding-http-helper")

    def test_managed_profile_and_bounds(self) -> None:
        self.create({"HMEM_API_KEY": "test-key", "HMEM_EMBEDDING_PROVIDER": "managed-tei",
                     "HMEM_EMBEDDING_BATCH_SIZE": "256", "HMEM_EMBEDDING_TIMEOUT_MS": "300000"})
        config = self.await_config()
        self.assertIn("mode: 'managed-tei'", config)
        self.assertIn("gpu_profile: 'native-tei-gte-qwen2-1.5b-instruct-cuda-sm120-f16-v1'", config)
        self.assertIn("batch_size: 256", config)
        self.assertNotIn("  endpoint:", config)

    def test_http_mode_emits_validated_profile_and_endpoint(self) -> None:
        self.create({"HMEM_API_KEY": "test-key", "HMEM_EMBEDDING_PROVIDER": "http",
                     "HMEM_EMBEDDING_ENDPOINT": "https://tei.example/embed"})
        config = self.await_config()
        self.assertIn("mode: 'http'", config)
        self.assertIn("endpoint: 'https://tei.example/embed'", config)
        self.assertIn("gpu_profile: 'native-tei-gte-qwen2-1.5b-instruct-cuda-sm120-f16-v1'", config)

    def test_private_home_is_ephemeral_across_restart(self) -> None:
        self.create({"HMEM_API_KEY": "stable-external-test-token"})
        self.await_config()
        assert self.cid is not None
        marker = "/var/lib/hmem/.hmem/ephemeral-marker"
        docker("exec", self.cid, "/bin/sh", "-c", f"printf old > {marker}")
        docker("stop", "--time", "1", self.cid, timeout=10)
        docker("start", self.cid)
        self.assertIn("mode: 'disabled'", self.await_config())
        docker("exec", self.cid, "test", "!", "-e", marker)

    def test_incompatible_http_profile_is_rejected_before_database(self) -> None:
        self.create({"HMEM_API_KEY": "test-key", "HMEM_EMBEDDING_PROVIDER": "http",
                     "HMEM_EMBEDDING_ENDPOINT": "https://tei.example/embed",
                     "HMEM_EMBEDDING_GPU_PROFILE": "other"})
        assert self.cid is not None
        self.assertNotEqual(docker("wait", self.cid, timeout=20), "0")
        self.assertIn("unsupported HTTP embedding GPU profile", docker("logs", self.cid))

    def test_rotation_allocation_above_private_home_budget_is_rejected(self) -> None:
        self.create({"HMEM_API_KEY": "test-key", "HMEM_LOG_MAX_SIZE_MB": "17",
                     "HMEM_LOG_BACKUP_COUNT": "5"})
        assert self.cid is not None
        self.assertNotEqual(docker("wait", self.cid, timeout=20), "0")
        self.assertIn("more than 96 MiB", docker("logs", self.cid))

    def test_unknown_create_response_is_recovered_by_authenticated_name(self) -> None:
        actual_docker = docker
        def lose_create_response(*args: str, **kwargs: object) -> str:
            result = actual_docker(*args, **kwargs)
            if args[0] == "create":
                raise AssertionError("simulated create response loss")
            return result
        with patch(__name__ + ".docker", side_effect=lose_create_response):
            with self.assertRaisesRegex(AssertionError, "response loss"):
                self.create({"HMEM_API_KEY": "test-key"})
        self.assertIsNone(self.cid)
        self.assertEqual(len(self.exact_id()), 64)

    def test_invalid_combination_and_auth_fail_closed(self) -> None:
        self.create({"HMEM_EMBEDDING_PROVIDER": "managed-tei", "HMEM_EMBEDDING_ENDPOINT": "http://x/embed"})
        assert self.cid is not None
        self.assertNotEqual(docker("wait", self.cid, timeout=20), "0")
        self.assertIn("managed embedding supplies its own endpoint", docker("logs", self.cid))

    def test_default_auth_fails_before_database(self) -> None:
        self.create({})
        assert self.cid is not None
        self.assertNotEqual(docker("wait", self.cid, timeout=20), "0")
        logs = docker("logs", self.cid)
        self.assertIn("requires HMEM_API_KEY", logs)
        self.assertNotIn("Running database migrations", logs)

    def test_invalid_batch_is_rejected(self) -> None:
        self.create({"HMEM_API_KEY": "test-key", "HMEM_EMBEDDING_BATCH_SIZE": "0"})
        assert self.cid is not None
        self.assertNotEqual(docker("wait", self.cid, timeout=20), "0")
        self.assertIn("HMEM_EMBEDDING_BATCH_SIZE must be >= 1", docker("logs", self.cid))


def main() -> None:
    global IMAGE
    parser = argparse.ArgumentParser()
    parser.add_argument("--image", help="run disposable read-only image tests using this exact image ID/tag")
    args, remainder = parser.parse_known_args()
    IMAGE = args.image
    unittest.main(argv=[sys.argv[0], *remainder], verbosity=2)


if __name__ == "__main__":
    main()
