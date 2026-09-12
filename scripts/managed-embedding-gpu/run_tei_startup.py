from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import sys
import threading
import time
import uuid
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from gpu_contract import ContractError, atomic_write_json, load_contract, read_json, sha256_file, validate_locked_files
from prepare_image import CommandError, run


CONTAINER_PREFIX = "hmem-aa30-gpu-startup-"
TASK_LABEL = "hmem.task=aa30a81c-2adf-4f54-b1eb-cfeaa39bac77"
CONTAINER_ID_LENGTH = 64


def build_create_command(contract: dict[str, Any], run_id: str) -> tuple[str, list[str]]:
    name = CONTAINER_PREFIX + run_id[:12]
    runtime = contract["runtime_contract"]
    model = str(Path(contract["artifact_root"]) / "model")
    command = [
        "docker", "create", "--name", name,
        "--label", TASK_LABEL,
        "--label", f"hmem.run={run_id}",
        "--gpus", f"device={runtime['gpu_device']}",
        "--user", "65532:65532",
        "--read-only",
        "--cap-drop", "ALL",
        "--security-opt", "no-new-privileges",
        "--memory", str(runtime["host_memory_bytes"]),
        "--memory-swap", str(runtime["host_memory_swap_bytes"]),
        "--cpus", str(runtime["host_cpus"]),
        "--pids-limit", str(runtime["pids_limit"]),
        "--log-driver", "local",
        "--log-opt", "max-size=8m",
        "--log-opt", "max-file=1",
        "--log-opt", "compress=false",
        "--shm-size", "1073741824",
        "--tmpfs", "/tmp:rw,noexec,nosuid,nodev,size=1073741824",
        "--tmpfs", "/data:rw,noexec,nosuid,nodev,size=67108864",
        "--network", "none",
        "--mount", f"type=bind,src={model},dst=/model,readonly",
        "--env", "USE_FLASH_ATTENTION=True",
        "--env", "HF_HUB_OFFLINE=1",
        "--env", "TRANSFORMERS_OFFLINE=1",
        "--env", "HF_HUB_DISABLE_TELEMETRY=1",
        "--env", "HOME=/tmp",
        contract["image"]["reference"],
        "--model-id", "/model",
        "--dtype", "float16",
        "--max-batch-tokens", "32768",
        "--auto-truncate", "false",
        "--max-concurrent-requests", "8",
        "--max-client-batch-size", "8",
        "--tokenization-workers", "2",
        "--hostname", "127.0.0.1",
        "--port", "8080",
        "--prometheus-port", "9000",
        "--json-output",
    ]
    return name, command


def validate_probe(probe: Any, contract: dict[str, Any]) -> dict[str, Any]:
    if not isinstance(probe, dict) or probe.get("schema_version") != 1:
        raise ContractError("invalid startup probe schema")
    if probe.get("task_id") != contract["task_id"] or probe.get("role") != "document":
        raise ContractError("unexpected startup probe identity or role")
    text = probe.get("text")
    if not isinstance(text, str) or not text:
        raise ContractError("startup probe text must be nonempty")
    observed = hashlib.sha256(text.encode("utf-8")).hexdigest()
    if observed != probe.get("utf8_sha256"):
        raise ContractError("startup probe UTF-8 checksum drift")
    if probe.get("request") != {"normalize": True, "truncate": False, "prompt_name": None}:
        raise ContractError("startup request contract drift")
    expected = probe.get("expected", {})
    if expected.get("eos_token_id") != contract["model"]["eos_token_id"] or expected.get("dimensions") != contract["model"]["dimensions"]:
        raise ContractError("startup probe semantic drift")
    return probe


def validate_info(info: Any) -> None:
    if not isinstance(info, dict):
        raise ContractError("/info response must be an object")
    expected = {
        "model_id": "/model",
        "model_dtype": "float16",
        "max_input_length": 32768,
        "max_batch_tokens": 32768,
        "max_concurrent_requests": 8,
        "max_client_batch_size": 8,
        "auto_truncate": False,
        "tokenization_workers": 2,
        "version": "1.9.3",
    }
    for key, value in expected.items():
        if info.get(key) != value:
            raise ContractError(f"/info mismatch for {key}: expected {value!r}, observed {info.get(key)!r}")
    model_type = info.get("model_type")
    if not isinstance(model_type, dict) or model_type.get("embedding", {}).get("pooling") not in {"last_token", "last-token"}:
        raise ContractError(f"/info does not prove last-token pooling: {model_type!r}")


def docker_http_json(container_id: str, method: str, path: str, payload: Any, timeout: int, max_bytes: int) -> Any:
    command = [
        "docker", "exec", container_id, "curl", "--silent", "--show-error", "--fail-with-body",
        "--max-time", str(timeout), "--request", method, "--header", "Accept: application/json",
    ]
    if payload is not None:
        command.extend([
            "--header", "Content-Type: application/json", "--data-binary",
            json.dumps(payload, ensure_ascii=False, separators=(",", ":")),
        ])
    command.append("http://127.0.0.1:8080" + path)
    stdout, _stderr = run(command, timeout + 5, max_bytes)
    try:
        value = json.loads(stdout)
    except json.JSONDecodeError as exc:
        raise ContractError(f"invalid JSON response from container path {path}: {exc}") from exc
    return value


def validate_embedding(value: Any, dimensions: int, norm_error: float) -> dict[str, float]:
    if not isinstance(value, list) or len(value) != 1 or not isinstance(value[0], list):
        raise ContractError("/embed response must contain exactly one vector")
    vector = value[0]
    if len(vector) != dimensions or any(not isinstance(item, (int, float)) or isinstance(item, bool) or not math.isfinite(item) for item in vector):
        raise ContractError("/embed vector is not finite 1536-dimensional data")
    norm = math.sqrt(math.fsum(float(item) * float(item) for item in vector))
    if abs(norm - 1.0) > norm_error:
        raise ContractError(f"/embed vector unit-norm drift: {norm}")
    return {"norm": norm, "minimum": min(vector), "maximum": max(vector)}


def parse_gpu_csv(text: str, required_compute: str, minimum_free_mib: int) -> dict[str, Any]:
    lines = [line.strip() for line in text.splitlines() if line.strip()]
    if len(lines) != 1:
        raise ContractError(f"expected one GPU, observed {len(lines)}")
    fields = [field.strip() for field in lines[0].split(",")]
    if len(fields) != 8:
        raise ContractError(f"unexpected nvidia-smi field count: {len(fields)}")
    result = {
        "index": int(fields[0]), "uuid": fields[1], "name": fields[2], "driver_version": fields[3],
        "memory_total_mib": int(fields[4]), "memory_free_mib": int(fields[5]), "memory_used_mib": int(fields[6]),
        "compute_capability": fields[7],
    }
    if result["compute_capability"] != required_compute:
        raise ContractError(f"GPU compute capability mismatch: {result['compute_capability']}")
    if result["memory_free_mib"] < minimum_free_mib:
        raise ContractError(f"insufficient free VRAM: {result['memory_free_mib']} MiB")
    return result


def absence_proven(by_id: str, by_label: str) -> bool:
    return not by_id.strip() and not by_label.strip()


def parse_container_id(text: str) -> str:
    value = text.strip()
    if len(value) != CONTAINER_ID_LENGTH or any(ch not in "0123456789abcdef" for ch in value):
        raise ContractError(f"invalid container ID: {value!r}")
    return value


def authenticate_container(inspect: Any, contract: dict[str, Any], run_id: str) -> dict[str, Any]:
    if not isinstance(inspect, list) or len(inspect) != 1 or not isinstance(inspect[0], dict):
        raise ContractError("container inspection did not return one record")
    record = inspect[0]
    labels = record.get("Config", {}).get("Labels", {})
    if labels.get("hmem.task") != contract["task_id"] or labels.get("hmem.run") != run_id:
        raise ContractError("container ownership labels drift")
    return record


def recover_owned_container(contract: dict[str, Any], run_id: str, *, runner=run) -> tuple[str | None, dict[str, Any] | None]:
    found, _ = runner(
        ["docker", "ps", "-aq", "--no-trunc", "--filter", f"label={TASK_LABEL}", "--filter", f"label=hmem.run={run_id}"],
        10,
        65536,
    )
    candidates = [parse_container_id(line) for line in found.splitlines() if line.strip()]
    if not candidates:
        return None, None
    if len(candidates) != 1:
        raise ContractError(f"ambiguous owned-container recovery: {len(candidates)} matches")
    inspected, _ = runner(["docker", "inspect", candidates[0]], 10, 1048576)
    record = authenticate_container(json.loads(inspected), contract, run_id)
    return candidates[0], record


def read_router_runtime(container_id: str) -> dict[str, Any]:
    environment_raw, _ = run(
        ["docker", "exec", container_id, "sh", "-c", "tr '\\000' '\\n' < /proc/1/environ"], 15, 1048576
    )
    cmdline, _ = run(
        ["docker", "exec", container_id, "sh", "-c", "tr '\\000' ' ' < /proc/1/cmdline"], 15, 1048576
    )
    maps, _ = run(
        ["docker", "exec", container_id, "sh", "-c", "grep -E 'libcuda|libcudart|libcublas' /proc/1/maps"],
        15,
        1048576,
    )
    environment = dict(line.split("=", 1) for line in environment_raw.splitlines() if "=" in line)
    if "text-embeddings-router" not in cmdline:
        raise ContractError("PID 1 is not the native text-embeddings-router")
    loader_path = environment.get("LD_LIBRARY_PATH", "")
    if "/usr/local/cuda/compat" in loader_path:
        raise ContractError("CUDA compatibility loader was unexpectedly active in the router process")
    return {"pid1_cmdline": cmdline.strip(), "environment": environment, "cuda_loaded_maps": maps.splitlines()}


class VramMonitor:
    def __init__(self, query: list[str], runtime: dict[str, Any], container_id: str, *, runner=run, interval: float = 1.0, max_samples: int = 1024):
        self.query = query
        self.runtime = runtime
        self.container_id = container_id
        self.runner = runner
        self.interval = interval
        self.max_samples = max_samples
        self.samples: list[dict[str, Any]] = []
        self.failure: str | None = None
        self.cleanup_trigger: str | None = None
        self._stop = threading.Event()
        self._first_sample_done = threading.Event()
        self._thread = threading.Thread(target=self._loop, daemon=True)

    def start(self) -> None:
        self._thread.start()

    def _loop(self) -> None:
        try:
            while not self._stop.is_set():
                raw, _ = self.runner(self.query, 8, 65536)
                sample = parse_gpu_csv(raw, self.runtime["required_compute_capability"], 0)
                self.samples.append(sample)
                allowed = sample["memory_total_mib"] - self.runtime["reserved_vram_mib"]
                if sample["memory_used_mib"] > allowed:
                    raise ContractError(f"VRAM reserve breached: {sample['memory_used_mib']} MiB used > {allowed} MiB")
                if len(self.samples) >= self.max_samples:
                    raise ContractError("VRAM monitor sample cap reached")
                self._first_sample_done.set()
                self._stop.wait(self.interval)
        except BaseException as exc:
            self.failure = f"{type(exc).__name__}: {exc}"
            self._first_sample_done.set()
            try:
                self.runner(["docker", "stop", "--time", "2", self.container_id], 6, 65536)
                self.cleanup_trigger = "stop_ok"
            except BaseException as stop_exc:
                self.cleanup_trigger = f"stop_failed: {stop_exc}"

    def check(self) -> None:
        if self.failure is not None:
            raise ContractError(f"VRAM monitoring failed: {self.failure}")

    def wait_first_sample(self, timeout: float) -> bool:
        return self._first_sample_done.wait(timeout) and bool(self.samples)

    def stop(self, timeout: float = 10.0) -> bool:
        self._stop.set()
        self._thread.join(timeout=timeout)
        return not self._thread.is_alive()


def cleanup_authenticated_container(
    container_id: str,
    run_id: str,
    result_root: Path,
    max_log_bytes: int,
    *,
    runner=run,
    log_writer=None,
) -> dict[str, Any]:
    cleanup: dict[str, Any] = {"container_id": container_id, "logs": None, "stop": None, "remove": None, "absent": None}
    log_bytes = b""
    try:
        log_out, log_err = runner(["docker", "logs", container_id], 8, max_log_bytes)
        log_bytes = (log_out + log_err).encode("utf-8")
        cleanup["logs"] = "ok"
    except BaseException as exc:
        if isinstance(exc, CommandError):
            log_bytes = exc.stdout + exc.stderr
            cleanup["logs"] = f"failed: {exc.reason}"
        else:
            cleanup["logs"] = f"failed: {type(exc).__name__}: {exc}"
    if log_bytes:
        try:
            writer = log_writer or (result_root / "tei-final.log").write_bytes
            writer(log_bytes)
            cleanup["log_sha256"] = hashlib.sha256(log_bytes).hexdigest()
        except BaseException as exc:
            cleanup["log_write"] = f"failed: {type(exc).__name__}: {exc}"
    try:
        runner(["docker", "stop", "--time", "20", container_id], 25, 65536)
        cleanup["stop"] = "ok"
    except BaseException as exc:
        cleanup["stop"] = f"failed: {type(exc).__name__}: {exc}"
    try:
        runner(["docker", "rm", "--force", container_id], 10, 65536)
        cleanup["remove"] = "ok"
    except BaseException as exc:
        cleanup["remove"] = f"failed: {type(exc).__name__}: {exc}"
    try:
        by_id, _ = runner(["docker", "ps", "-aq", "--no-trunc", "--filter", f"id={container_id}"], 5, 65536)
        by_label, _ = runner(["docker", "ps", "-aq", "--no-trunc", "--filter", f"label=hmem.run={run_id}"], 5, 65536)
        cleanup["absent"] = absence_proven(by_id, by_label)
    except BaseException as exc:
        cleanup["absence_check"] = f"failed: {type(exc).__name__}: {exc}"
        cleanup["absent"] = False
    return cleanup


def main() -> int:
    parser = argparse.ArgumentParser(description="Run one bounded native TEI CUDA startup and short embedding probe.")
    parser.add_argument("--contract", type=Path, required=True)
    parser.add_argument("--probe", type=Path, required=True)
    args = parser.parse_args()
    started = time.monotonic()
    started_utc = datetime.now(timezone.utc).isoformat()
    contract = load_contract(args.contract.resolve())
    probe = validate_probe(read_json(args.probe.resolve()), contract)
    runtime = contract["runtime_contract"]
    artifact_root = Path(contract["artifact_root"])
    run_id = uuid.uuid4().hex
    result_root = artifact_root / "startup" / run_id
    result_root.mkdir(parents=True, exist_ok=False)
    report: dict[str, Any] = {
        "schema_version": 1, "state": "started", "task_id": contract["task_id"], "run_id": run_id,
        "contract": {"path": str(args.contract.resolve()), "sha256": sha256_file(args.contract.resolve())},
        "probe": {"path": str(args.probe.resolve()), "sha256": sha256_file(args.probe.resolve())},
        "limits": runtime,
        "controller": {"pid": os.getpid(), "started_utc": started_utc, "monotonic_started": started},
    }
    print(json.dumps({"state": "launched", "run_id": run_id, "controller": report["controller"]}, sort_keys=True), flush=True)
    container_id: str | None = None
    monitor: VramMonitor | None = None
    first_error: BaseException | None = None
    try:
        preparation = read_json(artifact_root / "preparation-report-v1.json")
        if preparation.get("state") != "prepared" or preparation.get("contract", {}).get("sha256") != report["contract"]["sha256"]:
            raise ContractError("preparation report does not match this contract")
        validate_locked_files(artifact_root / "model", contract["model"]["artifacts"], exact=True)
        output_cap = runtime["max_log_bytes"]
        query = ["nvidia-smi", "--query-gpu=index,uuid,name,driver_version,memory.total,memory.free,memory.used,compute_cap", "--format=csv,noheader,nounits"]
        host_gpu_raw, _ = run(query, 15, 65536)
        report["host_gpu_before"] = parse_gpu_csv(host_gpu_raw, runtime["required_compute_capability"], runtime["minimum_free_vram_mib"])
        existing, _ = run(["docker", "ps", "-aq", "--filter", f"label={TASK_LABEL}"], 15, 65536)
        if existing.strip():
            raise ContractError("another task-owned GPU container exists; model processes must be serialized")
        local_image_raw, _ = run(["docker", "image", "inspect", contract["image"]["reference"]], 15, 1048576)
        local_images = json.loads(local_image_raw)
        if not isinstance(local_images, list) or len(local_images) != 1:
            raise ContractError("pinned image is not present locally")
        local_image = local_images[0]
        if local_image.get("Architecture") != "amd64" or local_image.get("Os") != "linux":
            raise ContractError("local image platform drift")
        if not any(value.endswith("@" + contract["image"]["index_digest"]) for value in local_image.get("RepoDigests", [])):
            raise ContractError("local image does not retain the pinned index digest")
        if local_image.get("Config", {}).get("Entrypoint") != ["./entrypoint.sh"]:
            raise ContractError("local image entrypoint drift")

        name, create_command = build_create_command(contract, run_id)
        report["container_name"] = name
        report["create_argv"] = create_command
        created, _ = run(create_command, 60, 65536)
        candidate_id = parse_container_id(created)
        inspect_raw, _ = run(["docker", "inspect", candidate_id], 15, 1048576)
        inspected = authenticate_container(json.loads(inspect_raw), contract, run_id)
        container_id = candidate_id
        report["container_id"] = container_id
        host = inspected.get("HostConfig", {})
        container_config = inspected.get("Config", {})
        if container_config.get("Image") != contract["image"]["reference"]:
            raise ContractError("container image reference drift")
        if inspected.get("Path") != "./entrypoint.sh":
            raise ContractError(f"container does not use the authenticated CUDA entrypoint: {inspected.get('Path')!r}")
        expected_host = {
            "Memory": runtime["host_memory_bytes"], "MemorySwap": runtime["host_memory_swap_bytes"],
            "NanoCpus": runtime["host_cpus"] * 1_000_000_000, "PidsLimit": runtime["pids_limit"],
            "ReadonlyRootfs": True,
        }
        for key, expected in expected_host.items():
            if host.get(key) != expected:
                raise ContractError(f"container cap mismatch for {key}: {host.get(key)!r}")
        if host.get("NetworkMode") != "none" or host.get("CapDrop") != ["ALL"] or "no-new-privileges" not in (host.get("SecurityOpt") or []):
            raise ContractError("container network/capability/security contract drift")
        expected_log_config = {"Type": "local", "Config": {"compress": "false", "max-file": "1", "max-size": "8m"}}
        if host.get("LogConfig") != expected_log_config:
            raise ContractError(f"container log rotation contract drift: {host.get('LogConfig')!r}")
        mounts = inspected.get("Mounts", [])
        if len(mounts) != 1 or mounts[0].get("Destination") != "/model" or mounts[0].get("RW") is not False:
            raise ContractError("container model mount is not the sole read-only bind")
        report["container_caps"] = expected_host
        report["container_log_config"] = host["LogConfig"]
        monitor = VramMonitor(query, runtime, container_id)
        monitor.start()
        if not monitor.wait_first_sample(10.0):
            raise ContractError("VRAM monitor did not produce an initial sample")
        monitor.check()
        run(["docker", "start", container_id], 30, 65536)

        ready_deadline = time.monotonic() + runtime["startup_timeout_seconds"]
        info = None
        while time.monotonic() < ready_deadline:
            monitor.check()
            state_raw, _ = run(["docker", "inspect", "--format", "{{json .State}}", container_id], 10, 65536)
            state = json.loads(state_raw)
            if state.get("Status") == "exited":
                raise ContractError(f"TEI exited before readiness with code {state.get('ExitCode')}")
            try:
                info = docker_http_json(container_id, "GET", "/info", None, 2, 1048576)
                break
            except (ContractError, CommandError):
                pass
            time.sleep(2)
        if info is None:
            raise ContractError("TEI startup deadline expired")
        monitor.check()
        ready_seconds = time.monotonic() - started
        validate_info(info)
        report["info"] = info
        report["startup_seconds"] = ready_seconds

        inside_raw, _ = run(["docker", "exec", container_id, *query], 15, 65536)
        report["container_gpu"] = parse_gpu_csv(inside_raw, runtime["required_compute_capability"], 0)
        if report["container_gpu"]["uuid"] != report["host_gpu_before"]["uuid"]:
            raise ContractError("container GPU UUID differs from the selected host GPU")
        report["router_runtime"] = read_router_runtime(container_id)

        tokens = docker_http_json(
            container_id, "POST", "/tokenize",
            {"inputs": probe["text"], "add_special_tokens": True}, runtime["short_request_timeout_seconds"], 1048576,
        )
        if not isinstance(tokens, list) or len(tokens) != 1 or not tokens[0]:
            raise ContractError("unexpected /tokenize result")
        token_ids = [token.get("id") for token in tokens[0]]
        if token_ids[-1] != probe["expected"]["eos_token_id"]:
            raise ContractError(f"tokenizer did not append EOS {probe['expected']['eos_token_id']}")
        report["tokenization"] = {"token_ids": token_ids, "count": len(token_ids), "last_token_id": token_ids[-1]}

        request_started = time.monotonic()
        embedding = docker_http_json(
            container_id, "POST", "/embed",
            {"inputs": [probe["text"]], **probe["request"]}, runtime["short_request_timeout_seconds"], 16 * 1024 * 1024,
        )
        monitor.check()
        report["embedding"] = validate_embedding(
            embedding, probe["expected"]["dimensions"], probe["expected"]["maximum_unit_norm_absolute_error"]
        )
        report["request_seconds"] = time.monotonic() - request_started
        report["embedding_sha256"] = hashlib.sha256(json.dumps(embedding, separators=(",", ":")).encode("utf-8")).hexdigest()
        stats_raw, _ = run(["docker", "stats", "--no-stream", "--format", "{{json .}}", container_id], 15, 1048576)
        report["container_stats"] = json.loads(stats_raw)
        if not monitor.stop():
            raise ContractError("VRAM monitor thread did not stop within its deadline")
        monitor.check()
        report["vram_samples"] = [report["host_gpu_before"], *monitor.samples]
        report["observed_peak_vram_mib"] = max(sample["memory_used_mib"] for sample in report["vram_samples"])
        allowed_vram = report["host_gpu_before"]["memory_total_mib"] - runtime["reserved_vram_mib"]
        if report["observed_peak_vram_mib"] > allowed_vram:
            raise ContractError(f"observed VRAM use left less than the {runtime['reserved_vram_mib']} MiB reserve")
        logs_raw, logs_err = run(["docker", "logs", container_id], 30, runtime["max_log_bytes"])
        logs = logs_raw + logs_err
        if "Starting FlashQwen2 model on Cuda" not in logs:
            raise ContractError("TEI logs do not prove FlashQwen2 CUDA dispatch")
        (result_root / "tei.log").write_bytes(logs.encode("utf-8"))
        report["log_sha256"] = hashlib.sha256(logs.encode("utf-8")).hexdigest()
        report["state"] = "startup_probe_passed"
    except BaseException as exc:
        first_error = exc
        report["state"] = "failed"
        report["error"] = {"type": type(exc).__name__, "message": str(exc)}
    finally:
        cleanup_started = time.monotonic()
        cleanup_deadline = cleanup_started + runtime["cleanup_timeout_seconds"]

        def cleanup_run(command: list[str], maximum_seconds: int, cap: int) -> tuple[str, str]:
            remaining = cleanup_deadline - time.monotonic()
            if remaining < 3.0:
                raise ContractError("cleanup deadline has no time for another Docker operation")
            return run(command, max(3, min(maximum_seconds, int(remaining))), cap)

        cleanup: dict[str, Any] = {"container_id": container_id, "logs": None, "stop": None, "remove": None, "absent": None}
        if monitor is not None:
            try:
                cleanup["vram_monitor_stopped"] = monitor.stop(timeout=min(10.0, max(0.0, cleanup_deadline - time.monotonic())))
                cleanup["vram_monitor_failure"] = monitor.failure
                cleanup["vram_monitor_trigger"] = monitor.cleanup_trigger
                report.setdefault("vram_samples", [report.get("host_gpu_before"), *monitor.samples])
            except BaseException as exc:
                cleanup["vram_monitor_stopped"] = False
                cleanup["vram_monitor_stop_error"] = f"{type(exc).__name__}: {exc}"
        if container_id is None:
            try:
                container_id, _recovered = recover_owned_container(contract, run_id, runner=cleanup_run)
                cleanup["container_id"] = container_id
                cleanup["recovery"] = "owned" if container_id is not None else "absent"
            except BaseException as exc:
                cleanup["recovery"] = f"failed: {type(exc).__name__}: {exc}"
        if container_id is not None:
            container_cleanup = cleanup_authenticated_container(
                container_id, run_id, result_root, runtime["max_log_bytes"], runner=cleanup_run
            )
            cleanup.update(container_cleanup)
        cleanup["seconds"] = time.monotonic() - cleanup_started
        cleanup["within_deadline"] = cleanup["seconds"] <= runtime["cleanup_timeout_seconds"]
        if cleanup["within_deadline"] is not True:
            cleanup["absent"] = False
        cleanup["failures"] = [
            key for key in ("vram_monitor_stop_error", "log_write", "absence_check") if key in cleanup
        ]
        cleanup["failures"].extend(
            key for key in ("recovery", "logs", "stop", "remove")
            if isinstance(cleanup.get(key), str) and cleanup[key].startswith("failed:")
        )
        if cleanup.get("vram_monitor_stopped") is False:
            cleanup["failures"].append("vram_monitor_stopped")
        report["cleanup"] = cleanup
        report["total_seconds"] = time.monotonic() - started
        if (cleanup["absent"] is not True and container_id is not None) or cleanup["failures"]:
            report["state"] = "cleanup_failed"
        try:
            atomic_write_json(result_root / "startup-report-v1.json", report, max_bytes=32 * 1024 * 1024)
        except BaseException as exc:
            print(f"startup report write failed after cleanup: {type(exc).__name__}: {exc}", file=sys.stderr)
            report["state"] = "report_write_failed"
    print(json.dumps({"state": report["state"], "report": str(result_root / "startup-report-v1.json")}, sort_keys=True))
    if first_error is not None:
        print(f"startup failed: {first_error}", file=sys.stderr)
    return 0 if report["state"] == "startup_probe_passed" and report["cleanup"]["absent"] is True else 1


if __name__ == "__main__":
    raise SystemExit(main())
