from __future__ import annotations

import argparse
import json
import os
import time
import uuid
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from gpu_contract import ContractError, atomic_write_json, load_contract, read_json, sha256_file, validate_locked_files
from prepare_image import run
from reference_cuda import validate_probes
from run_tei_startup import VramMonitor, parse_gpu_csv


IMAGE_ID = "sha256:24ece9d9b0ff4710e0c9b955e3c67c520b18b9504256043af1f7a324a8a51582"
REFERENCE_METHOD = "original-sdpa-math-cuda-f16-v3"
TASK_ID = "aa30a81c-2adf-4f54-b1eb-cfeaa39bac77"
TASK_LABEL = f"io.hmem.task={TASK_ID}"
TEI_TASK_LABEL = f"hmem.task={TASK_ID}"
TOTAL_SECONDS = 1200
CLEANUP_RESERVE_SECONDS = 120
OUTPUT_CAP_BYTES = 8 * 1024 * 1024
MEMORY_BYTES = 21_474_836_480
MEMORY_SWAP_BYTES = 25_769_803_776
CPUS = 6
PIDS = 256


def build_create_command(contract: dict[str, Any], probes: Path, output_root: Path, output_name: str, run_id: str) -> tuple[str, list[str]]:
    name = f"hmem-aa30-reference-{run_id[:12]}"
    command = [
        "docker", "create", "--name", name, "--label", TASK_LABEL, "--label", f"io.hmem.run={run_id}",
        "--gpus", "device=0", "--network", "none", "--memory", str(MEMORY_BYTES),
        "--memory-swap", str(MEMORY_SWAP_BYTES), "--cpus", str(CPUS), "--pids-limit", str(PIDS),
        "--read-only", "--tmpfs", "/tmp:rw,nosuid,nodev,size=2147483648", "--cap-drop", "ALL",
        "--security-opt", "no-new-privileges", "--log-driver", "local", "--log-opt", "max-size=8m",
        "--log-opt", "max-file=1", "--log-opt", "compress=false", "--env", "HF_HUB_OFFLINE=1",
        "--env", "TRANSFORMERS_OFFLINE=1", "--env", "HF_DATASETS_OFFLINE=1", "--env", "HF_HOME=/tmp/hf",
        "--env", "HF_MODULES_CACHE=/tmp/hf/modules", "--env", "TOKENIZERS_PARALLELISM=false",
        "--env", "NVIDIA_DRIVER_CAPABILITIES=compute,utility",
        "--mount", f"type=bind,src={contract['model']['source_root']},dst=/model,readonly",
        "--mount", f"type=bind,src={probes},dst=/inputs/probes.json,readonly",
        "--mount", f"type=bind,src={output_root},dst=/output", IMAGE_ID,
        "--source", "/model", "--probes", "/inputs/probes.json", "--output", f"/output/{output_name}",
    ]
    return name, command


def parse_container_id(value: str) -> str:
    candidate = value.strip()
    if len(candidate) != 64 or any(character not in "0123456789abcdef" for character in candidate):
        raise ContractError("Docker did not return one full lowercase container ID")
    return candidate


def _path(value: str) -> str:
    return os.path.normcase(os.path.normpath(value))


def make_deadline_runner(deadline: float, phase: str, *, clock=time.monotonic, runner=run):
    def bounded(command: list[str], maximum_seconds: float, cap: int) -> tuple[str, str]:
        remaining = deadline - clock()
        if remaining <= 0:
            raise ContractError(f"reference {phase} deadline exhausted before subprocess launch")
        return runner(command, min(float(maximum_seconds), remaining), cap)

    return bounded


def authenticate_ownership(value: Any, run_id: str, name: str) -> dict[str, Any]:
    if not isinstance(value, list) or len(value) != 1 or not isinstance(value[0], dict):
        raise ContractError("Docker inspect did not return exactly one container")
    item = value[0]
    if item.get("Name") != "/" + name or item.get("Config", {}).get("Image") != IMAGE_ID:
        raise ContractError("reference container name/image drift")
    labels = item.get("Config", {}).get("Labels") or {}
    if labels.get("io.hmem.task") != TASK_ID or labels.get("io.hmem.run") != run_id:
        raise ContractError("reference container ownership labels drift")
    return item


def authenticate_container(value: Any, contract: dict[str, Any], probes: Path, output_root: Path, run_id: str, name: str) -> dict[str, Any]:
    item = authenticate_ownership(value, run_id, name)
    host = item.get("HostConfig", {})
    expected = {
        "NetworkMode": "none", "Memory": MEMORY_BYTES, "MemorySwap": MEMORY_SWAP_BYTES,
        "NanoCpus": CPUS * 1_000_000_000, "PidsLimit": PIDS, "ReadonlyRootfs": True,
        "LogConfig": {"Type": "local", "Config": {"compress": "false", "max-file": "1", "max-size": "8m"}},
    }
    if any(host.get(key) != expected_value for key, expected_value in expected.items()):
        raise ContractError("reference container host contract drift")
    if host.get("CapDrop") != ["ALL"] or "no-new-privileges" not in (host.get("SecurityOpt") or []):
        raise ContractError("reference container security contract drift")
    requests = host.get("DeviceRequests") or []
    if len(requests) != 1 or requests[0].get("DeviceIDs") != ["0"] or ["gpu"] not in (requests[0].get("Capabilities") or []):
        raise ContractError("reference container GPU selection drift")
    mounts = {mount.get("Destination"): mount for mount in item.get("Mounts", [])}
    expected_mounts = {
        "/model": (_path(contract["model"]["source_root"]), False),
        "/inputs/probes.json": (_path(str(probes)), False),
        "/output": (_path(str(output_root)), True),
    }
    if set(mounts) != set(expected_mounts):
        raise ContractError("reference container mount set drift")
    for destination, (source, writable) in expected_mounts.items():
        if _path(mounts[destination].get("Source", "")) != source or mounts[destination].get("RW") is not writable:
            raise ContractError(f"reference container mount drift for {destination}")
    return item


def recover_owned_container(name: str, run_id: str, *, runner=run) -> str | None:
    recovered, _ = runner(
        ["docker", "ps", "-aq", "--no-trunc", "--filter", f"name=^/{name}$", "--filter", f"label={TASK_LABEL}", "--filter", f"label=io.hmem.run={run_id}"],
        15,
        65536,
    )
    if not recovered.strip():
        return None
    candidate = parse_container_id(recovered)
    inspected_raw, _ = runner(["docker", "inspect", candidate], 15, 1048576)
    authenticate_ownership(json.loads(inspected_raw), run_id, name)
    return candidate


def cleanup_failures(cleanup: dict[str, Any]) -> list[str]:
    failures = [
        key for key in ("recovery", "logs", "log_write", "authentication", "stop", "remove", "absence_check")
        if key in cleanup and (cleanup[key] is None or str(cleanup[key]).startswith("failed:"))
    ]
    if cleanup.get("absent") is not True:
        failures.append("absence")
    if cleanup.get("within_deadline") is False:
        failures.append("cleanup_deadline")
    if cleanup.get("vram_monitor_stopped") is False or cleanup.get("vram_monitor_failure"):
        failures.append("vram_monitor")
    return failures


def validate_reference_output(value: Any, probe_sha256: str, contract: dict[str, Any]) -> dict[str, Any]:
    from run_foundation_validation import reference_validation_summary, validate_reference_report

    validate_reference_report(value, probe_sha256, contract)
    return reference_validation_summary(value)


def main() -> int:
    parser = argparse.ArgumentParser(description="Run one bounded independent CUDA reference with authenticated cleanup.")
    parser.add_argument("--contract", type=Path, required=True)
    parser.add_argument("--probes", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--inspect-only", action="store_true", help="Create, authenticate, and remove the container without starting it.")
    args = parser.parse_args()
    started = time.monotonic()
    execution_deadline = started + TOTAL_SECONDS - CLEANUP_RESERVE_SECONDS
    cleanup_deadline = started + TOTAL_SECONDS
    execution_run = make_deadline_runner(execution_deadline, "execution")
    cleanup_run = make_deadline_runner(cleanup_deadline, "cleanup")
    contract = load_contract(args.contract.resolve())
    probes = args.probes.resolve()
    validate_probes(read_json(probes))
    output = args.output.resolve()
    expected_output_root = (Path(contract["artifact_root"]) / "reference-output").resolve()
    if output.parent != expected_output_root or output.exists():
        raise ContractError("reference output must be create-new in the declared reference-output directory")
    expected_output_root.mkdir(parents=True, exist_ok=True)
    validate_locked_files(Path(contract["model"]["source_root"]), contract["model"]["artifacts"], exact=True)
    run_id = uuid.uuid4().hex
    report_path = output.with_name(output.name + ".execution.json")
    if report_path.exists():
        raise ContractError("reference execution report must be create-new")
    report: dict[str, Any] = {
        "schema_version": 1, "state": "started", "task_id": TASK_ID, "run_id": run_id,
        "reference_method": REFERENCE_METHOD,
        "controller": {"pid": os.getpid(), "started_utc": datetime.now(timezone.utc).isoformat(), "monotonic_started": started},
        "mode": "inspect_only" if args.inspect_only else "numerical",
        "limits": {"total_seconds": TOTAL_SECONDS, "cleanup_reserve_seconds": CLEANUP_RESERVE_SECONDS, "output_cap_bytes": OUTPUT_CAP_BYTES,
                   "memory_bytes": MEMORY_BYTES, "memory_swap_bytes": MEMORY_SWAP_BYTES, "cpus": CPUS, "pids": PIDS, "reserved_vram_mib": 2048},
        "contract_sha256": sha256_file(args.contract.resolve()), "probe_sha256": sha256_file(probes), "image_id": IMAGE_ID,
    }
    print(json.dumps({"state": "launched", "run_id": run_id, "controller": report["controller"]}, sort_keys=True), flush=True)
    container_id: str | None = None
    name, command = build_create_command(contract, probes, expected_output_root, output.name, run_id)
    monitor: VramMonitor | None = None
    first_error: BaseException | None = None
    create_attempted = False
    query = ["nvidia-smi", "--query-gpu=index,uuid,name,driver_version,memory.total,memory.free,memory.used,compute_cap", "--format=csv,noheader,nounits"]
    try:
        for label in (TASK_LABEL, TEI_TASK_LABEL):
            existing, _ = execution_run(["docker", "ps", "-aq", "--no-trunc", "--filter", f"label={label}"], 15, 65536)
            if existing.strip():
                raise ContractError("another task-owned reference or TEI container exists")
        gpu_raw, _ = execution_run(query, 15, 65536)
        report["host_gpu_before"] = parse_gpu_csv(gpu_raw, "12.0", 18432)
        report["container_name"] = name
        report["create_argv"] = command
        create_attempted = True
        try:
            created, _ = execution_run(command, 60, 65536)
            candidate = parse_container_id(created)
        except BaseException:
            candidate = recover_owned_container(name, run_id, runner=execution_run)
            if candidate is None:
                raise
        inspected_raw, _ = execution_run(["docker", "inspect", candidate], 15, 1048576)
        inspected_value = json.loads(inspected_raw)
        authenticate_ownership(inspected_value, run_id, name)
        container_id = candidate
        authenticate_container(inspected_value, contract, probes, expected_output_root, run_id, name)
        report["container_id"] = container_id
        report["observed_mounts"] = [
            {key: mount.get(key) for key in ("Type", "Source", "Destination", "RW", "Propagation")}
            for mount in inspected_value[0].get("Mounts", [])
        ]
        if args.inspect_only:
            report["state"] = "inspect_passed"
        else:
            runtime = {"required_compute_capability": "12.0", "reserved_vram_mib": 2048}
            monitor = VramMonitor(query, runtime, container_id, max_samples=TOTAL_SECONDS + 1)
            monitor.start()
            if not monitor.wait_first_sample(10.0):
                raise ContractError("reference VRAM monitor did not produce an initial sample")
            monitor.check()
            remaining = execution_deadline - time.monotonic()
            if remaining < 3:
                raise ContractError("reference execution deadline exhausted before start")
            stdout, stderr = execution_run(["docker", "start", "--attach", container_id], remaining, OUTPUT_CAP_BYTES)
            report["attach_output"] = {"stdout": stdout, "stderr": stderr}
            monitor.check()
            if not output.is_file():
                raise ContractError("reference driver did not create its output")
            result = read_json(output)
            report["reference_validation"] = validate_reference_output(result, report["probe_sha256"], contract)
            report["reference_output"] = {"path": str(output), "bytes": output.stat().st_size, "sha256": sha256_file(output)}
            report["state"] = "reference_passed"
    except BaseException as exc:
        first_error = exc
        report["state"] = "failed"
        report["error"] = {"type": type(exc).__name__, "message": str(exc)}
    finally:
        cleanup_started = time.monotonic()
        cleanup: dict[str, Any] = {"container_id": container_id, "logs": None, "stop": None, "remove": None, "absent": None}
        if monitor is not None:
            cleanup["vram_monitor_stopped"] = monitor.stop(min(10.0, max(0.0, cleanup_deadline - time.monotonic())))
            cleanup["vram_monitor_failure"] = monitor.failure
            report["vram_samples"] = [report.get("host_gpu_before"), *monitor.samples]
        if container_id is None and create_attempted:
            try:
                container_id = recover_owned_container(name, run_id, runner=cleanup_run)
                cleanup["container_id"] = container_id
                cleanup["recovery"] = "owned" if container_id is not None else "absent"
                if container_id is None:
                    cleanup["absent"] = True
            except BaseException as exc:
                cleanup["recovery"] = f"failed: {type(exc).__name__}: {exc}"
        if container_id is not None:
            try:
                current_raw, _ = cleanup_run(["docker", "inspect", container_id], 10, 1048576)
                authenticate_ownership(json.loads(current_raw), run_id, name)
                cleanup["authentication"] = "ok"
            except BaseException as exc:
                cleanup["authentication"] = f"failed: {type(exc).__name__}: {exc}"
            if cleanup.get("authentication") == "ok":
                log_bytes: bytes | None = None
                try:
                    logs, log_err = cleanup_run(["docker", "logs", container_id], 10, OUTPUT_CAP_BYTES)
                    log_bytes = (logs + log_err).encode("utf-8")
                    cleanup["logs"] = "ok"
                except BaseException as exc:
                    cleanup["logs"] = f"failed: {type(exc).__name__}: {exc}"
                if log_bytes is not None:
                    try:
                        (expected_output_root / f"reference-{run_id}.log").write_bytes(log_bytes)
                        cleanup["log_write"] = "ok"
                    except BaseException as exc:
                        cleanup["log_write"] = f"failed: {type(exc).__name__}: {exc}"
                if args.inspect_only:
                    cleanup["stop"] = "not_started"
                else:
                    try:
                        cleanup_run(["docker", "stop", "--time", "10", container_id], 15, 65536)
                        cleanup["stop"] = "ok"
                    except BaseException as exc:
                        cleanup["stop"] = f"failed: {type(exc).__name__}: {exc}"
                try:
                    cleanup_run(["docker", "rm", "--force", container_id], 10, 65536)
                    cleanup["remove"] = "ok"
                except BaseException as exc:
                    cleanup["remove"] = f"failed: {type(exc).__name__}: {exc}"
            try:
                by_id, _ = cleanup_run(["docker", "ps", "-aq", "--no-trunc", "--filter", f"id={container_id}"], 5, 65536)
                by_labels, _ = cleanup_run(["docker", "ps", "-aq", "--no-trunc", "--filter", f"label={TASK_LABEL}", "--filter", f"label=io.hmem.run={run_id}"], 5, 65536)
                cleanup["absent"] = not by_id.strip() and not by_labels.strip()
            except BaseException as exc:
                cleanup["absence_check"] = f"failed: {type(exc).__name__}: {exc}"
        cleanup["seconds"] = time.monotonic() - cleanup_started
        cleanup["within_deadline"] = time.monotonic() <= cleanup_deadline
        cleanup["failures"] = cleanup_failures(cleanup)
        if cleanup.get("vram_monitor_stopped") is False:
            cleanup["failures"].append("vram_monitor_stopped")
        report["cleanup"] = cleanup
        report["total_seconds"] = time.monotonic() - started
        if cleanup["failures"]:
            report["state"] = "cleanup_failed"
        atomic_write_json(report_path, report, max_bytes=16 * 1024 * 1024)
    print(json.dumps({"state": report["state"], "report": str(report_path)}, sort_keys=True))
    expected_state = "inspect_passed" if args.inspect_only else "reference_passed"
    return 0 if first_error is None and report["state"] == expected_state else 1


if __name__ == "__main__":
    raise SystemExit(main())
