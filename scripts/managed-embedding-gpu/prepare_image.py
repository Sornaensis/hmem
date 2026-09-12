from __future__ import annotations

import argparse
import ctypes
import json
import shutil
import subprocess
import sys
import threading
import time
from pathlib import Path

from gpu_contract import (
    CONFIG_DIGEST,
    PLATFORM_DIGEST,
    ContractError,
    atomic_write_json,
    load_contract,
    read_json,
    sha256_file,
    validate_index_manifest,
    validate_locked_files,
    validate_platform_manifest,
    validate_source_facts,
)


class CommandError(ContractError):
    def __init__(self, message: str, *, reason: str, elapsed: float, stdout: bytes, stderr: bytes):
        super().__init__(message)
        self.reason = reason
        self.elapsed = elapsed
        self.stdout = stdout
        self.stderr = stderr


class _WindowsJob:
    """Own one child tree; closing the job terminates descendants that retain pipes."""

    class _BasicLimit(ctypes.Structure):
        _fields_ = [
            ("PerProcessUserTimeLimit", ctypes.c_int64), ("PerJobUserTimeLimit", ctypes.c_int64),
            ("LimitFlags", ctypes.c_uint32), ("MinimumWorkingSetSize", ctypes.c_size_t),
            ("MaximumWorkingSetSize", ctypes.c_size_t), ("ActiveProcessLimit", ctypes.c_uint32),
            ("Affinity", ctypes.c_size_t), ("PriorityClass", ctypes.c_uint32),
            ("SchedulingClass", ctypes.c_uint32),
        ]

    class _IoCounters(ctypes.Structure):
        _fields_ = [(name, ctypes.c_uint64) for name in (
            "ReadOperationCount", "WriteOperationCount", "OtherOperationCount",
            "ReadTransferCount", "WriteTransferCount", "OtherTransferCount",
        )]

    class _ExtendedLimit(ctypes.Structure):
        pass

    class _ThreadEntry(ctypes.Structure):
        _fields_ = [
            ("dwSize", ctypes.c_uint32), ("cntUsage", ctypes.c_uint32), ("th32ThreadID", ctypes.c_uint32),
            ("th32OwnerProcessID", ctypes.c_uint32), ("tpBasePri", ctypes.c_long), ("tpDeltaPri", ctypes.c_long),
            ("dwFlags", ctypes.c_uint32),
        ]

    _ExtendedLimit._fields_ = [
        ("BasicLimitInformation", _BasicLimit), ("IoInfo", _IoCounters),
        ("ProcessMemoryLimit", ctypes.c_size_t), ("JobMemoryLimit", ctypes.c_size_t),
        ("PeakProcessMemoryUsed", ctypes.c_size_t), ("PeakJobMemoryUsed", ctypes.c_size_t),
    ]

    def __init__(self, process: subprocess.Popen):
        kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
        kernel32.CreateJobObjectW.restype = ctypes.c_void_p
        kernel32.SetInformationJobObject.argtypes = [ctypes.c_void_p, ctypes.c_int, ctypes.c_void_p, ctypes.c_uint32]
        kernel32.AssignProcessToJobObject.argtypes = [ctypes.c_void_p, ctypes.c_void_p]
        kernel32.TerminateJobObject.argtypes = [ctypes.c_void_p, ctypes.c_uint32]
        kernel32.CloseHandle.argtypes = [ctypes.c_void_p]
        kernel32.CreateToolhelp32Snapshot.argtypes = [ctypes.c_uint32, ctypes.c_uint32]
        kernel32.CreateToolhelp32Snapshot.restype = ctypes.c_void_p
        kernel32.Thread32First.argtypes = [ctypes.c_void_p, ctypes.c_void_p]
        kernel32.Thread32Next.argtypes = [ctypes.c_void_p, ctypes.c_void_p]
        kernel32.OpenThread.argtypes = [ctypes.c_uint32, ctypes.c_int, ctypes.c_uint32]
        kernel32.OpenThread.restype = ctypes.c_void_p
        kernel32.ResumeThread.argtypes = [ctypes.c_void_p]
        kernel32.ResumeThread.restype = ctypes.c_uint32
        self._kernel32 = kernel32
        self._handle = kernel32.CreateJobObjectW(None, None)
        if not self._handle:
            raise OSError(ctypes.get_last_error(), "CreateJobObjectW failed")
        limits = self._ExtendedLimit()
        limits.BasicLimitInformation.LimitFlags = 0x00002000  # JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE
        if not kernel32.SetInformationJobObject(self._handle, 9, ctypes.byref(limits), ctypes.sizeof(limits)):
            self.close()
            raise OSError(ctypes.get_last_error(), "SetInformationJobObject failed")
        if not kernel32.AssignProcessToJobObject(self._handle, ctypes.c_void_p(int(process._handle))):
            self.close()
            raise OSError(ctypes.get_last_error(), "AssignProcessToJobObject failed")

    def resume(self, process_id: int) -> None:
        snapshot = self._kernel32.CreateToolhelp32Snapshot(0x00000004, 0)
        invalid = ctypes.c_void_p(-1).value
        if snapshot == invalid:
            raise OSError(ctypes.get_last_error(), "CreateToolhelp32Snapshot failed")
        resumed = 0
        try:
            entry = self._ThreadEntry()
            entry.dwSize = ctypes.sizeof(entry)
            present = self._kernel32.Thread32First(snapshot, ctypes.byref(entry))
            while present:
                if entry.th32OwnerProcessID == process_id:
                    thread = self._kernel32.OpenThread(0x0002, False, entry.th32ThreadID)
                    if not thread:
                        raise OSError(ctypes.get_last_error(), "OpenThread failed")
                    try:
                        if self._kernel32.ResumeThread(thread) == 0xFFFFFFFF:
                            raise OSError(ctypes.get_last_error(), "ResumeThread failed")
                        resumed += 1
                    finally:
                        self._kernel32.CloseHandle(thread)
                present = self._kernel32.Thread32Next(snapshot, ctypes.byref(entry))
        finally:
            self._kernel32.CloseHandle(snapshot)
        if resumed != 1:
            raise OSError(f"expected one suspended primary thread, observed {resumed}")

    def terminate(self) -> None:
        if self._handle and not self._kernel32.TerminateJobObject(self._handle, 124):
            raise OSError(ctypes.get_last_error(), "TerminateJobObject failed")

    def close(self) -> None:
        if self._handle:
            self._kernel32.CloseHandle(self._handle)
            self._handle = None


def run(command: list[str], timeout_seconds: int, output_cap: int) -> tuple[str, str]:
    started = time.monotonic()
    deadline = started + timeout_seconds
    cleanup_reserve = min(10.0, max(2.0, timeout_seconds * 0.1))
    execution_deadline = deadline - cleanup_reserve
    creationflags = (subprocess.CREATE_NEW_PROCESS_GROUP | 0x00000004) if sys.platform == "win32" else 0
    process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, creationflags=creationflags)
    job = None
    if sys.platform == "win32":
        try:
            job = _WindowsJob(process)
            job.resume(process.pid)
        except BaseException:
            try:
                if job is not None:
                    job.terminate()
                else:
                    process.kill()
                process.wait(timeout=max(0.1, deadline - time.monotonic()))
            finally:
                if job is not None:
                    job.close()
                process.stdout.close()
                process.stderr.close()
            raise
    chunks = {"stdout": bytearray(), "stderr": bytearray()}
    lock = threading.Lock()
    overflow = threading.Event()

    def drain(name: str, stream) -> None:
        while True:
            chunk = stream.read(65536)
            if not chunk:
                return
            with lock:
                remaining = output_cap - len(chunks["stdout"]) - len(chunks["stderr"])
                if remaining > 0:
                    chunks[name].extend(chunk[:remaining])
                if len(chunk) > remaining:
                    overflow.set()

    threads = [
        threading.Thread(target=drain, args=("stdout", process.stdout), daemon=True),
        threading.Thread(target=drain, args=("stderr", process.stderr), daemon=True),
    ]
    for thread in threads:
        thread.start()
    reason = None
    while process.poll() is None:
        if overflow.is_set():
            reason = "output_cap"
            break
        if time.monotonic() >= execution_deadline:
            reason = "timeout"
            break
        time.sleep(0.05)
    if reason is not None and process.poll() is None:
        if job is not None:
            try:
                job.terminate()
            except OSError:
                pass
        if process.poll() is None:
            process.kill()
    remaining = max(0.0, deadline - time.monotonic())
    try:
        process.wait(timeout=remaining)
    except subprocess.TimeoutExpired:
        if job is not None:
            try:
                job.terminate()
            except OSError:
                process.kill()
        else:
            process.kill()
    if job is not None:
        job.close()
    for thread in threads:
        thread.join(timeout=max(0.0, deadline - time.monotonic()))
    elapsed = time.monotonic() - started
    stdout = bytes(chunks["stdout"])
    stderr = bytes(chunks["stderr"])
    unresolved_streams = any(thread.is_alive() for thread in threads)
    if unresolved_streams:
        raise CommandError(
            f"command streams did not resolve after {elapsed:.3f}s: {' '.join(command)}",
            reason="unresolved_streams",
            elapsed=elapsed,
            stdout=stdout,
            stderr=stderr,
        )
    process.stdout.close()
    process.stderr.close()
    if overflow.is_set() and reason is None:
        reason = "output_cap"
    if reason is not None:
        raise CommandError(
            f"command {reason} after {elapsed:.3f}s: {' '.join(command)}",
            reason=reason,
            elapsed=elapsed,
            stdout=stdout,
            stderr=stderr,
        )
    if process.returncode != 0:
        decoded = stderr.decode("utf-8", errors="replace").strip()
        raise CommandError(
            f"command failed ({process.returncode}, {elapsed:.3f}s): {' '.join(command)}\n{decoded}",
            reason="exit_code",
            elapsed=elapsed,
            stdout=stdout,
            stderr=stderr,
        )
    return stdout.decode("utf-8", errors="strict"), stderr.decode("utf-8", errors="strict")


def docker_json(command: list[str], timeout_seconds: int, output_cap: int):
    stdout, _stderr = run(command, timeout_seconds, output_cap)
    try:
        value = json.loads(stdout)
    except json.JSONDecodeError as exc:
        raise ContractError(f"Docker returned invalid JSON for {' '.join(command)}: {exc}") from exc
    return value


def validate_model_and_source(contract: dict, artifact_root: Path) -> dict:
    source_model = Path(contract["model"]["source_root"])
    model_inventory = validate_locked_files(source_model, contract["model"]["artifacts"], exact=True)
    archive = Path(contract["tei"]["source_archive"]["path"])
    if not archive.is_file() or archive.stat().st_size != contract["tei"]["source_archive"]["bytes"]:
        raise ContractError(f"TEI source archive identity drift: {archive}")
    if sha256_file(archive) != contract["tei"]["source_archive"]["sha256"]:
        raise ContractError(f"TEI source archive checksum drift: {archive}")
    source_root = archive.parent.parent / "tei-source"
    source_inventory = validate_locked_files(source_root, contract["tei"]["source_files"], exact=False)
    validate_source_facts(source_root)

    prepared_model = artifact_root / "model"
    prepared_model.mkdir(parents=True, exist_ok=True)
    for entry in contract["model"]["artifacts"]:
        source = source_model / Path(*entry["path"].split("/"))
        destination = prepared_model / Path(*entry["path"].split("/"))
        destination.parent.mkdir(parents=True, exist_ok=True)
        if destination.exists():
            if not destination.is_file() or sha256_file(destination) != entry["sha256"]:
                raise ContractError(f"prepared model path has unexpected content: {destination}")
            continue
        try:
            destination.hardlink_to(source)
        except OSError:
            shutil.copyfile(source, destination)
        if sha256_file(destination) != entry["sha256"]:
            raise ContractError(f"prepared model copy checksum drift: {destination}")
    prepared_inventory = validate_locked_files(prepared_model, contract["model"]["artifacts"], exact=True)
    return {
        "source_archive": {"path": str(archive), "bytes": archive.stat().st_size, "sha256": sha256_file(archive)},
        "source_files": source_inventory,
        "model_source": model_inventory,
        "model_prepared": prepared_inventory,
    }


def main() -> int:
    parser = argparse.ArgumentParser(description="Prepare and authenticate the pinned native TEI GPU image and model snapshot.")
    parser.add_argument("--contract", type=Path, required=True)
    parser.add_argument("--skip-pull", action="store_true", help="Validate registry metadata and local inputs without downloading the image.")
    args = parser.parse_args()
    started = time.monotonic()
    try:
        contract = load_contract(args.contract.resolve())
        limits = contract["preparation_limits"]
        artifact_root = Path(contract["artifact_root"])
        artifact_root.mkdir(parents=True, exist_ok=True)
        usage = shutil.disk_usage(artifact_root)
        if usage.free < limits["minimum_free_disk_bytes"]:
            raise ContractError(f"insufficient free disk: {usage.free} < {limits['minimum_free_disk_bytes']}")

        image = contract["image"]
        index = docker_json(
            ["docker", "manifest", "inspect", image["reference"]],
            limits["manifest_timeout_seconds"],
            limits["command_output_bytes"],
        )
        validate_index_manifest(contract, index)
        platform_reference = "ghcr.io/huggingface/text-embeddings-inference@" + PLATFORM_DIGEST
        platform = docker_json(
            ["docker", "manifest", "inspect", platform_reference],
            limits["manifest_timeout_seconds"],
            limits["command_output_bytes"],
        )
        validate_platform_manifest(contract, platform)
        local_inputs = validate_model_and_source(contract, artifact_root)

        pull_seconds = None
        if not args.skip_pull:
            pull_started = time.monotonic()
            run(
                ["docker", "pull", "--platform", "linux/amd64", image["reference"]],
                limits["pull_timeout_seconds"],
                limits["command_output_bytes"],
            )
            pull_seconds = time.monotonic() - pull_started

        inspect_values = docker_json(
            ["docker", "image", "inspect", image["reference"]],
            limits["manifest_timeout_seconds"],
            limits["command_output_bytes"],
        )
        if not isinstance(inspect_values, list) or len(inspect_values) != 1 or not isinstance(inspect_values[0], dict):
            raise ContractError("docker image inspect must return exactly one image")
        inspect = inspect_values[0]
        repo_digests = inspect.get("RepoDigests")
        if not isinstance(repo_digests, list) or not any(value.endswith("@" + image["index_digest"]) for value in repo_digests):
            raise ContractError("local image does not retain the pinned index digest")

        elapsed = time.monotonic() - started
        if elapsed > limits["total_timeout_seconds"]:
            raise ContractError(f"preparation exceeded total deadline: {elapsed:.3f}s")
        report = {
            "schema_version": 1,
            "state": "prepared",
            "task_id": contract["task_id"],
            "contract": {"path": str(args.contract.resolve()), "sha256": sha256_file(args.contract.resolve())},
            "image": {
                "reference": image["reference"],
                "index_digest": image["index_digest"],
                "platform_manifest_digest": image["platform_manifest_digest"],
                "expected_config_digest": CONFIG_DIGEST,
                "docker_image_id": inspect.get("Id"),
                "repo_digests": repo_digests,
                "architecture": inspect.get("Architecture"),
                "os": inspect.get("Os"),
                "size": inspect.get("Size"),
                "labels": inspect.get("Config", {}).get("Labels"),
                "entrypoint": inspect.get("Config", {}).get("Entrypoint"),
                "cmd": inspect.get("Config", {}).get("Cmd"),
                "environment": inspect.get("Config", {}).get("Env"),
                "rootfs_diff_ids": inspect.get("RootFS", {}).get("Layers"),
                "compressed_layer_bytes": sum(entry["bytes"] for entry in image["layers"]),
            },
            "local_inputs": local_inputs,
            "limits": limits,
            "timing": {"total_seconds": elapsed, "pull_seconds": pull_seconds},
        }
        output = artifact_root / "preparation-report-v1.json"
        atomic_write_json(output, report, max_bytes=limits["command_output_bytes"])
        print(json.dumps({"state": "prepared", "report": str(output), "seconds": elapsed}, sort_keys=True))
        return 0
    except (ContractError, OSError, subprocess.TimeoutExpired) as exc:
        print(f"preparation failed: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
