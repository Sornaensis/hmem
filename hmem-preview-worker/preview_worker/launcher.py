"""Trusted Docker lifecycle owner. Imports no image, PDF or HTML decoder."""
import argparse
from dataclasses import asdict
import io
import json
import os
import re
import signal
import subprocess
import sys
import threading
import time
import uuid
from .policy import Limits, PreviewError, canonical, digest, require
from .protocol import (MAX_STDERR, MAX_WIRE, VERSION, encode_header, read_header,
                       read_request, request, response_bytes, verify_response)

IMAGE = re.compile(r"(?:[a-zA-Z0-9./:_-]+@)?sha256:[0-9a-f]{64}\Z")
_ADMISSION = threading.BoundedSemaphore(2)
_FAILED_OWNERS = []


class LifecycleError(PreviewError):
    """Ownership is retained; callers must not report successful cleanup."""
    def __init__(self, owner_id, process=None):
        self.owner_id = owner_id
        self.process = process
        super().__init__("cleanup_unverified")


def _environment():
    # Host Docker context configuration is used only by the trusted CLI, never -e forwarded.
    allowed = {"PATH", "SYSTEMROOT", "WINDIR", "COMSPEC", "TEMP", "TMP", "HOME", "USERPROFILE",
               "DOCKER_HOST", "DOCKER_CONTEXT", "DOCKER_CONFIG", "PATHEXT"}
    return {key: value for key, value in os.environ.items() if key.upper() in allowed}


def _process(args, data, deadline, maximum=MAX_WIRE):
    require(time.monotonic() < deadline, "deadline_exceeded")
    try:
        child = subprocess.Popen(args, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                 env=_environment(), creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
    except OSError:
        raise PreviewError("confinement_unavailable") from None
    stdout, stderr, failed = bytearray(), bytearray(), threading.Event()

    def reader(pipe, destination, cap):
        try:
            while chunk := pipe.read(65536):
                space = cap - len(destination)
                destination.extend(chunk[:max(space, 0)])
                if len(chunk) > space:
                    failed.set()
                    break
        except (OSError, ValueError):
            failed.set()
        finally:
            pipe.close()

    def writer():
        try:
            if data:
                for offset in range(0, len(data), 65536):
                    child.stdin.write(data[offset:offset + 65536])
                child.stdin.flush()
        except (OSError, ValueError):
            pass
        finally:
            try: child.stdin.close()
            except OSError: pass

    threads = [threading.Thread(target=reader, args=(child.stdout, stdout, maximum), daemon=True),
               threading.Thread(target=reader, args=(child.stderr, stderr, MAX_STDERR), daemon=True),
               threading.Thread(target=writer, daemon=True)]
    for thread in threads: thread.start()
    failure = None
    try:
        while child.poll() is None:
            if failed.is_set():
                failure = "output_limit"
                break
            if time.monotonic() >= deadline:
                failure = "deadline_exceeded"
                break
            time.sleep(0.01)
    except BaseException:
        failure = "cancelled"
        raise
    finally:
        if child.poll() is None:
            child.kill()
        try:
            child.wait(timeout=3)
            for thread in threads: thread.join(timeout=3)
        except subprocess.TimeoutExpired:
            _FAILED_OWNERS.append(child)
            raise LifecycleError(None, child) from None
        if any(thread.is_alive() for thread in threads):
            _FAILED_OWNERS.append((child, threads))
            raise LifecycleError(None, child)
    if failure or failed.is_set():
        raise PreviewError(failure or "output_limit")
    return child.returncode, bytes(stdout), bytes(stderr)


def _docker(args, deadline=None, data=None, maximum=1024 * 1024):
    return _process(["docker", *args], data, deadline or time.monotonic() + 5, maximum)


def _inspect(reference, deadline=None):
    code, output, _ = _docker(["container", "inspect", reference], deadline)
    if code != 0:
        return None
    try:
        rows = json.loads(output)
        require(isinstance(rows, list) and len(rows) == 1, "container_identity_invalid")
        return rows[0]
    except (ValueError, KeyError, TypeError):
        raise PreviewError("container_identity_invalid") from None


def _owned(row, owner_id, image):
    return (row.get("Config", {}).get("Labels", {}).get("org.hmem.preview.owner") == owner_id
            and row.get("Config", {}).get("Image") == image)


def cleanup_owner(owner_id, image):
    """Recovery for a supervised launcher loss; never signal an unproven container."""
    require(str(uuid.UUID(owner_id)) == owner_id and IMAGE.fullmatch(image), "owner_invalid")
    deadline = time.monotonic() + 5
    for phase in ("inspect", "render"):
        name = "hmem-preview-" + owner_id + "-" + phase
        row = _inspect(name, deadline)
        if row is None:
            # Confirm absence rather than interpreting every inspect error as absence.
            code, raw, _ = _docker(["container", "ls", "-a", "--filter", "name=^/" + name + "$", "--format", "{{.ID}}"], deadline)
            if code != 0 or raw.strip():
                raise LifecycleError(owner_id)
            continue
        require(_owned(row, owner_id, image), "container_ownership_conflict")
        identifier = row["Id"]
        code, _, _ = _docker(["container", "rm", "-f", identifier], deadline)
        if code != 0 or _inspect(identifier, deadline) is not None:
            raise LifecycleError(owner_id)
        code, raw, _ = _docker(["container", "ls", "-a", "--no-trunc", "--filter", "id=" + identifier, "--format", "{{.ID}}"], deadline)
        if code != 0 or raw.strip():
            raise LifecycleError(owner_id)


class Generation:
    """One deadline includes metadata inspection, admission wait, render and launch cost."""
    def __init__(self, raw, media_type, image, limits=None, owner_id=None):
        require(not _FAILED_OWNERS, "cleanup_unverified")
        require(bool(IMAGE.fullmatch(image)), "image_must_be_immutable")
        require(type(raw) is bytes, "original_invalid")
        self.limits = limits or Limits()
        require(0 < len(raw) <= self.limits.original_bytes, "original_limit")
        self.owner_id = owner_id or str(uuid.uuid4())
        require(str(uuid.UUID(self.owner_id)) == self.owner_id, "owner_invalid")
        require(_ADMISSION.acquire(blocking=False), "launcher_busy")
        self.raw, self.media_type, self.image = raw, media_type, image
        self.started = time.monotonic()
        self.deadline = self.started + (120 if media_type == "application/pdf" else 30)
        self.plan = None
        self.closed = False
        self.creation_uncertain = False
        self.dirty = False

    def __enter__(self): return self

    def __exit__(self, *_): self.close()

    def close(self):
        if self.closed: return
        try:
            if self.dirty:
                cleanup_owner(self.owner_id, self.image)
                self.dirty = False
            if self.creation_uncertain:
                raise LifecycleError(self.owner_id)
        except BaseException:
            _FAILED_OWNERS.append(self)
            raise
        else:
            self.closed = True
            _ADMISSION.release()

    def _operation(self, operation):
        require(not self.closed and not _FAILED_OWNERS, "cleanup_unverified")
        require(time.monotonic() < self.deadline, "deadline_exceeded")
        wire = request(self.raw, self.media_type, operation, self.plan, self.limits)
        header, _, _ = read_request(io.BytesIO(wire))
        name = "hmem-preview-" + self.owner_id + "-" + operation
        args = ["container", "create", "-i", "--name", name, "--pull", "never", "--network", "none",
                "--read-only", "--user", "65534:65534", "--cap-drop", "ALL", "--security-opt", "no-new-privileges",
                "--memory", "536870912", "--memory-swap", "536870912", "--pids-limit", "64", "--cpus", "1",
                "--ulimit", "nofile=128:128", "--ipc", "none", "--log-driver", "none",
                "--tmpfs", "/tmp:rw,noexec,nosuid,nodev,size=67108864,mode=1777",
                "--label", "org.hmem.preview.owner=" + self.owner_id, self.image]
        try:
            self.dirty = True
            try:
                code, identifier, _ = _docker(args, min(self.deadline, time.monotonic() + 10))
            except BaseException:
                # An interrupted create may still commit at the daemon after the CLI exits.
                # Current absence cannot certify that asynchronous outcome.
                self.creation_uncertain = True
                _FAILED_OWNERS.append(self)
                raise LifecycleError(self.owner_id) from None
            require(code == 0 and re.fullmatch(rb"[0-9a-f]{64}\s*", identifier), "confinement_unavailable")
            identifier = identifier.decode("ascii").strip()
            row = _inspect(identifier)
            require(row is not None and _owned(row, self.owner_id, self.image), "container_identity_invalid")
            config = row["HostConfig"]
            require(config["NetworkMode"] == "none" and config["ReadonlyRootfs"]
                    and config["Memory"] == config["MemorySwap"] == 536870912
                    and config["PidsLimit"] == 64 and row["Config"]["User"] == "65534:65534"
                    and "ALL" in config["CapDrop"]
                    and any(x in {"no-new-privileges", "no-new-privileges=true"} for x in config["SecurityOpt"])
                    and not row["Mounts"] and config["LogConfig"]["Type"] == "none", "confinement_unavailable")
            code, output, _ = _docker(["container", "start", "-a", "-i", identifier], self.deadline, wire, MAX_WIRE)
            require(code == 0, "worker_crashed")
            result, outputs = verify_response(output, header, self.limits)
            runtime = result.get("runtime", {})
            require(runtime.get("uid") == 65534 and runtime.get("cap_eff") == runtime.get("cap_bnd") == 0
                    and runtime.get("no_new_privs") == 1 and runtime.get("seccomp") == 2
                    and runtime.get("root_read_only") is True and runtime.get("interfaces") == ["lo"]
                    and runtime.get("pdfium_flags") == [], "confinement_unavailable")
        finally:
            cleanup_owner(self.owner_id, self.image)
            if not self.creation_uncertain:
                self.dirty = False
        # Success is published only after native exit, container removal and
        # confirmed absence, all within this generation's original deadline.
        require(time.monotonic() < self.deadline, "deadline_exceeded")
        result["launcher"] = {"owner_id": self.owner_id,
                              "elapsed_ms": int((time.monotonic() - self.started) * 1000),
                              "remaining_ms": max(0, int((self.deadline - time.monotonic()) * 1000))}
        return result, outputs

    def inspect(self):
        require(self.plan is None, "operation_invalid")
        result, outputs = self._operation("inspect")
        self.plan = result["plan"]
        return result

    def render(self, plan=None):
        require(self.plan is not None and (plan is None or plan == self.plan), "plan_mismatch")
        return self._operation("render")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--image", required=True)
    parser.add_argument("--owner-id", required=True)
    parser.add_argument("--cleanup", action="store_true")
    args = parser.parse_args()
    from .transport import DeadlineReader, write
    def cancel(*_): raise PreviewError("cancelled")
    for sig in (signal.SIGINT, signal.SIGTERM): signal.signal(sig, cancel)
    try:
        if args.cleanup:
            cleanup_owner(args.owner_id, args.image)
            return
        # Source transport is bounded before any container exists. Session deadline begins here.
        started = time.monotonic()
        source = DeadlineReader(sys.stdin.buffer, started + 30)
        header, raw, limits = read_request(source, eof=False)
        require(header["operation"] == "inspect" and "plan" not in header, "operation_invalid")
        with Generation(raw, header["media_type"], args.image, limits, args.owner_id) as generation:
            generation.started = started
            generation.deadline = started + (120 if header["media_type"] == "application/pdf" else 30)
            inspected = generation.inspect()
            write(sys.stdout.buffer, response_bytes(inspected, []), generation.deadline)
            source.deadline = generation.deadline
            ack = read_header(source, 1024)
            require(ack == {"render_plan_sha256": digest(canonical(generation.plan))}, "admission_mismatch")
            require(not source.read(1), "trailing_input")
            result, outputs = generation.render()
            write(sys.stdout.buffer, response_bytes(result, [data for _, data in outputs]), generation.deadline)
    except PreviewError as error:
        try:
            write(sys.stdout.buffer, encode_header({"version": VERSION, "status": "error", "code": error.code}), time.monotonic() + 0.25)
        except (PreviewError, OSError):
            pass
        raise SystemExit(1)


if __name__ == "__main__": main()
