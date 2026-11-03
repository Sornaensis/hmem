"""Actual immutable-image sandbox tests; run separately from native policy units."""
import argparse
import io
from pathlib import Path
import os
import shutil
import socket
import subprocess
import sys
import time
import unittest
import uuid
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from preview_worker import launcher
from preview_worker.launcher import Generation, cleanup_owner
from preview_worker.policy import PreviewError, Limits
from preview_worker.policy import canonical, digest
from preview_worker.protocol import request, encode_header, read_header
from preview_worker.render import inspect
from test_worker import raster, pdf, active_pdf

IMAGE = None
SCRATCH = Path(__file__).resolve().parents[2] / ".tmp" / "task-preview-worker-8559047b"


class ContainerAcceptance(unittest.TestCase):
    def generation(self, raw, media):
        owner = str(uuid.uuid4())
        with Generation(raw, media, IMAGE, owner_id=owner) as generation:
            before = generation.inspect()
            self.assertEqual(before["work"]["decoded_pixels"], 0)
            self.assertEqual(before["work"]["pdf_decoded_bytes"], 0)
            result, outputs = generation.render()
            self.assertLess(result["launcher"]["remaining_ms"], before["launcher"]["remaining_ms"])
            self.assertEqual(result["runtime"]["pdfium_flags"], [])
            self.assertEqual(result["runtime"]["interfaces"], ["lo"])
            self.assertTrue(result["runtime"]["root_read_only"])
        cleanup_owner(owner, IMAGE)
        return result, outputs

    def test_actual_png_jpeg_webp_static(self):
        for format, media in (("PNG", "image/png"), ("JPEG", "image/jpeg"), ("WEBP", "image/webp")):
            result, outputs = self.generation(raster(format), media)
            self.assertEqual(len(outputs), 2)
            self.assertEqual(result["work"]["decoded_pixels"], 2048)

    def test_actual_pdf_all_pages_and_cover(self):
        result, outputs = self.generation(pdf(3), "application/pdf")
        self.assertEqual(len(outputs), 7)
        self.assertEqual({record["page"] for record, _ in outputs if record["variant"] == "pdf_page"}, {1, 2, 3})

    def test_actual_pdf_actions_and_image_polyglot_are_inert_rasters(self):
        for raw, media in ((active_pdf(), "application/pdf"),
                           (raster() + b"<script>EVIL</script>", "image/png")):
            _, outputs = self.generation(raw, media)
            for record, data in outputs:
                self.assertEqual(record["media_type"], "image/png")
                self.assertTrue(data.startswith(b"\x89PNG\r\n\x1a\n"))
                self.assertNotIn(b"EVIL", data)

    def test_actual_malicious_styled_html_and_host_credentials_absent(self):
        raw = b'''<style>div{display:flex;gap:10px;color:red;background:url(https://evil)}@import "https://evil"</style><div><script>EVIL()</script><img src="https://evil"><a href="https://evil">Text</a></div>'''
        with patch.dict(os.environ, {"HMEM_APP_SECRET": "never-forward-this", "AUTHORIZATION": "private-value"}):
            result, outputs = self.generation(raw, "text/html")
        text = outputs[0][1].decode()
        self.assertIn("display:flex", text)
        for forbidden in ("EVIL", "https://evil", "url(", "<script", "href=", "never-forward-this", "private-value"):
            self.assertNotIn(forbidden, text)

    def test_cli_successful_two_stage_exchange(self):
        from preview_worker.protocol import read_request, verify_response
        raw = raster()
        plan = inspect(raw, "image/png", Limits())
        owner = str(uuid.uuid4())
        command = [sys.executable, str(Path(__file__).resolve().parents[1] / "launch.py"),
                   "--image", IMAGE, "--owner-id", owner]
        data = request(raw, "image/png") + encode_header({"render_plan_sha256": digest(canonical(plan))})
        try:
            code, output, errors = launcher._process(command, data, time.monotonic() + 40)
            self.assertEqual(code, 0, errors.decode(errors="replace"))
            stream = io.BytesIO(output)
            first = read_header(stream, 256 * 1024)
            self.assertEqual(first["operation"], "inspect")
            self.assertEqual(first["plan_sha256"], digest(canonical(plan)))
            header, _, _ = read_request(io.BytesIO(request(raw, "image/png", "render", plan)))
            self.assertEqual(len(verify_response(output[stream.tell():], header)[1]), 2)
        finally:
            cleanup_owner(owner, IMAGE)

    def test_malformed_and_type_spoofing_fail(self):
        for raw, media in ((b"bad PNG", "image/png"), (b"%PDF-1.7\nbad", "application/pdf"),
                           (b"<svg onload='EVIL()'></svg>", "image/png")):
            with self.assertRaises(PreviewError): self.generation(raw, media)

    def test_resource_bounds_execute_inside_image(self):
        from dataclasses import replace
        for raw, media, limits in ((raster(), "image/png", replace(Limits(), image_pixels=10)),
                                   (pdf(2), "application/pdf", replace(Limits(), pdf_pages=1)),
                                   (b"<p>x</p>" * 20, "text/html", replace(Limits(), dom_nodes=5))):
            with Generation(raw, media, IMAGE, limits=limits) as generation:
                with self.assertRaises(PreviewError):
                    generation.inspect(); generation.render()

    def test_real_worker_crash_fails_and_joins(self):
        fixture_owner, image = str(uuid.uuid4()), None
        name = "hmem-preview-" + fixture_owner + "-inspect"
        try:
            code, identifier, _ = launcher._docker(["container", "create", "--name", name,
                "--label", "org.hmem.preview.owner=" + fixture_owner, IMAGE])
            self.assertEqual(code, 0)
            identifier = identifier.decode().strip()
            code, output, _ = launcher._docker(["container", "commit", "--change",
                'ENTRYPOINT ["python","-c","import os;os._exit(7)"]', "--change",
                "LABEL org.hmem.preview.test=" + fixture_owner, identifier], time.monotonic() + 30)
            self.assertEqual(code, 0)
            image = output.decode().strip()
            cleanup_owner(fixture_owner, IMAGE)
            with Generation(raster(), "image/png", image) as generation:
                with self.assertRaisesRegex(PreviewError, "worker_crashed"): generation.inspect()
        finally:
            cleanup_owner(fixture_owner, IMAGE)
            if image:
                code, _, _ = launcher._docker(["image", "rm", image], time.monotonic() + 10)
                self.assertEqual(code, 0)

    def test_one_generation_deadline_includes_admission_delay(self):
        owner = str(uuid.uuid4())
        with Generation(raster(), "image/png", IMAGE, owner_id=owner) as generation:
            generation.inspect()
            generation.deadline = time.monotonic() - 1
            with self.assertRaisesRegex(PreviewError, "deadline_exceeded"): generation.render()
        cleanup_owner(owner, IMAGE)

    def test_real_worker_start_timeout_removes_exact_container(self):
        original = launcher._docker
        owner = str(uuid.uuid4())
        def timed(args, deadline=None, data=None, maximum=1024 * 1024):
            if args[:2] == ["container", "start"]:
                deadline = min(deadline, time.monotonic() + 0.05)
            return original(args, deadline, data, maximum)
        with patch.object(launcher, "_docker", timed):
            with Generation(raster(), "image/png", IMAGE, owner_id=owner) as generation:
                with self.assertRaisesRegex(PreviewError, "deadline_exceeded"): generation.inspect()
        cleanup_owner(owner, IMAGE)

    def test_immutable_image_required(self):
        with self.assertRaisesRegex(PreviewError, "image_must_be_immutable"):
            Generation(raster(), "image/png", "hmem-preview-worker:latest")

    def test_cleanup_expiry_never_reports_success(self):
        owner = str(uuid.uuid4())
        original = launcher.cleanup_owner
        with Generation(raster(), "image/png", IMAGE, owner_id=owner) as generation:
            def delayed(*args):
                original(*args)
                generation.deadline = time.monotonic() + 0.02
                time.sleep(0.04)
            with patch.object(launcher, "cleanup_owner", delayed):
                with self.assertRaisesRegex(PreviewError, "deadline_exceeded"): generation.inspect()
        original(owner, IMAGE)

    def _blocked_cli(self, hold_output):
        from PIL import Image
        import io
        raw = raster()
        if hold_output:
            # Synthetic incompressible raster forces a response larger than the pipe.
            buffer = io.BytesIO()
            Image.frombytes("RGB", (512, 512), os.urandom(512 * 512 * 3)).save(buffer, format="PNG")
            raw = buffer.getvalue()
        owner = str(uuid.uuid4())
        env = dict(os.environ, PYTHONPATH=str(Path(__file__).resolve().parents[1]))
        child = subprocess.Popen([sys.executable, "-m", "preview_worker.launcher", "--image", IMAGE, "--owner-id", owner],
                                 stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=env,
                                 creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
        started = time.monotonic()
        try:
            child.stdin.write(request(raw, "image/png")); child.stdin.flush()
            if hold_output:
                plan = inspect(raw, "image/png", Limits())
                child.stdin.write(encode_header({"render_plan_sha256": digest(canonical(plan))})); child.stdin.close()
            # Deliberately keep stdout unread; no-ack also holds stdin open.
            child.wait(timeout=50)
            self.assertNotEqual(child.returncode, 0)
            self.assertGreaterEqual(time.monotonic() - started, 29)
            self.assertLess(time.monotonic() - started, 50)
            first = read_header(io.BytesIO(child.stdout.read(16384)), 256 * 1024)
            self.assertEqual((first["status"], first["operation"]), ("ok", "inspect"))
        finally:
            if child.poll() is None:
                child.kill(); child.wait(timeout=3)
            for pipe in (child.stdin, child.stdout, child.stderr):
                if pipe and not pipe.closed: pipe.close()
            cleanup_owner(owner, IMAGE)

    def test_cli_unread_render_output_exits_and_joins(self): self._blocked_cli(True)

    def test_cli_open_stdin_without_ack_exits_and_joins(self): self._blocked_cli(False)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--image-file", required=True)
    args, remaining = parser.parse_known_args()
    IMAGE = Path(args.image_file).read_text().strip()
    unittest.main(argv=[sys.argv[0], *remaining])
