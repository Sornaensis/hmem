import base64
import copy
from dataclasses import replace
import io
import os
from pathlib import Path
import struct
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import av
from PIL import Image
import pypdfium2 as pdfium
from preview_worker import POLICY_VERSION
from preview_worker.policy import Limits, PreviewError, canonical, digest
from preview_worker.protocol import request, read_request, verify_response, response_bytes, encode_header
from preview_worker.render import inspect, render
from preview_worker.sanitize import sanitize


def raster(format="PNG", size=(64, 32)):
    buffer = io.BytesIO()
    Image.new("RGB", size, "#336699").save(buffer, format=format)
    return buffer.getvalue()


def mp4(codec="libx264", size=(640, 320), with_audio=False):
    buffer = io.BytesIO()
    with av.open(buffer, "w", format="mp4") as container:
        stream = container.add_stream(codec, rate=10)
        stream.width, stream.height = size
        stream.pix_fmt = "yuv420p"
        if with_audio:
            audio = add_silent_aac(container)
        for color in ("#dd2200", "#0033dd"):
            frame = av.VideoFrame.from_image(Image.new("RGB", size, color))
            for packet in stream.encode(frame):
                container.mux(packet)
        for packet in stream.encode():
            container.mux(packet)
        if with_audio:
            mux_silent_aac(container, audio)
    return buffer.getvalue()


def audio_only_mp4():
    buffer = io.BytesIO()
    with av.open(buffer, "w", format="mp4", options={"brand": "isom"}) as container:
        mux_silent_aac(container, add_silent_aac(container))
    return buffer.getvalue()


def add_silent_aac(container):
    stream = container.add_stream("aac", rate=48000)
    stream.layout = "mono"
    return stream


def mux_silent_aac(container, stream):
    frame = av.AudioFrame(format="fltp", layout="mono", samples=1024)
    frame.sample_rate = 48000
    frame.planes[0].update(bytes(frame.planes[0].buffer_size))
    for packet in stream.encode(frame) + stream.encode():
        container.mux(packet)


def external_mp4(raw, uri):
    # The fixture muxes mdat before moov. Widen the data reference and its
    # containing boxes without changing any original sample offsets.
    offset = raw.index(b"moov") - 4
    ancestors = [offset]
    for kind in (b"trak", b"mdia", b"minf", b"dinf", b"dref", b"url "):
        offset = raw.index(kind, offset + 8) - 4
        ancestors.append(offset)
    entry_end = offset + 12
    assert raw[offset:entry_end] == b"\0\0\0\x0curl \0\0\0\1"
    path = uri + b"\0"
    result = bytearray(raw[:entry_end] + path + raw[entry_end:])
    result[offset + 11] = 0
    for box in ancestors:
        struct.pack_into(">I", result, box, struct.unpack_from(">I", raw, box)[0] + len(path))
    return bytes(result)


def pdf(pages=2):
    document = pdfium.PdfDocument.new()
    for _ in range(pages): document.new_page(100, 140)
    buffer = io.BytesIO()
    document.save(buffer)
    document.close()
    return buffer.getvalue()


def active_pdf():
    objects = [b"<< /Type /Catalog /Pages 2 0 R /OpenAction 5 0 R >>",
               b"<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
               b"<< /Type /Page /Parent 2 0 R /MediaBox [0 0 100 140] /Contents 4 0 R /Annots [6 0 R] >>",
               b"<< /Length 0 >>\nstream\n\nendstream",
               b"<< /S /JavaScript /JS (app.alert\\(EVIL\\);this.getURL\\(https://evil\\)) >>",
               b"<< /Type /Annot /Subtype /Link /Rect [0 0 10 10] /A << /S /URI /URI (https://evil) >> >>"]
    raw, offsets = bytearray(b"%PDF-1.7\n"), [0]
    for index, obj in enumerate(objects, 1):
        offsets.append(len(raw))
        raw.extend(str(index).encode() + b" 0 obj\n" + obj + b"\nendobj\n")
    xref = len(raw)
    raw.extend(b"xref\n0 7\n0000000000 65535 f \n")
    for offset in offsets[1:]: raw.extend(f"{offset:010d} 00000 n \n".encode())
    raw.extend(b"trailer\n<< /Size 7 /Root 1 0 R >>\nstartxref\n" + str(xref).encode() + b"\n%%EOF\n")
    return bytes(raw)


class WorkerTests(unittest.TestCase):
    def test_protocol_manifest_and_finite_input(self):
        raw = raster()
        wire = request(raw, "image/png")
        header, actual, _ = read_request(io.BytesIO(wire))
        self.assertEqual(actual, raw)
        for bad in (wire[:-1], wire + b"x", wire[:-1] + b"x"):
            with self.assertRaises(PreviewError): read_request(io.BytesIO(bad))
        duplicate = b'{"version":1,"version":1}'
        with self.assertRaises(PreviewError): read_request(io.BytesIO(struct.pack(">I", len(duplicate)) + duplicate))
        self.assertEqual(header["original_sha256"], digest(raw))

    def test_three_rasters_roundtrip_static(self):
        for format, media in (("PNG", "image/png"), ("JPEG", "image/jpeg"), ("WEBP", "image/webp")):
            raw = raster(format)
            plan = inspect(raw, media, Limits())
            records, payloads, work = render(raw, media, Limits(), plan)
            self.assertEqual(len(payloads), 2)
            self.assertEqual(plan["inspection_decoded_pixels"], 0)
            self.assertEqual(work["decoded_pixels"], 64 * 32)
            with Image.open(io.BytesIO(payloads[0])) as image:
                self.assertEqual(image.size, (64, 32)); self.assertEqual(image.format, "PNG")
                self.assertEqual(getattr(image, "n_frames", 1), 1)

    def test_mp4_first_frame_and_thumbnail_are_inert_rasters(self):
        raw, media, limits = mp4(with_audio=True), "video/mp4", Limits()
        plan = inspect(raw, media, limits)
        records, payloads, work = render(raw, media, limits, plan)
        self.assertEqual(plan["info"], {"width": 640, "height": 320})
        self.assertEqual(plan["inspection_decoded_pixels"], 0)
        self.assertEqual(work["decoded_pixels"], 640 * 320)
        self.assertEqual([(x["variant"], x["page"], x["media_type"]) for x in records],
                         [("image", 0, "image/png"), ("thumbnail", 0, "image/png")])
        for payload, size in zip(payloads, ((640, 320), (320, 160))):
            with Image.open(io.BytesIO(payload)) as image:
                self.assertEqual((image.format, image.n_frames, image.size), ("PNG", 1, size))
                red, _, blue = image.getpixel((20, 20))
                self.assertGreater(red, 180)
                self.assertLess(blue, 40)

    def test_mp4_rejects_external_tracks_unsupported_codecs_and_small_budgets(self):
        raw = mp4()
        for uri in (b"file:///etc/passwd", b"https://example.invalid/external.mp4"):
            with self.assertRaisesRegex(PreviewError, "video_external_reference"):
                inspect(external_mp4(raw, uri), "video/mp4", Limits())
        for invalid in (mp4("mpeg4"), audio_only_mp4(), raw[:-20], b"not MP4"):
            with self.assertRaises((PreviewError, av.error.FFmpegError)):
                inspect(invalid, "video/mp4", Limits())
        with self.assertRaises((PreviewError, av.error.FFmpegError)):
            inspect(raw, "video/mp4", replace(Limits(), image_pixels=100))

    def test_animation_all_frames_bound_and_first_only(self):
        out = io.BytesIO()
        frames = [Image.new("RGB", (20, 10), color) for color in ("red", "blue", "green")]
        frames[0].save(out, format="WEBP", save_all=True, append_images=frames[1:], duration=10)
        raw = out.getvalue()
        plan = inspect(raw, "image/webp", Limits())
        _, payloads, work = render(raw, "image/webp", Limits(), plan)
        self.assertEqual(work["decoded_pixels"], 600)
        self.assertTrue(work["animated_first_frame"])
        with Image.open(io.BytesIO(payloads[0])) as image: self.assertEqual(image.n_frames, 1)
        tiny = replace(Limits(), image_pixels=400)
        with self.assertRaises(PreviewError): render(raw, "image/webp", tiny, inspect(raw, "image/webp", tiny))

    def test_pdf_complete_pages_and_bounds(self):
        raw = pdf()
        plan = inspect(raw, "application/pdf", Limits())
        records, payloads, work = render(raw, "application/pdf", Limits(), plan)
        self.assertEqual(plan["output_count"], 5)
        self.assertEqual({x["page"] for x in records if x["variant"] == "pdf_page"}, {1, 2})
        self.assertEqual(len(payloads), 5)
        self.assertGreater(work["pdf_decoded_bytes"], 0)
        with self.assertRaises(PreviewError): inspect(raw, "application/pdf", replace(Limits(), pdf_pages=1))
        with self.assertRaises(PreviewError): inspect(raw, "application/pdf", replace(Limits(), pdf_decoded_bytes=100))

    def test_render_plan_and_output_integrity(self):
        raw, media, limits = raster(), "image/png", Limits()
        plan = inspect(raw, media, limits)
        records, outputs, work = render(raw, media, limits, plan)
        request_header, _, _ = read_request(io.BytesIO(request(raw, media, "render", plan)))
        h = {key: request_header[key] for key in ("operation", "media_type", "original_bytes", "original_sha256", "policy_version")}
        h.update(version=1, status="ok", plan=plan, plan_sha256=digest(canonical(plan)), outputs=records, work=work)
        wire = response_bytes(h, outputs)
        self.assertEqual(len(verify_response(wire, request_header)[1]), 2)
        for bad in (wire[:-1], wire + b"x", wire[:-1] + b"x"):
            with self.assertRaises(PreviewError): verify_response(bad, request_header)
        with self.assertRaises(PreviewError): render(raw + b"x", media, limits, plan)
        forged = copy.deepcopy(h)
        forged["outputs"][0].update(variant="pdf_page", page=1)
        forged["outputs"][1].update(variant="pdf_thumbnail", page=1)
        with self.assertRaises(PreviewError): verify_response(response_bytes(forged, outputs), request_header)
        forged = copy.deepcopy(h)
        del forged["work"]
        with self.assertRaises(PreviewError): verify_response(response_bytes(forged, outputs), request_header)
        forged = copy.deepcopy(h)
        forged["plan"]["info"]["width"] = True
        with self.assertRaises(PreviewError): verify_response(response_bytes(forged, outputs), request_header)

    def test_sanitized_styled_html_and_variables(self):
        raw = b'''<style>:root{--c:#123456}.card{display:grid;grid-template-columns:repeat(2,1fr);gap:12px;color:var(--c);padding:10px;font-family:Arial,sans-serif}</style><main class="card"><h1>Title</h1><p style="--s:24px;font-size:var(--s);font-weight:700">Text</p></main>'''
        result, _ = sanitize(raw, Limits())
        text = result.decode()
        for value in ("display:grid", "repeat(2,1fr)", "#123456", "font-size:24px", "font-weight:700", "<h1>Title</h1>"):
            self.assertIn(value, text)
        self.assertNotIn("var(", text)

    def test_html_active_content_and_css_loads_removed(self):
        raw = b'''<meta http-equiv="refresh" content="0;url=https://evil"><base href="https://evil"><script>EVIL()</script><svg><script>BAD</script></svg><math><mtext>BAD</mtext></math><iframe srcdoc="BAD"></iframe><form action="https://evil"><button onclick="BAD">Go</button><input value="Hi"></form><a href="javascript:BAD" ping="https://evil">Link</a><div style="background:url(https://evil);filter:blur(50px);animation:a 1s;color:red;position:fixed">Safe</div><style>@import 'https://evil';@font-face{font-family:x;src:url(https://evil)}div{background-image:url(data:image/svg+xml,BAD);color:blue} :visited{color:red}</style>'''
        text = sanitize(raw, Limits())[0].decode()
        for bad in ("EVIL", "BAD", "https://evil", "javascript:", "<svg", "<math", "<iframe", "<form", "<button", "<input", "href=", "onclick", "@import", "@font-face", "url(", "filter:", "animation:", "position:fixed"):
            self.assertNotIn(bad, text)
        self.assertIn("color:red", text); self.assertIn("color:blue", text)

    def test_css_variables_cycles_and_indirection(self):
        raw = b'''<style>:root{--a:var(--b);--b:var(--a);--evil:url(https://evil)}p{color:var(--a);background:var(--evil);width:expression(x);height:calc(1px);font-size:16px}</style><p style="color:var(--missing,red);padding:var(--x,10px)">Ok</p>'''
        text = sanitize(raw, Limits())[0].decode()
        for bad in ("url(", "https:", "var(", "expression", "calc("):
            self.assertNotIn(bad, text)
        self.assertIn("color:red", text); self.assertIn("padding:10px", text)

    def test_embedded_raster_validated_and_reencoded(self):
        src = base64.b64encode(raster()).decode()
        raw = ('<img src="data:image/png;base64,' + src + '"><img src="data:image/png;base64,PHN2Zz48L3N2Zz4="><img src="https://evil">').encode()
        text, work = sanitize(raw, Limits())
        self.assertEqual(text.count(b"data:image/png;base64,"), 1)
        self.assertEqual(text.count(b"Image unavailable"), 2)
        self.assertEqual(work, 64 * 32)
        with self.assertRaises(PreviewError): sanitize(raw, replace(Limits(), embedded_images=1))

    def test_html_structure_rule_and_numeric_limits(self):
        for raw, limits in ((b"<div>" * 10 + b"x" + b"</div>" * 10, replace(Limits(), dom_depth=5)),
                            (b"<p>x</p>" * 20, replace(Limits(), dom_nodes=10)),
                            (b"<style>p{color:red}p{color:blue}</style>", replace(Limits(), css_rules=1))):
            with self.assertRaises(PreviewError): sanitize(raw, limits)
        text = sanitize(b'<p style="width:999999999px;height:1e999px;color:red">x</p>', Limits())[0]
        self.assertNotIn(b"width:", text); self.assertNotIn(b"height:", text)
        text = sanitize(b'<p style="grid-template-columns:repeat(32,repeat(32,1fr));display:grid">x</p>', Limits())[0]
        self.assertNotIn(b"grid-template-columns", text)

    def test_embedded_animation_labeled_and_receipted(self):
        buffer = io.BytesIO()
        first = Image.new("RGB", (20, 10), "red")
        first.save(buffer, format="WEBP", save_all=True, append_images=[Image.new("RGB", (20, 10), "blue")])
        raw = ('<img src="data:image/webp;base64,' + base64.b64encode(buffer.getvalue()).decode() + '">').encode()
        plan = inspect(raw, "text/html", Limits())
        _, outputs, work = render(raw, "text/html", Limits(), plan)
        self.assertTrue(work["animated_first_frame"])
        self.assertIn("Animated image — first frame only", outputs[0].decode())
        self.assertEqual(work["decoded_pixels"], 400)

    def test_polyglot_image_is_reencoded_without_active_tail(self):
        raw = raster() + b"<script>EVIL</script>"
        plan = inspect(raw, "image/png", Limits())
        _, outputs, _ = render(raw, "image/png", Limits(), plan)
        self.assertNotIn(b"EVIL", outputs[0])


if __name__ == "__main__": unittest.main()
