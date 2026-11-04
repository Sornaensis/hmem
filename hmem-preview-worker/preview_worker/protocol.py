"""Finite length-delimited protocol. No paths, URLs, archives or partial success."""
import io
import json
import math
import re
import struct
from . import POLICY_VERSION
from .policy import Limits, PreviewError, VIDEO_MEDIA, canonical, digest, require, valid_video_size

VERSION = 1
MAX_REQUEST_HEADER = 64 * 1024
MAX_RESPONSE_HEADER = 256 * 1024
MAX_OUTPUTS = 401
MAX_STDERR = 16 * 1024
MAX_WIRE = Limits().output_bytes + MAX_RESPONSE_HEADER + 8 * MAX_OUTPUTS + 4
MEDIA = {"image/png", "image/jpeg", "image/webp", "application/pdf", "text/html", VIDEO_MEDIA}
SHA = re.compile(r"[0-9a-f]{64}\Z")


def _object(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise PreviewError("duplicate_json_key")
        result[key] = value
    return result


def json_object(raw):
    try:
        result = json.loads(raw.decode("utf-8"), object_pairs_hook=_object,
                            parse_constant=lambda _: (_ for _ in ()).throw(PreviewError("json_invalid")))
    except (ValueError, UnicodeError, RecursionError):
        raise PreviewError("json_invalid") from None
    require(isinstance(result, dict), "json_invalid")
    return result


def read_exact(stream, count):
    require(type(count) is int and count >= 0, "length_invalid")
    result = bytearray()
    while len(result) < count:
        chunk = stream.read(min(count - len(result), 65536))
        require(bool(chunk), "truncated_input")
        result.extend(chunk)
    return bytes(result)


def read_header(stream, maximum):
    count = struct.unpack(">I", read_exact(stream, 4))[0]
    require(0 < count <= maximum, "header_limit")
    return json_object(read_exact(stream, count))


def encode_header(value, maximum=MAX_RESPONSE_HEADER):
    raw = canonical(value)
    require(0 < len(raw) <= maximum, "header_limit")
    return struct.pack(">I", len(raw)) + raw


def request(raw, media_type, operation="inspect", plan=None, limits=None):
    limits = limits or Limits()
    require(operation in {"inspect", "render"}, "operation_invalid")
    require(media_type in MEDIA, "unsupported")
    require(len(raw) <= limits.original_bytes, "original_limit")
    header = {"version": VERSION, "operation": operation, "media_type": media_type,
              "original_bytes": len(raw), "original_sha256": digest(raw),
              "policy_version": POLICY_VERSION, "limits": limits.__dict__}
    if plan is not None:
        header["plan"] = plan
    return encode_header(header, MAX_REQUEST_HEADER) + raw


def read_request(stream, eof=True):
    h = read_header(stream, MAX_REQUEST_HEADER)
    require(set(h) <= {"version", "operation", "media_type", "original_bytes", "original_sha256",
                       "policy_version", "limits", "plan"}, "request_invalid")
    require(h.get("version") == VERSION and type(h.get("version")) is int, "version_invalid")
    require(h.get("operation") in {"inspect", "render"}, "operation_invalid")
    require(h.get("media_type") in MEDIA, "unsupported")
    require(h.get("policy_version") == POLICY_VERSION, "policy_mismatch")
    require((h["operation"] == "render") == ("plan" in h), "request_invalid")
    limits = Limits.parse(h.get("limits"))
    count = h.get("original_bytes")
    require(type(count) is int and 0 < count <= limits.original_bytes, "original_limit")
    require(isinstance(h.get("original_sha256"), str) and bool(SHA.fullmatch(h["original_sha256"])),
            "manifest_invalid")
    raw = read_exact(stream, count)
    if eof:
        require(not stream.read(1), "trailing_input")
    require(digest(raw) == h["original_sha256"], "original_hash_mismatch")
    return h, raw, limits


def response_bytes(header, outputs):
    result = bytearray(encode_header(header))
    for payload in outputs:
        result.extend(struct.pack(">Q", len(payload)))
        result.extend(payload)
    require(len(result) <= MAX_WIRE, "output_limit")
    return bytes(result)


def validate_plan(plan, request_header, limits):
    require(isinstance(plan, dict) and set(plan) == {"original_sha256", "media_type", "policy_version",
            "limits_sha256", "output_count", "maximum_bytes", "inspection_decoded_pixels", "inspection_raster_bytes", "info"}, "plan_invalid")
    require(plan["limits_sha256"] == limits.binding() and plan["original_sha256"] == request_header["original_sha256"]
            and plan["media_type"] == request_header["media_type"] and plan["policy_version"] == POLICY_VERSION,
            "plan_invalid")
    require(type(plan["inspection_decoded_pixels"]) is int and plan["inspection_decoded_pixels"] == 0
            and type(plan["inspection_raster_bytes"]) is int and plan["inspection_raster_bytes"] == 0
            and type(plan["maximum_bytes"]) is int and plan["maximum_bytes"] == limits.output_bytes,
            "plan_invalid")
    info, media = plan["info"], plan["media_type"]
    require(isinstance(info, dict), "plan_invalid")
    if media.startswith("image/") or media == VIDEO_MEDIA:
        require(set(info) == {"width", "height"} and all(type(info[x]) is int and info[x] > 0 for x in info)
                and info["width"] * info["height"] <= limits.image_pixels, "plan_invalid")
        if media == VIDEO_MEDIA:
            require(valid_video_size(info["width"], info["height"], limits), "plan_invalid")
        shape = {("image", 0, "image/png"), ("thumbnail", 0, "image/png")}
    elif media == "application/pdf":
        require(set(info) == {"pages", "planned_decoded_bytes"} and isinstance(info["pages"], list)
                and 0 < len(info["pages"]) <= limits.pdf_pages, "plan_invalid")
        work = 0
        for page in info["pages"]:
            require(isinstance(page, dict) and set(page) == {"width", "height", "scale"}, "plan_invalid")
            require(all(type(page[x]) is int and 0 < page[x] <= limits.longest_side for x in ("width", "height"))
                    and page["width"] * page["height"] <= limits.page_pixels
                    and type(page["scale"]) in {int, float} and math.isfinite(page["scale"])
                    and 0 < page["scale"] <= 2, "plan_invalid")
            work += page["width"] * page["height"] * 4
        require(type(info["planned_decoded_bytes"]) is int and info["planned_decoded_bytes"] == work
                and work <= limits.pdf_decoded_bytes, "plan_invalid")
        shape = {("thumbnail", 0, "image/png")}
        shape.update((variant, page, "image/png") for page in range(1, len(info["pages"]) + 1)
                     for variant in ("pdf_page", "pdf_thumbnail"))
    else:
        require(media == "text/html" and info == {}, "plan_invalid")
        shape = {("styled_html_preview", 0, "text/html")}
    require(type(plan["output_count"]) is int and plan["output_count"] == len(shape)
            and len(shape) <= MAX_OUTPUTS, "plan_invalid")
    return shape


def verify_response(wire, request_header, limits=None):
    limits = limits or Limits.parse(request_header.get("limits"))
    require(len(wire) <= MAX_WIRE, "output_limit")
    stream = io.BytesIO(wire)
    h = read_header(stream, MAX_RESPONSE_HEADER)
    require(h.get("version") == VERSION and type(h.get("version")) is int, "version_invalid")
    if h.get("status") != "ok":
        require(h.get("status") == "error" and isinstance(h.get("code"), str)
                and re.fullmatch(r"[a-z0-9_]{1,80}", h["code"]) is not None, "response_invalid")
        require(not stream.read(1), "trailing_output")
        raise PreviewError(h["code"])
    for field in ("operation", "media_type", "original_bytes", "original_sha256", "policy_version"):
        require(h.get(field) == request_header.get(field), "response_binding_mismatch")
    plan = h.get("plan")
    shape = validate_plan(plan, request_header, limits)
    require(h.get("plan_sha256") == digest(canonical(plan)), "plan_hash_mismatch")
    work = h.get("work")
    require(isinstance(work, dict) and set(work) == {"decoded_pixels", "pdf_decoded_bytes", "animated_first_frame"}
            and type(work["decoded_pixels"]) is int and 0 <= work["decoded_pixels"] <= limits.image_pixels
            and type(work["pdf_decoded_bytes"]) is int and 0 <= work["pdf_decoded_bytes"] <= limits.pdf_decoded_bytes
            and type(work["animated_first_frame"]) is bool, "work_receipt_invalid")
    records = h.get("outputs")
    require(isinstance(records, list), "response_invalid")
    expected = 0 if h["operation"] == "inspect" else plan["output_count"]
    require(len(records) == expected, "output_count_mismatch")
    if h["operation"] == "render":
        require(plan == request_header.get("plan"), "plan_mismatch")
    result, total, seen = [], 0, set()
    for record in records:
        require(isinstance(record, dict) and set(record) == {"variant", "page", "media_type", "bytes", "sha256"},
                "output_manifest_invalid")
        require(record["variant"] in {"image", "thumbnail", "pdf_page", "pdf_thumbnail", "styled_html_preview"}
                and type(record["page"]) is int and 0 <= record["page"] <= limits.pdf_pages,
                "output_manifest_invalid")
        require(record["media_type"] in {"image/png", "text/html"}, "output_manifest_invalid")
        require((record["variant"], record["page"], record["media_type"]) in shape, "output_shape_mismatch")
        key = (record["variant"], record["page"])
        require(key not in seen, "duplicate_output")
        seen.add(key)
        count = record["bytes"]
        require(type(count) is int and 0 < count <= limits.output_bytes - total, "output_limit")
        require(struct.unpack(">Q", read_exact(stream, 8))[0] == count, "output_length_mismatch")
        data = read_exact(stream, count)
        require(digest(data) == record["sha256"], "output_hash_mismatch")
        if record["media_type"] == "image/png":
            require(data.startswith(b"\x89PNG\r\n\x1a\n"), "output_type_mismatch")
        total += count
        result.append((record, data))
    require(total <= plan["maximum_bytes"] and not stream.read(1), "trailing_output")
    return h, result
