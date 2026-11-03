"""Deterministic plans bind inspection to a complete inert generation."""
from . import POLICY_VERSION
from .policy import canonical, digest, require


def inspect(raw, media, limits):
    if media.startswith("image/"):
        from .images import inspect_image
        info = inspect_image(raw, media, limits)
        count = 2
    elif media == "application/pdf":
        from .pdf import inspect_pdf
        info = inspect_pdf(raw, limits)
        count = len(info["pages"]) * 2 + 1
    else:
        require(media == "text/html", "unsupported")
        require(len(raw) <= limits.html_bytes, "html_input_limit")
        raw.decode("utf-8", errors="strict")
        info, count = {}, 1
    return {"original_sha256": digest(raw), "media_type": media, "policy_version": POLICY_VERSION,
            "limits_sha256": limits.binding(), "output_count": count,
            "maximum_bytes": limits.output_bytes, "inspection_decoded_pixels": 0,
            "inspection_raster_bytes": 0, "info": info}


def render(raw, media, limits, plan):
    require(plan == inspect(raw, media, limits), "plan_mismatch")
    pixels, pdf_bytes, animated = 0, 0, False
    if media.startswith("image/"):
        from .images import render_image
        work = {"pixels": 0}
        image, thumbnail, animated = render_image(raw, media, limits, work)
        outputs = [("image", 0, "image/png", image), ("thumbnail", 0, "image/png", thumbnail)]
        pixels = work["pixels"]
    elif media == "application/pdf":
        from .pdf import render_pdf
        outputs, pdf_bytes = render_pdf(raw, plan["info"], limits)
    else:
        from .sanitize import sanitize
        metadata = {}
        html, pixels = sanitize(raw, limits, metadata)
        animated = metadata.get("animated_first_frame", False)
        outputs = [("styled_html_preview", 0, "text/html", html)]
    require(len(outputs) == plan["output_count"], "output_count_mismatch")
    require(sum(len(x[3]) for x in outputs) <= limits.output_bytes, "output_limit")
    records = [{"variant": variant, "page": page, "media_type": content_type,
                "bytes": len(data), "sha256": digest(data)}
               for variant, page, content_type, data in outputs]
    return records, [x[3] for x in outputs], {"decoded_pixels": pixels,
            "pdf_decoded_bytes": pdf_bytes, "animated_first_frame": animated}
