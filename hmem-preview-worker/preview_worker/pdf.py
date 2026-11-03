"""Non-V8 PDFium rasterization; never execute document actions or JavaScript."""
import math
from contextlib import closing
import pypdfium2 as pdfium
from pypdfium2.version import PDFIUM_INFO
from .images import png_bytes
from .policy import require


def runtime_flags():
    flags = tuple(PDFIUM_INFO.flags)
    require(not flags, "pdfium_unsafe_features")
    return flags


def inspect_pdf(raw, limits):
    runtime_flags()
    sizes, work = [], 0
    with pdfium.PdfDocument(raw) as document:
        require(0 < len(document) <= limits.pdf_pages, "pdf_page_limit")
        for index in range(len(document)):
            with closing(document[index]) as page:
                width, height = page.get_size()
                require(math.isfinite(width) and math.isfinite(height) and width > 0 and height > 0,
                        "pdf_dimensions")
                scale = min(2.0, limits.longest_side / max(width, height),
                            math.sqrt(limits.page_pixels / (width * height)))
                # PDFium rounds dimensions upward. Tighten rather than exceed caps.
                while math.ceil(width * scale) * math.ceil(height * scale) > limits.page_pixels:
                    scale *= 0.999
                w, h = math.ceil(width * scale), math.ceil(height * scale)
                require(w > 0 and h > 0 and max(w, h) <= limits.longest_side, "pdf_dimensions")
                work += w * h * 4
                require(work <= limits.pdf_decoded_bytes, "pdf_work_limit")
                sizes.append({"width": w, "height": h, "scale": scale})
    return {"pages": sizes, "planned_decoded_bytes": work}


def render_pdf(raw, info, limits):
    runtime_flags()
    outputs, total, work = [], 0, 0
    with pdfium.PdfDocument(raw) as document:
        require(len(document) == len(info["pages"]), "plan_mismatch")
        for index, expected in enumerate(info["pages"]):
            with closing(document[index]) as page:
                with closing(page.render(scale=expected["scale"], may_draw_forms=False,
                                         draw_annots=False)) as bitmap:
                    image = bitmap.to_pil().convert("RGB")
                    require(image.size == (expected["width"], expected["height"]), "pdf_dimensions")
                    work += image.width * image.height * 4
                    require(work <= limits.pdf_decoded_bytes, "pdf_work_limit")
                    data = png_bytes(image, limits.output_bytes - total)
                    total += len(data)
                    outputs.append(("pdf_page", index + 1, "image/png", data))
                    image.thumbnail((320, 240))
                    thumb = png_bytes(image, limits.output_bytes - total)
                    total += len(thumb)
                    outputs.append(("pdf_thumbnail", index + 1, "image/png", thumb))
                    if index == 0:
                        # Separate cover descriptor; count its stored bytes too.
                        require(total + len(thumb) <= limits.output_bytes, "output_limit")
                        total += len(thumb)
                        outputs.append(("thumbnail", 0, "image/png", thumb))
    return outputs, work
