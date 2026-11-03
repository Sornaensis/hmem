"""Raster-only decoding. Caller must already be inside the confined worker."""
from io import BytesIO
import warnings
from PIL import Image, ImageOps
from .policy import PreviewError, require

FORMATS = {"image/png": "PNG", "image/jpeg": "JPEG", "image/webp": "WEBP"}
Image.MAX_IMAGE_PIXELS = 40_000_000
warnings.simplefilter("error", Image.DecompressionBombWarning)


def open_image(raw, media, limits):
    image = Image.open(BytesIO(raw), formats=list(FORMATS.values()))
    require(image.format == FORMATS.get(media), "image_type_mismatch")
    require(image.width > 0 and image.height > 0, "image_dimensions")
    require(image.width * image.height <= limits.image_pixels, "image_pixel_limit")
    return image


def inspect_image(raw, media, limits):
    with open_image(raw, media, limits) as image:
        # Do not ask for n_frames here: some plugins seek/decode to discover it.
        return {"width": image.width, "height": image.height}


def png_bytes(image, maximum):
    out = BytesIO()
    image.save(out, format="PNG", optimize=False)
    require(0 < out.tell() <= maximum, "output_limit")
    return out.getvalue()


def render_image(raw, media, limits, budget=None):
    budget = budget if budget is not None else {"pixels": 0}
    first = None
    with open_image(raw, media, limits) as image:
        frames = getattr(image, "n_frames", 1)
        require(type(frames) is int and 1 <= frames <= limits.image_pixels // (image.width * image.height),
                "image_pixel_limit")
        for index in range(frames):
            image.seek(index)
            budget["pixels"] += image.width * image.height
            require(budget["pixels"] <= limits.image_pixels, "image_pixel_limit")
            image.load()
            if index == 0:
                first = ImageOps.exif_transpose(image).convert("RGBA")
        require(first is not None, "image_invalid")
    first.thumbnail((limits.longest_side, limits.longest_side), Image.Resampling.LANCZOS)
    preview = png_bytes(first, limits.output_bytes)
    thumb = first.copy()
    thumb.thumbnail((320, 240), Image.Resampling.LANCZOS)
    return preview, png_bytes(thumb, limits.output_bytes - len(preview)), frames > 1
