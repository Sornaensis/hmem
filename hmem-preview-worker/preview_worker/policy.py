"""Absolute v1 work bounds. Operators may tighten, never silently widen them."""
from dataclasses import asdict, dataclass
import hashlib
import json

VIDEO_MEDIA = "video/mp4"
VIDEO_SIDE = 4096
VIDEO_PIXELS = 3840 * 2160


def valid_video_size(width, height, limits):
    return (0 < width <= VIDEO_SIDE and 0 < height <= VIDEO_SIDE
            and width * height <= min(VIDEO_PIXELS, limits.image_pixels))


class PreviewError(Exception):
    def __init__(self, code):
        self.code = code
        super().__init__(code)


@dataclass(frozen=True)
class Limits:
    original_bytes: int = 25 * 1024 * 1024
    image_pixels: int = 40_000_000
    pdf_pages: int = 200
    page_pixels: int = 16_000_000
    longest_side: int = 4096
    pdf_decoded_bytes: int = 2 * 1024**3
    output_bytes: int = 100 * 1024**2
    html_bytes: int = 5 * 1024**2
    dom_nodes: int = 20_000
    dom_depth: int = 64
    css_rules: int = 2000
    embedded_images: int = 100

    @classmethod
    def parse(cls, value=None):
        if value is None:
            return cls()
        if not isinstance(value, dict) or set(value) - set(asdict(cls())):
            raise PreviewError("limits_invalid")
        defaults = asdict(cls())
        if any(type(v) is not int or not 1 <= v <= defaults[k] for k, v in value.items()):
            raise PreviewError("limits_invalid")
        return cls(**value)

    def binding(self):
        return hashlib.sha256(canonical(asdict(self))).hexdigest()


def canonical(value):
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True,
                      allow_nan=False).encode("ascii")


def digest(data):
    return hashlib.sha256(data).hexdigest()


def require(ok, code):
    if not ok:
        raise PreviewError(code)
