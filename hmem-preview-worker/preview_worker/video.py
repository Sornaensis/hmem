"""Decode a single inert poster from a confined, self-contained MP4."""
from contextlib import contextmanager
from io import BytesIO
from struct import unpack_from

import av
from PIL import Image

from .images import png_bytes
from .policy import PreviewError, VIDEO_PIXELS, require, valid_video_size

MP4_BRANDS = {b"isom", b"iso2", b"mp41", b"mp42", b"avc1"}
BOX_CHILDREN = {b"moov": b"trak", b"trak": b"mdia", b"mdia": b"minf", b"minf": b"dinf", b"dinf": b"dref"}
MAX_BOXES = 2048
MAX_PACKETS = 64
MAX_STREAMS = 8


def check_local_references(raw):
    """Read bounded box headers, including every track's data references."""
    require(len(raw) >= 16 and raw[4:8] == b"ftyp" and raw[8:12] in MP4_BRANDS, "video_type_mismatch")
    ftyp_size = unpack_from(">I", raw)[0]
    require(16 <= ftyp_size <= min(4096, len(raw)) and ftyp_size % 4 == 0, "video_invalid")
    remaining_boxes = MAX_BOXES

    def boxes(start, end):
        nonlocal remaining_boxes
        while start < end:
            remaining_boxes -= 1
            require(remaining_boxes >= 0 and end - start >= 8, "video_invalid")
            size, name = unpack_from(">I4s", raw, start)
            header = 8
            if size == 1:
                require(end - start >= 16, "video_invalid")
                size = unpack_from(">Q", raw, start + 8)[0]
                header = 16
            elif size == 0:
                size = end - start
            require(header <= size <= end - start, "video_invalid")
            yield name, start + header, start + size
            start += size

    def check_track_boxes(start, end, parent=None):
        for name, body, limit in boxes(start, end):
            if (parent is None and name == b"moov") or name == BOX_CHILDREN.get(parent):
                if name == b"dref":
                    require(limit - body >= 8, "video_invalid")
                    version, entries = unpack_from(">II", raw, body)
                    require(version == 0 and 0 < entries <= MAX_BOXES, "video_invalid")
                    seen = 0
                    for kind, flags, entry_end in boxes(body + 8, limit):
                        # The only accepted reference is an empty, self-contained URL entry.
                        require(kind == b"url " and entry_end - flags == 4
                                and raw[flags:entry_end] == b"\0\0\0\1", "video_external_reference")
                        seen += 1
                    require(seen == entries, "video_invalid")
                else:
                    check_track_boxes(body, limit, name)
            elif name == b"cmov":
                raise PreviewError("video_invalid")

    check_track_boxes(0, len(raw))


def reject_secondary_io(_url, _flags, _options):
    raise PreviewError("video_external_reference")


@contextmanager
def open_video(raw, limits):
    check_local_references(raw)
    # These codec options also apply to stream-info probing, before dimensions
    # are available to Python. FFmpeg checks max_pixels when allocating frames.
    decoder_options = {
        "threads": "1",
        "max_pixels": str(min(VIDEO_PIXELS, limits.image_pixels)),
        "err_detect": "explode",
    }
    with av.open(BytesIO(raw), mode="r", format="mov", options=decoder_options, container_options={
        "enable_drefs": "0",
        "use_absolute_path": "0",
        "protocol_whitelist": "pipe",
        "codec_whitelist": "h264",
        "max_streams": str(MAX_STREAMS),
        "max_probe_packets": str(MAX_PACKETS),
        "probesize": str(1024 * 1024),
        "analyzeduration": "1000000",
    }, io_open=reject_secondary_io, timeout=2.0) as container:
        require(len(container.streams) <= MAX_STREAMS and len(container.streams.video) == 1, "video_stream_count")
        stream = container.streams.video[0]
        codec = stream.codec_context
        attached_picture = stream.disposition & av.stream.Disposition.attached_pic
        require(codec is not None and codec.name == "h264" and not attached_picture, "video_codec")
        require(valid_video_size(codec.width, codec.height, limits), "video_dimensions")
        codec.options = decoder_options
        yield container, stream


def inspect_video(raw, limits):
    with open_video(raw, limits) as (_, stream):
        return {"width": stream.codec_context.width, "height": stream.codec_context.height}


def render_video(raw, limits, info):
    with open_video(raw, limits) as (container, stream):
        for packet_index, packet in enumerate(container.demux(stream)):
            require(packet_index < MAX_PACKETS, "video_packet_limit")
            for frame in packet.decode():
                require(not frame.is_corrupt and frame.width == info["width"] and frame.height == info["height"]
                        and valid_video_size(frame.width, frame.height, limits), "video_dimensions")
                pixels = frame.width * frame.height
                raster = frame.to_image()
                raster.thumbnail((limits.longest_side, limits.longest_side), Image.Resampling.LANCZOS)
                image = png_bytes(raster, limits.output_bytes)
                raster.thumbnail((320, 240), Image.Resampling.LANCZOS)
                return image, png_bytes(raster, limits.output_bytes - len(image)), pixels
    raise PreviewError("video_no_frame")
