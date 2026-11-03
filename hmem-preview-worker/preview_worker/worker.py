"""One finite inspect or render operation inside an already confined container."""
import sys
import time
from .confinement import check
from .policy import PreviewError, canonical, digest
from .protocol import VERSION, read_request, response_bytes


def main():
    started = time.monotonic_ns()
    try:
        runtime = check()
        from .pdf import runtime_flags, PDFIUM_INFO
        runtime["pdfium_flags"] = list(runtime_flags())
        runtime["pdfium_version"] = str(PDFIUM_INFO)
        from .render import inspect, render
        header, raw, limits = read_request(sys.stdin.buffer)
        plan = inspect(raw, header["media_type"], limits)
        outputs, records, work = [], [], {"decoded_pixels": 0, "pdf_decoded_bytes": 0,
                                          "animated_first_frame": False}
        if header["operation"] == "render":
            records, outputs, work = render(raw, header["media_type"], limits, header.get("plan"))
        response = {key: header[key] for key in ("operation", "media_type", "original_bytes", "original_sha256", "policy_version")}
        response.update(version=VERSION, status="ok", plan=plan, plan_sha256=digest(canonical(plan)), outputs=records, work=work,
                        runtime=runtime, elapsed_ms=(time.monotonic_ns() - started) // 1_000_000)
        wire = response_bytes(response, outputs)
    except PreviewError as error:
        wire = response_bytes({"version": VERSION, "status": "error", "code": error.code}, [])
    except (Exception, MemoryError):
        # No original markup, parser exception, local path or credential enters diagnostics.
        wire = response_bytes({"version": VERSION, "status": "error", "code": "invalid_or_exhausted_input"}, [])
    sys.stdout.buffer.write(wire)
    sys.stdout.buffer.flush()


if __name__ == "__main__":
    main()
