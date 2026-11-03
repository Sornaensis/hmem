"""Deadline-bound launcher pipe IO; no abandoned blocking reader on timeout."""
import os
import select
import sys
import threading
import time
from .policy import PreviewError, require

_UNJOINED_WRITERS = []


class DeadlineReader:
    def __init__(self, stream, deadline):
        self.fd = stream.fileno()
        self.deadline = deadline
        if os.name != "nt":
            os.set_blocking(self.fd, False)
        else:
            import msvcrt
            msvcrt.setmode(self.fd, os.O_BINARY)

    def read(self, count):
        require(type(count) is int and count >= 0, "length_invalid")
        if count == 0: return b""
        while True:
            require(time.monotonic() < self.deadline, "deadline_exceeded")
            if os.name == "nt":
                import ctypes
                import msvcrt
                from ctypes import wintypes
                available = wintypes.DWORD()
                kernel = ctypes.WinDLL("kernel32", use_last_error=True)
                peek = kernel.PeekNamedPipe
                peek.argtypes = [wintypes.HANDLE, ctypes.c_void_p, wintypes.DWORD,
                                 ctypes.c_void_p, ctypes.POINTER(wintypes.DWORD), ctypes.c_void_p]
                peek.restype = wintypes.BOOL
                if not peek(msvcrt.get_osfhandle(self.fd), None, 0, None, ctypes.byref(available), None):
                    if ctypes.get_last_error() in {109, 232}: return b""
                    raise PreviewError("transport_requires_pipes")
                if available.value:
                    return os.read(self.fd, min(count, available.value))
                time.sleep(min(0.01, max(0, self.deadline - time.monotonic())))
            else:
                ready, _, _ = select.select([self.fd], [], [], min(0.02, max(0, self.deadline - time.monotonic())))
                if ready:
                    try: return os.read(self.fd, count)
                    except BlockingIOError: pass


def write(stream, data, deadline):
    fd = stream.fileno()
    if os.name != "nt":
        os.set_blocking(fd, False)
        offset = 0
        while offset < len(data):
            require(time.monotonic() < deadline, "deadline_exceeded")
            _, ready, _ = select.select([], [fd], [], min(0.02, max(0, deadline - time.monotonic())))
            if ready:
                try: offset += os.write(fd, data[offset:offset + 65536])
                except BlockingIOError: pass
        return
    import msvcrt
    msvcrt.setmode(fd, os.O_BINARY)
    # Anonymous Windows pipes are synchronous. Cancel this exact owned thread's
    # pending WriteFile and join it before reporting a timeout.
    result = []
    def writer():
        try:
            offset = 0
            while offset < len(data): offset += os.write(fd, data[offset:offset + 4096])
            result.append(None)
        except OSError as error: result.append(error)
    thread = threading.Thread(target=writer, daemon=True)
    thread.start()
    interrupted = None
    try:
        thread.join(max(0, deadline - time.monotonic()))
    except BaseException as error:
        interrupted = error
    if thread.is_alive():
        import ctypes
        from ctypes import wintypes
        kernel = ctypes.WinDLL("kernel32", use_last_error=True)
        kernel.OpenThread.argtypes = [wintypes.DWORD, wintypes.BOOL, wintypes.DWORD]
        kernel.OpenThread.restype = wintypes.HANDLE
        kernel.CancelSynchronousIo.argtypes = [wintypes.HANDLE]
        kernel.CancelSynchronousIo.restype = wintypes.BOOL
        kernel.CloseHandle.argtypes = [wintypes.HANDLE]
        handle = kernel.OpenThread(1, False, thread.native_id)
        if handle:
            try: kernel.CancelSynchronousIo(handle)
            finally: kernel.CloseHandle(handle)
        thread.join(1)
        if thread.is_alive():
            _UNJOINED_WRITERS.append(thread)
            raise PreviewError("transport_join_unverified")
        if interrupted is not None:
            raise interrupted
        raise PreviewError("deadline_exceeded")
    if interrupted is not None:
        raise interrupted
    if result and result[0] is not None:
        raise PreviewError("transport_closed")
    require(time.monotonic() < deadline, "deadline_exceeded")
