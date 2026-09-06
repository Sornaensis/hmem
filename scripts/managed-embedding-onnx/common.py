from __future__ import annotations

import hashlib
import json
import math
import os
import stat
from contextlib import contextmanager
from pathlib import Path
from typing import Any, Iterable, Iterator

from transformers import dynamic_module_utils
from transformers.utils import is_flash_attn_2_available


SCRIPT_DIR = Path(__file__).resolve().parent
SOURCE_LOCK_PATH = SCRIPT_DIR / "source-lock.json"
PINNED_MODELING_SHA256 = "8851d692b05bbf3b06a9ada6c0c9c857df6461f2a2b093e7fa831c1078040602"


def load_json(path: Path) -> Any:
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def write_json(path: Path, value: Any) -> None:
    with path.open("w", encoding="utf-8", newline="\n") as handle:
        json.dump(value, handle, indent=2, sort_keys=True, ensure_ascii=False)
        handle.write("\n")


def sha256_file(path: Path, chunk_size: int = 8 * 1024 * 1024) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        while chunk := handle.read(chunk_size):
            digest.update(chunk)
    return digest.hexdigest()


def verify_source_snapshot(root: Path) -> dict[str, Any]:
    root = root.resolve(strict=True)
    lock = load_json(SOURCE_LOCK_PATH)
    expected = lock["files"]
    observed: dict[str, dict[str, Any]] = {}
    for path in sorted(root.rglob("*")):
        relative = path.relative_to(root).as_posix()
        mode = path.lstat().st_mode
        if stat.S_ISDIR(mode):
            continue
        if not stat.S_ISREG(mode) or path.is_symlink():
            raise ValueError(f"unsafe source artifact: {relative}")
        observed[relative] = {
            "sha256": sha256_file(path),
            "size": path.stat().st_size,
        }
    if set(observed) != set(expected):
        missing = sorted(set(expected) - set(observed))
        extra = sorted(set(observed) - set(expected))
        raise ValueError(f"source inventory mismatch: missing={missing}, extra={extra}")
    drift = {
        name: {"expected": expected[name], "actual": observed[name]["sha256"]}
        for name in expected
        if observed[name]["sha256"] != expected[name]
    }
    if drift:
        raise ValueError(f"source checksum mismatch: {drift}")
    canonical = json.dumps(lock, sort_keys=True, separators=(",", ":")).encode("utf-8")
    return {
        "model_id": lock["model_id"],
        "revision": lock["revision"],
        "lock_sha256": hashlib.sha256(canonical).hexdigest(),
        "files": observed,
    }


def prepared_text(item: dict[str, str], query_prompt: str) -> str:
    kind = item["kind"]
    text = item["text"]
    if kind == "document":
        return text
    if kind == "query":
        return query_prompt + text
    raise ValueError(f"unknown input kind: {kind}")


def last_token_pool(hidden_states: Any, attention_mask: Any) -> Any:
    left_padding = bool((attention_mask[:, -1].sum() == attention_mask.shape[0]).item())
    if left_padding:
        return hidden_states[:, -1]
    sequence_lengths = attention_mask.sum(dim=1) - 1
    batch_size = hidden_states.shape[0]
    return hidden_states[range(batch_size), sequence_lengths]


def ensure_finite(values: Iterable[float], label: str) -> None:
    if not all(math.isfinite(float(value)) for value in values):
        raise ValueError(f"non-finite values in {label}")


@contextmanager
def permit_guarded_flash_attention_import() -> Iterator[None]:
    """Allow the audited CPU branch of the pinned remote module to load.

    Transformers' static import scanner treats the module's guarded flash-attn
    import as unconditional. The exact pinned file imports flash-attn only when
    Transformers reports it available; the CPU experiment forces eager/SDPA and
    never executes that branch. Restrict the scanner exception to the audited
    file hash and restore the original scanner immediately after loading.
    """

    if is_flash_attn_2_available():
        raise ValueError("flash-attn must be unavailable in the pinned CPU export/reference toolchain")
    original = dynamic_module_utils.get_imports

    def audited_imports(filename: str) -> list[str]:
        path = Path(filename)
        imports = original(filename)
        if path.name != "modeling_qwen.py":
            return imports
        if sha256_file(path) != PINNED_MODELING_SHA256:
            raise ValueError("refusing import exception for unpinned modeling_qwen.py")
        return [name for name in imports if name != "flash_attn"]

    dynamic_module_utils.get_imports = audited_imports
    try:
        yield
    finally:
        dynamic_module_utils.get_imports = original


def clean_offline_environment() -> None:
    os.environ["HF_HUB_OFFLINE"] = "1"
    os.environ["TRANSFORMERS_OFFLINE"] = "1"
    os.environ["TOKENIZERS_PARALLELISM"] = "false"
    os.environ.pop("HF_TOKEN", None)
    os.environ.pop("HUGGING_FACE_HUB_TOKEN", None)
