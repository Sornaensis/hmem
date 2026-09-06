from __future__ import annotations

import argparse
import inspect
import os
import platform
import threading
import time
from pathlib import Path

import numpy
import onnx
import onnxruntime
import psutil
import tokenizers
import torch
import transformers
from transformers import AutoModel, AutoTokenizer

from common import (
    clean_offline_environment,
    permit_guarded_flash_attention_import,
    sha256_file,
    verify_source_snapshot,
    write_json,
)


MODEL_SHA = "8851d692b05bbf3b06a9ada6c0c9c857df6461f2a2b093e7fa831c1078040602"
OPSET = 17


class FeatureExtraction(torch.nn.Module):
    def __init__(self, model: torch.nn.Module) -> None:
        super().__init__()
        self.model = model

    def forward(self, input_ids: torch.Tensor, attention_mask: torch.Tensor) -> torch.Tensor:
        return self.model(
            input_ids=input_ids,
            attention_mask=attention_mask,
            use_cache=False,
            return_dict=True,
            is_causal=True,
        ).last_hidden_state


class PeakRss:
    def __init__(self) -> None:
        self._stop = threading.Event()
        self.peak = 0
        self._thread = threading.Thread(target=self._sample, daemon=True)

    def _sample(self) -> None:
        process = psutil.Process()
        while not self._stop.wait(0.1):
            total = process.memory_info().rss
            for child in process.children(recursive=True):
                try:
                    total += child.memory_info().rss
                except psutil.Error:
                    pass
            self.peak = max(self.peak, total)

    def __enter__(self) -> "PeakRss":
        self._thread.start()
        return self

    def __exit__(self, *_: object) -> None:
        self._stop.set()
        self._thread.join()


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--threads", type=int, default=8)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    clean_offline_environment()
    source = args.source.resolve(strict=True)
    output = args.output.resolve(strict=True)
    if any(output.iterdir()):
        raise ValueError("output directory must be empty")
    source_evidence = verify_source_snapshot(source)
    if args.threads < 1:
        raise ValueError("threads must be positive")
    torch.set_num_threads(args.threads)
    torch.set_num_interop_threads(1)
    torch.manual_seed(1729)

    started = time.monotonic()
    with PeakRss() as rss:
        tokenizer = AutoTokenizer.from_pretrained(
            source,
            trust_remote_code=True,
            local_files_only=True,
        )
        with permit_guarded_flash_attention_import():
            model = AutoModel.from_pretrained(
                source,
                trust_remote_code=True,
                local_files_only=True,
                torch_dtype=torch.float32,
                attn_implementation="eager",
            )
        model.eval()
        model.config.use_cache = False
        forward_default = inspect.signature(model.forward).parameters["is_causal"].default
        if forward_default is not True:
            raise ValueError(f"unexpected AutoModel is_causal default: {forward_default!r}")
        loaded_modeling = Path(inspect.getfile(model.__class__))
        if sha256_file(loaded_modeling) != MODEL_SHA:
            raise ValueError("executed remote modeling code does not match the pinned source hash")
        encoded = tokenizer(
            ["export calibration", "short"],
            padding=True,
            truncation=False,
            add_special_tokens=True,
            return_tensors="pt",
        )
        if not bool(torch.all(encoded["input_ids"][:, -1] == 151643)):
            raise ValueError("pinned tokenizer did not append EOS 151643 to export inputs")
        wrapper = FeatureExtraction(model).eval()
        graph_path = output / "model.onnx"
        export_signature = str(inspect.signature(torch.onnx.export))
        with torch.inference_mode():
            torch.onnx.export(
                wrapper,
                (encoded["input_ids"], encoded["attention_mask"]),
                str(graph_path),
                export_params=True,
                input_names=["input_ids", "attention_mask"],
                output_names=["last_hidden_state"],
                dynamic_axes={
                    "input_ids": {0: "batch", 1: "sequence"},
                    "attention_mask": {0: "batch", 1: "sequence"},
                    "last_hidden_state": {0: "batch", 1: "sequence"},
                },
                do_constant_folding=True,
                opset_version=OPSET,
                verbose=False,
            )
        attention_implementation = getattr(model, "_attn_implementation", None)

    files = []
    for path in sorted(output.iterdir()):
        if path.name == "export-metadata.json":
            continue
        if not path.is_file() or path.is_symlink():
            raise ValueError(f"unsafe export artifact: {path.name}")
        files.append({"path": path.name, "size": path.stat().st_size, "sha256": sha256_file(path)})
    write_json(
        output / "export-metadata.json",
        {
            "schema_version": 1,
            "source": source_evidence,
            "export": {
                "opset": OPSET,
                "input_names": ["input_ids", "attention_mask"],
                "output_names": ["last_hidden_state"],
                "dynamic_axes": ["batch", "sequence"],
                "dtype": "float32",
                "use_cache": False,
                "is_causal": True,
                "attention_implementation": attention_implementation,
                "torch_onnx_export_signature": export_signature,
            },
            "versions": {
                "python": platform.python_version(),
                "numpy": numpy.__version__,
                "onnx": onnx.__version__,
                "onnxruntime": onnxruntime.__version__,
                "tokenizers": tokenizers.__version__,
                "torch": torch.__version__,
                "transformers": transformers.__version__,
            },
            "runtime": {
                "platform": platform.platform(),
                "machine": platform.machine(),
                "threads": args.threads,
                "elapsed_seconds": time.monotonic() - started,
                "peak_rss_bytes": rss.peak,
                "pid": os.getpid(),
            },
            "artifacts": files,
        },
    )


if __name__ == "__main__":
    main()
