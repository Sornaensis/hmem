from __future__ import annotations

import argparse
import shutil
import stat
from pathlib import Path
from typing import Any

from common import sha256_file, verify_source_snapshot, write_json
from verify_graph import verify_graph


SUPPORT_FILES = [
    "1_Pooling/config.json",
    "added_tokens.json",
    "config.json",
    "config_sentence_transformers.json",
    "merges.txt",
    "modules.json",
    "sentence_bert_config.json",
    "special_tokens_map.json",
    "tokenizer.json",
    "tokenizer_config.json",
    "vocab.json",
]


def copy_regular(source: Path, destination: Path) -> None:
    mode = source.lstat().st_mode
    if not stat.S_ISREG(mode) or source.is_symlink():
        raise ValueError(f"source is not a regular non-symlink file: {source}")
    destination.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(source, destination)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", type=Path, required=True)
    parser.add_argument("--export", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--inventory", type=Path, required=True)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    source = args.source.resolve(strict=True)
    export = args.export.resolve(strict=True)
    output = args.output.resolve(strict=True)
    if any(output.iterdir()):
        raise ValueError("serving output directory must be empty")
    source_evidence = verify_source_snapshot(source)
    graph_evidence = verify_graph(export / "model.onnx")

    roles: dict[str, str] = {}
    for relative in SUPPORT_FILES:
        copy_regular(source / Path(relative), output / Path(relative))
        roles[Path(relative).as_posix()] = "pinned-source-support"
    for graph_file in graph_evidence["files"]:
        relative = graph_file["path"]
        copy_regular(export / relative, output / relative)
        roles[relative] = "derived-onnx-graph" if relative == "model.onnx" else "derived-onnx-external-data"

    inventory: list[dict[str, Any]] = []
    for path in sorted(output.rglob("*")):
        if path.is_dir():
            continue
        relative = path.relative_to(output).as_posix()
        mode = path.lstat().st_mode
        if not stat.S_ISREG(mode) or path.is_symlink():
            raise ValueError(f"unsafe serving artifact: {relative}")
        inventory.append(
            {
                "path": relative,
                "role": roles[relative],
                "size": path.stat().st_size,
                "sha256": sha256_file(path),
            }
        )
    write_json(
        args.inventory,
        {
            "schema_version": 1,
            "status": "experimental-only-not-promoted",
            "source": source_evidence,
            "graph": graph_evidence["graph"],
            "files": inventory,
        },
    )


if __name__ == "__main__":
    main()
