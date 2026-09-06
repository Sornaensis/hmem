from __future__ import annotations

import argparse
import json
import math
import stat
from collections import Counter, defaultdict
from pathlib import Path, PurePosixPath
from typing import Any

import onnx
from onnx import TensorProto
from onnx.external_data_helper import uses_external_data

from common import sha256_file, write_json


ALLOWED_DOMAINS = {"", "ai.onnx"}
FORBIDDEN_FLOAT_TYPES = {
    TensorProto.FLOAT16,
    TensorProto.BFLOAT16,
    TensorProto.DOUBLE,
    TensorProto.COMPLEX64,
    TensorProto.COMPLEX128,
    TensorProto.FLOAT8E4M3FN,
    TensorProto.FLOAT8E4M3FNUZ,
    TensorProto.FLOAT8E5M2,
    TensorProto.FLOAT8E5M2FNUZ,
}
FIXED_WIDTH_BYTES = {
    TensorProto.FLOAT: 4,
    TensorProto.UINT8: 1,
    TensorProto.INT8: 1,
    TensorProto.UINT16: 2,
    TensorProto.INT16: 2,
    TensorProto.INT32: 4,
    TensorProto.INT64: 8,
    TensorProto.BOOL: 1,
    TensorProto.UINT32: 4,
    TensorProto.UINT64: 8,
}


def _shape(value: Any) -> list[int | str | None]:
    result: list[int | str | None] = []
    for dimension in value.type.tensor_type.shape.dim:
        if dimension.HasField("dim_value"):
            result.append(dimension.dim_value)
        elif dimension.HasField("dim_param"):
            result.append(dimension.dim_param)
        else:
            result.append(None)
    return result


def _safe_external_path(root: Path, location: str) -> Path:
    if not location or "://" in location or "\\" in location:
        raise ValueError(f"unsafe external-data location: {location!r}")
    relative = PurePosixPath(location)
    if relative.is_absolute() or any(part in {"", ".", ".."} for part in relative.parts):
        raise ValueError(f"unsafe external-data location: {location!r}")
    if len(relative.parts) != 1:
        raise ValueError(f"external-data files must be flat siblings of model.onnx: {location!r}")
    candidate = root.joinpath(*relative.parts)
    try:
        mode = candidate.lstat().st_mode
    except FileNotFoundError as error:
        raise ValueError(f"external data file is missing: {location}") from error
    if not stat.S_ISREG(mode) or candidate.is_symlink():
        raise ValueError(f"external data is not a regular non-symlink file: {location}")
    resolved = candidate.resolve(strict=True)
    if resolved.parent != root and root not in resolved.parents:
        raise ValueError(f"external data escapes graph root: {location}")
    return resolved


def _graph_tensors(graph: Any, prefix: str = "graph") -> list[tuple[str, Any]]:
    tensors: list[tuple[str, Any]] = []
    tensors.extend((f"{prefix}.initializer[{tensor.name}]", tensor) for tensor in graph.initializer)
    for index, sparse in enumerate(graph.sparse_initializer):
        tensors.append((f"{prefix}.sparse_initializer[{index}].values", sparse.values))
        tensors.append((f"{prefix}.sparse_initializer[{index}].indices", sparse.indices))
    for node_index, node in enumerate(graph.node):
        node_label = node.name or f"{node.op_type}:{node_index}"
        for attribute in node.attribute:
            label = f"{prefix}.node[{node_label}].{attribute.name}"
            if attribute.type == onnx.AttributeProto.TENSOR:
                tensors.append((label, attribute.t))
            elif attribute.type == onnx.AttributeProto.TENSORS:
                tensors.extend((f"{label}[{index}]", tensor) for index, tensor in enumerate(attribute.tensors))
            elif attribute.type == onnx.AttributeProto.GRAPH:
                tensors.extend(_graph_tensors(attribute.g, label))
            elif attribute.type == onnx.AttributeProto.GRAPHS:
                for index, nested in enumerate(attribute.graphs):
                    tensors.extend(_graph_tensors(nested, f"{label}[{index}]"))
    return tensors


def _graph_nodes(graph: Any) -> list[Any]:
    nodes: list[Any] = []
    for node in graph.node:
        nodes.append(node)
        for attribute in node.attribute:
            if attribute.type == onnx.AttributeProto.GRAPH:
                nodes.extend(_graph_nodes(attribute.g))
            elif attribute.type == onnx.AttributeProto.GRAPHS:
                for nested in attribute.graphs:
                    nodes.extend(_graph_nodes(nested))
    return nodes


def _graph_values(graph: Any) -> list[Any]:
    values = [*graph.input, *graph.output, *graph.value_info]
    for node in graph.node:
        for attribute in node.attribute:
            if attribute.type == onnx.AttributeProto.GRAPH:
                values.extend(_graph_values(attribute.g))
            elif attribute.type == onnx.AttributeProto.GRAPHS:
                for nested in attribute.graphs:
                    values.extend(_graph_values(nested))
    return values


def _external_tensor_size(tensor_label: str, tensor: Any) -> int:
    element_bytes = FIXED_WIDTH_BYTES.get(tensor.data_type)
    if element_bytes is None:
        raise ValueError(
            f"external tensor type has no supported fixed-width storage: "
            f"{tensor_label}, dtype={tensor.data_type}"
        )
    dimensions = list(tensor.dims)
    if any(dimension < 0 for dimension in dimensions):
        raise ValueError(f"external tensor has a variable/negative dimension: {tensor_label}")
    size = math.prod(dimensions) * element_bytes
    if size <= 0:
        raise ValueError(f"external tensor must have positive fixed storage: {tensor_label}")
    return size


def verify_graph(graph_path: Path) -> dict[str, Any]:
    graph_path = graph_path.absolute()
    if graph_path.name != "model.onnx":
        raise ValueError("TEI graph must be named model.onnx")
    if graph_path.is_symlink() or not graph_path.is_file():
        raise ValueError("graph must be a regular non-symlink file")
    graph_path = graph_path.resolve(strict=True)
    root = graph_path.parent
    model = onnx.load(graph_path, load_external_data=False)

    opset_entries = [(entry.domain, entry.version) for entry in model.opset_import]
    if len({domain for domain, _ in opset_entries}) != len(opset_entries):
        raise ValueError(f"duplicate opset domains are forbidden: {opset_entries!r}")
    opsets = dict(opset_entries)
    if opsets.get("") != 17:
        raise ValueError(f"expected default ONNX opset 17, got {opsets!r}")
    if set(opsets) - ALLOWED_DOMAINS:
        raise ValueError(f"custom opset imports are forbidden: {opsets!r}")
    if model.functions:
        raise ValueError("local model functions are forbidden in the TEI graph")
    all_nodes = _graph_nodes(model.graph)
    bad_domains = sorted({node.domain for node in all_nodes} - ALLOWED_DOMAINS)
    if bad_domains:
        raise ValueError(f"custom operator domains are forbidden: {bad_domains}")
    for node in all_nodes:
        for attribute in node.attribute:
            if attribute.name == "dtype":
                if attribute.type != onnx.AttributeProto.INT:
                    raise ValueError(f"operator dtype attribute must be an integer: {node.op_type}")
                if attribute.i in FORBIDDEN_FLOAT_TYPES:
                    raise ValueError(
                        f"operator-generated non-fp32 floating type is forbidden: "
                        f"{node.op_type}, dtype={attribute.i}"
                    )
        if node.op_type == "CastLike":
            raise ValueError("CastLike is forbidden because its internal output precision is implicit")
        if node.op_type != "Cast":
            continue
        to_values = [attribute.i for attribute in node.attribute if attribute.name == "to"]
        if len(to_values) != 1:
            raise ValueError("Cast node must declare exactly one target type")
        if to_values[0] in FORBIDDEN_FLOAT_TYPES:
            raise ValueError(f"Cast to non-fp32 floating type is forbidden: {to_values[0]}")
    for value in _graph_values(model.graph):
        if value.type.HasField("tensor_type") and value.type.tensor_type.elem_type in FORBIDDEN_FLOAT_TYPES:
            raise ValueError(f"non-fp32 internal tensor type is forbidden: {value.name}")
    inferred_model = onnx.shape_inference.infer_shapes(
        model, check_type=True, strict_mode=False, data_prop=False
    )
    for value in _graph_values(inferred_model.graph):
        if value.type.HasField("tensor_type") and value.type.tensor_type.elem_type in FORBIDDEN_FLOAT_TYPES:
            raise ValueError(f"inferred non-fp32 internal tensor type is forbidden: {value.name}")

    inputs = list(model.graph.input)
    outputs = list(model.graph.output)
    if [value.name for value in inputs] != ["input_ids", "attention_mask"]:
        raise ValueError(f"unexpected graph inputs: {[value.name for value in inputs]}")
    for value in inputs:
        if value.type.tensor_type.elem_type != TensorProto.INT64 or _shape(value) != ["batch", "sequence"]:
            raise ValueError(f"invalid input ABI for {value.name}: type={value.type.tensor_type.elem_type}, shape={_shape(value)}")
    if len(outputs) != 1 or outputs[0].name != "last_hidden_state":
        raise ValueError(f"unexpected graph outputs: {[value.name for value in outputs]}")
    output = outputs[0]
    if output.type.tensor_type.elem_type != TensorProto.FLOAT:
        raise ValueError("last_hidden_state must be float32")
    if _shape(output) != ["batch", "sequence", 1536]:
        raise ValueError(f"invalid last_hidden_state shape: {_shape(output)}")

    references: dict[str, list[dict[str, Any]]] = defaultdict(list)
    all_tensors = _graph_tensors(model.graph)
    for tensor_label, tensor in all_tensors:
        if tensor.data_type in FORBIDDEN_FLOAT_TYPES:
            raise ValueError(f"non-fp32 floating tensor: {tensor_label}")
        if not uses_external_data(tensor):
            continue
        entries = [(entry.key, entry.value) for entry in tensor.external_data]
        keys = [key for key, _ in entries]
        if len(keys) != len(set(keys)):
            raise ValueError(f"duplicate external-data keys for {tensor_label}: {keys}")
        fields = dict(entries)
        if set(fields) - {"location", "offset", "length"}:
            raise ValueError(f"unknown external-data fields for {tensor_label}: {fields}")
        location = fields.get("location", "")
        path = _safe_external_path(root, location)
        expected_length = _external_tensor_size(tensor_label, tensor)
        try:
            offset = int(fields.get("offset", "0"))
            length = int(fields.get("length", str(path.stat().st_size - offset)))
        except ValueError as error:
            raise ValueError(f"external range missing/invalid for {tensor_label}: {fields}") from error
        if offset < 0 or length <= 0 or offset + length > path.stat().st_size:
            raise ValueError(f"external range outside file for {tensor_label}: offset={offset}, length={length}")
        if length != expected_length:
            raise ValueError(
                f"external range length does not match tensor dtype/dimensions for {tensor_label}: "
                f"length={length}, expected={expected_length}"
            )
        references[location].append(
            {
                "tensor": tensor.name or tensor_label,
                "offset": offset,
                "length": length,
                "offset_declared": "offset" in fields,
                "length_declared": "length" in fields,
            }
        )
    if not references:
        raise ValueError("the 1.5B fp32 graph must use ONNX external data")

    for location, ranges in references.items():
        ordered = sorted(ranges, key=lambda value: (value["offset"], value["length"], value["tensor"]))
        for left, right in zip(ordered, ordered[1:]):
            left_range = (left["offset"], left["offset"] + left["length"])
            right_range = (right["offset"], right["offset"] + right["length"])
            if left_range[1] > right_range[0] and left_range != right_range:
                raise ValueError(f"partially overlapping external ranges in {location}: {left} and {right}")

    referenced_files = set(references)
    data_files = {
        path.name
        for path in root.iterdir()
        if path.name not in {"model.onnx", "export-metadata.json", "graph-report.json"}
    }
    if data_files != referenced_files:
        raise ValueError(
            f"external-data inventory mismatch: missing={sorted(referenced_files-data_files)}, extra={sorted(data_files-referenced_files)}"
        )

    onnx.checker.check_model(str(graph_path), full_check=False)

    files = []
    for name in ["model.onnx", *sorted(referenced_files)]:
        path = root / name
        files.append({"path": name, "size": path.stat().st_size, "sha256": sha256_file(path)})
    return {
        "schema_version": 1,
        "graph": {
            "ir_version": model.ir_version,
            "opsets": opsets,
            "inputs": [
                {"name": value.name, "dtype": "int64", "shape": _shape(value)} for value in inputs
            ],
            "outputs": [{"name": output.name, "dtype": "float32", "shape": _shape(output)}],
            "operator_counts": dict(sorted(Counter(node.op_type for node in all_nodes).items())),
            "operator_domains": sorted({node.domain for node in all_nodes}),
            "initializer_count": len(model.graph.initializer),
            "external_tensor_count": sum(len(values) for values in references.values()),
        },
        "external_data": {
            location: sorted(values, key=lambda value: (value["offset"], value["tensor"]))
            for location, values in sorted(references.items())
        },
        "files": files,
    }


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--graph", type=Path, required=True)
    parser.add_argument("--report", type=Path, required=True)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    write_json(args.report, verify_graph(args.graph))


if __name__ == "__main__":
    main()
