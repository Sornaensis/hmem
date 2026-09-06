from __future__ import annotations

import json
import shutil
import tempfile
from pathlib import Path
from typing import Callable

import numpy as np
import onnx
from onnx import TensorProto, helper, numpy_helper

from verify_graph import verify_graph


def make_fixture(root: Path) -> Path:
    input_ids = helper.make_tensor_value_info("input_ids", TensorProto.INT64, ["batch", "sequence"])
    attention_mask = helper.make_tensor_value_info("attention_mask", TensorProto.INT64, ["batch", "sequence"])
    output = helper.make_tensor_value_info(
        "last_hidden_state", TensorProto.FLOAT, ["batch", "sequence", 1536]
    )
    weight = numpy_helper.from_array(np.zeros((1, 1, 1536), dtype=np.float32), name="weight")
    graph = helper.make_graph(
        [helper.make_node("Identity", ["weight"], ["last_hidden_state"])],
        "external-data-fixture",
        [input_ids, attention_mask],
        [output],
        [weight],
    )
    model = helper.make_model(graph, opset_imports=[helper.make_opsetid("", 17)])
    graph_path = root / "model.onnx"
    onnx.save_model(
        model,
        graph_path,
        save_as_external_data=True,
        all_tensors_to_one_file=True,
        location="weights.bin",
        size_threshold=0,
    )
    return graph_path


def expect_rejection(label: str, mutation: Callable[[Path, Path], None]) -> dict[str, str]:
    with tempfile.TemporaryDirectory() as temporary:
        root = Path(temporary)
        graph = make_fixture(root)
        mutation(root, graph)
        try:
            verify_graph(graph)
        except Exception as error:  # The checker and the policy validator have distinct exception types.
            return {"case": label, "result": "rejected", "error_type": type(error).__name__}
        raise AssertionError(f"negative fixture was accepted: {label}")


def expect_acceptance(label: str, mutation: Callable[[Path, Path], None]) -> dict[str, str]:
    with tempfile.TemporaryDirectory() as temporary:
        root = Path(temporary)
        graph = make_fixture(root)
        mutation(root, graph)
        verify_graph(graph)
        return {"case": label, "result": "accepted"}


def set_external_field(graph: Path, key: str, value: str | None) -> None:
    model = onnx.load(graph, load_external_data=False)
    tensor = model.graph.initializer[0]
    retained = [(entry.key, entry.value) for entry in tensor.external_data if entry.key != key]
    tensor.ClearField("external_data")
    for entry_key, entry_value in retained:
        entry = tensor.external_data.add()
        entry.key = entry_key
        entry.value = entry_value
    if value is not None:
        entry = tensor.external_data.add()
        entry.key = key
        entry.value = value
    graph.write_bytes(model.SerializeToString())


def rewrite_location(_: Path, graph: Path) -> None:
    model = onnx.load(graph, load_external_data=False)
    for entry in model.graph.initializer[0].external_data:
        if entry.key == "location":
            entry.value = "../escape.bin"
    graph.write_bytes(model.SerializeToString())


def nested_external_file(root: Path, graph: Path) -> None:
    nested = root / "nested"
    nested.mkdir()
    shutil.move(root / "weights.bin", nested / "weights.bin")
    model = onnx.load(graph, load_external_data=False)
    for entry in model.graph.initializer[0].external_data:
        if entry.key == "location":
            entry.value = "nested/weights.bin"
    graph.write_bytes(model.SerializeToString())


def duplicate_external_key(_: Path, graph: Path) -> None:
    model = onnx.load(graph, load_external_data=False)
    entry = model.graph.initializer[0].external_data.add()
    entry.key = "location"
    entry.value = "weights.bin"
    graph.write_bytes(model.SerializeToString())


def add_ignored_basepath(_: Path, graph: Path) -> None:
    model = onnx.load(graph, load_external_data=False)
    entry = model.graph.initializer[0].external_data.add()
    entry.key = "basepath"
    entry.value = "/ignored"
    graph.write_bytes(model.SerializeToString())


def custom_opset_import(_: Path, graph: Path) -> None:
    model = onnx.load(graph, load_external_data=False)
    model.opset_import.append(helper.make_opsetid("unsafe.example", 1))
    graph.write_bytes(model.SerializeToString())


def duplicate_default_opset(_: Path, graph: Path) -> None:
    model = onnx.load(graph, load_external_data=False)
    model.opset_import.append(helper.make_opsetid("", 17))
    graph.write_bytes(model.SerializeToString())


def add_extra(root: Path, _: Path) -> None:
    (root / "unlisted.bin").write_bytes(b"unlisted")


def remove_external(root: Path, _: Path) -> None:
    (root / "weights.bin").unlink()


def short_external_length(_: Path, graph: Path) -> None:
    set_external_field(graph, "length", "1")


def long_external_length(root: Path, graph: Path) -> None:
    with (root / "weights.bin").open("ab") as handle:
        handle.write(b"x")
    set_external_field(graph, "length", "6145")


def omit_exact_external_length(_: Path, graph: Path) -> None:
    set_external_field(graph, "length", None)


def omit_oversized_external_length(root: Path, graph: Path) -> None:
    with (root / "weights.bin").open("ab") as handle:
        handle.write(b"x")
    set_external_field(graph, "length", None)


def offset_exact_external_range(root: Path, graph: Path) -> None:
    data_path = root / "weights.bin"
    data_path.write_bytes(b"x" + data_path.read_bytes())
    set_external_field(graph, "offset", "1")


def unsupported_external_type(_: Path, graph: Path) -> None:
    model = onnx.load(graph, load_external_data=False)
    model.graph.initializer[0].data_type = TensorProto.STRING
    graph.write_bytes(model.SerializeToString())


def variable_external_dimension(_: Path, graph: Path) -> None:
    model = onnx.load(graph, load_external_data=False)
    model.graph.initializer[0].dims[0] = -1
    graph.write_bytes(model.SerializeToString())


def rename_output(_: Path, graph: Path) -> None:
    model = onnx.load(graph, load_external_data=False)
    model.graph.output[0].name = "sentence_embedding"
    graph.write_bytes(model.SerializeToString())


def nested_custom_domain(_: Path, graph: Path) -> None:
    model = onnx.load(graph, load_external_data=False)
    nested = helper.make_graph(
        [helper.make_node("Identity", ["nested-in"], ["nested-out"], domain="unsafe.example")],
        "nested",
        [],
        [],
    )
    attribute = model.graph.node[0].attribute.add()
    attribute.name = "nested_graph"
    attribute.type = onnx.AttributeProto.GRAPH
    attribute.g.CopyFrom(nested)
    graph.write_bytes(model.SerializeToString())


def cast_to_float16(_: Path, graph: Path) -> None:
    model = onnx.load(graph, load_external_data=False)
    model.graph.node.append(helper.make_node("Cast", ["input_ids"], ["unused-half"], to=TensorProto.FLOAT16))
    graph.write_bytes(model.SerializeToString())


def random_normal_float16(_: Path, graph: Path) -> None:
    model = onnx.load(graph, load_external_data=False)
    del model.graph.node[:]
    model.graph.node.extend(
        [
            helper.make_node(
                "RandomNormal", [], ["generated-half"], dtype=TensorProto.FLOAT16, shape=[1]
            ),
            helper.make_node("Cast", ["generated-half"], ["generated-float"], to=TensorProto.FLOAT),
            helper.make_node("Add", ["weight", "generated-float"], ["last_hidden_state"]),
        ]
    )
    graph.write_bytes(model.SerializeToString())


def nested_random_normal_float16(_: Path, graph: Path) -> None:
    model = onnx.load(graph, load_external_data=False)
    condition = numpy_helper.from_array(np.asarray(True, dtype=np.bool_), name="condition")
    model.graph.initializer.append(condition)
    branch_output = helper.make_tensor_value_info("branch-output", TensorProto.FLOAT16, [1])
    then_graph = helper.make_graph(
        [helper.make_node("RandomNormal", [], ["branch-output"], dtype=TensorProto.FLOAT16, shape=[1])],
        "then-half",
        [],
        [branch_output],
    )
    else_graph = helper.make_graph(
        [helper.make_node("RandomNormal", [], ["branch-output"], dtype=TensorProto.FLOAT16, shape=[1])],
        "else-half",
        [],
        [branch_output],
    )
    del model.graph.node[:]
    model.graph.node.extend(
        [
            helper.make_node("If", ["condition"], ["generated-half"], then_branch=then_graph, else_branch=else_graph),
            helper.make_node("Cast", ["generated-half"], ["generated-float"], to=TensorProto.FLOAT),
            helper.make_node("Add", ["weight", "generated-float"], ["last_hidden_state"]),
        ]
    )
    graph.write_bytes(model.SerializeToString())


def symlink_graph(root: Path, graph: Path) -> None:
    target = root / "real.onnx"
    shutil.move(graph, target)
    graph.symlink_to(target.name)


def symlink_external(root: Path, _: Path) -> None:
    original = root / "weights.bin"
    target = root / "target.bin"
    shutil.move(original, target)
    original.symlink_to(target.name)


def main() -> None:
    with tempfile.TemporaryDirectory() as temporary:
        valid = verify_graph(make_fixture(Path(temporary)))
        if valid["graph"]["outputs"][0]["shape"] != ["batch", "sequence", 1536]:
            raise AssertionError("valid fixture did not preserve the TEI output ABI")
    positive_variants = [
        expect_acceptance("omitted exact external-data length", omit_exact_external_length),
        expect_acceptance("nonzero offset exact external-data range", offset_exact_external_range),
    ]
    results = [
        expect_rejection("escaping external-data path", rewrite_location),
        expect_rejection("nested external-data path", nested_external_file),
        expect_rejection("duplicate external-data key", duplicate_external_key),
        expect_rejection("ignored external-data basepath", add_ignored_basepath),
        expect_rejection("custom opset import", custom_opset_import),
        expect_rejection("duplicate default opset import", duplicate_default_opset),
        expect_rejection("unlisted support file", add_extra),
        expect_rejection("missing external-data file", remove_external),
        expect_rejection("short external-data length", short_external_length),
        expect_rejection("long external-data length within file", long_external_length),
        expect_rejection("omitted oversized external-data length", omit_oversized_external_length),
        expect_rejection("unsupported variable-width external tensor", unsupported_external_type),
        expect_rejection("variable external tensor dimension", variable_external_dimension),
        expect_rejection("prepooled output name", rename_output),
        expect_rejection("nested custom operator domain", nested_custom_domain),
        expect_rejection("internal float16 cast", cast_to_float16),
        expect_rejection("operator-generated float16", random_normal_float16),
        expect_rejection("nested operator-generated float16", nested_random_normal_float16),
        expect_rejection("symlink external-data file", symlink_external),
        expect_rejection("symlink graph", symlink_graph),
    ]
    print(
        json.dumps(
            {
                "valid_fixture": "accepted",
                "positive_variants": positive_variants,
                "negative_fixtures": results,
            },
            sort_keys=True,
        )
    )


if __name__ == "__main__":
    main()
