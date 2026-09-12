from __future__ import annotations

import argparse
import hashlib
import importlib.util
import importlib.metadata
import inspect
import json
import math
import os
import platform
import subprocess
import sys
import time
from pathlib import Path, PurePosixPath
from typing import Any


TASK_ID = "aa30a81c-2adf-4f54-b1eb-cfeaa39bac77"
MODEL_ID = "Alibaba-NLP/gte-Qwen2-1.5B-instruct"
MODEL_REVISION = "1cad2ab3ff41c2671f34e135d29831368ee26b68"
MODEL_DIMENSIONS = 1536
EOS_TOKEN_ID = 151643
MAX_PROBE_TOKENS = 2048
QUERY_PREFIX = "Instruct: Given a web search query, retrieve relevant passages that answer the query\nQuery: "
REFERENCE_METHOD = "original-sdpa-math-cuda-f16-v3"
MODELING_QWEN_SHA256 = "8851d692b05bbf3b06a9ada6c0c9c857df6461f2a2b093e7fa831c1078040602"
ORIGINAL_MODEL_MODULE_NAME = f"_hmem_reference_modeling_qwen_{MODELING_QWEN_SHA256[:16]}"
TOKENIZATION_QWEN_SHA256 = "e15b8ea81d39b3dda3acf751b4a00a7dabbf9f687e026608c06b3849925e3169"
CONFIG_SHA256 = "6be8440493f63c39e989842d4747127a71b1d680839af44dc105245e06a333d1"
MAX_INPUT_BYTES = 16 * 1024 * 1024
MAX_OUTPUT_BYTES = 4 * 1024 * 1024
EXPECTED_ATTENTION_LAYERS = 28
EXPECTED_ROTARY_CACHE_LENGTH = 131072
EXPECTED_ROTARY_BUFFER_SHAPES = {
    "inv_freq": (64,),
    "cos_cached": (131072, 128),
    "sin_cached": (131072, 128),
}
MATH_ATTENTION_OPERATOR = "aten::_scaled_dot_product_attention_math"
SDPA_DISPATCH_OPERATOR = "aten::scaled_dot_product_attention"
ATTENTION_IMPLEMENTATION_PREFIX = "aten::_scaled_dot_product_"

MODEL_ARTIFACTS = (
    (".gitattributes", "11ad7efa24975ee4b0c3c3a38ed18737f0658a5f75a0a96787b576a78a023361"),
    ("1_Pooling/config.json", "40d120b92655c16390bd52c43540ada5f74ab94991d00838df6742c0d640d35b"),
    ("README.md", "06f46ea6b3ab7eb581f1fb170e6144c94493c076232dae7a65a75bef45133fea"),
    ("added_tokens.json", "6a475432c61f8d6154d10d28c37671a36e5717daf3d15002a988968fee54a500"),
    ("config.json", CONFIG_SHA256),
    ("config_sentence_transformers.json", "89df38eb06c9f934ee613d846fd9ea88468cc99791e036b5bbc412411fbb1c95"),
    ("generation_config.json", "71e135315a5c53cfbd7418a5fa02b03dad5e59df3e28c16f9970553c157805a9"),
    ("merges.txt", "8831e4f1a044471340f7c0a83d7bd71306a5b867e95fd870f74d0c5308a904d5"),
    ("model-00001-of-00002.safetensors", "0842014813f9ecd814eac671e2577d84281cd02d74f70436583c044fac724072"),
    ("model-00002-of-00002.safetensors", "661830fd8d0426f38747a747ec37cbdc05466331790dcf7b83e12a0e000ce0cc"),
    ("model.safetensors.index.json", "6ed4fc9a5fed84fc2401db6223b1f673eb2e0c01597660d67eddf63b525fbc97"),
    ("modeling_qwen.py", MODELING_QWEN_SHA256),
    ("modules.json", "84e40c8e006c9b1d6c122e02cba9b02458120b5fb0c87b746c41e0207cf642cf"),
    ("scripts/eval_mteb.py", "2a53266b072c36d8ecb0577af67d3439b4a240250d9c870f18a57c777b52232a"),
    ("sentence_bert_config.json", "1140b92d307aec9383d897169c9f489f3a568787b80bf760f5d7cc9d25169a65"),
    ("special_tokens_map.json", "daf48284de8f4779b1dbf20963a68180002fba2a34a5da72292380c5d9fb6af2"),
    ("tokenization_qwen.py", TOKENIZATION_QWEN_SHA256),
    ("tokenizer.json", "f7c9b2dba4a296b1aa76c16a34b8225c0c118978400d4bb66bff0902d702f5b8"),
    ("tokenizer_config.json", "d1b4928d0e7e7c1881a23eb235f6081d6d9db3d67f6ce5272f571feaf12fd944"),
    ("vocab.json", "ca10d7e9fb3ed18575dd1e277a2579c16d108e32f27439684afa0e10b1440910"),
)

PACKAGE_VERSIONS = {
    "certifi": "2025.4.26", "charset-normalizer": "3.4.2", "filelock": "3.18.0",
    "fsspec": "2025.5.1", "huggingface-hub": "0.23.4", "idna": "3.10",
    "jinja2": "3.1.6", "markupsafe": "3.0.2", "mpmath": "1.3.0", "networkx": "3.5",
    "numpy": "1.26.4", "nvidia-cublas-cu12": "12.8.3.14",
    "nvidia-cuda-cupti-cu12": "12.8.57", "nvidia-cuda-nvrtc-cu12": "12.8.61",
    "nvidia-cuda-runtime-cu12": "12.8.57", "nvidia-cudnn-cu12": "9.7.1.26",
    "nvidia-cufft-cu12": "11.3.3.41", "nvidia-cufile-cu12": "1.13.0.11",
    "nvidia-curand-cu12": "10.3.9.55", "nvidia-cusolver-cu12": "11.7.2.55",
    "nvidia-cusparse-cu12": "12.5.7.53", "nvidia-cusparselt-cu12": "0.6.3",
    "nvidia-nccl-cu12": "2.26.2", "nvidia-nvjitlink-cu12": "12.8.61",
    "nvidia-nvtx-cu12": "12.8.55", "packaging": "24.2", "pyyaml": "6.0.2",
    "regex": "2024.11.6", "requests": "2.32.4", "safetensors": "0.4.3",
    "setuptools": "80.9.0", "sympy": "1.14.0", "tokenizers": "0.19.1",
    "torch": "2.7.1+cu128", "tqdm": "4.67.1", "transformers": "4.41.2",
    "triton": "3.3.1", "typing-extensions": "4.14.0", "urllib3": "2.4.0",
}


class ReferenceError(ValueError):
    pass


def require(condition: bool, message: str) -> None:
    if not condition:
        raise ReferenceError(message)


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        while chunk := handle.read(4 * 1024 * 1024):
            digest.update(chunk)
    return digest.hexdigest()


def read_json(path: Path, maximum_bytes: int = MAX_INPUT_BYTES) -> Any:
    size = path.stat().st_size
    require(0 < size <= maximum_bytes, f"JSON size is outside bounds: {size}")
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError) as exc:
        raise ReferenceError(f"cannot read JSON {path}: {exc}") from exc


def validate_model_source(source: Path) -> list[dict[str, Any]]:
    require(source.is_dir(), f"model source is not a directory: {source}")
    expected = {name: digest for name, digest in MODEL_ARTIFACTS}
    actual = {
        item.relative_to(source).as_posix()
        for item in source.rglob("*")
        if item.is_file()
    }
    require(actual == set(expected), f"model file set drift: missing={sorted(set(expected)-actual)}, extra={sorted(actual-set(expected))}")
    inventory = []
    for name in sorted(expected):
        candidate = source / Path(*PurePosixPath(name).parts)
        observed = sha256_file(candidate)
        require(observed == expected[name], f"model checksum drift: {name}")
        inventory.append({"path": name, "bytes": candidate.stat().st_size, "sha256": observed})
    config = read_json(source / "config.json")
    require(config.get("auto_map", {}).get("AutoModel") == "modeling_qwen.Qwen2Model", "original AutoModel mapping drift")
    require(config.get("is_causal") is False, "model config must explicitly set is_causal=false")
    require(config.get("hidden_size") == MODEL_DIMENSIONS, "model hidden size drift")
    require(config.get("eos_token_id") == EOS_TOKEN_ID, "model EOS drift")
    return inventory


def validate_probes(value: Any) -> list[dict[str, Any]]:
    require(isinstance(value, dict), "probe root must be an object")
    require(value.get("schema_version") == 1, "unsupported probe schema")
    require(value.get("task_id") == TASK_ID, "probe task identity drift")
    require(value.get("model") == {"id": MODEL_ID, "revision": MODEL_REVISION}, "probe model identity drift")
    cases = value.get("cases")
    require(isinstance(cases, list) and 1 <= len(cases) <= 16, "probe cases must contain 1..16 entries")
    ids = [case.get("id") for case in cases if isinstance(case, dict)]
    require(len(ids) == len(cases) and all(isinstance(case_id, str) and case_id for case_id in ids), "probe case IDs are invalid")
    require(len(set(ids)) == len(ids), "probe case IDs must be unique")
    require(ids == sorted(ids), "probe cases must be ordered by ID")
    for case in cases:
        role = case.get("role")
        text = case.get("text")
        require(role in {"document", "query"}, f"invalid role for {case.get('id')}")
        require(isinstance(text, str), f"text is not a string for {case.get('id')}")
        encoded = text.encode("utf-8")
        require(hashlib.sha256(encoded).hexdigest() == case.get("utf8_sha256"), f"text checksum drift for {case.get('id')}")
        if "raw_text" in case:
            raw = case["raw_text"]
            require(isinstance(raw, str), f"raw_text is not a string for {case.get('id')}")
            expected_text = QUERY_PREFIX + raw if role == "query" else raw
            require(text == expected_text, f"prepared text identity drift for {case.get('id')}")
        if role == "query":
            require(text.startswith(QUERY_PREFIX), f"query prefix missing for {case.get('id')}")
            require(text.count(QUERY_PREFIX) == 1, f"query prefix count drift for {case.get('id')}")
        token_ids = case.get("token_ids")
        require(isinstance(token_ids, list) and 1 <= len(token_ids) <= MAX_PROBE_TOKENS, f"token count outside 1..{MAX_PROBE_TOKENS} for {case.get('id')}")
        require(all(isinstance(token, int) and not isinstance(token, bool) and 0 <= token < 151646 for token in token_ids), f"invalid token ID for {case.get('id')}")
        require(token_ids[-1] == EOS_TOKEN_ID and token_ids.count(EOS_TOKEN_ID) == 1, f"exactly one final EOS is required for {case.get('id')}")
    return cases


def validate_token_identity(tokenizer: Any, case: dict[str, Any]) -> None:
    encoded = tokenizer(case["text"], add_special_tokens=False, truncation=False)
    token_ids = encoded["input_ids"]
    require(isinstance(token_ids, list) and all(isinstance(token, int) for token in token_ids), f"tokenizer output is invalid for {case['id']}")
    require(EOS_TOKEN_ID not in token_ids, f"probe text encodes protocol EOS before append for {case['id']}")
    observed = token_ids + [EOS_TOKEN_ID]
    require(observed == case["token_ids"], f"token identity drift for {case['id']}")


def prepare_token_cases(value: Any, tokenizer: Any) -> list[dict[str, Any]]:
    require(isinstance(value, dict), "tokenizer input root must be an object")
    require(value.get("schema_version") == 1, "unsupported tokenizer input schema")
    require(value.get("task_id") == TASK_ID, "tokenizer input task identity drift")
    require(value.get("model") == {"id": MODEL_ID, "revision": MODEL_REVISION}, "tokenizer input model identity drift")
    inputs = value.get("cases")
    require(isinstance(inputs, list) and 1 <= len(inputs) <= 16, "tokenizer input must contain 1..16 cases")
    ids = [case.get("id") for case in inputs if isinstance(case, dict)]
    require(len(ids) == len(inputs) and all(isinstance(case_id, str) and case_id for case_id in ids), "tokenizer input case IDs are invalid")
    require(len(set(ids)) == len(ids) and ids == sorted(ids), "tokenizer input case IDs must be unique and ordered")
    prepared = []
    for case in inputs:
        case_id = case["id"]
        role = case.get("role")
        raw = case.get("raw_text")
        require(role in {"document", "query"}, f"invalid tokenizer input role for {case_id}")
        require(isinstance(raw, str), f"raw_text is not a string for {case_id}")
        text = QUERY_PREFIX + raw if role == "query" else raw
        base_ids = tokenizer(text, add_special_tokens=False, truncation=False)["input_ids"]
        require(isinstance(base_ids, list) and all(isinstance(token, int) for token in base_ids), f"invalid tokenizer result for {case_id}")
        target_tokens = case.get("target_tokens")
        if target_tokens is not None:
            require(role == "document", f"target_tokens is only valid for document cases: {case_id}")
            require(isinstance(target_tokens, int) and not isinstance(target_tokens, bool) and 2 <= target_tokens <= MAX_PROBE_TOKENS, f"invalid target_tokens for {case_id}")
            require(len(base_ids) >= target_tokens - 1, f"raw_text is too short for requested tokenizer boundary: {case_id}")
            base_ids = base_ids[: target_tokens - 1]
            text = tokenizer.decode(base_ids, clean_up_tokenization_spaces=False, skip_special_tokens=False)
            raw = text
            roundtrip = tokenizer(text, add_special_tokens=False, truncation=False)["input_ids"]
            require(roundtrip == base_ids, f"token-boundary text is not a stable tokenizer round trip: {case_id}")
        require(EOS_TOKEN_ID not in base_ids, f"prepared text contains protocol EOS for {case_id}")
        token_ids = base_ids + [EOS_TOKEN_ID]
        require(1 <= len(token_ids) <= MAX_PROBE_TOKENS, f"prepared token count outside bounds for {case_id}: {len(token_ids)}")
        prepared.append({
            "id": case_id, "role": role, "raw_text": raw, "text": text,
            "utf8_sha256": hashlib.sha256(text.encode("utf-8")).hexdigest(), "token_ids": token_ids,
        })
    validate_probes({
        "schema_version": 1, "task_id": TASK_ID,
        "model": {"id": MODEL_ID, "revision": MODEL_REVISION}, "cases": prepared,
    })
    return prepared


def last_valid_indices(attention_masks: list[list[int]]) -> list[int]:
    result = []
    for row in attention_masks:
        valid = [index for index, value in enumerate(row) if value == 1]
        require(valid and all(value in {0, 1} for value in row), "attention mask must be binary and nonempty")
        result.append(valid[-1])
    return result


def package_inventory() -> dict[str, str]:
    observed = {}
    for name, expected in sorted(PACKAGE_VERSIONS.items()):
        try:
            value = importlib.metadata.version(name)
        except importlib.metadata.PackageNotFoundError as exc:
            raise ReferenceError(f"locked distribution is missing: {name}") from exc
        require(value == expected, f"package version drift for {name}: {value} != {expected}")
        observed[name] = value
    return observed


def loaded_source_identity(value: Any, expected_hash: str, kind: str) -> dict[str, str]:
    source_path = Path(inspect.getfile(value.__class__)).resolve()
    observed = sha256_file(source_path)
    require(observed == expected_hash, f"loaded {kind} source checksum drift: {source_path}")
    return {"class": value.__class__.__name__, "module": value.__class__.__module__, "path": str(source_path), "sha256": observed}


def load_original_model_class(source: Path) -> tuple[Any, type[Any], dict[str, str]]:
    source_path = (source / "modeling_qwen.py").resolve()
    require(source_path.is_file(), f"original model source is missing: {source_path}")
    observed = sha256_file(source_path)
    require(observed == MODELING_QWEN_SHA256, f"original model source checksum drift: {source_path}")
    require(ORIGINAL_MODEL_MODULE_NAME not in sys.modules, f"owned model module name is already registered: {ORIGINAL_MODEL_MODULE_NAME}")

    spec = importlib.util.spec_from_file_location(ORIGINAL_MODEL_MODULE_NAME, source_path)
    require(spec is not None and spec.loader is not None, f"cannot construct original model module spec: {source_path}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[ORIGINAL_MODEL_MODULE_NAME] = module
    try:
        spec.loader.exec_module(module)
        model_class = getattr(module, "Qwen2Model", None)
        require(isinstance(model_class, type), "original module does not define Qwen2Model")
        require(model_class.__name__ == "Qwen2Model", f"unexpected original model class: {model_class.__name__}")
        require(model_class.__module__ == ORIGINAL_MODEL_MODULE_NAME, f"original model class module drift: {model_class.__module__}")
    except BaseException:
        if sys.modules.get(ORIGINAL_MODEL_MODULE_NAME) is module:
            del sys.modules[ORIGINAL_MODEL_MODULE_NAME]
        raise
    return module, model_class, {
        "class": model_class.__name__, "module": model_class.__module__,
        "path": str(source_path), "sha256": observed,
    }


def unload_original_model_module(module: Any) -> None:
    require(sys.modules.get(ORIGINAL_MODEL_MODULE_NAME) is module, "owned model module registration drift")
    del sys.modules[ORIGINAL_MODEL_MODULE_NAME]


def configure_sdpa_math_f16_runtime(torch_module: Any) -> dict[str, Any]:
    torch_module.set_float32_matmul_precision("highest")
    torch_module.backends.cuda.matmul.allow_tf32 = False
    torch_module.backends.cudnn.allow_tf32 = False
    torch_module.backends.cuda.matmul.allow_fp16_reduced_precision_reduction = False
    torch_module.backends.cuda.allow_fp16_bf16_reduction_math_sdp(False)

    matmul_precision = torch_module.get_float32_matmul_precision()
    cuda_matmul_allow_tf32 = torch_module.backends.cuda.matmul.allow_tf32
    cudnn_allow_tf32 = torch_module.backends.cudnn.allow_tf32
    allow_fp16_reduced_precision_reduction = (
        torch_module.backends.cuda.matmul.allow_fp16_reduced_precision_reduction
    )
    fp16_bf16_reduction_math_sdp_allowed = (
        torch_module.backends.cuda.fp16_bf16_reduction_math_sdp_allowed()
    )
    autocast_enabled = torch_module.is_autocast_enabled("cuda")
    require(matmul_precision == "highest", f"float32 matmul precision drift: {matmul_precision}")
    require(cuda_matmul_allow_tf32 is False, f"CUDA matmul TF32 remained enabled: {cuda_matmul_allow_tf32}")
    require(cudnn_allow_tf32 is False, f"cuDNN TF32 remained enabled: {cudnn_allow_tf32}")
    require(
        allow_fp16_reduced_precision_reduction is False,
        "FP16 reduced-precision GEMM reduction remained enabled",
    )
    require(
        fp16_bf16_reduction_math_sdp_allowed is False,
        "FP16/BF16 reduced-precision math SDPA reduction remained enabled",
    )
    require(autocast_enabled is False, f"CUDA autocast remained enabled: {autocast_enabled}")
    return {
        "reference_method": REFERENCE_METHOD,
        "smoke_dtype": "torch.float16",
        "float32_matmul_precision": matmul_precision,
        "cuda_matmul_allow_tf32": cuda_matmul_allow_tf32,
        "cudnn_allow_tf32": cudnn_allow_tf32,
        "allow_fp16_reduced_precision_reduction": allow_fp16_reduced_precision_reduction,
        "fp16_bf16_reduction_math_sdp_allowed": fp16_bf16_reduction_math_sdp_allowed,
        "autocast_enabled": autocast_enabled,
        "torch_compile": False,
    }


def effective_sdpa_math_flags(torch_module: Any) -> dict[str, Any]:
    flags = {
        "math_sdp_enabled": torch_module.backends.cuda.math_sdp_enabled(),
        "flash_sdp_enabled": torch_module.backends.cuda.flash_sdp_enabled(),
        "mem_efficient_sdp_enabled": torch_module.backends.cuda.mem_efficient_sdp_enabled(),
        "cudnn_sdp_enabled": torch_module.backends.cuda.cudnn_sdp_enabled(),
        "cuda_matmul_allow_tf32": torch_module.backends.cuda.matmul.allow_tf32,
        "cudnn_allow_tf32": torch_module.backends.cudnn.allow_tf32,
        "float32_matmul_precision": torch_module.get_float32_matmul_precision(),
        "allow_fp16_reduced_precision_reduction": (
            torch_module.backends.cuda.matmul.allow_fp16_reduced_precision_reduction
        ),
        "fp16_bf16_reduction_math_sdp_allowed": (
            torch_module.backends.cuda.fp16_bf16_reduction_math_sdp_allowed()
        ),
        "autocast_enabled": torch_module.is_autocast_enabled("cuda"),
    }
    expected = {
        "math_sdp_enabled": True,
        "flash_sdp_enabled": False,
        "mem_efficient_sdp_enabled": False,
        "cudnn_sdp_enabled": False,
        "cuda_matmul_allow_tf32": False,
        "cudnn_allow_tf32": False,
        "float32_matmul_precision": "highest",
        "allow_fp16_reduced_precision_reduction": False,
        "fp16_bf16_reduction_math_sdp_allowed": False,
        "autocast_enabled": False,
    }
    require(flags == expected, f"effective MATH SDPA settings drift: {flags}")
    return flags


def load_sdpa_f16_model(model_class: type[Any], source: Path, torch_module: Any) -> Any:
    model = model_class.from_pretrained(
        str(source), local_files_only=True, use_safetensors=True,
        torch_dtype=torch_module.float32, attn_implementation="sdpa",
    ).eval().to("cuda:0")
    require(model.__class__ is model_class, "loaded model is not the authenticated original Qwen2Model class")
    converted = model.half()
    require(converted is model, "model.half() did not preserve the original model object")
    return model


def inspect_sdpa_attention_modules(model: Any, original_module: Any) -> dict[str, Any]:
    expected_class = original_module.Qwen2SdpaAttention
    modules = [
        (name, module)
        for name, module in model.named_modules()
        if name.endswith(".self_attn")
    ]
    require(len(modules) == EXPECTED_ATTENTION_LAYERS, f"unexpected attention layer count: {len(modules)}")
    expected_names = [f"layers.{index}.self_attn" for index in range(EXPECTED_ATTENTION_LAYERS)]
    require([name for name, _ in modules] == expected_names, "attention module names or order drift")
    require(
        all(module.__class__ is expected_class for _, module in modules),
        "model contains a non-original Qwen2SdpaAttention implementation",
    )
    return {
        "count": len(modules),
        "class": expected_class.__name__,
        "module": expected_class.__module__,
        "names": [name for name, _ in modules],
    }


def capture_rotary_modules(
    model: Any, original_module: Any, torch_module: Any,
) -> tuple[list[dict[str, Any]], list[tuple[str, int, int, int]]]:
    rotary_class = original_module.Qwen2RotaryEmbedding
    modules = [
        (name, module)
        for name, module in model.named_modules()
        if name.endswith(".rotary_emb")
    ]
    require(len(modules) == EXPECTED_ATTENTION_LAYERS, f"unexpected rotary module count: {len(modules)}")
    expected_names = [
        f"layers.{index}.self_attn.rotary_emb"
        for index in range(EXPECTED_ATTENTION_LAYERS)
    ]
    require([name for name, _ in modules] == expected_names, "rotary module names or order drift")
    inventory = []
    identities = []
    for module_name, module in modules:
        require(module.__class__ is rotary_class, f"non-original rotary module: {module_name}")
        require(
            int(module.max_seq_len_cached) == EXPECTED_ROTARY_CACHE_LENGTH,
            f"rotary cache length drift: {module_name}: {module.max_seq_len_cached}",
        )
        buffers = dict(module.named_buffers(recurse=False))
        require(set(buffers) == {"inv_freq", "cos_cached", "sin_cached"}, f"rotary buffer set drift: {module_name}")
        buffer_inventory = []
        for buffer_name in ("inv_freq", "cos_cached", "sin_cached"):
            buffer = buffers[buffer_name]
            require(str(buffer.device) == "cuda:0", f"rotary buffer left cuda:0: {module_name}.{buffer_name}")
            require(buffer.dtype == torch_module.float16, f"rotary buffer is not F16: {module_name}.{buffer_name}")
            require(
                tuple(buffer.shape) == EXPECTED_ROTARY_BUFFER_SHAPES[buffer_name],
                f"rotary buffer shape drift: {module_name}.{buffer_name}: {tuple(buffer.shape)}",
            )
            qualified_name = f"{module_name}.{buffer_name}"
            buffer_inventory.append({
                "name": qualified_name,
                "shape": [int(value) for value in buffer.shape],
                "numel": int(buffer.numel()),
                "dtype": str(buffer.dtype),
                "device": str(buffer.device),
                "version": int(buffer._version),
            })
            identities.append((qualified_name, id(buffer), int(buffer.data_ptr()), int(buffer._version)))
        inventory.append({
            "name": module_name,
            "class": rotary_class.__name__,
            "module": rotary_class.__module__,
            "max_seq_len_cached": int(module.max_seq_len_cached),
            "buffers": buffer_inventory,
        })
    return inventory, identities


def require_rotary_unchanged(
    expected_inventory: list[dict[str, Any]],
    expected_identities: list[tuple[str, int, int, int]],
    observed: tuple[list[dict[str, Any]], list[tuple[str, int, int, int]]],
) -> None:
    inventory, identities = observed
    require(inventory == expected_inventory, "rotary cache metadata changed during reference forwards")
    require(identities == expected_identities, "rotary cache object, storage, or version changed during reference forwards")


def attention_backend_evidence(profile: Any, flags_before: dict[str, Any], flags_after: dict[str, Any]) -> dict[str, Any]:
    counts = {event.key: int(event.count) for event in profile.key_averages()}
    implementation_counts = {
        key: count
        for key, count in sorted(counts.items())
        if key.startswith(ATTENTION_IMPLEMENTATION_PREFIX)
    }
    require(
        counts.get(SDPA_DISPATCH_OPERATOR, 0) == EXPECTED_ATTENTION_LAYERS,
        f"unexpected SDPA dispatch count: {counts.get(SDPA_DISPATCH_OPERATOR, 0)}",
    )
    require(
        implementation_counts == {MATH_ATTENTION_OPERATOR: EXPECTED_ATTENTION_LAYERS},
        f"unexpected SDPA implementation operators: {implementation_counts}",
    )
    require(flags_after == flags_before, "effective MATH SDPA settings changed during forward")
    return {
        "profiler": {
            "activities": ["CPU"],
            "record_shapes": False,
            "profile_memory": False,
            "with_stack": False,
        },
        "effective_flags_before": flags_before,
        "effective_flags_after": flags_after,
        "scaled_dot_product_attention_calls": counts[SDPA_DISPATCH_OPERATOR],
        "implementation_operator_counts": implementation_counts,
    }


def profiled_sdpa_math_forward(
    model: Any,
    input_ids: Any,
    attention_mask: Any,
    position_ids: Any,
    torch_module: Any,
    sdpa_kernel: Any,
    math_backend: Any,
) -> tuple[Any, dict[str, Any]]:
    with sdpa_kernel([math_backend]):
        flags_before = effective_sdpa_math_flags(torch_module)
        with torch_module.profiler.profile(
            activities=[torch_module.profiler.ProfilerActivity.CPU],
            record_shapes=False,
            profile_memory=False,
            with_stack=False,
        ) as profile:
            output = model(
                input_ids=input_ids,
                attention_mask=attention_mask,
                position_ids=position_ids,
                is_causal=False,
                use_cache=False,
                output_attentions=False,
                return_dict=True,
            ).last_hidden_state
            torch_module.cuda.synchronize()
        flags_after = effective_sdpa_math_flags(torch_module)
    return output, attention_backend_evidence(profile, flags_before, flags_after)


def pool_and_normalize(
    output: Any, attention_mask: Any, torch_module: Any, case_id: str,
) -> tuple[Any, Any, dict[str, str]]:
    require(output.device.type == "cuda", f"model hidden activation left CUDA for {case_id}")
    require(output.dtype == torch_module.float16, f"model hidden activation is not F16 for {case_id}: {output.dtype}")
    require(bool(torch_module.isfinite(output).all().item()), f"nonfinite hidden activation for {case_id}")
    indices = last_valid_indices(attention_mask.detach().cpu().tolist())
    pooled_f16 = output[
        torch_module.arange(output.shape[0], device="cuda:0"),
        torch_module.tensor(indices, device="cuda:0"),
    ]
    require(pooled_f16.dtype == torch_module.float16, f"pooled vector is not F16 for {case_id}")
    require(bool(torch_module.isfinite(pooled_f16).all().item()), f"nonfinite pooled vector for {case_id}")
    pooled_f32 = pooled_f16.to(dtype=torch_module.float32)
    raw_norm = torch_module.linalg.vector_norm(pooled_f32, ord=2, dim=1, keepdim=True)
    require(
        bool(torch_module.isfinite(raw_norm).all().item()) and float(raw_norm.item()) > 0.0,
        f"invalid pooled norm for {case_id}",
    )
    normalized = pooled_f32 / raw_norm
    require(normalized.shape == (1, MODEL_DIMENSIONS), f"embedding shape drift for {case_id}: {tuple(normalized.shape)}")
    require(normalized.dtype == torch_module.float32, f"normalized vector is not FP32 for {case_id}: {normalized.dtype}")
    require(bool(torch_module.isfinite(normalized).all().item()), f"nonfinite normalized vector for {case_id}")
    return normalized, raw_norm, {
        "input_dtype": str(pooled_f16.dtype),
        "compute_dtype": str(pooled_f32.dtype),
        "normalized_dtype": str(normalized.dtype),
    }


def nvidia_inventory() -> dict[str, Any]:
    command = [
        "nvidia-smi", "--query-gpu=index,uuid,name,driver_version,compute_cap,memory.total",
        "--format=csv,noheader,nounits",
    ]
    completed = subprocess.run(command, capture_output=True, timeout=10, check=False)
    require(len(completed.stdout) + len(completed.stderr) <= 65536, "nvidia-smi output cap exceeded")
    require(completed.returncode == 0, f"nvidia-smi failed: {completed.stderr.decode('utf-8', errors='replace').strip()}")
    lines = completed.stdout.decode("utf-8", errors="strict").strip().splitlines()
    require(len(lines) == 1, f"expected exactly one visible CUDA device, observed {len(lines)}")
    fields = [part.strip() for part in lines[0].split(",")]
    require(len(fields) == 6, "unexpected nvidia-smi inventory shape")
    return {
        "index": int(fields[0]), "uuid": fields[1], "name": fields[2], "driver_version": fields[3],
        "compute_capability": fields[4], "memory_total_mib": int(fields[5]),
    }


def create_new_json(path: Path, value: Any) -> None:
    payload = (json.dumps(value, ensure_ascii=False, indent=2, sort_keys=True) + "\n").encode("utf-8")
    require(len(payload) <= MAX_OUTPUT_BYTES, f"output exceeds {MAX_OUTPUT_BYTES} bytes")
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("xb") as handle:
        handle.write(payload)
        handle.flush()
        os.fsync(handle.fileno())


def run_tokenizer_only(source: Path, input_path: Path) -> dict[str, Any]:
    started = time.monotonic()
    model_inventory = validate_model_source(source)
    inputs = read_json(input_path)
    distributions = package_inventory()
    import transformers
    from transformers import AutoTokenizer

    require(platform.python_version() == "3.11.9", f"Python version drift: {platform.python_version()}")
    require(transformers.__version__ == "4.41.2", f"Transformers version drift: {transformers.__version__}")
    tokenizer = AutoTokenizer.from_pretrained(
        str(source), trust_remote_code=True, local_files_only=True, use_fast=True
    )
    tokenizer_identity = loaded_source_identity(tokenizer, TOKENIZATION_QWEN_SHA256, "tokenizer")
    require(tokenizer.eos_token_id == EOS_TOKEN_ID and tokenizer.pad_token_id == EOS_TOKEN_ID, "tokenizer EOS/PAD identity drift")
    cases = prepare_token_cases(inputs, tokenizer)
    return {
        "schema_version": 1, "state": "tokenized", "task_id": TASK_ID,
        "model": {"id": MODEL_ID, "revision": MODEL_REVISION, "artifacts": model_inventory},
        "runtime": {
            "python": platform.python_version(), "transformers": transformers.__version__,
            "packages": distributions, "tokenizer_class": tokenizer_identity,
            "script_sha256": sha256_file(Path(__file__).resolve()), "model_loaded": False,
            "elapsed_seconds": time.monotonic() - started,
        },
        "input_sha256": sha256_file(input_path), "cases": cases,
    }


def run_reference(source: Path, probe_path: Path) -> dict[str, Any]:
    started = time.monotonic()
    model_inventory = validate_model_source(source)
    probes = read_json(probe_path)
    cases = validate_probes(probes)
    distributions = package_inventory()

    import torch
    import transformers
    from transformers import AutoTokenizer

    require(platform.python_version() == "3.11.9", f"Python version drift: {platform.python_version()}")
    require(torch.__version__ == "2.7.1+cu128", f"Torch version drift: {torch.__version__}")
    require(torch.version.cuda == "12.8", f"Torch CUDA version drift: {torch.version.cuda}")
    require(transformers.__version__ == "4.41.2", f"Transformers version drift: {transformers.__version__}")
    require(torch.cuda.is_available(), "CUDA is unavailable; CPU inference is forbidden")
    require(torch.cuda.device_count() == 1, f"expected exactly one visible CUDA device, observed {torch.cuda.device_count()}")
    torch.cuda.set_device(0)
    properties = torch.cuda.get_device_properties(0)
    require((properties.major, properties.minor) == (12, 0), f"unexpected compute capability: {(properties.major, properties.minor)}")
    supported_arches = torch.cuda.get_arch_list()
    require("sm_120" in supported_arches, f"Torch binary lacks sm_120 support: {supported_arches}")
    tokenizer = AutoTokenizer.from_pretrained(
        str(source), trust_remote_code=True, local_files_only=True, use_fast=True
    )
    tokenizer_identity = loaded_source_identity(tokenizer, TOKENIZATION_QWEN_SHA256, "tokenizer")
    require(tokenizer.eos_token_id == EOS_TOKEN_ID and tokenizer.pad_token_id == EOS_TOKEN_ID, "tokenizer EOS/PAD identity drift")
    for case in cases:
        validate_token_identity(tokenizer, case)

    original_module, model_class, authenticated_model_identity = load_original_model_class(source)
    try:
        with torch.autocast(device_type="cuda", enabled=False):
            precision_settings = configure_sdpa_math_f16_runtime(torch)
            smoke = torch.tensor([1.0, 2.0], dtype=torch.float16, device="cuda:0")
            require(smoke.device.type == "cuda" and smoke.dtype == torch.float16, "CUDA F16 smoke tensor identity drift")
            require(float((smoke * smoke).sum().item()) == 5.0, "CUDA F16 smoke operation failed")
            del smoke
            torch.cuda.synchronize()

            model = load_sdpa_f16_model(model_class, source, torch)
            model_identity = loaded_source_identity(model, MODELING_QWEN_SHA256, "model")
            require(model_identity == authenticated_model_identity, "loaded model class identity drift")
            require(model.config.auto_map.get("AutoModel") == "modeling_qwen.Qwen2Model", "loaded AutoModel mapping drift")
            require(model.config.is_causal is False, "loaded model config is not explicitly noncausal")
            require(model.config._attn_implementation == "sdpa", "loaded model is not using SDPA attention")
            attention_modules = inspect_sdpa_attention_modules(model, original_module)
            parameter_devices = sorted({str(parameter.device) for parameter in model.parameters()})
            parameter_dtypes = sorted({str(parameter.dtype) for parameter in model.parameters()})
            require(parameter_devices == ["cuda:0"], f"model parameters are not exclusively CUDA: {parameter_devices}")
            require(parameter_dtypes == ["torch.float16"], f"model parameters are not exclusively F16: {parameter_dtypes}")
            rotary_modules, rotary_identities = capture_rotary_modules(model, original_module, torch)

            activation_devices: set[str] = set()
            activation_dtypes: set[str] = set()
            pooling_input_dtypes: set[str] = set()
            pooling_compute_dtypes: set[str] = set()
            normalized_vector_dtypes: set[str] = set()
            effective_runtime_flags: dict[str, Any] | None = None
            results = []
            from torch.nn.attention import SDPBackend, sdpa_kernel

            with torch.inference_mode():
                for case in cases:
                    case_started = time.monotonic()
                    token_ids = case["token_ids"]
                    input_ids = torch.tensor([token_ids], dtype=torch.long, device="cuda:0")
                    attention_mask = torch.ones_like(input_ids, dtype=torch.long, device="cuda:0")
                    position_ids = attention_mask.cumsum(dim=-1) - 1
                    position_ids.masked_fill_(attention_mask == 0, 0)
                    require(
                        input_ids.dtype == attention_mask.dtype == position_ids.dtype == torch.long,
                        f"integer input dtype drift for {case['id']}",
                    )
                    output, backend_evidence = profiled_sdpa_math_forward(
                        model,
                        input_ids,
                        attention_mask,
                        position_ids,
                        torch,
                        sdpa_kernel,
                        SDPBackend.MATH,
                    )
                    flags_before = backend_evidence["effective_flags_before"]
                    if effective_runtime_flags is None:
                        effective_runtime_flags = flags_before
                    require(flags_before == effective_runtime_flags, "effective MATH SDPA flags changed between forwards")
                    activation_devices.add(str(output.device))
                    activation_dtypes.add(str(output.dtype))
                    normalized, raw_norm, pooling_dtypes = pool_and_normalize(
                        output, attention_mask, torch, case["id"]
                    )
                    pooling_input_dtypes.add(pooling_dtypes["input_dtype"])
                    pooling_compute_dtypes.add(pooling_dtypes["compute_dtype"])
                    normalized_vector_dtypes.add(pooling_dtypes["normalized_dtype"])
                    torch.cuda.synchronize()
                    vector = normalized[0].cpu().tolist()
                    norm = math.sqrt(math.fsum(coordinate * coordinate for coordinate in vector))
                    require(abs(norm - 1.0) <= 0.0001, f"serialized norm drift for {case['id']}: {norm}")
                    results.append({
                        "id": case["id"], "token_ids": token_ids, "vector": vector, "norm": norm,
                        "attention_backend_evidence": backend_evidence,
                        "elapsed_seconds": time.monotonic() - case_started,
                    })

            activation_devices = sorted(activation_devices)
            activation_dtypes = sorted(activation_dtypes)
            pooling_input_dtypes = sorted(pooling_input_dtypes)
            pooling_compute_dtypes = sorted(pooling_compute_dtypes)
            normalized_vector_dtypes = sorted(normalized_vector_dtypes)
            require(activation_devices == ["cuda:0"], f"hidden activations are not exclusively CUDA: {activation_devices}")
            require(activation_dtypes == ["torch.float16"], f"hidden activations are not exclusively F16: {activation_dtypes}")
            require(pooling_input_dtypes == ["torch.float16"], f"pooling input dtype drift: {pooling_input_dtypes}")
            require(pooling_compute_dtypes == ["torch.float32"], f"pooling compute dtype drift: {pooling_compute_dtypes}")
            require(normalized_vector_dtypes == ["torch.float32"], f"normalized vector dtype drift: {normalized_vector_dtypes}")
            require(effective_runtime_flags is not None, "no effective MATH SDPA flags were observed")
            require_rotary_unchanged(
                rotary_modules,
                rotary_identities,
                capture_rotary_modules(model, original_module, torch),
            )

        cuda_inventory = nvidia_inventory()
        require(cuda_inventory["index"] == 0 and cuda_inventory["compute_capability"] == "12.0", "nvidia-smi device identity drift")
        return {
            "schema_version": 1,
            "state": "passed",
            "runtime": {
                "python": platform.python_version(), "torch": torch.__version__, "torch_cuda": torch.version.cuda,
                "transformers": transformers.__version__, "packages": distributions,
                "device": cuda_inventory, "torch_device_name": properties.name,
                "torch_total_memory_bytes": properties.total_memory, "torch_supported_architectures": supported_arches,
                "parameter_devices": parameter_devices, "parameter_dtypes": parameter_dtypes,
                "activation_devices": activation_devices, "activation_dtypes": activation_dtypes,
                "pooling_input_dtype": pooling_input_dtypes[0],
                "pooling_compute_dtype": pooling_compute_dtypes[0],
                "normalized_vector_dtype": normalized_vector_dtypes[0],
                "attention_modules": attention_modules,
                "rotary_modules": rotary_modules,
                "attention_implementation": model.config._attn_implementation, "is_causal": False,
                "use_cache": False, "output_attentions": False,
                **precision_settings, **effective_runtime_flags,
                "model_class": model_identity, "tokenizer_class": tokenizer_identity,
                "model_loads": 1, "forwards_completed": len(results),
                "script_sha256": sha256_file(Path(__file__).resolve()), "elapsed_seconds": time.monotonic() - started,
            },
            "model": {
                "id": MODEL_ID, "revision": MODEL_REVISION, "source": str(source),
                "config_sha256": CONFIG_SHA256, "artifacts": model_inventory,
            },
            "probe_sha256": sha256_file(probe_path),
            "cases": results,
        }
    finally:
        unload_original_model_module(original_module)


def main() -> int:
    parser = argparse.ArgumentParser(description="Run the pinned independent CUDA embedding reference.")
    parser.add_argument("--source", type=Path, required=True)
    parser.add_argument("--probes", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--tokenize-only", action="store_true")
    args = parser.parse_args()
    try:
        output = args.output.resolve()
        require(not output.exists(), f"output already exists: {output}")
        report = (
            run_tokenizer_only(args.source.resolve(), args.probes.resolve())
            if args.tokenize_only
            else run_reference(args.source.resolve(), args.probes.resolve())
        )
        create_new_json(output, report)
        print(json.dumps({"state": report["state"], "output": str(output), "sha256": sha256_file(output)}, sort_keys=True))
        return 0
    except (OSError, ReferenceError, subprocess.SubprocessError) as exc:
        print(f"reference failed: {type(exc).__name__}: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
