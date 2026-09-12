from __future__ import annotations

import copy
import contextlib
import hashlib
import json
import sys
import types
import unittest
from pathlib import Path
from unittest.mock import MagicMock, mock_open, patch

import prepare_reference_image
import reference_cuda


ROOT = Path(__file__).resolve().parents[2]
FOUNDATION_CONTRACT = ROOT / "hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json"
LOCK_PATH = Path(__file__).resolve().parent / "reference-requirements.lock"
DOCKERFILE_PATH = Path(__file__).resolve().parent / "Dockerfile.reference-cuda"


class FakeTokenizer:
    eos_token_id = reference_cuda.EOS_TOKEN_ID
    pad_token_id = reference_cuda.EOS_TOKEN_ID

    def __call__(self, text, *, add_special_tokens, truncation):
        if add_special_tokens or truncation:
            raise AssertionError("unexpected tokenizer flags")
        return {"input_ids": list(text.encode("utf-8"))}

    def decode(self, token_ids, *, clean_up_tokenization_spaces, skip_special_tokens):
        if clean_up_tokenization_spaces or skip_special_tokens:
            raise AssertionError("unexpected decode flags")
        return bytes(token_ids).decode("utf-8")


def token_input():
    return {
        "schema_version": 1,
        "task_id": reference_cuda.TASK_ID,
        "model": {"id": reference_cuda.MODEL_ID, "revision": reference_cuda.MODEL_REVISION},
        "cases": [
            {"id": "document", "role": "document", "raw_text": "abcdef", "target_tokens": 4},
            {"id": "query", "role": "query", "raw_text": "where"},
        ],
    }


class ReferenceContractTests(unittest.TestCase):
    def test_model_lock_matches_foundation_contract_exactly(self) -> None:
        contract = json.loads(FOUNDATION_CONTRACT.read_text(encoding="utf-8"))
        expected = [(item["path"], item["sha256"]) for item in contract["model"]["artifacts"]]
        self.assertEqual(list(reference_cuda.MODEL_ARTIFACTS), expected)

    def test_package_lock_is_exact_and_complete(self) -> None:
        self.assertEqual(reference_cuda.sha256_file(LOCK_PATH), prepare_reference_image.LOCK_SHA256)
        lines = [line for line in LOCK_PATH.read_text(encoding="utf-8").splitlines() if line]
        self.assertEqual(len(lines), 39)
        self.assertEqual(len(reference_cuda.PACKAGE_VERSIONS), 39)
        for name, version in reference_cuda.PACKAGE_VERSIONS.items():
            self.assertTrue(any(line.lower().startswith(f"{name}=={version} ".lower()) for line in lines), name)

    def test_checked_in_wheel_metadata_is_complete_and_authenticated(self) -> None:
        packages = prepare_reference_image.validate_inputs(
            prepare_reference_image.DEFAULT_METADATA,
            prepare_reference_image.DEFAULT_METADATA_PROVENANCE,
            prepare_reference_image.DEFAULT_REQUIREMENTS,
        )
        self.assertEqual(len(packages), 39)
        self.assertEqual(sum(item["size"] for item in packages), prepare_reference_image.EXPECTED_WHEEL_BYTES)

    def test_dockerfile_is_pinned_offline_and_independent(self) -> None:
        value = DOCKERFILE_PATH.read_text(encoding="utf-8")
        self.assertIn(f"FROM {prepare_reference_image.BASE_IMAGE}", value)
        self.assertIn("--no-index", value)
        self.assertIn("--require-hashes", value)
        self.assertIn("--only-binary=:all:", value)
        self.assertIn('USER 65532:65532', value)
        self.assertIn('["/usr/local/bin/python3.11", "-I", "-B", "/scripts/reference_cuda.py"]', value)
        self.assertNotIn("text-embeddings-inference", value)

    def test_probe_validation_requires_order_identity_and_one_eos(self) -> None:
        prepared = reference_cuda.prepare_token_cases(token_input(), FakeTokenizer())
        manifest = {
            "schema_version": 1, "task_id": reference_cuda.TASK_ID,
            "model": {"id": reference_cuda.MODEL_ID, "revision": reference_cuda.MODEL_REVISION},
            "cases": prepared,
        }
        reference_cuda.validate_probes(manifest)
        changed = copy.deepcopy(manifest)
        changed["cases"].reverse()
        with self.assertRaisesRegex(reference_cuda.ReferenceError, "ordered"):
            reference_cuda.validate_probes(changed)
        changed = copy.deepcopy(manifest)
        changed["cases"][0]["token_ids"].insert(0, reference_cuda.EOS_TOKEN_ID)
        with self.assertRaisesRegex(reference_cuda.ReferenceError, "exactly one final EOS"):
            reference_cuda.validate_probes(changed)

    def test_tokenizer_preparation_formats_query_once_and_stably_truncates(self) -> None:
        prepared = reference_cuda.prepare_token_cases(token_input(), FakeTokenizer())
        document, query = prepared
        self.assertEqual(document["text"], "abc")
        self.assertEqual(document["raw_text"], "abc")
        self.assertEqual(document["token_ids"], [97, 98, 99, reference_cuda.EOS_TOKEN_ID])
        self.assertEqual(query["text"], reference_cuda.QUERY_PREFIX + "where")
        self.assertEqual(query["token_ids"][-1], reference_cuda.EOS_TOKEN_ID)
        self.assertEqual(query["token_ids"].count(reference_cuda.EOS_TOKEN_ID), 1)
        self.assertEqual(query["utf8_sha256"], hashlib.sha256(query["text"].encode("utf-8")).hexdigest())

    def test_last_valid_pool_index_handles_both_padding_sides(self) -> None:
        self.assertEqual(reference_cuda.last_valid_indices([[1, 1, 0], [0, 1, 1]]), [1, 2])
        with self.assertRaisesRegex(reference_cuda.ReferenceError, "binary"):
            reference_cuda.last_valid_indices([[0, 0]])

    def test_original_model_source_hash_is_checked_before_import(self) -> None:
        with (
            patch.object(Path, "is_file", return_value=True),
            patch.object(reference_cuda, "sha256_file", return_value="0" * 64),
            patch.object(reference_cuda.importlib.util, "spec_from_file_location") as make_spec,
        ):
            with self.assertRaisesRegex(reference_cuda.ReferenceError, "checksum drift"):
                reference_cuda.load_original_model_class(Path("/model"))
        make_spec.assert_not_called()

    def test_original_model_module_rejects_collision_and_cleans_failed_import(self) -> None:
        module_name = reference_cuda.ORIGINAL_MODEL_MODULE_NAME
        sentinel = types.ModuleType(module_name)
        sys.modules[module_name] = sentinel
        try:
            with (
                patch.object(Path, "is_file", return_value=True),
                patch.object(reference_cuda, "sha256_file", return_value=reference_cuda.MODELING_QWEN_SHA256),
                patch.object(reference_cuda.importlib.util, "spec_from_file_location") as make_spec,
            ):
                with self.assertRaisesRegex(reference_cuda.ReferenceError, "already registered"):
                    reference_cuda.load_original_model_class(Path("/model"))
            make_spec.assert_not_called()
        finally:
            self.assertIs(sys.modules.pop(module_name), sentinel)

        module = types.ModuleType(module_name)
        loader = MagicMock()
        loader.exec_module.side_effect = RuntimeError("import failed")
        spec = MagicMock(loader=loader)
        with (
            patch.object(Path, "is_file", return_value=True),
            patch.object(reference_cuda, "sha256_file", return_value=reference_cuda.MODELING_QWEN_SHA256),
            patch.object(reference_cuda.importlib.util, "spec_from_file_location", return_value=spec),
            patch.object(reference_cuda.importlib.util, "module_from_spec", return_value=module),
        ):
            with self.assertRaisesRegex(RuntimeError, "import failed"):
                reference_cuda.load_original_model_class(Path("/model"))
        self.assertNotIn(module_name, sys.modules)

    def test_sdpa_math_f16_runtime_settings_are_asserted_and_reported(self) -> None:
        class Matmul:
            def __init__(self, *, tf32=False, reduced=False, sticky_tf32=False, sticky_reduced=False):
                self._tf32 = tf32
                self._reduced = reduced
                self.sticky_tf32 = sticky_tf32
                self.sticky_reduced = sticky_reduced

            @property
            def allow_tf32(self):
                return self._tf32

            @allow_tf32.setter
            def allow_tf32(self, value):
                if not self.sticky_tf32:
                    self._tf32 = value

            @property
            def allow_fp16_reduced_precision_reduction(self):
                return self._reduced

            @allow_fp16_reduced_precision_reduction.setter
            def allow_fp16_reduced_precision_reduction(self, value):
                if not self.sticky_reduced:
                    self._reduced = value

        class Cudnn:
            def __init__(self, value=False, sticky=False):
                self._value = value
                self.sticky = sticky

            @property
            def allow_tf32(self):
                return self._value

            @allow_tf32.setter
            def allow_tf32(self, value):
                if not self.sticky:
                    self._value = value

        class CudaBackend:
            def __init__(
                self, *, cuda_tf32=False, cudnn_tf32=False, gemm_reduced=False,
                math_reduced=False, sticky_cuda=False, sticky_cudnn=False,
                sticky_gemm=False, sticky_math=False, math=True, flash=False,
                efficient=False, cudnn_sdp=False,
            ):
                self.matmul = Matmul(
                    tf32=cuda_tf32, reduced=gemm_reduced,
                    sticky_tf32=sticky_cuda, sticky_reduced=sticky_gemm,
                )
                self.cudnn = Cudnn(cudnn_tf32, sticky_cudnn)
                self.math_reduced = math_reduced
                self.sticky_math = sticky_math
                self.sdp = (math, flash, efficient, cudnn_sdp)

            def allow_fp16_bf16_reduction_math_sdp(self, value):
                if not self.sticky_math:
                    self.math_reduced = value

            def fp16_bf16_reduction_math_sdp_allowed(self):
                return self.math_reduced

            def math_sdp_enabled(self):
                return self.sdp[0]

            def flash_sdp_enabled(self):
                return self.sdp[1]

            def mem_efficient_sdp_enabled(self):
                return self.sdp[2]

            def cudnn_sdp_enabled(self):
                return self.sdp[3]

        class FakeTorch:
            def __init__(self, *, precision="high", ignore_precision=False, autocast=False, **cuda_args):
                self.precision = precision
                self.ignore_precision = ignore_precision
                self.autocast = autocast
                cuda = CudaBackend(**cuda_args)
                self.backends = types.SimpleNamespace(cuda=cuda, cudnn=cuda.cudnn)

            def set_float32_matmul_precision(self, value):
                if not self.ignore_precision:
                    self.precision = value

            def get_float32_matmul_precision(self):
                return self.precision

            def is_autocast_enabled(self, device_type):
                self.asserted_device_type = device_type
                return self.autocast

        torch_module = FakeTorch()
        settings = reference_cuda.configure_sdpa_math_f16_runtime(torch_module)
        self.assertEqual(settings, {
            "reference_method": "original-sdpa-math-cuda-f16-v3",
            "smoke_dtype": "torch.float16",
            "float32_matmul_precision": "highest",
            "cuda_matmul_allow_tf32": False,
            "cudnn_allow_tf32": False,
            "allow_fp16_reduced_precision_reduction": False,
            "fp16_bf16_reduction_math_sdp_allowed": False,
            "autocast_enabled": False,
            "torch_compile": False,
        })
        self.assertEqual(reference_cuda.effective_sdpa_math_flags(torch_module), {
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
        })

        failures = (
            (FakeTorch(ignore_precision=True), "matmul precision drift"),
            (FakeTorch(cuda_tf32=True, sticky_cuda=True), "CUDA matmul TF32 remained enabled"),
            (FakeTorch(cudnn_tf32=True, sticky_cudnn=True), "cuDNN TF32 remained enabled"),
            (FakeTorch(gemm_reduced=True, sticky_gemm=True), "FP16 reduced-precision GEMM"),
            (FakeTorch(math_reduced=True, sticky_math=True), "FP16/BF16 reduced-precision math SDPA"),
            (FakeTorch(autocast=True), "CUDA autocast remained enabled"),
        )
        for bad, message in failures:
            with self.subTest(message=message), self.assertRaisesRegex(reference_cuda.ReferenceError, message):
                reference_cuda.configure_sdpa_math_f16_runtime(bad)
        with self.assertRaisesRegex(reference_cuda.ReferenceError, "effective MATH SDPA settings drift"):
            bad_flags = FakeTorch(flash=True)
            reference_cuda.configure_sdpa_math_f16_runtime(bad_flags)
            reference_cuda.effective_sdpa_math_flags(bad_flags)

    def test_model_load_sequence_is_original_fp32_then_cuda_half(self) -> None:
        events = []

        class Model:
            @classmethod
            def from_pretrained(cls, source, **kwargs):
                events.append(("load", source, kwargs))
                return cls()

            def eval(self):
                events.append(("eval",))
                return self

            def to(self, device):
                events.append(("to", device))
                return self

            def half(self):
                events.append(("half",))
                return self

        torch_module = types.SimpleNamespace(float32="torch.float32")
        model = reference_cuda.load_sdpa_f16_model(Model, Path("/model"), torch_module)
        self.assertIsInstance(model, Model)
        self.assertEqual([event[0] for event in events], ["load", "eval", "to", "half"])
        self.assertEqual(events[0][2], {
            "local_files_only": True,
            "use_safetensors": True,
            "torch_dtype": "torch.float32",
            "attn_implementation": "sdpa",
        })
        self.assertEqual(events[2], ("to", "cuda:0"))

    def test_original_sdpa_classes_and_half_rotary_caches_are_exact_and_stable(self) -> None:
        class Qwen2SdpaAttention:
            pass

        class Buffer:
            next_pointer = 1000

            def __init__(self, shape):
                self.shape = shape
                self.dtype = "torch.float16"
                self.device = "cuda:0"
                self._version = 1
                self.pointer = Buffer.next_pointer
                Buffer.next_pointer += 1

            def numel(self):
                result = 1
                for value in self.shape:
                    result *= value
                return result

            def data_ptr(self):
                return self.pointer

        class Qwen2RotaryEmbedding:
            def __init__(self):
                self.max_seq_len_cached = reference_cuda.EXPECTED_ROTARY_CACHE_LENGTH
                self.buffers = {
                    "inv_freq": Buffer((64,)),
                    "cos_cached": Buffer((131072, 128)),
                    "sin_cached": Buffer((131072, 128)),
                }

            def named_buffers(self, *, recurse):
                self.recurse = recurse
                return list(self.buffers.items())

        module = types.SimpleNamespace(
            Qwen2SdpaAttention=Qwen2SdpaAttention,
            Qwen2RotaryEmbedding=Qwen2RotaryEmbedding,
        )
        attention = [
            (f"layers.{index}.self_attn", Qwen2SdpaAttention())
            for index in range(reference_cuda.EXPECTED_ATTENTION_LAYERS)
        ]
        rotary = [
            (f"layers.{index}.self_attn.rotary_emb", Qwen2RotaryEmbedding())
            for index in range(reference_cuda.EXPECTED_ATTENTION_LAYERS)
        ]
        model = types.SimpleNamespace(named_modules=lambda: attention + rotary)
        identity = reference_cuda.inspect_sdpa_attention_modules(model, module)
        self.assertEqual(identity["count"], 28)
        self.assertEqual(identity["names"], [name for name, _ in attention])
        wrong_attention = attention.copy()
        wrong_attention[0] = (wrong_attention[0][0], object())
        with self.assertRaisesRegex(reference_cuda.ReferenceError, "non-original Qwen2SdpaAttention"):
            reference_cuda.inspect_sdpa_attention_modules(
                types.SimpleNamespace(named_modules=lambda: wrong_attention + rotary), module
            )
        inventory, identities = reference_cuda.capture_rotary_modules(
            model, module, types.SimpleNamespace(float16="torch.float16")
        )
        self.assertEqual(len(inventory), 28)
        self.assertEqual(inventory[0]["buffers"][0]["shape"], [64])
        self.assertEqual(inventory[0]["buffers"][1]["shape"], [131072, 128])
        self.assertEqual(inventory[0]["buffers"][1]["numel"], 16777216)
        reference_cuda.require_rotary_unchanged(
            inventory,
            identities,
            reference_cuda.capture_rotary_modules(
                model, module, types.SimpleNamespace(float16="torch.float16")
            ),
        )
        rotary[0][1].buffers["cos_cached"]._version += 1
        with self.assertRaisesRegex(reference_cuda.ReferenceError, "rotary cache metadata changed"):
            reference_cuda.require_rotary_unchanged(
                inventory,
                identities,
                reference_cuda.capture_rotary_modules(
                    model, module, types.SimpleNamespace(float16="torch.float16")
                ),
            )
        rotary[0][1].max_seq_len_cached -= 1
        with self.assertRaisesRegex(reference_cuda.ReferenceError, "rotary cache length drift"):
            reference_cuda.capture_rotary_modules(
                model, module, types.SimpleNamespace(float16="torch.float16")
            )

    def test_profiled_forward_requires_math_only_dispatch_and_exact_noncausal_call(self) -> None:
        flags = {
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

        class Profile:
            def __enter__(self):
                return self

            def __exit__(self, *_args):
                return False

            def key_averages(self):
                return [
                    types.SimpleNamespace(key=reference_cuda.SDPA_DISPATCH_OPERATOR, count=28),
                    types.SimpleNamespace(key=reference_cuda.MATH_ATTENTION_OPERATOR, count=28),
                ]

        profile = Profile()
        profiler = types.SimpleNamespace(
            ProfilerActivity=types.SimpleNamespace(CPU="CPU"),
            profile=MagicMock(return_value=profile),
        )
        torch_module = types.SimpleNamespace(
            profiler=profiler,
            cuda=types.SimpleNamespace(synchronize=MagicMock()),
        )
        model = MagicMock(return_value=types.SimpleNamespace(last_hidden_state="hidden"))
        seen_backends = []

        def sdpa_kernel(backends):
            seen_backends.append(backends)
            return contextlib.nullcontext()

        with patch.object(reference_cuda, "effective_sdpa_math_flags", side_effect=[flags, flags]):
            output, evidence = reference_cuda.profiled_sdpa_math_forward(
                model, "ids", "mask", "positions", torch_module, sdpa_kernel, "MATH"
            )
        self.assertEqual(output, "hidden")
        self.assertEqual(seen_backends, [["MATH"]])
        model.assert_called_once_with(
            input_ids="ids", attention_mask="mask", position_ids="positions",
            is_causal=False, use_cache=False, output_attentions=False, return_dict=True,
        )
        profiler.profile.assert_called_once_with(
            activities=["CPU"], record_shapes=False, profile_memory=False, with_stack=False,
        )
        self.assertEqual(evidence["scaled_dot_product_attention_calls"], 28)
        self.assertEqual(
            evidence["implementation_operator_counts"],
            {reference_cuda.MATH_ATTENTION_OPERATOR: 28},
        )

        unexpected = Profile()
        unexpected.key_averages = lambda: profile.key_averages() + [
            types.SimpleNamespace(key="aten::_scaled_dot_product_flash_attention", count=1)
        ]
        with self.assertRaisesRegex(reference_cuda.ReferenceError, "unexpected SDPA implementation"):
            reference_cuda.attention_backend_evidence(unexpected, flags, flags)
        wrong_count = Profile()
        wrong_count.key_averages = lambda: [
            types.SimpleNamespace(key=reference_cuda.SDPA_DISPATCH_OPERATOR, count=27),
            types.SimpleNamespace(key=reference_cuda.MATH_ATTENTION_OPERATOR, count=27),
        ]
        with self.assertRaisesRegex(reference_cuda.ReferenceError, "unexpected SDPA dispatch count"):
            reference_cuda.attention_backend_evidence(wrong_count, flags, flags)

    def test_pooling_promotes_f16_last_valid_vector_before_fp32_normalization(self) -> None:
        class Scalar:
            def __init__(self, value):
                self.value = value

            def item(self):
                return self.value

        class Finite:
            def all(self):
                return Scalar(True)

        mask = MagicMock()
        mask.detach.return_value.cpu.return_value.tolist.return_value = [[1, 1]]
        output = MagicMock()
        output.device.type = "cuda"
        output.dtype = "torch.float16"
        output.shape = (1, 2, reference_cuda.MODEL_DIMENSIONS)
        pooled_f16 = MagicMock(dtype="torch.float16")
        pooled_f32 = MagicMock(dtype="torch.float32")
        normalized = MagicMock(dtype="torch.float32")
        normalized.shape = (1, reference_cuda.MODEL_DIMENSIONS)
        output.__getitem__.return_value = pooled_f16
        pooled_f16.to.return_value = pooled_f32
        raw_norm = MagicMock()
        raw_norm.item.return_value = 1.0
        pooled_f32.__truediv__.return_value = normalized
        torch_module = types.SimpleNamespace(
            float16="torch.float16",
            float32="torch.float32",
            isfinite=lambda _tensor: Finite(),
            arange=MagicMock(return_value="rows"),
            tensor=MagicMock(return_value="indices"),
            linalg=types.SimpleNamespace(vector_norm=MagicMock(return_value=raw_norm)),
        )
        observed, norm, dtypes = reference_cuda.pool_and_normalize(
            output, mask, torch_module, "mixed_long"
        )
        self.assertIs(observed, normalized)
        self.assertIs(norm, raw_norm)
        pooled_f16.to.assert_called_once_with(dtype="torch.float32")
        self.assertEqual(dtypes, {
            "input_dtype": "torch.float16",
            "compute_dtype": "torch.float32",
            "normalized_dtype": "torch.float32",
        })
        finite_checks = iter([True, False])
        torch_module.isfinite = lambda _tensor: types.SimpleNamespace(
            all=lambda: Scalar(next(finite_checks))
        )
        with self.assertRaisesRegex(reference_cuda.ReferenceError, "nonfinite pooled vector"):
            reference_cuda.pool_and_normalize(output, mask, torch_module, "mixed_long")

    def test_output_uses_exclusive_create_and_fsync(self) -> None:
        path = MagicMock(spec=Path)
        opened = mock_open()
        path.open = opened
        with patch.object(reference_cuda.os, "fsync") as fsync:
            reference_cuda.create_new_json(path, {"ok": True})
        path.parent.mkdir.assert_called_once_with(parents=True, exist_ok=True)
        opened.assert_called_once_with("xb")
        fsync.assert_called_once()

    def test_build_command_applies_explicit_buildkit_resources(self) -> None:
        command = prepare_reference_image.build_command(Path("context"), Path("iid"), Path("metadata"))
        joined = "\n".join(command)
        for expected in (
            "buildx", "--network=none", f"memory={prepare_reference_image.INSTALL_MEMORY_BYTES}",
            f"memory-swap={prepare_reference_image.INSTALL_MEMORY_SWAP_BYTES}",
            f"cpu-quota={prepare_reference_image.INSTALL_CPUS * 100000}",
            f"nproc={prepare_reference_image.BUILD_NPROC_RLIMIT}:{prepare_reference_image.BUILD_NPROC_RLIMIT}",
            "--no-cache", "--load",
        ):
            self.assertIn(expected, joined)

    def test_v4_generation_uses_fresh_root_pin_and_tag(self) -> None:
        old_root, old_pin, old_tag = prepare_reference_image.generation_outputs(prepare_reference_image.ARTIFACT_ROOT, "v2")
        self.assertEqual(old_root, prepare_reference_image.ARTIFACT_ROOT / "reference-image-v2")
        self.assertEqual(old_pin, old_root / "pinned-image-v2.json")
        self.assertEqual(old_tag, prepare_reference_image.DERIVED_TAG + "-v2")
        v3_root, v3_pin, v3_tag = prepare_reference_image.generation_outputs(
            prepare_reference_image.ARTIFACT_ROOT, "v3"
        )
        self.assertEqual(v3_root, prepare_reference_image.ARTIFACT_ROOT / "reference-image-v3")
        self.assertEqual(v3_pin, v3_root / "pinned-image-v3.json")
        self.assertEqual(v3_tag, prepare_reference_image.DERIVED_TAG + "-v3")
        root, pin, tag = prepare_reference_image.generation_outputs(
            prepare_reference_image.ARTIFACT_ROOT, "v4"
        )
        self.assertEqual(root, prepare_reference_image.ARTIFACT_ROOT / "reference-image-v4")
        self.assertEqual(pin, root / "pinned-image-v4.json")
        self.assertEqual(tag, prepare_reference_image.DERIVED_TAG + "-v4")
        command = prepare_reference_image.build_command(root, root / "image.id", root / "metadata.json", tag)
        self.assertEqual(command[command.index("--tag") + 1], tag)
        v1_id = "sha256:" + "1" * 64
        v2_id = "sha256:" + "2" * 64
        v4_id = "sha256:" + "4" * 64
        image = {
            "Id": v4_id, "Architecture": "amd64", "Os": "linux", "Size": 1024,
            "Config": {
                "User": "65532:65532",
                "Entrypoint": ["/usr/local/bin/python3.11", "-I", "-B", "/scripts/reference_cuda.py"],
                "Labels": {
                    "io.hmem.task": prepare_reference_image.TASK_ID,
                    "io.hmem.reference.kind": "independent-pytorch-cuda",
                },
            },
        }
        with patch.object(
            prepare_reference_image,
            "inspect_image",
            side_effect=lambda reference: (
                {"Id": v1_id} if reference == prepare_reference_image.DERIVED_TAG
                else {"Id": v2_id} if reference == old_tag
                else image
            ),
        ) as inspect_image:
            derived, config, labels = prepare_reference_image.authenticate_derived_image(v4_id, tag)
        inspect_image.assert_called_once_with(tag)
        record = prepare_reference_image.derived_image_record(tag, derived, config, labels)
        self.assertEqual(record["id"], v4_id)
        self.assertEqual(record["tag"], tag)
        with self.assertRaisesRegex(prepare_reference_image.PreparationError, "unsupported"):
            prepare_reference_image.generation_outputs(prepare_reference_image.ARTIFACT_ROOT, "v5")


if __name__ == "__main__":
    unittest.main()
