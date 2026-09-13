# Managed embedding GPU contract

The only managed production profile is
`native-tei-gte-qwen2-1.5b-instruct-cuda-sm120-f16-v1`. It serves the
unchanged `Alibaba-NLP/gte-Qwen2-1.5B-instruct` revision
`1cad2ab3ff41c2671f34e135d29831368ee26b68` with native TEI 1.9.3 Candle
CUDA `FlashQwen2Model`, compute capability 12.0, and explicit
`--dtype float16`. There is no ONNX, CPU, ORT, MKL, or provider fallback.
Disabled mode starts no embedding process and ordinary non-vector features do
not depend on this profile.

The semantic space remains
`Alibaba-NLP/gte-Qwen2-1.5B-instruct@1cad2ab3ff41c2671f34e135d29831368ee26b68:1536:attention=noncausal:v1`.
Precision and runtime identity are carried by the profile above; they do not
rename historical vectors. The original `config.json` bytes are preserved
with `torch_dtype=float32` and `is_causal=false`. Serving overrides only the
runtime dtype to float16. Pooling is the last valid token, EOS and PAD are
151643, output is 1536 finite coordinates with deterministic L2 normalization,
documents are unchanged, and the fixed query prefix is applied exactly once.
The prior CPU authority remains linked by its exact path, 133506-byte size, and
SHA-256 in the manifest, with `active_production_authority=false`; its bytes
and historical findings are not relabeled as GPU evidence.

## Immutable image and runtime boundary

The runtime image is the OCI index
`ghcr.io/huggingface/text-embeddings-inference@sha256:aedf3b34836dc57289583142adcf2b93836cda0736ac8e6ce43691b9c2c67170`.
For `linux/amd64`, the manifest is
`sha256:144aaa80ddcb520d49df83f915dc188ddd7cc6b1b3b9684a829c21dd39cbe3c5`
and the configuration is
`sha256:affa793eda6c6c6583d9c4041372dd710d74a55ab0a89025a1f322dd71b92eb2`.
The manifest lock contains every compressed layer in order. The discovery tag
`120-1.9.3` is evidence only and must never be used as the runtime reference.

The source image binds Ubuntu 24.04 and CUDA 12.9.1. The installed bundle
contains exact copies of the image's `/entrypoint.sh` and
`/usr/local/bin/text-embeddings-router`. The manifest records their source
paths, sizes, and hashes. It also records the resolved direct ELF loader
closure observed for the router. That ELF list is not a claim that CUDA
dependencies loaded dynamically or linked into the router are separately
complete; all such image content is instead bound by the exact platform
manifest, configuration, and ordered layer digests.

`libcuda.so.1` belongs to the NVIDIA container runtime and host driver. It is
not an image file and has no image checksum. Deployment must not copy host
`libcuda` into the bundle, override the loader, set `LD_PRELOAD`, inherit MKL
settings, or inject the CUDA compatibility directory on the qualified driver
CUDA 13.2 host. The selected allocation is device index 0 with
`NVIDIA_DRIVER_CAPABILITIES=compute,utility`. The qualified device is an RTX
5090 with SM 12.0 and driver 596.49. Lower drivers are outside this reviewed
contract; the image's `NVIDIA_REQUIRE_CUDA` constraint must also pass.

## Installed layout and launch

| Purpose | Absolute installed path |
| --- | --- |
| Installation root | `/opt/hmem/managed-embedding` |
| Embedded manifest | `/opt/hmem/managed-embedding/manifest/managed-embedding-provenance.yaml` |
| Original model snapshot | `/opt/hmem/managed-embedding/model` |
| Runtime bundle | `/opt/hmem/managed-embedding/tei-runtime` |
| CUDA entrypoint | `/opt/hmem/managed-embedding/tei-runtime/entrypoint.sh` |
| TEI router | `/opt/hmem/managed-embedding/tei-runtime/text-embeddings-router` |

The supervisor must clear inherited environment and construct only the loader
environment in the manifest. `PATH` begins with the runtime bundle so the
entrypoint resolves the installed router. `HF_HUB_OFFLINE=1` and
`TRANSFORMERS_OFFLINE=1` forbid serving-time retrieval. The exact argument
list is locked in the manifest: model path, float16, 32768 batch tokens, no
automatic truncation, four TEI input permits, four client batch items, two
tokenizer workers, and loopback router and metrics listeners. TEI acquires one
of those four permits for every input in a request. The four-permit and
four-item values are router ceilings. They do not by themselves establish the
number of logical HTTP requests or inputs that hmem may admit.

The launch argument sets `model_id=/opt/hmem/managed-embedding/model`, so that
installed path must be accepted when TEI preserves it in `/info`. For both
`model_id` and `served_model_name`, the complete accepted alias set is the
installed path, `/model`, and
`Alibaba-NLP/gte-Qwen2-1.5B-instruct`. No other path or name is accepted. This
retains the `/model` fixture and the upstream identity while matching the
managed launch. `model_sha=null` is accepted only together with an explicit
assertion of this profile and the independent probes. A non-null revision must
equal the locked model revision. Required metadata also includes TEI
1.9.3/source `06670157fb6c1523482219bdb2d1660277d38088`, float16, last-token
pooling, maximum input length 32768, and `auto_truncate=false`.

## Input and numerical authority

TEI accepts at most 32768 tokens. hmem retains the more conservative limit of
32767 fully formatted UTF-8 bytes and rejects over-limit input without
truncation. Requests use `normalize=true`, `truncate=false`, and no default
prompt.

The immutable numerical authority consists of
`foundation-contract-v1.json`, `parity-probes-v1.json`, and
`reference-golden-v1.json`, with their exact sizes and hashes in the
manifest. The reference method is
`original-sdpa-math-cuda-f16-v3`. The fixed cases are `doc_short`,
`mixed_long`, `multilingual`, and `query_search`; their roles, formatted
UTF-8 hashes, and token counts are locked. Acceptance requires cosine at least
0.9995, L2 distance at most 0.032, maximum coordinate error at most 0.005,
1536 finite coordinates, and unit-norm error at most 0.0001. The wrong-prompt
negative must fail.

Measured operational recommendations are 120 seconds for startup, 30 seconds
for compact requests, and 300 seconds for full-context requests. Evidence
covers one four-item compact batch and two simultaneous mixed two-item
requests whose inputs are at most 2048 tokens. The hmem controller may admit
at most two logical compact HTTP requests and four aggregate inputs: either
one four-input request or two requests of two inputs each. It must not admit
two four-input requests together. Requests above that qualified compact range
must use singleton admission, and hmem must keep the aggregate token budget at
or below TEI's 32768-token batch ceiling. Four maximum-length requests and
concurrent full-context requests are unqualified. The larger limits in the
foundation contract describe the historical trial envelope and are not
production defaults.

## Offline verification and redistribution

`scripts/check-managed-embedding-provenance.hs` embeds the trusted schema,
profile, image, model, runtime, numerical, and license locks. The installed
manifest cannot authorize different bytes by changing its own hashes. Image
preparation must run the checker against both the complete 20-file original
model tree and the two-file runtime bundle. It streams large-file hashing and
rejects missing, extra, duplicate, unsafe, symlinked, non-regular, wrong-size,
or wrong-hash files.

The TEI and model Apache notices remain separate from the NVIDIA Deep Learning
Container License copied from `/NGC-DL-CONTAINER-LICENSE` in the exact pinned
image. Apache notices do not grant rights for the NVIDIA CUDA runtime. A
packaged GPU image must ship every notice recorded in the manifest and comply
with each corresponding license.
