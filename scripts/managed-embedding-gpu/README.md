# Native TEI GPU foundation

This directory prepares the GPU-only foundation for task `aa30a81c-2adf-4f54-b1eb-cfeaa39bac77`. It accepts only the immutable SM120 TEI 1.9.3 image and the original 20-file GTE-Qwen2 snapshot. It does not provide CPU inference or a runtime download fallback.

The machine-readable contract is `hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json`. `prepare_image.py` authenticates both OCI manifests and all layers, checks the pinned source archive and source compatibility files, verifies the complete original model snapshot, creates a checksum-identical read-only-ready model tree under the declared GPU artifact root, pulls the image once within the frozen byte/time/output/disk limits, and records the actual local image config without inventing labels.

Run the model-free checks with:

```text
python -m unittest discover -s scripts/managed-embedding-gpu -p "test_*.py"
```

Run preparation from the repository root with Docker Desktop available:

```text
python scripts/managed-embedding-gpu/prepare_image.py --contract hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json
```

The resulting `preparation-report-v1.json` is evidence of authenticated inputs and a locally present image. It is not evidence of CUDA access, TEI startup, or model inference.

Run one serialized startup probe:

```text
python scripts/managed-embedding-gpu/run_tei_startup.py --contract hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json --probe hmem-server/test/fixtures/embedding-gpu-viability/startup-probe-v1.json
```

The launcher uses the image's authenticated CUDA entrypoint and its native router. It binds the verified model read-only, disables networking, probes the router through its container-local loopback listener without publishing a host port, applies finite host resource and time limits, proves the container sees the selected SM120 GPU, validates `/info`, EOS tokenization, and one normalized 1536-coordinate response, then stops and removes only an ownership-authenticated container ID. Host memory limits are not VRAM limits; the launcher continuously monitors VRAM through startup and the request, preserves samples, enforces a display-memory reserve by stopping the owned container, and serializes task-owned GPU model containers.

Prepare the independent CUDA reference image from the checked-in 39-wheel lock and authenticated metadata with:

```text
python scripts/managed-embedding-gpu/prepare_reference_image.py --metadata hmem-server/test/fixtures/embedding-gpu-viability/reference-wheel-metadata-v1.json --metadata-provenance hmem-server/test/fixtures/embedding-gpu-viability/reference-wheel-size-provenance-v1.json --requirements scripts/managed-embedding-gpu/reference-requirements.lock --dockerfile scripts/managed-embedding-gpu/Dockerfile.reference-cuda --driver scripts/managed-embedding-gpu/reference_cuda.py --generation v4
```

The build downloads each exact wheel at most once, verifies its size and SHA-256, installs offline with `--require-hashes`, and records the derived image ID. The v4 reference image is `sha256:24ece9d9b0ff4710e0c9b955e3c67c520b18b9504256043af1f7a324a8a51582`. Its BuildKit configuration digest is recorded separately in the qualification fixture.

`reference_cuda.py` authenticates the original model and custom Qwen2 class, uses CUDA float16 parameters and the original half-converted rotary buffers, and forces noncausal PyTorch SDPA through the MATH backend. It verifies 28 original SDPA attention modules and exactly 28 MATH operator calls for each forward, disables other SDPA backends, TF32, autocast, and reduced-precision reductions, then performs last-valid pooling and normalization in float32. Production TEI remains native Candle CUDA float16. The frozen compact inputs are `parity-probes-v1.json`; reference inference must finish and its owned container must be absent before TEI validation starts.

Run the bounded reference supervisor first:

```text
python scripts/managed-embedding-gpu/run_reference_validation.py --contract hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json --probes hmem-server/test/fixtures/embedding-gpu-viability/parity-probes-v1.json --output D:\hmem-artifacts\aa30a81c-2adf-4f54-b1eb-cfeaa39bac77\gpu-foundation-v1\reference-output\reference-v4.json
```

The supervisor authenticates the exact derived image, selected GPU, ownership labels, host caps, and mounts; continuously enforces the VRAM reserve; and bounds controller output and total/cleanup time. A successful sibling `.execution.json` attestation is issued only after authenticated container absence. Failures also retain a sidecar with the error and cleanup outcome, but that record is not a successful attestation and cannot satisfy the TEI validation consumer. The consumer requires both the passed reference result and the successful execution attestation.

After the reference supervisor succeeds, run TEI validation against its create-new report:

```text
python scripts/managed-embedding-gpu/run_foundation_validation.py --contract hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json --probes hmem-server/test/fixtures/embedding-gpu-viability/parity-probes-v1.json --reference D:\hmem-artifacts\aa30a81c-2adf-4f54-b1eb-cfeaa39bac77\gpu-foundation-v1\reference-output\reference-v4.json
```

This final run checks exact probe token IDs, the prospective cosine/L2/coordinate/unit-norm thresholds, singleton/repeat/batch/concurrent ordering, and a wrong-prompt negative. It also executes real 32,768-token embedding, authenticates the exact 32,769-token input and TEI length diagnostic, records named timings and container/GPU measurements, and derives timeout recommendations from the frozen formula in the driver. It keeps request bodies in a read-only bind so full-context validation does not depend on command-line argument limits. Reference and TEI model processes are always serialized; neither path provides CPU inference or fallback.

The accepted reference vectors are checked in as `hmem-server/test/fixtures/embedding-gpu-viability/reference-golden-v1.json`. They bind all four token sequences to the exact reference output, execution attestation, v4 image, driver, model revision, and method. `gpu-foundation-qualification-v1.json` is the compact downstream handoff for the actual native runtime, numerical results, cleanup evidence, and measured recommendations. The complete immutable reports remain under the declared artifact root and are bound by path, byte length, and SHA-256.

On the qualified RTX 5090 host, native TEI startup took 51.03 seconds. Compact singleton requests took 0.32–0.39 seconds, the four-item batch took 0.47 seconds, and two concurrent mixed batches took 0.50 seconds. The real 32,768-token embedding took 2.08 seconds; 32,769 tokens were rejected without truncation. The recorded recommendations are 120 seconds for startup, 30 seconds for compact requests, 300 seconds for full-context requests, four items per compact batch, and two concurrent requests. Peak observed VRAM was 8,724 MiB. That measurement is not a hard VRAM limit; deployments must retain serialized model processes, continuous monitoring, a 2 GiB reserve, and fresh headroom checks.

The compact numerical qualification covers the four frozen probes through 2,048 tokens. The 32,768-token result is a practical native TEI inference and limit test, not an independent full-context reference comparison. Hmem continues to admit at most 32,767 fully formatted UTF-8 bytes. The earlier eager CUDA float32 reference, failed TEI comparison, and weight-rounding diagnostic remain recorded separately; the accepted SDPA MATH float16 method does not relabel those historical results.
