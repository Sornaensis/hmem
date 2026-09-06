from __future__ import annotations

import argparse
import json
from pathlib import Path

from common import load_json, sha256_file


TASK_ID = "aa30a81c-2adf-4f54-b1eb-cfeaa39bac77"
MODEL_REVISION = "1cad2ab3ff41c2671f34e135d29831368ee26b68"
TEI_IMAGE = "sha256:ad950d30878eceb72aaf32024d26fa2b1d04a75304fa0b4776b49aa1941fea07"
THRESHOLDS_SHA256 = "3ee04181139c7a72623bd7e42f011555321e61f05b96ba52ce942f2b0a12319e"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--fixtures", type=Path, required=True)
    parser.add_argument("--artifact-root", type=Path, required=True)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    fixtures = args.fixtures.resolve(strict=True)
    artifact_root = args.artifact_root.resolve(strict=True)
    report = load_json(fixtures / "report.json")
    schema = load_json(fixtures / "report.schema.json")
    candidate = load_json(fixtures / "candidate-summary.json")

    required = set(schema["required"])
    allowed = set(schema["properties"])
    if not required <= set(report) or set(report) - allowed:
        raise ValueError("report root does not match the checked schema surface")
    if report["task_id"] != TASK_ID or report["fixed_inputs"]["model_revision"] != MODEL_REVISION:
        raise ValueError("report task/model identity drift")
    if report["fixed_inputs"]["tei_image"] != TEI_IMAGE or report["verdict"] != "not_viable":
        raise ValueError("report runtime identity or measured verdict drift")
    if any(gate["status"] not in {"pass", "fail", "incomplete"} for gate in report["gates"].values()):
        raise ValueError("invalid gate status")
    if sha256_file(fixtures / "thresholds.json") != THRESHOLDS_SHA256:
        raise ValueError("frozen thresholds drift")

    license_summary = report["toolchain"]["license_audit"]
    license_path = fixtures / license_summary["path"]
    if license_path.stat().st_size != license_summary["size"] or sha256_file(license_path) != license_summary["sha256"]:
        raise ValueError("toolchain license audit drift")
    license_audit = load_json(license_path)
    if license_audit["toolchain_image"] != report["toolchain"]["image"]:
        raise ValueError("license audit toolchain image drift")
    if license_audit["base_image"] != report["toolchain"]["base"]:
        raise ValueError("license audit base image drift")
    if license_audit["requirements_sha256"] != report["toolchain"]["requirements_lock_sha256"]:
        raise ValueError("license audit requirements drift")
    if len(license_audit["locked_wheels"]) != license_summary["locked_wheel_count"]:
        raise ValueError("license audit locked-wheel count drift")
    if len(license_audit["installed_debian_packages"]) != license_summary["installed_debian_package_count"]:
        raise ValueError("license audit Debian-package count drift")
    if license_audit["redistribution_gate"] != "incomplete":
        raise ValueError("license audit unexpectedly approves redistribution")
    if len(license_audit["unresolved_redistribution_items"]) != license_summary["unresolved_item_count"]:
        raise ValueError("license audit unresolved-item count drift")

    for evidence in report["evidence_files"]:
        path = artifact_root.joinpath(*Path(evidence["path"]).parts)
        if path.stat().st_size != evidence["size"] or sha256_file(path) != evidence["sha256"]:
            raise ValueError(f"evidence drift: {evidence['path']}")

    export_a = load_json(artifact_root / "export-a" / "export-metadata.json")
    export_b = load_json(artifact_root / "export-b" / "export-metadata.json")
    if export_a["artifacts"] != export_b["artifacts"]:
        raise ValueError("clean export artifact records differ")
    graph_record = next(item for item in export_a["artifacts"] if item["path"] == "model.onnx")
    if graph_record["sha256"] != candidate["derived_serving_tree"]["model_onnx_sha256"]:
        raise ValueError("candidate graph summary drift")

    inspect = json.loads((artifact_root / "evidence" / "tei-container-inspect.json").read_text("utf-8"))[0]
    state = inspect["State"]
    host = inspect["HostConfig"]
    if not state["OOMKilled"] or state["ExitCode"] != 137:
        raise ValueError("TEI OOM result drift")
    if host["NetworkMode"] != "none" or host["Memory"] != 28 * 1024**3 or host["MemorySwap"] != 32 * 1024**3:
        raise ValueError("TEI offline/resource-bound launch drift")
    logs = (artifact_root / "evidence" / "tei-startup.jsonl").read_text("utf-8")
    if "Maximum number of tokens per request: 32768" not in logs or "Warming up model" not in logs:
        raise ValueError("TEI full-context warmup evidence missing")
    if '"message":"Ready"' in logs:
        raise ValueError("OOM-killed TEI evidence unexpectedly claims readiness")

    print(
        json.dumps(
            {
                "candidate_artifacts_identical": True,
                "evidence_files_verified": len(report["evidence_files"]),
                "gate_count": len(report["gates"]),
                "license_audit_verified": True,
                "tei_oom_verified": True,
                "verdict": report["verdict"],
            },
            sort_keys=True,
        )
    )


if __name__ == "__main__":
    main()
