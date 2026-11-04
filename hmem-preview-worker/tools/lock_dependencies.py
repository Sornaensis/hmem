"""Resolve release wheel hashes from PyPI for the supported Linux/Windows ABIs."""
import json
from pathlib import Path
from urllib.request import urlopen

PINS = {"av": "19.0.2", "Pillow": "13.0.0", "pypdfium2": "5.14.0", "selectolax": "1.0.2",
        "tinycss2": "1.5.1", "cssselect2": "0.10.1", "webencodings": "0.5.1"}


def main():
    lines = ["# PyPI release hashes; CPython 3.13 Linux x86_64 and 3.14 Windows x86_64."]
    for name, version in PINS.items():
        with urlopen(f"https://pypi.org/pypi/{name}/{version}/json", timeout=30) as response:
            metadata = json.load(response)
        wheels = []
        for entry in metadata["urls"]:
            filename = entry["filename"]
            universal = filename.endswith("none-any.whl")
            abi3 = "cp312-abi3-" in filename
            linux = "x86_64" in filename and "manylinux" in filename and (abi3 or "cp313-" in filename or "py3-none-" in filename)
            windows = "win_amd64" in filename and (abi3 or "cp314-" in filename or "py3-none-" in filename)
            if entry["packagetype"] == "bdist_wheel" and (universal or linux or windows):
                wheels.append(entry["digests"]["sha256"])
        if not wheels:
            raise RuntimeError(f"No supported wheel: {name}")
        lines.append(name + "==" + version + " \\")
        lines.extend("    --hash=sha256:" + sha + (" \\" if index + 1 < len(wheels) else "")
                     for index, sha in enumerate(sorted(set(wheels))))
    destination = Path(__file__).resolve().parents[1] / "requirements.lock"
    destination.write_text("\n".join(lines) + "\n", encoding="ascii", newline="\n")


if __name__ == "__main__":
    main()
