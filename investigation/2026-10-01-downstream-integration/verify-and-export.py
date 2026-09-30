"""Verify preserved input state and export small durable evidence.

Usage: verify-and-export.py REPO ISOLATION_ROOT [NEW_EXPORT_DIRECTORY]
"""
import csv
import hashlib
from pathlib import Path
import shutil
import subprocess
import sys

repo, root = map(lambda p: Path(p).resolve(), sys.argv[1:3])
artifact = Path(sys.argv[3]).resolve() if len(sys.argv) > 3 else Path(__file__).resolve().parent
artifact.mkdir(parents=True, exist_ok=True)
import json
if (artifact / "preservation.json").exists():
    previous = json.loads((artifact / "preservation.json").read_text())
    if previous["isolation_root"] != str(root):
        raise RuntimeError("Use a new export directory; do not overwrite dated evidence")
hashof = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
assert subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=repo) == (root / "head.txt").read_bytes()
changed = subprocess.check_output(["git", "diff", "HEAD", "--name-only"], cwd=repo).decode().splitlines()
assert all(p.startswith("investigation/2026-10-01-downstream-integration/") or
           p == "investigation/downstream-integration-2026-10-01.md" for p in changed)
source_count = 0
for line in (root / "source-sha256.tsv").read_text().splitlines():
    rel, digest = line.split("\t")
    assert hashof(repo / rel) == digest, rel
    source_count += 1

dependencies = list(csv.DictReader((root / "dependencies-origin.tsv").open(), delimiter="\t"))
dependency_files = []
for row in dependencies:
    if row["Priority"] in ("base", "recommended"):
        continue
    origin = Path(row["LibPath"]) / row["Package"]
    clone = root / "library" / row["Package"]
    for p in sorted(origin.rglob("*")):
        if not p.is_file():
            continue
        rel = p.relative_to(origin)
        assert (clone / rel).is_file(), p
        digest = hashof(p)
        assert hashof(clone / rel) == digest, p
        dependency_files.append(dict(package=row["Package"], path=str(rel), sha256=digest))

# Read declaration evidence from the final installed fixture source.
declarations = []
for package in sorted((root / "consumers").iterdir()):
    desc = (package / "DESCRIPTION").read_text()
    declared = desc.split("Imports: ", 1)[1].splitlines()[0].split(", ")
    code = (package / "R" / "consumer.R").read_text()
    import re
    qualified = sorted(set(re.findall(r"([A-Za-z][A-Za-z0-9.]*)::", code)))
    assert set(qualified) <= set(declared), (package.name, qualified, declared)
    namespace = (package / "NAMESPACE").read_text()
    if "qualified" in package.name:
        assert "importFrom(marginplyr" not in namespace
    if "imports" in package.name:
        assert "importFrom(marginplyr" in namespace
    declarations.append(dict(package=package.name, imports=", ".join(declared),
                             qualified_packages=", ".join(qualified)))

cases = []
paths = sorted((root / "results").glob("*/cases.csv"))
paths += sorted((root / "calibration-initial" / "results").glob("*/cases.csv"))
for path in paths:
    for row in csv.DictReader(path.open()):
        row["run"] = path.parent.name
        if "calibration-initial" in str(path):
            row["run"] = "initial-" + row["run"]
        if row["status"] == "UNCLASSIFIED":
            if row["id"] == "W4-contextual" and row["backend"] in ("sqlite", "arrow"):
                row["status"] = "UNSUPPORTED SPELLING OR WORKFLOW"
            elif row["id"] == "W6-finite-typed" and row["backend"] == "sqlite":
                row["status"] = "HARNESS / ENVIRONMENT ISSUE"
            elif row["id"] in ("C03-lexical-delayed", "C05-no-early-summary") and row["backend"] == "dtplyr":
                row["status"] = "EXPECTED R / UPSTREAM BEHAVIOR"
            elif row["id"] == "C04-input-inside" and row["backend"] == "dtplyr":
                row["status"] = "CONSUMER PACKAGE ERROR"
            else:
                raise RuntimeError("Unclassified final observation: " + str(row))
        cases.append(row)
final = [r for r in cases if r["run"].endswith("-final")]
assert final and all(r["status"] == "NO VIOLATION FOUND" for r in final)

def table(name, rows):
    with (artifact / name).open("w") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]), lineterminator="\n")
        writer.writeheader(); writer.writerows(rows)

table("all-attempts.csv", cases)
table("final-cases.csv", final)
table("consumer-declarations.csv", declarations)
table("dependency-sha256.csv", dependency_files)
for name in ("dependencies-origin.tsv", "source-sha256.tsv", "head.txt", "tarball-sha256.txt", "calibration-controls.R"):
    shutil.copy2(root / name, artifact / ("dependencies.tsv" if name == "dependencies-origin.tsv" else name))
evidence = artifact / "evidence"
evidence.mkdir(exist_ok=True)
def copy_evidence(source, destination):
    # dput observations may contain external-pointer display text. They are
    # evidence, not R source to parse or execute.
    for p in source.rglob("*"):
        if p.is_file():
            rel = p.relative_to(source)
            if rel.suffix == ".R":
                rel = rel.with_suffix(".txt")
            target = destination / rel
            target.parent.mkdir(parents=True, exist_ok=True)
            raw = p.read_bytes().replace(b"\r\n", b"\n")
            if p.suffix != ".csv":
                raw = b"\n".join(line.rstrip(b" \t") for line in raw.split(b"\n"))
            if p.suffix == ".log" and raw:
                raw = raw.rstrip(b"\n") + b"\n"
            target.write_bytes(raw)

for path in sorted((root / "results").iterdir()):
    if path.is_dir():
        copy_evidence(path, evidence / path.name)
for name in ("calibration-initial",):
    copy_evidence(root / name, evidence / name)
copy_evidence(root / "logs", evidence / "logs")
for p in root.glob("*-processes.csv"):
    (evidence / p.name).write_bytes(p.read_bytes().replace(b"\r\n", b"\n"))
# This control is executable dput output; only presentation whitespace changes.
control = artifact / "calibration-controls.R"
control.write_bytes(b"\n".join(line.rstrip(b" \t") for line in control.read_bytes().split(b"\n")))
size = sum(p.stat().st_size for p in artifact.rglob("*") if p.is_file())
assert size < 10 * 1024 * 1024
summary = {"head": (root / "head.txt").read_text().strip(),
           "tracked_files_unchanged": source_count,
           "dependency_files_equal_to_read_only_origins": len(dependency_files),
           "final_cases": len(final), "all_final_cases_passed": True,
           "durable_bytes": size, "isolation_root": str(root)}
import json
(artifact / "preservation.json").write_text(json.dumps(summary, indent=2) + "\n")
print(json.dumps(summary, indent=2))
