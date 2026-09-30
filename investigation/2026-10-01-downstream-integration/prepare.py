"""Reproduce the installed fixtures without changing the source repository.

Usage: python3 prepare.py REPO ISOLATION_ROOT
Requires the recorded dependency versions in the current R installation.
"""
import csv
import hashlib
import os
from pathlib import Path
import shutil
import subprocess
import sys

repo, root = map(lambda p: Path(p).resolve(), sys.argv[1:3])
artifact = Path(__file__).resolve().parent
recorded_head = artifact / "head.txt"
if recorded_head.exists():
    actual_head = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=repo)
    if actual_head != recorded_head.read_bytes():
        raise RuntimeError("Use the recorded marginplyr commit for this reproduction")
recorded_source = artifact / "source-sha256.tsv"
if recorded_source.exists():
    for line in recorded_source.read_text().splitlines():
        rel, digest = line.split("\t")
        if hashlib.sha256((repo / rel).read_bytes()).hexdigest() != digest:
            raise RuntimeError("Source differs from recorded experiment: " + rel)
root.mkdir(parents=True, exist_ok=True)
(root / "library").mkdir(exist_ok=True)
(root / "logs").mkdir(exist_ok=True)
(root / "results").mkdir(exist_ok=True)
(root / "cwd").mkdir(exist_ok=True)
env = os.environ.copy()
env.update(R_LIBS_USER=str(root / "library"), R_LIBS_SITE="NULL",
           OMP_NUM_THREADS="1", ARROW_NUM_THREADS="1")

def run(command, name, cwd=root):
    with (root / "logs" / (name + ".log")).open("w") as out:
        subprocess.run(command, cwd=cwd, env=env, stdout=out,
                       stderr=subprocess.STDOUT, check=True, timeout=900)

origin = root / "dependencies-origin.tsv"
if not origin.exists():
    # Discovery uses the regular installation read-only, before isolation.
    code = '''ip <- installed.packages(); roots <- c("dplyr", "dbplyr",
      "rlang", "tidyselect", "tibble", "dtplyr", "data.table", "DBI",
      "RSQLite", "duckdb", "arrow"); deps <- unique(c(roots,
      unlist(tools::package_dependencies(roots, db=ip,
        which=c("Depends", "Imports", "LinkingTo"), recursive=TRUE))));
      write.table(ip[intersect(deps, rownames(ip)),
        c("Package", "Version", "LibPath", "Priority"), drop=FALSE],
        commandArgs(TRUE)[1], sep="\\t", row.names=FALSE, quote=FALSE)'''
    subprocess.run(["Rscript", "--vanilla", "-e", code, str(origin)], check=True)
with origin.open() as stream:
    origin_rows = list(csv.DictReader(stream, delimiter="\t"))
expected_file = artifact / "dependencies.tsv"
if expected_file.exists():
    with expected_file.open() as stream:
        expected = {x["Package"]: x["Version"] for x in csv.DictReader(stream, delimiter="\t")}
    actual = {x["Package"]: x["Version"] for x in origin_rows}
    if actual != expected:
        raise RuntimeError("Dependency graph differs from the recorded experiment; do not silently update it")
for row in origin_rows:
    if row["Priority"] in ("base", "recommended"):
        continue
    dest = root / "library" / row["Package"]
    if not dest.exists():
        shutil.copytree(Path(row["LibPath"]) / row["Package"], dest,
                        symlinks=False)

source = root / "source"
if not source.exists():
    source.mkdir()
    files = subprocess.check_output(["git", "ls-files", "-z"], cwd=repo).split(b"\0")
    for raw in files:
        if not raw:
            continue
        rel = Path(os.fsdecode(raw))
        if not (repo / rel).is_file():
            continue
        target = source / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(repo / rel, target)
    (root / "head.txt").write_bytes(subprocess.check_output(
        ["git", "rev-parse", "HEAD"], cwd=repo))
    (root / "start-status.txt").write_bytes(subprocess.check_output(
        ["git", "status", "--porcelain"], cwd=repo))
    (root / "start-diff.patch").write_bytes(subprocess.check_output(
        ["git", "diff", "HEAD", "--binary"], cwd=repo))
    with (root / "source-sha256.tsv").open("w") as out:
        for p in sorted(source.rglob("*")):
            if p.is_file():
                out.write(f"{p.relative_to(source)}\t{hashlib.sha256(p.read_bytes()).hexdigest()}\n")
    run(["R", "CMD", "build", "--no-build-vignettes", "--no-manual", str(source)], "build")
tarball = root / "marginplyr_0.1.0.tar.gz"
(root / "tarball-sha256.txt").write_text(hashlib.sha256(tarball.read_bytes()).hexdigest() + "\n")
if not (root / "library" / "marginplyr").exists():
    run(["R", "CMD", "INSTALL", "-l", str(root / "library"), str(tarball)], "install-marginplyr")

mp = ["grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec",
      "summarize_with_margins", "summarise_with_margins", "expand_with_margins",
      "nest_with_margins", "nest_by_with_margins", "grouping_bit", "grouping_id",
      "share_of_parent", "share_of_total", "inspect_grouping", "last_sent_queries"]
dp = ["n", "across", "collect", "compute", "summarise", "group_by"]
exports = ["make_spec", "report", "forward", "splice", "lexical", "quosure_report",
           "contextual", "selections", "verb", "inspect", "finish", "inside_report",
           "baseline", "audit", "lexical_baseline", "shadow_contextual", "fixed"]
for backend in ("local", "dtplyr", "sqlite", "duckdb", "arrow"):
    for mode in ("qualified", "imports"):
        name = "mpconsumer" + mode + backend
        package = root / "consumers" / name
        (package / "R").mkdir(parents=True, exist_ok=True)
        (package / "man").mkdir(exist_ok=True)
        deps = ["marginplyr", "dplyr", "dbplyr", "rlang", "tidyselect", "utils"]
        deps += {"local": [], "dtplyr": ["dtplyr", "data.table"],
                 "sqlite": ["DBI", "RSQLite"], "duckdb": ["DBI", "duckdb"],
                 "arrow": ["arrow"]}[backend]
        (package / "DESCRIPTION").write_text(f'''Package: {name}
Version: 0.0.1
Title: Installed Downstream Integration Fixture
Description: Minimal declared consumer for reproducible integration research.
Authors@R: person("Integration", "Fixture", email="fixture@example.org", role=c("aut", "cre"))
License: CC0
Encoding: UTF-8
Imports: {", ".join(deps)}
''')
        namespace = [f"export({x})" for x in exports]
        if mode == "imports":
            namespace += ["importFrom(marginplyr," + ",".join(mp) + ")",
                          "importFrom(dplyr," + ",".join(dp) + ")",
                          "importFrom(dbplyr,sql_render)"]
        (package / "NAMESPACE").write_text("\n".join(namespace) + "\n")
        code = (artifact / "consumer-template.R.in").read_text()
        for token, owner in (("@MP@", "marginplyr"), ("@DP@", "dplyr"), ("@DB@", "dbplyr")):
            code = code.replace(token, "" if mode == "imports" else owner + "::")
        # dtplyr is used only by that variant: other fixtures have no hidden dependency.
        if backend != "dtplyr":
            code = code.replace("  if (inside) x <- dtplyr::lazy_dt(x)",
                                '  if (inside) stop("inside input only exists in dtplyr fixture")')
        else:
            code += "\n.datatable.aware <- TRUE\n"
        (package / "R" / "consumer.R").write_text(code)
        aliases = "\n".join("\\alias{" + x + "}" for x in exports)
        (package / "man" / "consumer.Rd").write_text(
            "\\name{consumer}\n" + aliases + "\n\\title{Research fixture}\n"
            "\\description{Installed integration fixture; see the investigation scripts.}\n")
        run(["R", "CMD", "INSTALL", "-l", str(root / "library"), str(package)], "install-" + name)
print(root)
