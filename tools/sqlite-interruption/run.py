"""Run reached-checkpoint SIGINT acceptance separately from controlled injection.

Usage: python3 tools/sqlite-interruption/run.py <evidence directory>
Only this supervisor's identified R worker receives a signal. Native SQLite
statement cancellation, timeouts, second interrupts and other OS/R versions
are outside this checkpoint experiment.
"""
import hashlib
import itertools
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import time

root = Path(__file__).resolve().parents[2]
evidence = Path(sys.argv[1]).resolve()
evidence.mkdir(parents=True, exist_ok=True)
snapshot = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=root, text=True).strip()
if subprocess.check_output(["git", "status", "--porcelain"], cwd=root, text=True).strip():
    raise SystemExit("Evidence requires a clean committed source snapshot")
manifest = {"snapshot": snapshot, "supervisor_pid": os.getpid(), "scripts": {}}
for path in [Path(__file__), Path(__file__).with_name("worker.R"),
             root / "tests/testthat/helper-sqlite-interruption.R"]:
    manifest["scripts"][str(path.relative_to(root))] = hashlib.sha256(path.read_bytes()).hexdigest()
(evidence / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")

cases = []
def add(checkpoint, **options):
    case = dict(checkpoint=checkpoint, mode="sigint", flag=False, schema="main",
                overwrite=True, sorted=True, expansion=False, outer=False, commit=False)
    case.update(options)
    if case not in cases:
        cases.append(case)

for flag in [False, True]:
    for checkpoint in ["before", "acquire", "drop", "create", "index", "insert",
                       "analyze", "result", "rollback", "cleanup_release", "release"]:
        add(checkpoint, flag=flag)
    for checkpoint, commit in itertools.product(
        ["acquire", "drop", "create", "index", "insert", "analyze", "result", "release"],
        [False, True]
    ):
        add(checkpoint, flag=flag, outer=True, commit=commit)
    for schema, overwrite in itertools.product(["main", "temp", "other"], [False, True]):
        for checkpoint in ["insert", "analyze", "result"]:
            add(checkpoint, flag=flag, schema=schema, overwrite=overwrite)
    for schema, overwrite, commit in itertools.product(
        ["main", "temp", "other"], [False, True], [False, True]
    ):
        add("insert", flag=flag, schema=schema, overwrite=overwrite,
            sorted=False, outer=True, commit=commit)
    for sorted_result, expansion in itertools.product([False, True], [False, True]):
        add("result", flag=flag, sorted=sorted_result, expansion=expansion)
    for mode in ["healthy", "error", "controlled"]:
        add("insert", flag=flag, mode=mode)

summaries = []
for index, case in enumerate(cases):
    directory = evidence / f"{index:03d}-{case['mode']}-{case['checkpoint']}"
    directory.mkdir()
    (directory / "case.json").write_text(json.dumps(case, indent=2) + "\n")
    with (directory / "worker.log").open("w") as log:
        worker = subprocess.Popen(["Rscript", str(Path(__file__).with_name("worker.R")),
                                   str(root), str(directory)], cwd=root, stdout=log,
                                  stderr=subprocess.STDOUT)
        try:
            delivered = None
            deadline = time.monotonic() + 45
            if case["mode"] == "sigint":
                reached = directory / "reached.json"
                while not reached.exists() and worker.poll() is None and time.monotonic() < deadline:
                    time.sleep(0.01)
                if not reached.exists():
                    raise RuntimeError("Worker never confirmed checkpoint")
                # The worker closes the checkpoint file before awaiting resume.
                notification = json.loads(reached.read_text())
                if notification["pid"] != worker.pid or notification["checkpoint"] != case["checkpoint"]:
                    raise RuntimeError("Checkpoint worker identity mismatch")
                worker.send_signal(signal.SIGINT)
                delivered = {"pid": worker.pid, "signal": "SIGINT", "time_ns": time.time_ns()}
                (directory / "delivery.json").write_text(json.dumps(delivered, indent=2) + "\n")
                (directory / "resume").touch()
            returncode = worker.wait(timeout=max(1, deadline - time.monotonic()))
        except BaseException:
            worker.terminate()
            worker.wait(timeout=10)
            raise
    verdict = json.loads((directory / "verdict.json").read_text()) if (directory / "verdict.json").exists() else None
    row = dict(case=directory.name, configuration=case, returncode=returncode,
               delivered=delivered, verdict=verdict)
    summaries.append(row)
    (evidence / "summary.json").write_text(json.dumps(summaries, indent=2) + "\n")
    print(f"{index + 1}/{len(cases)} {directory.name}: exit {returncode}", flush=True)
    if returncode or not verdict or not verdict["passed"]:
        raise SystemExit(f"Failed: {directory / 'worker.log'}")
