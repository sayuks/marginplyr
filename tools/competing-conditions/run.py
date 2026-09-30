"""Compare controlled notification and reached-checkpoint SIGINT for #756.

Run with an evidence directory outside the checkout. Native-statement/timeout
cancellation, actual second SIGINT, other R/OS versions are unverified.
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
evidence.mkdir(parents=True, exist_ok=False)
snapshot = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=root, text=True).strip()
if subprocess.check_output(["git", "status", "--porcelain"], cwd=root, text=True).strip():
    raise SystemExit("Acceptance requires a clean committed source snapshot")
paths = [Path(__file__), Path(__file__).with_name("worker.R"),
         root / "tests/testthat/helper-sqlite-interruption.R"]
manifest = {"snapshot": snapshot, "supervisor_pid": os.getpid(), "scripts": {
    str(path.relative_to(root)): hashlib.sha256(path.read_bytes()).hexdigest() for path in paths
}}
(evidence / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
cases = []
for mode in ["controlled", "sigint", "healthy"]:
    for warn, warnings in itertools.product([1, 2], [False, True]):
        cases.append(dict(operation="summary", mode=mode, warn=warn, warnings=warnings))
    for flag, outer in itertools.product([False, True], [False, True]):
        for commit in ([False, True] if outer else [False]):
            cases.append(dict(operation="sqlite", mode=mode, flag=flag, outer=outer, commit=commit))
summaries = []
for index, case in enumerate(cases):
    directory = evidence / f"{index:03d}-{case['operation']}-{case['mode']}"
    directory.mkdir()
    (directory / "case.json").write_text(json.dumps(case, indent=2) + "\n")
    with (directory / "worker.log").open("w") as log:
        worker = subprocess.Popen(["Rscript", str(Path(__file__).with_name("worker.R")),
                                   str(root), str(directory)], cwd=root, stdout=log,
                                  stderr=subprocess.STDOUT)
        delivered = None
        try:
            deadline = time.monotonic() + 30
            reached = directory / "reached.json"
            while not reached.exists() and worker.poll() is None and time.monotonic() < deadline:
                time.sleep(0.01)
            if not reached.exists():
                raise RuntimeError("Worker never confirmed checkpoint")
            notification = json.loads(reached.read_text())
            if notification["pid"] != worker.pid or notification["operation"] != case["operation"]:
                raise RuntimeError("Checkpoint worker identity mismatch")
            if case["operation"] == "summary" and notification["effects"] != [2, 5]:
                raise RuntimeError("Summary effects not established before signal")
            if case["operation"] == "sqlite" and not (notification["inserted"] and notification["rolled_back"]):
                raise RuntimeError("SQLite insert/rollback not established before signal")
            if case["mode"] == "sigint":
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
    summaries.append(dict(case=directory.name, configuration=case, returncode=returncode,
                          delivered=delivered, verdict=verdict))
    (evidence / "summary.json").write_text(json.dumps(summaries, indent=2) + "\n")
    print(f"{index + 1}/{len(cases)} {directory.name}: exit {returncode}", flush=True)
    if returncode or not verdict or not verdict["passed"]:
        raise SystemExit(f"Failed: {directory / 'worker.log'}")
