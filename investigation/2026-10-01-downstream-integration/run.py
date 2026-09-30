"""Run fresh, unattached R processes: run.py ROOT calibration|main|load-order."""
import csv
import os
from pathlib import Path
import subprocess
import sys

root = Path(sys.argv[1]).resolve()
phase = sys.argv[2]
artifact = Path(__file__).resolve().parent
env = os.environ.copy()
env.update(R_LIBS_USER=str(root / "library"), R_LIBS_SITE="NULL",
           OMP_NUM_THREADS="1", ARROW_NUM_THREADS="1")
backends = ["local", "dtplyr", "sqlite"] if phase == "calibration" else ["local", "dtplyr", "sqlite", "duckdb", "arrow"]
if os.environ.get("DOWNSTREAM_BACKENDS"):
    backends = os.environ["DOWNSTREAM_BACKENDS"].split(",")
order = "backend-first" if phase == "load-order" else "consumer-first"
rphase = "calibration" if phase == "load-order" else phase
processes = []
for backend in backends:
    for mode in env.get("DOWNSTREAM_MODES", "direct,qualified,imports").split(","):
        tag = "-".join([rphase, mode, backend, order])
        if env.get("DOWNSTREAM_SUFFIX"):
            tag += "-" + env["DOWNSTREAM_SUFFIX"]
        command = ["Rscript", "--vanilla", str(artifact / "run-case.R"),
                   str(root), mode, backend, rphase, order, str(artifact)]
        outcome = ""
        with (root / "logs" / (tag + ".log")).open("w") as stream:
            try:
                result = subprocess.run(command, cwd=root / "cwd", env=env,
                    stdout=stream, stderr=subprocess.STDOUT, timeout=120)
                code = result.returncode
            except subprocess.TimeoutExpired:
                code = -1
                outcome = "120-second timeout"
        print(tag, code, flush=True)
        processes.append(dict(tag=tag, exit=code, detail=outcome))
batch = phase + ("-" + env["DOWNSTREAM_SUFFIX"] if env.get("DOWNSTREAM_SUFFIX") else "")
with (root / (batch + "-processes.csv")).open("w") as stream:
    writer = csv.DictWriter(stream, fieldnames=["tag", "exit", "detail"], lineterminator="\n")
    writer.writeheader(); writer.writerows(processes)
