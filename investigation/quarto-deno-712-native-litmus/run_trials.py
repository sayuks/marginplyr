"""Execute the predeclared four trials once; retain failures without retry."""
import datetime
import hashlib
import json
import pathlib
import subprocess
import time

root = pathlib.Path(__file__).resolve().parent
plan = json.loads((root / "plan.json").read_text())
for name, expected in plan["hashes"].items():
    assert hashlib.sha256((root / name).read_bytes()).hexdigest() == expected, name
assert json.loads((root / "selftest.json").read_text())["exit"] == 0
assert not (root / "runs.json").exists(), "Existing execution record; do not retry"
runs = []
for index, variant in enumerate(plan["order"], start=1):
    stem = f"trial-{index}-{variant}"
    command = [str(root / "native-litmus"), variant, str(plan["iterations_each"])]
    record = {
        "order": index,
        "variant": variant,
        "command": command,
        "started_utc": datetime.datetime.now(datetime.timezone.utc).isoformat(),
        "status": "running",
    }
    runs.append(record)
    (root / "runs.json").write_text(json.dumps(runs, indent=2) + "\n")
    started = time.monotonic()
    with (root / f"{stem}.stdout").open("w") as stdout, (root / f"{stem}.stderr").open("w") as stderr:
        try:
            process = subprocess.run(command, stdout=stdout, stderr=stderr,
                                     timeout=plan["outer_timeout_seconds_each"])
            record.update(status="completed", exit=process.returncode)
        except subprocess.TimeoutExpired:
            record.update(status="outer_timeout", exit=None)
    record["elapsed_seconds"] = time.monotonic() - started
    record["finished_utc"] = datetime.datetime.now(datetime.timezone.utc).isoformat()
    (root / "runs.json").write_text(json.dumps(runs, indent=2) + "\n")
    lines = (root / f"{stem}.stdout").read_text().splitlines()
    print(json.dumps(record), flush=True)
    print("\n".join(lines[-2:]), flush=True)
    if record["exit"] != 0:
        print((root / f"{stem}.stderr").read_text(), flush=True)
        raise SystemExit(1)
