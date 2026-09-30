# Exception safety evidence archive

Investigated: 2026-09-30
Target HEAD: `535e7c87de1149bfcf9c90588c0952e217b33806`

This archive supported the [dated investigation](../exception-safety-recovery-2026-09-30.md). It held observations and experimental source, not production tests, a supported test runner, or a new contract. The source package excluded `investigation/`.

## Contents and transformations

- `cases.csv` held the 276 completed matrix runs, admission decisions, classifications, and relative result paths. Twenty runs were excluded; their observations remained available.
- `observations.jsonl` held all 2,094 JSON artifacts from the results directory, one object per line with `path`, `original_sha256`, and `value`. Each path corresponded to its original bundle path, including pilot notifications, immediate state, independent observations, recovery, and supervisor outcomes. The hash described the original JSON file's bytes, before normalization; it was not a hash of the normalized value.
- `summary.json` held the admitted-run counts. `final-verification.json` held the source and disposal checks at execution completion. Its 274 terminal supervisor records and 276 matrix entries were different counters: the first excluded interrupt and SIGINT pilot runs had no manager JSON record. Those runs were not admitted as evidence. Minimal reproductions and ordinary upstream controls were additional evidence, not part of the matrix counts.
- `manifest.json` held target source hashes, initial clean status, limits, and dependency identities. `dependencies.csv` recorded package versions; `loaded-environment.json` recorded the namespaces actually loaded in the private library.
- `scripts/*.txt` preserved the original experimental scripts with the whitespace normalization described below. Text suffixes kept them as historical source evidence rather than executable repository tooling. `plan.md` preserved the accepted experimental order and ownership map.

JSON strings replaced the experiment root, original user library, and R framework prefix with `<EXPERIMENT_ROOT>`, `<ORIGINAL_USER_LIBRARY>`, and `<R_FRAMEWORK>`. The loaded-environment inventory omitted host/login/user identity fields; the dependency CSV omitted original library paths. CSV line endings were normalized to LF. One whitespace-only line in `worker.R.txt` lost its trailing space; `script-hashes.json` recorded original and archived script hashes. Conditions, SQL, data values, admission decisions, and resource-state observations were otherwise retained. Binary packages, source tarballs, console logs, and disposable database files were not committed. Database files had already been disposed before publication.

## Read an observation without executing an experiment

For example, this printed the immediate state after the calibrated actual-interrupt INSERT pilot:

```sh
python3 - <<'PY'
import json
from pathlib import Path
archive = Path('investigation/exception-safety-recovery-2026-09-30')
wanted = 'results/insert_after--sigint--new--2/after.json'
for line in (archive / 'observations.jsonl').open():
    record = json.loads(line)
    if record['path'] == wanted:
        print(json.dumps(record['value'], indent=2))
        break
else:
    raise SystemExit('Observation not found')
PY
```

The minimum ordinary-error reproduction was under `results/minimal--error--1/minimal.json`, with repetitions 2 and 3. The controlled-interrupt minimum was under `results/minimal--interrupt--1/minimal.json`. Warning-replay controls were under `results/controls/diagnostic-minimal.json` and repetitions `diagnostic-minimal-2.json` and `diagnostic-minimal-3.json`. Plain DBI lock and dbplyr collection comparisons were under `results/controls/upstream-lock.json`.

## Pilot admission

The INSERT pilot's repetitions 2–4 passed for ordinary error, controlled interrupt, and actual SIGINT. The admitted observations established all of these points before experimental cleanup:

| Check | Ordinary error | Controlled interrupt | Actual SIGINT |
| --- | --- | --- | --- |
| Healthy public seed | Three rows; totals 2, 5, 7 | Same | Same |
| Arrival evidence | One reached event after real successful INSERT | Same | Flushed reached event and worker PID matched by supervisor |
| Escaping condition | `fault_injected`, `error`, `condition` | `interrupt`, `condition` | `interrupt`, `condition` |
| Immediate destination | Newly created destination removed | Three inserted rows retained | Three inserted rows retained |
| Owned savepoint after operation | Absent | Retained | Retained |
| Input and sentinel | Preserved | Preserved | Preserved |
| Same-session healthy operation | Succeeded | Succeeded, but retained transaction affected persistence | Same |

The first repetitions were excluded because the condition observer incorrectly assumed an interrupt message was non-null. Their exclusion was recorded rather than replaced by successful repetitions. Real SIGINT was delivered at an explicit R checkpoint after successful native work, not inside SQLite native execution.

## Reconstruct for a separately authorized replay

Use a newly created disposable directory as `bundle`, outside the original working tree. Copy each archived script there under `bundle/scripts/`, removing only its final `.txt` suffix. Create empty `bundle/home/`, `bundle/tmp/`, and `bundle/results/` directories. Provision `bundle/library/` from an isolated copy of the dependency versions recorded here, and install a Git archive of the target SHA into that library. The archived package-source hashes allow comparison with that fixed checkout. No dependencies or binaries were included, so exact replay remained contingent on their availability; silently substituting versions would produce a different experiment.

Run all workers through a newly launched supervisor with `--vanilla`, private HOME/TMPDIR/library variables, and `TESTTHAT` unset. Do not source workers into a normal R session. The archived manager determined its bundle root from its own location and refused existing run directories. Representative commands from the disposable bundle were:

```sh
python3 scripts/manage.py insert_after sigint new 20
python3 scripts/manage.py begin_after error overwrite 20
python3 scripts/manage.py local_replay error new 20
python3 scripts/manage.py collect_repair interrupt new 20
python3 scripts/run-minimal.py
```

`run-minimal.py` used fresh minimum-case directory names, so it required an empty results directory. `diagnostic-minimal.R` accepted the bundle root and an output JSON path; `upstream-controls.R` accepted the same arguments. Run them only as separate supervised R processes under the same private environment.

The recorded limits were 100 source rows, 20 MiB per database, 60 seconds per case, at most two experiment R processes, and a 500 MiB retained bundle. Scripts were experimental evidence and did not constitute enforcement of every resource ceiling. The manager observed before requesting disposal, waited for owned workers to finish, and removed only experimental database files. A replay needs an independent management owner even when product cleanup is intentionally interrupted. Never reset a namespace, cache, connection, or transaction before the product-state verdict. Caller commit and rollback require separate cases, with no retry overwriting the interrupted destination before that decision.

Timeout/watchdog termination, natural backend lock failure, controlled conditions, and actual SIGINT were separate evidence classes. Native statement cancellation, a second interrupt inside an interrupt handler, other platforms/versions, long-query timeouts, server backends, disk exhaustion, OOM, and SIGKILL were not established by this archive.
