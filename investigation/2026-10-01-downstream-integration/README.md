# Reproduction bundle

Investigated: 2026-10-01

The accompanying [investigation note](../downstream-integration-2026-10-01.md)
records scope, contracts, controls, outcomes, exclusions, and preservation.

- `consumer-template.R.in` and `prepare.py`: regenerate and install the ten small
  consumers and the unchanged marginplyr source package in a dedicated library.
- `run.py` and `run-case.R`: start fresh R processes, compare public workflows,
  and record data, SQL, conditions, loaded namespaces, search path, and paths.
- `probe-calibration.R`: ordinary dtplyr controls for the root-environment
  lookup and missing-function observations.
- `final-cases.csv`: 395 final outcome records; all passed their assertions.
- `all-attempts.csv`: classified calibration and follow-up attempts, including
  superseded harness expectations and unsupported requests.
- `consumer-declarations.csv`, `dependencies.tsv`, and hash tables: installed
  consumer declarations, dependency versions and origins, source identity,
  and preservation evidence.
- `evidence/`: durable observed values, types, queries, conditions, logs, and
  process states. A recorded process exit of zero is insufficient by itself:
  the runner deliberately catches case errors and saves them in `cases.csv`.
  Observations use `.txt`: data.table's external-pointer display text in a
  `dput()` is diagnostic text, not executable R serialization.
  Repository copies normalize line endings and trailing presentation spaces;
  the isolation root retains the original logs and observations.
- `verify-and-export.py`: check preserved source/dependency files and export
  evidence. For a later reproduction, pass a **new** third-argument export
  directory rather than overwriting this dated evidence.

The scripts use the R packages already installed on the reproduction machine
read-only and copy their dependency closure. They do not download dependencies
or write into the regular R library. `prepare.py` refuses a dependency graph
whose package versions differ from `dependencies.tsv`.
It also requires the recorded HEAD and tracked source hashes before building.

Reproduction commands and focused-case environment variables are in the note.
The recorded source tarball and binary library remain in the isolation root
listed in `preservation.json`; neither is required as the sole evidence for
any conclusion.
