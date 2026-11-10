# Windows DuckDB JSON staging lifetime

Investigated: 2026-11-10

## Native reproduction

Issue #1566 was investigated from main
`372e40cd37908df31e2974f5b919b084a21fd05e` in a dedicated worktree.
The temporary diagnostic command was retained in
[commit 39ef775a](https://github.com/sayuks/marginplyr/blob/39ef775a5b2adb1c166f875ee0acd1848aab555d/tools/diagnose-duckdb-json.R).
It ran the real publication helper with official HTTPS acquisition in two fresh
R processes, changing only whether validation performed `LOAD` and the JSON
control query. The deletion wrapper reported remaining files without changing
the initial deletion result.

Both native runs used Windows Server 2025, R 4.6.2 ucrt, DuckDB 2.0.0 and
`windows_amd64_mingw`:

- [First native job](https://github.com/sayuks/marginplyr/actions/runs/42818895735/job/129839002164),
  at `1c7b045394323f0ba8e5d5072eda17daa10ef073`.
- [Second native job](https://github.com/sayuks/marginplyr/actions/runs/42819574062/job/129841327584),
  at `39ef775a5b2adb1c166f875ee0acd1848aab555d`.

In both runs, INSTALL-only publication returned successfully and removed its
stage. Adding LOAD validation reproduced the reported staging-removal error.
Only the installed `json.duckdb_extension` remained; its mode was `666`, and
`file.access(path, 2)` returned zero. Deleting it directly failed with
`Permission denied`. Disconnecting with `shutdown = TRUE` and running `gc()`
did not permit removal. The second run also attempted removal through the
normalized forward-slash path, which failed in the same way.

The second run removed the cache successfully immediately after the child
process exited. The first run's immediate post-exit recursive removal returned
one; that probe did not record the remaining paths or retry, so its post-exit
failure was not explained by these measurements. No delayed-deletion or retry
policy was established from it.

These observations isolated LOAD's native-library lifetime from installation,
path spelling, and ordinary write access as the cause of the in-process staging
failure. They did not establish an earlier preparation error in the reported
main job. On macOS R 4.6.1 / DuckDB 1.5.5, both probe modes returned successfully.

## Independent diagnostic masking

A macOS reproduction sourced the unmodified helper into a private environment,
made its download seam raise `primary download sentinel`, and made its deletion
seam return one. `duckdb_json_publish()` reported only the staging-removal
error. An assertion requiring the primary diagnostic failed. This synthetic
fault established masking, separately from the native Windows LOAD failure.

The command fixtures exercised the same failure through acquisition and tested
preparation-only, combined preparation/cleanup, and cleanup-only failures.
The combined-failure assertion failed before the cleanup diagnostic change.
