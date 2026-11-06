# Independent SQLite derived-input confirmation

Investigated: 2026-11-06
Tested main SHA: `9ffb24614a71512170298a3827bc11c80354e742`

The coordinator used a separate `git archive` source copy and fresh R processes
to challenge the SQL lane's one-row TRUE/window witness. The independent
[script](independent-sqlite-input.R) changed the table and key names, used three
rows and mixed-case `FaLsE`, and included a missing key. Its payloads were
17, 21 and 35. A window rank made the input an ordinary derived relation.

Source collection, projection, no-op mutation, root-table expansion, renaming
the Boolean-word output before expansion, and expansion of a stored source
copy all passed exact public-name/column-value comparisons. Default expansion
of the derived source instead raised `simpleError: undefined columns selected`.
Observation mode exited zero after asserting controls and printing the failure.
The desired-contract mode exited one at `stopifnot(ok)`, after the controls had
passed. These were defect confirmations, not passing package regression tests.

An initial ordinary `compute(source)` control itself failed upstream with
`no such column: FaLsE`; its failing result was not credited as a successful
control. The stored-source control explicitly wrote the already collected rows
through DBI instead. No general ordinary dbplyr-composition guarantee was inferred.

Main was fetched again before the independent replay and still named the tested
SHA. The initial command's terminal exit was zero; assertion session `87967`
was retained and collected with terminal exit one. Each script disconnected
its private in-memory database through `finally`. Session/package information
was retained in the observation log. The probe introduced no implementation
change.

```sh
Rscript <absolute-evidence>/independent-sqlite-input.R <archived-package-root>
Rscript <absolute-evidence>/independent-sqlite-input.R <archived-package-root> --assert
```
