# Independent witnesses and current-main confirmation

Investigated: 2026-11-06
Baseline and fetched confirmation SHA: `952a0d9b18fd3ff4631f153432f6024ad62fcac2`

The coordinator created different witnesses after the three track findings.

- [List array](independent-list-array.R): two-row rank-three list array,
  different values and an actual NULL source cell. Ordinary typed-missing
  mutation/union retained exact shape, cells and payload copies; included-only
  and payload controls passed. Rollup instead falsely refused the missing
  prototype.
- [Empty projection](independent-empty-projection.R): two-row, two-column
  data.table selected only an absent column name. The scalar-zero projection
  had no columns. Explicitly collected and directly empty-root expansion
  returned the expected empty integer identifier, while the derived step
  falsely refused. Caller data remained unchanged.
- [Pattern proxy](independent-pattern-proxy.R): a regex rather than fixed
  pattern, inherited rather than exact-class proxy method, and different
  two-row input. Direct typed selection and wrapped-pattern expansion passed;
  unwrapped expansion called the method against integer placeholders and
  falsely refused.

Observation executions all exited zero after their separating controls.
Each script's `--assert` mode expressed the desired contract rather than
asserting the known error. After fetching main again and confirming the same
SHA, the coordinator exported that commit to a separate disposable source
directory with `git archive` and ran each independent script there in a fresh
R process. The three expected-contract executions exited **1**, confirming
the unfixed findings separately from the evidence worktree. The terminal
results were `aac07a` (list array), `913012` (empty projection) and `346ec0`
(pattern proxy). The `main-*-assert.log` files retained their output.

To repeat, export the named SHA, use the exported package as the working
directory, and invoke each witness by its absolute path with `Rscript` and
`--assert`. Without that flag, the scripts print the observed mismatch after
checking their successful controls. The scripts loaded the package from the
working directory, not an installed marginplyr. They disabled testthat
parallel execution and owned no child processes or persistent external state.

The environment was R 4.6.1, dplyr 1.2.1, vctrs 0.7.3, tidyselect 1.2.1,
stringr 1.6.0, dtplyr 1.3.3 and data.table 1.18.6.1 on arm64 macOS.
