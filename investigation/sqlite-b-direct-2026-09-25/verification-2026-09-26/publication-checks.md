# Evidence-branch publication checks

Checked: 2026-09-26

This branch archived historical evidence only. Its root package, tests, build
configuration, and maintained check tools were unchanged. It was not a package
implementation branch or a proposal to merge the archived source copies into the
maintained code. The package Review-ready check was therefore not invoked.

- Archive integrity, patch/source equality, portable reconstruction, and the
  98/98 replay are recorded separately in this directory.
- Package-aware lintr, after loading the package namespace, passed.
- The always-loaded-context budget check passed (20,008 / 22,005 bytes).
- Full `jarl check .` reported one finding: `if_always_true` in the preserved
  rejected C prototype. The literal `if (TRUE)` was part of that historical
  experiment. The source bytes were retained, without a suppression or fix.
  A separate invocation excluding only that archived C source file passed.
  This scoped result does not change the full invocation's failed verdict.
- The document-reference verifier failed on eleven historical review paths
  written relative to the original review root. The files were present in the
  bundle at those root-relative paths. It also began interpreting four existing
  generated/output paths as citations when the archive introduced `.rds` and
  `.txt` extensions into its tracked-extension inventory. The original reports
  and the verifier were left unchanged. The archival index links and inventory
  were checked against the saved files and reconstruction inputs instead.
- A separate exploratory jarl invocation naming the normally omitted hidden
  workflow directory surfaced eight findings in unchanged baseline scripts.
  Those scripts were not edited as part of evidence preservation.
- No formal code review was run and no pull request was opened for this
  repository-only archive. Its artifact integrity and reproducibility were
  verified; production review and package gates remain implementation work.

These known archival-check failures are reported rather than made green by
rewriting dated evidence or changing repository-wide check configuration.
