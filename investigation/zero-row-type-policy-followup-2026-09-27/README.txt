PROTOTYPE ONLY: Issue #706 direct-helper and literal across-lambda integer declarations.

variant/ is a disposable copy, not production code. Root repository was not modified.
Against /private/tmp/marginplyr-706-compare.KS6SQ3/base/R, diff is +58/-8 lines (net +50).
- R/grouping-context.R: new 49-line grouping_output_names() with direct+literal across recognition.
- R/summarize_with_margins.R: +8/-7, declarations and anchor condition.
- R/sqlite-typed-order.R: +1/-1, always return tibble from typed collector.

Artifacts:
- probe-across.R: formula/function/single-block/parenthesized/mixed-list across and unstable .names controls.
- base-across.txt: same script against current main.
- variant-across.txt: same script against variant.
- across.rds: parent probe.R 114 queries / 570 observations (352 nonempty), with variant before final parenthesized-.fns normalization. This normalization was separately verified in probe-across.R.

Parent probe comparison:
- All 570 observations: zero column-name or row-count differences.
- All 352 nonempty observations: zero data/value/typeof differences.
- Existing sorted collectors change data.frame -> tibble in 112 nonempty observations from the unconditional tibble conversion. Without this conversion, original extension changes four new nonempty collect observations in the reverse direction.
- Ordinary empty CAST columns may lose their inferred types upon compute when a query newly enters typed materialization, as in parent extension.

Limitations:
- The +50 line variant is not a completed implementation. Unstable .names ('{.col}_{bump()}') is allowed by baseline, but predictor output differs from actual dbplyr output. Variant errors for nonempty collect and appends a ghost column for empty collect. Reproductions and output are included.
- Filtering declarations against actual output names would prevent corruption but would not guarantee these helper outputs. Guaranteeing them requires sharing actual name expansion with declaration registration, or mapping provenance after dbplyr expansion.
- The original public API permits arbitrary ordinary across naming and callable handling; the new prototype only recognizes literal formulas/functions whose whole result is a grouping helper. Other expressions can deliberately change result type and should not inherit numeric guarantees.
- No tests/roxygen/ADR edits were made. Four focused integration test groups would be ~100-160 handwritten lines before additional dynamic-name resolution coverage (estimate, not measured).
