# Grouping specification algebra and namespace boundary investigation

Investigated: 2026-11-07

Tested revision: `e1dabaabb392a3791177e5c48e983f780c7c93ec`.

## Outcome

No new confirmed defect was established in this lane. Three independent properties passed over the same 1,500 deterministic generated specifications (seed 71107):

1. With duplicate occurrences retained, all grouping-set memberships and occurrence multiplicities agreed with a recursive algebra model for singleton sets, unions, rollups, cubes and Cartesian products.
2. With duplicates dropped, grouping-set memberships and first-occurrence order agreed with that model's deduplicated expansion.
3. The complete duplicate-drop inspection tibble agreed with duplicate-keep inspection followed by first-membership deduplication and consecutive `set_id` renumbering. This compared included/omitted dimensions, fixed keys, named Grouping bits, Grouping identifiers and order.

The generator used four source dimensions, nesting depth up to three and zero through three arguments per applicable node, including empty sets, empty identity products, overlapping dimensions, multi-column selections and nonempty composite dimensions. All runs used actual public `inspect_grouping()` calls. The membership oracle canonicalized members only for equality; the plan comparison preserved all public field values and order.

The initial model incorrectly treated multi-column selectors inside rollup/cube as one composite dimension. The documented per-column rule corrected that oracle; all final counts above refer to the corrected scripts. An early namespace stress harness likewise expected arbitrary specification-valued ordinary selections to be admitted and expected unbound lexical `a` reads to succeed. Those expectations were rejected, and its raw mismatch count was not treated as bug evidence.

## Qualified namespace stress controls

A corrected 160-case finite matrix crossed public/private namespace operators, one through five wrapper levels, bound-specification/constructor-call payloads, ordinary-selection/specification returns and four parent kinds (union/product/rollup/cube). Public wrappers and ordinary-selection controls used faithful character metadata and their expected once-per-call namespace lookup counts. Private-qualified specification-valued wrappers retained the pre-existing refusal described below.

A private-qualified identity wrapper around a bound specification was investigated as a candidate:

```r
s <- grouping_set(a)
inspect_grouping(data.frame(a = "x"),
                 .grouping = grouping_sets(base:::`(`(s)))
```

It raised the Package condition explaining that the nested argument was an unrecognized specification-valued selection. The public-qualified identity control accepted it. This behavior also reproduced in a separate archive of pre-#1396 main `9ffb24614a71512170298a3827bc11c80354e742`.

The shared matcher used `rlang::is_call(expr, "(")`: it recognized `base::` qualification and did not recognize `base:::` qualification. The complete #1288 triage discussion explicitly excluded new recognized spellings including the outer `base:::` form. #1396's first acceptance criterion spoke broadly about public/private authored calls; its original ordinary-selection private controls did not establish nested-specification admission. No new ticket was recommended for this known recognition boundary without first resolving that specification scope. It was neither a newly introduced #1396 regression nor a silent wrong result in the tested matrix.

A further attempted primitive-coercion counterexample was discarded: coercing a symbol to a function creates a closure rather than forcing the active binding. Reading a function's names did not provide a typed-selection dependency. It did not establish a package defect.

## Replay

From the directory containing these four scripts, set the source checkout explicitly. Outputs default to `results/` beside each script; `MARGINPLYR_HUNT_OUTPUT` overrides that destination. No current-working-directory discovery is used for source or outputs:

- `MARGINPLYR_HUNT_SOURCE=/path/to/pinned-checkout Rscript algebra.R`
- `MARGINPLYR_HUNT_SOURCE=/path/to/pinned-checkout Rscript algebra-drop.R`
- `MARGINPLYR_HUNT_SOURCE=/path/to/pinned-checkout Rscript plan-drop.R`
- `MARGINPLYR_HUNT_SOURCE=/path/to/pinned-checkout Rscript namespace-controls.R`

The first three print `tested 1500 fail 0`. The corrected namespace control matrix prints `tested 160 fail 0`.

## Sources and limits

Required full reads completed: `CLAUDE.md`, `AGENTS.md` (364 lines), `CONTEXT.md` (316), ADR 0008 (912), ADR 0013, ADR 0019 (735), and ADR 0026 (281), all from the tested revision. Additional searches/readings covered the relevant Grouping-plan implementation and tests, the 2026-11-06 expansion and summary/share investigation sections, and complete issue bodies/comments for #1396, #897 and #1288. No maintained source or test was edited.

The finite comparison did not establish arbitrary-depth recursion, huge distinct-set performance, every custom source class or all backends. It used local tibble inputs for the algebra and local data.frame inputs for namespace controls. No complete test suite, package build/check or optional-backend test was run in this lane.

Environment: aarch64 macOS; R 4.6.1, marginplyr 0.1.0 (loaded from the pinned source), dplyr 1.2.1, tidyr 1.3.2, rlang 1.3.0, tidyselect 1.2.1, dtplyr 1.3.3, stringr 1.6.0, vctrs 0.7.3. `TESTTHAT_PARALLEL=false`; all owned executions terminated and were collected.

Portable reruns retained one text log and one CSV summary per script, plus textual `dput()` failure records. All four portable reruns completed successfully against the tested revision: 4,500 algebra/inspection comparisons and 160 namespace controls. The scripts require an explicit source environment variable and exit nonzero for any unexpected result. No RDS artifact is required for replay.
