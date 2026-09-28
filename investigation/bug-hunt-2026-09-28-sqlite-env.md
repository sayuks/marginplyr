# SQLite summary queries with `.env` as a column name

Investigated: 2026-09-28
Baseline: `814dcc98be48008e07a92f24b7d4c329e37a6104`

## Reproduction

From the repository root, run:

```sh
Rscript investigation/bug-hunt-2026-09-28-sqlite-env.R
```

The script is a public-API regression probe. It compares local results with
directly collected SQLite results for three one-row cases: `.env` as a Grouping
dimension, as a named ordinary summary output, and as `.id`. On the baseline
above, all three exited red with `Cannot translate a <rlang_ctxt_pronoun>
object to SQL.` A one-set plan and `.margin_label = NULL` reproduce the
dimension failure; no label conversion, sorting, or branch union is needed.
The local calls returned the expected values. Direct `dplyr::summarise()` on
the same SQLite connection accepted an output named `.env`, while direct
`dplyr::summarise(..., .by = tidyselect::all_of(".env"))` failed with the same
pronoun translation error. The dimension case therefore crosses an upstream
dbplyr limitation; the summary-output case fails only after marginplyr's
additional query construction in this comparison.

This was measured with R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0, RSQLite 3.53.3,
dtplyr 1.3.3, and testthat 3.3.2. The versions describe the experiment.

## Boundary observed

`inspect_grouping()` and `expand_with_margins()` accepted a `.env` dimension
with a missing Margin label on the same SQLite input. The summary adapter
copied grouping columns into private keys. Its deferred dbplyr select
contained a bare `.env` symbol for the source column and its copy; at SQL
rendering, dbplyr resolved that spelling to the tidy-evaluation pronoun.
`dbplyr::ident(".env")` rendered the intended quoted SQL identifier in a
standalone projection. With two grouping sets, the placeholder projection,
identifier attachment, final type anchor, and Margin order could each add
another deferred reference. A local patch to the first copy made the one-set
case pass but left the other cases red. Broader provisional changes passed
the narrow collect case but left fixed-key materialization and a text Margin
label failing, and introduced a sort-without-`.id` failure. All provisional
package and test changes were removed after those checks.

This differs from the local label-conversion defect recorded in
`investigation/bug-hunt-2026-09-26-post-699.md` and ticket #702: the
reproduction here uses a missing label, fails only when the SQLite query is
rendered, and also reaches output and identifier names.

## Other edge-case probes

The existing full testthat suite passed on the baseline. Additional dated
checks found no mismatch in 300 randomized local direct-versus-expanded
comparisons; 120 local-versus-SQLite grouping comparisons; 80 randomized
Parent and Total share cases on each of local and SQLite; 60 seeds crossed
with nine key types and three labels for local direct-versus-expanded results;
80 local-versus-dtplyr cases crossed with three Margin orders; 90 randomized
label-collision inputs on each of local and SQLite; and 20 unusual summary
output names excluding `.env`. The randomized checks used fixed seeds and
temporary scripts; their passing outcomes do not establish exhaustive
correctness. The saved probe above retains the reproducible failure.
