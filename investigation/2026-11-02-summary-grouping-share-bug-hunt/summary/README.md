# Local summary investigation: forwarded selectors

Investigated: 2026-11-02
Repository snapshot: `978d7bef4247bf1348af5b90bfab47924db25500`

The investigation used the new worktree created from fetched main. The
October 29, 30, and 31 summary/Grouping specification/share investigations
guided the remaining scope and forwarding boundaries. No package code,
package test, generated documentation, or dependency metadata was changed.
All scripts below were standalone investigation artifacts, loaded against the
checkout with pkgload where package execution was involved.

## Confirmed finding: local selection rewrites lose dots forwarding

This was the minimized ordinary-dplyr/Margin pair:

```r
data <- data.frame(x = 1:3)
dplyr::summarise(
  data, (function(...) dplyr::across(..., sum))("x")
)
# x = 6
summarize_with_margins(
  data, (function(...) dplyr::across(..., sum))("x")
)
# Caused by error in list2():
# '...' used in an incorrect context
```

No grouping column, Grouping specification, share, preceding summary, or
external binding was needed. Replacing the selector with `..1` was a second
spelling of the same boundary: ordinary dplyr returned six, while the Margin
verb reported that column `..1` did not exist. Here `..1` was a reference to
the inline function's first dots argument, not an input or output column name.

The ordinary summary contract came from the reference's `...` parameter
(name-value pairs as used by dplyr), ADR 0007's captured-expression environment
boundary, and ADR 0019's environment rule wherever evaluation happened. The
helper's static spelling was visible and recognized; the failure concerned
the ordinary R forwarding that supplied its selection.

### Scope and independent controls

`reproduce-ellipsis.R` compared 432 cases:

- three eager input classes: plain data frame, tibble, and data.table;
- `across()`, `if_any()`, and `if_all()`;
- positional `...`, explicitly named `.cols = ...`, forwarding the entire
  helper argument list through `helper(...)`, and a `..1` selector;
- an unpartitioned summary, fixed `.by` grouping, and a rollup;
- named or unnamed output, with or without a preceding independent summary.

Every ordinary-dplyr oracle succeeded and every Margin call failed. The 324
ellipsis cases had the incorrect-context error; the 108 `..1` cases had the
nonexistent-column error. The rollup oracle combined independent ordinary
detail and grand-total summaries. It did not derive expected values from a
Margin result.

Forty-five controls matched ordinary values and schemas. They wrapped dots
selectors in `c(...)` or `all_of(...)`, or a `..1` selector in `all_of(..1)`.
A proposed `c(..1)` control was removed because ordinary tidyselect also
treated that nested symbol as a nonexistent column. That ordinary refusal
was not a package finding. Supplemental manual function-forwarding controls
(`across(x, ...)`, `.fns = ...`, and `.fns = list(...)`) also agreed.

The initial forwarding differential had already found this boundary in four
of 120 comparisons. Its other 92 comparisons matched values and schemas;
24 were paired refusals. Its inline scopes included dots, formal arguments,
delayed/active bindings, environment reads, local predicate functions,
indirect calls and tidy-evaluation spellings. Paired refusals were not
assertions of identical diagnostics.

### Causal separation

The ranked hypotheses were loss of R argument/promise capture at the helper
boundary, loss of the inline function's dots binding, and first-dot preflight.
The named/preceded matrix removed preflight as the explanation. Successful
`c(...)`/`all_of(...)` controls showed the dots binding remained available.
`capture-control.R` separated promise capture from Margin preparation and
private grouping keys: a helper capturing its formal with `enquo()` received
the literal expression `"x"` from both `...` and `..1`, and tidyselect selected
x. Giving tidyselect the quoted raw symbols instead reproduced the two
failure modes while `..1` still evaluated to `"x"` in the caller's frame.

`local_summary_selection_expr()` quoted the written selector before dplyr
could capture its `.cols` formal. `local_summary_selection()` subsequently
resolved that raw expression with tidyselect. Ordinary dots expansion and
promise forwarding had therefore been replaced by a different operation.
This was not a claim that all runtime-assembled helper calls should become
statically recognized, nor a request to weaken grouping-column exclusion or
share-source dependency checks.

### Duplicate search and ticket boundary

The all-state GitHub issue and PR searches were `"ellipsis"`,
`"forward" "across"`, and `"dot-dot" "selection"`. The ellipsis searches
returned no issues or PRs. The relevant nearby issues were read with complete
bodies and comments:

- [#621](https://github.com/sayuks/marginplyr/issues/621) addressed partial
  argument matching, not forwarding through R dots;
- [#925](https://github.com/sayuks/marginplyr/issues/925) addressed omitted
  arguments through parenthesized tidy forwarding, not a populated selector;
- [#827](https://github.com/sayuks/marginplyr/issues/827) addressed dot-dot
  output names;
- [#1002](https://github.com/sayuks/marginplyr/issues/1002) addressed generated
  reads of admitted signed dot-dot column names.

The last two did not concern dots-argument references in an ordinary inline
function. The investigated snapshot already contained those fixes.

One independent P2 ticket was recommended, with no blockers. Its delivery
was dplyr-compatible populated dots/`..1` forwarding for visible local
selectors across the three helper families, while retaining the successful
selection wrappers, function forwarding, grouping exclusions, collision
checks and dependency refusals. The coordinator owned publication and the
final main-freshness check.

## Other bounded results

`scope-differential.R` compared 336 cases crossing six selection/predicate
expressions, fourteen expression scopes, preceding-summary presence and
named/unnamed output. It recorded 276 matching values/schemas, 56 paired
errors, and four differences involving `eval(parse(text = ...))` and a
`where()` selector. The reference explicitly stated that parsed helper calls
were searched but not rewritten. Those four raw differences were retained
as an excluded boundary, not promoted to a new rewriting guarantee.

Supplemental first-across function/factory controls compared ordinary
dplyr, an explicit independent Total share, and a selection-based Total
share. Direct closures/formulas, factory closures, delayed `n()` arguments,
and explicitly captured environments produced matching ordinary values or
paired ordinary refusals. No additional cached-closure defect was established.

These were bounded grammars and fixtures, not a claim about arbitrary R
callbacks or every extension class. This subtask established no live remote
backend or lazy-dtplyr forwarding promise, no performance limit, and no
interruption/concurrency behavior. Broader share arithmetic, Grouping
specifications, and remote result typing were investigated separately.

## Replay and environment

Run from the repository root:

```sh
Rscript investigation/2026-11-02-summary-grouping-share-bug-hunt/summary/reproduce-ellipsis.R
Rscript investigation/2026-11-02-summary-grouping-share-bug-hunt/summary/reproduce-ellipsis.R --assert
Rscript investigation/2026-11-02-summary-grouping-share-bug-hunt/summary/capture-control.R
Rscript investigation/2026-11-02-summary-grouping-share-bug-hunt/summary/scope-differential.R
Rscript investigation/2026-11-02-summary-grouping-share-bug-hunt/summary/forwarding-differential.R
```

Report mode checked successful controls and recorded the failing comparisons.
The desired-behavior `--assert` mode intentionally failed on this snapshot.
Adjacent CSVs and logs retained the observations. These were investigation
signals, not changes to the package regression suite.

R 4.6.1 (2026-06-24), aarch64-apple-darwin23; dplyr 1.2.1, rlang 1.3.0,
tidyselect 1.2.1, vctrs 0.7.3, tibble 3.3.1, data.table 1.18.6.1,
dtplyr 1.3.3, and pkgload 1.5.3 were installed. The claimed matrix concerned
eager local inputs, including eager data.tables.
