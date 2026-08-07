# Convenience API proposal

Investigated: 2026-08-04
Revised: 2026-08-07 — `R/share.R`, ADR 0018
Prototype: `sayuks/proto-convenience-api` (`740e577`), deleted 2026-08-07
Baseline: `main` at `f0195eb` (0.1.0, pre-CRAN)

This note proposes additions that make marginplyr easier to use without
changing what it already promises. Every proposal is ranked by how much
caller code it removes, and the top two carry an executable prototype rather
than an argument.

The method was to write the reports a user actually writes — a store/region
rollup, a percentage report, a cube by product and channel, the same report
against DuckDB — and record what could not be expressed with the released
interface. Several plausible-sounding gaps turned out not to exist and are
listed at the end so they are not re-proposed.

## 1. `share_of_total()` — share of the grand total

**Gap.** `share_of_parent()` answers "how much of its parent is this row?".
The more common reporting question is "how much of the whole is this row?",
and marginplyr cannot answer it on a lazy backend at all. Locally a caller
extracts the root row and divides:

```r
report <- summarize_with_margins(
  sales,
  revenue = sum(revenue),
  level = grouping_id(region, store),
  .grouping = rollup(region, store)
)
total <- report$revenue[report$level == 3L]
dplyr::mutate(report, pct = revenue / total)
```

That reaches into the result by bit mask, hard-codes the mask value, breaks
under `.by` (each partition needs its own denominator), and cannot run on a
database without a self-join the caller writes by hand.

**Proposal.** A second contextual summary helper, used exactly like
`share_of_parent()`:

```r
summarize_with_margins(
  sales,
  revenue = sum(revenue),
  pct_parent = share_of_parent(revenue),
  pct_total = share_of_total(revenue),
  .grouping = rollup(region, store)
)
```

The denominator is the same summary on the grand-total set, within each fixed
`.by` partition; the grand-total row is `1`. Missing numerator, zero
denominator, and missing denominator follow the Parent-share rules unchanged.

**Why it is cheap.** The Parent-share module is already parameterized by one
vector that maps each Grouping-set occurrence to its denominator occurrence
(`parent_set_ids()`). A Total share is the same machinery with a different
vector: every occurrence maps to the grand-total occurrence. The prototype
adds `total_set_ids()`, threads a `target_ids` argument through the three
existing adapters, and tags each request with its kind. No adapter, join,
validation, or condition path is duplicated.

**What it unlocks.** `share_of_parent()` requires one pure `rollup()`, because
only a rollup defines an unambiguous parent. A grand total is unambiguous in
any plan that contains it, so `share_of_total()` also works with `cube()` and
with `grouping_sets()` that include an empty `grouping_set()`. Verified in the
prototype against `cube(product, channel)` locally and on DuckDB. A plan
without a grand-total set raises a `marginplyr_error` naming the fix.

**Open questions for the maintainer.**

- Whether Arrow should reject Total shares the same way it rejects Parent
  shares. The prototype inherits that rejection because it shares the
  admission path; the reasoning behind ADR-adjacent Arrow limits applies
  identically, so this is probably correct as-is.
- Whether the two helpers should share one reference page and one
  `@family`, or split. The value rules are identical except for the
  denominator, so one page with a denominator table is likely clearer.
- Naming. `share_of_total()` reads well next to `share_of_parent()`, but
  "total" is also the default Margin label, and the two are unrelated. If
  that collision is a concern, `share_of_grand_total()` is unambiguous and
  ugly; `share_of_root()` matches the internal vocabulary but not the user's.

## 2. `.sort` — report order that `arrange()` cannot express

**Gap.** Row order is unspecified and the documentation directs callers to
`dplyr::arrange()`. For a multi-grain result, `arrange()` on the result
cannot produce report order, because the sort key is not in the result. The
displayed label sorts as an ordinary value:

```r
report |> dplyr::arrange(region, store)
#>   region         store revenue
#> 1   East        Boston    6000
#> 2   East      New York    3000
#> 3   East         Total   11200   <- subtotal above a detail row
#> 4   East          <NA>    2200
#> 5  Total         Total   23300   <- grand total between East and West
#> 6   West San Francisco    7200
```

Correct order requires the Grouping bits, so the caller must add one
`grouping_bit()` summary per dimension, interleave them with the dimensions
in `arrange()`, and drop them again:

```r
report |>
  dplyr::arrange(region_bit, region, store_bit, store) |>
  dplyr::select(-region_bit, -store_bit)
```

Under `.margin_label = NULL` no `arrange()` works at all: a margin and a
source missing value are then the same displayed value, and only the bits
separate them.

**Proposal.** `.sort = FALSE` (default, unchanged behaviour), `TRUE` for
margins after the rows they summarize, `"first"` for margins before them:

```r
summarize_with_margins(
  sales,
  revenue = sum(revenue),
  .grouping = rollup(region, store),
  .sort = TRUE
)
#>   region         store revenue
#> 1   East        Boston    6000
#> 2   East      New York    3000
#> 3   East          <NA>    2200
#> 4   East         Total   11200
#> 5   West San Francisco    7200
#> 6   West       Seattle    4900
#> 7   West         Total   12100
#> 8  Total         Total   23300
```

Order is `.by` keys, then for each dimension in plan order its Grouping bit
and then its value. It is structural: displayed labels never choose the
order, so it is identical under `.margin_label = NULL`, under per-dimension
labels, and under factor dimensions.

**This is not the argument that was removed.** #15 removed a `.sort` whose
implementation was `dplyr::arrange(result, !!!margin_cols)` — ordering by
displayed value, which is exactly the broken output above and is what
`dplyr::arrange()` already does. Removing it was right. ADR 0001's amendment
and ADR 0009 record the consequence — the finalizer does not order rows —
and this proposal does reverse that. It needs an ADR and an amendment to
0001, not a quiet reinstatement, and the argument for it is that "callers use
`dplyr::arrange()`" is not achievable for the one order reports need.

**Lazy backends: the constraint the prototype found.** The first
implementation materialized bit columns, sorted, then projected them away.
dbplyr pushed the sort into a subquery and both DuckDB and dbplyr warned that
`ORDER BY` in a subquery is ignored — the ordering was silently lost on every
SQL backend. Keeping the bits as inline expressions instead lets dbplyr fold
them into the outermost query:

```sql
SELECT ... FROM ( ... GROUP BY GROUPING SETS ((region, store), (region), ()) ) AS q01
ORDER BY
  CASE WHEN ("..marginplyr_parent_set_1" IN (3)) THEN 1 ELSE 0 END,
  region,
  CASE WHEN ("..marginplyr_parent_set_1" IN (2, 3)) THEN 1 ELSE 0 END,
  store
```

One query, one `ORDER BY`, no warning. A test asserts both.

**Costs to weigh.**

- `.sort` needs an internal Grouping-set identifier, which under
  `.duplicates = "keep"` forces the portable `UNION ALL` adapter instead of
  native `GROUPING SETS` — the same trade `.id` already makes.
- Ordering of missing values inside a bit group is the backend's, not
  marginplyr's: SQL dialects disagree on `NULLS FIRST`/`NULLS LAST`. The
  contract should promise the bit-then-value structure and leave missing-value
  placement to the backend, or document `NULLS` handling as out of scope.
- Any verb downstream of a lazy `.sort` may discard the order, as with any
  `arrange()` on a lazy table. `.sort` is a presentation request for the final
  query.
- The prototype covers the summary verbs only. `expand_with_margins()` and
  the nesting verbs would use the same seam; whether they should is a separate
  question, since their results are usually consumed programmatically.

## 3. `count_with_margins()`

`dplyr::count()` exists because `summarize(n = n())` is written constantly.
The margin equivalent is written just as often:

```r
summarize_with_margins(sales, n = dplyr::n(), .grouping = rollup(region, store))
```

A thin wrapper with `wt` and `name` arguments, forwarding the common Margin
options, removes it. Low risk, low cost, and no new semantics — but it does
widen the exported surface, and a first CRAN release is a poor time to add
sugar. Proposed for after 0.1.0 is out.

## 4. Documentation

These need no interface change.

**A "recipes" vignette.** The existing guides explain concepts. The tasks a
reader arrives with are not covered end to end: percentage-of-total and
percentage-of-parent reports, keeping only one level, labelling each row's
level, sorting a report, formatting a multi-grain result for presentation.
Each is short; the value is that they exist together.

**Naming each row's level.** This works today and is written down nowhere. It
deserves a recipe rather than a new argument:

```r
report <- summarize_with_margins(sales, revenue = sum(revenue),
                                 .grouping = spec, .id = ".set")
plan <- inspect_grouping(sales, .grouping = spec)
dplyr::left_join(report, dplyr::select(plan, set_id, level = included),
                 by = c(".set" = "set_id"))
#>   region  store .set revenue           level
#> 1   East Boston    1    6000 (region, store)
#> 6   East  Total    2   11200        (region)
#> 8  Total  Total    3   23300              ()
```

A lazy `report` needs `copy = TRUE`, which the recipe should show.

**README.** The README shows `share_of_parent()` but not a
percentage-of-grand-total report, which is the more common question. It also
does not show a sorted report. Both follow from the proposals above.

## Checked and found not to be gaps

These were tested against the released interface and already work. Recording
them so they are not re-proposed:

- **Programmatic dimensions.** `rollup(all_of(dims))` and
  `rollup(!!!rlang::syms(dims))` both work, as does splicing a list of sets
  into `grouping_sets(!!!sets)`.
- **Per-dimension Margin labels.** `.margin_label = c(region = "All regions",
  store = "All stores")` works.
- **Distinguishing a source `NA` from a margin in the output.** With a Margin
  label set, source missing values stay missing, so
  `dplyr::coalesce(store, "(missing)")` after the summary is enough.
- **Reading the plan without executing.** `inspect_grouping()` covers it,
  including for lazy input.

## Suggested sequencing

1. Ship 0.1.0 unchanged. None of this belongs in a first submission.
2. `share_of_total()` — additive, reuses proven machinery, no decision to
   reverse.
3. `.sort` — needs an ADR first, because it reverses a recorded decision.
4. Recipes vignette and README examples, once the above exist.
5. `count_with_margins()` last, if at all.

## Prototype status

Branch `sayuks/proto-convenience-api` (`740e577`), not for merge:

- `testthat::test_local()` passes in full, including 17 new assertions in
  `tests/testthat/test-prototype-convenience.R`;
- `lintr::lint_package()` reports no lints;
- verified locally and against live DuckDB, including the generated SQL.

Not done, and required before any of this could merge: roxygen documentation
and `man/` regeneration, `NEWS.md`, ADR updates for `.sort`, generalizing the
Arrow rejection message beyond `share_of_parent()`, dtplyr and SQLite
coverage for Total shares, and snapshot expectations for the new errors.

## Revisions (2026-08-07)

This note reached `main` three days after it was written, by which time its
first two proposals had shipped. Its gap statements are written in the present
tense — "marginplyr cannot answer it on a lazy backend at all", "Row order is
unspecified" — so they are read as of `Investigated:` above and nowhere else.
The package is authoritative for what exists.

**Proposals 1 and 2 were accepted and implemented**, and the artifacts, not
this note, say what they do. `share_of_total()` is exported and documented on
`share_of_parent()`'s reference page. ADR 0018, "Order Margin results by
grouping structure", records the `.sort` decision this note said would need an
ADR, and CONTEXT.md gained **Margin order** and **Total share** as defined
terms.

`.sort` did not ship with the interface proposed here. Section 2 proposed
`FALSE`/`TRUE`/`"first"`; the argument takes `"none"` (the default), `"last"`,
and `"first"`. The proposed boolean does not appear anywhere in the package,
so section 2's code block does not run as written.

The three open questions in section 1 were all settled the way the section
guessed. The name is `share_of_total()`. Arrow rejects Total shares through
the same admission path that rejects Parent shares. The two helpers share one
reference page rather than splitting.

Section 4's documentation items exist: `vignettes/recipes.qmd` is the recipes
vignette, and the README shows a percentage-of-grand-total report.
`count_with_margins()` (section 3) was not implemented, and this note is still
the only place it has been proposed.

The prototype the note was written behind is gone. Branch
`sayuks/proto-convenience-api` was deleted locally and from `origin` on
2026-08-07, so `740e577` is reachable from no ref and
`tests/testthat/test-prototype-convenience.R` exists nowhere in the
repository. The `Baseline:` above is unaffected: `f0195eb` is an ancestor of
`main` and still resolves.

The deletion was the intent rather than an accident: a prototype is throwaway
from the day it is written, and this note is the answer that was kept from it.
What the prototype established about lazy backends — that materialized bit
columns lose their `ORDER BY` to a dbplyr subquery, and that inline
expressions survive — is preserved above and is the durable part.
