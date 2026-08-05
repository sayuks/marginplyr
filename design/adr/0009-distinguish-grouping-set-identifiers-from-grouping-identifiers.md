# Distinguish Grouping set identifiers from Grouping identifiers

Margin verbs will use `.id` to add a one-based Grouping set identifier for
each grouping-set occurrence in Grouping plan order, while `grouping_id()`
will remain a contextual summary helper that encodes absent dimensions as a
SQL-compatible bit mask and `grouping_bit()` will remain its single-dimension
counterpart. The concepts are deliberately distinct: a set identifier is
stable only within one ordered plan and distinguishes duplicate occurrences,
whereas a Grouping identifier is stable for one chosen absence pattern and
may be non-consecutive. When `.id` must distinguish duplicate sets retained
with `.duplicates = "keep"`, a native summary may use the portable
`UNION ALL` adapter so every backend preserves the same identifier semantics.

`inspect_grouping()` always exposes this occurrence number in a column named
`set_id`. Given the same resolved `.by`, `.grouping`, and `.duplicates`,
`inspect_grouping()$set_id` is exactly the value produced by a Margin verb
whose `.id` names an output column. The caller may choose any non-conflicting
`.id` column name without changing its values.

This correspondence lets callers inspect which occurrence an output row came
from and is part of the comparison documentation for `.id`,
`inspect_grouping()`, `grouping_bit()`, and `grouping_id()`. It does not make
`set_id` a durable identifier across differently ordered or deduplicated
Grouping plans, nor a business key for joining otherwise unrelated results.

The rows returned by `inspect_grouping()` are guaranteed to be in Grouping
plan order, so `set_id` increases from `1L`. Margin-operation result rows have
no implicit order after removal of `.sort`: `.id` records plan occurrence but
does not promise that the physical rows arrive in `.id` or key order. Callers
use an explicit `dplyr::arrange()` when presentation order matters.

Documentation will include a dedicated `vignettes/grouping_identity.qmd`
article, linked from Get Started and the relevant function references. It
will compare `.id`, `inspect_grouping()$set_id`, `grouping_bit()`, and
`grouping_id()` in one table; show why a two-dimension rollup has identifiers
`0`, `1`, and `3` but not `2`; contrast the cube's `0`, `1`, `2`, and `3`;
and distinguish plan identity from Margin labels and physical row order. Each
function reference will retain a compact comparison and link to the complete
article rather than relying on the article alone.

## Amendment: the comparison table lives in the reference, not the article

The two sentences above place the single comparison table in the
`grouping_identity` article and give each function reference a compact copy of
it. That placement is superseded. Function references are the canonical
user-facing contract, and a shared reference section is inherited from one
canonical home rather than copied, so the table now lives only in the
`grouping_bit()` reference; `summarize_with_margins()`, `inspect_grouping()`,
`grouping_set()`, and the `grouping_identity` article carry prose plus a
cross-link to it. `tests/testthat/test-documentation.R` asserts that it has
exactly one home.

The distinction this ADR decides is unaffected. A Grouping set identifier is
stable only within one ordered Grouping plan and separates duplicate
occurrences; a Grouping identifier is stable for one absence pattern and may be
non-consecutive. Only where that comparison is written down has changed, and
the article still exists and still explains the concepts — it links to the
table instead of holding it.

## Amendment: row order is unspecified by default, not always

One sentence above reads "Margin-operation result rows have no implicit order
after removal of `.sort`". `.sort` exists again under ADR 0018, so that
sentence now holds only as a default: result rows have no implicit order
unless the caller asks for a Margin order, and callers still use an explicit
`dplyr::arrange()` otherwise.

What this ADR decides about `.id` is unchanged. `.id` still records which
plan occurrence a row came from and still promises nothing about physical row
order — a Margin order is produced by finalization from the Grouping plan's
structure, not by `.id`, and `.id` is neither required for it nor implied by
it. The two do meet in one place: when `.id` names a column, that column is
the Margin order's final tiebreaker, so duplicate occurrences retained under
`.duplicates = "keep"` come out adjacent and in plan order. That is a
property of the ordering, not a new guarantee about the identifier, which
remains stable only within one ordered plan.

`inspect_grouping()`'s guaranteed Grouping-plan order is likewise unchanged,
and is still not the same thing as a Margin order.
