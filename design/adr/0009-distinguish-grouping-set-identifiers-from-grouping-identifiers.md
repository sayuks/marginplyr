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
`inspect_grouping()$set_id` is exactly the value produced by a margin verb
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
