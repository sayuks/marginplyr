# Tidyselect helper lookup during local name planning

Investigated: 2026-11-03
Tested package SHA: `7925e0309b2650e56aec4c975bf1f5bed03eeda6`
Environment: R 4.6.1, dplyr 1.2.1, tidyselect 1.2.1, rlang 1.3.0

An incremental review proposed that a caller-defined bare `all_of()` would
execute during the new local name cache's selection resolution. A direct
ordinary/Margin comparison refuted that specific premise. With a caller
`all_of` function that incremented a counter and stopped, both calls succeeded
and the counter remained zero. Each call selected `x` from `data.frame(x =
1:3, y = 5:7)` after `prior = sum(x)`; the Margin call also requested a Total
share of `prior`. Both used `.names = "{.col}_{seq_along(.col)}"` and returned
`x_1 = 6L`.

The inspected installed `tidyselect::vars_select_helpers` contained `all_of`.
Its context-mask binding explained why the caller binding did not run.
`tidyselect:::call_kind()` separately classified syntactic selection operators
only when the call head was a symbol.

A helper's ordinary argument had a different result. For
`dplyr::all_of(c("x", "y"))`, a caller `c` function recorded `dplyr::n()` and
returned `base::c(...)`. Ordinary dplyr called it once with `n() = 0L` while
expanding the summary. The tested Margin cache instead failed with `Must only
be used inside data-masking verbs`, before a summary mask existed. This
confirmed premature caller evaluation at the helper-argument boundary, rather
than at the bare helper's function binding.

Both controls loaded an archived copy of the pinned package SHA with
`pkgload::load_all()`. The combined assertion script completed with exit zero;
its desired-contract Margin call for the helper argument was captured as an
error. No suite, build or package check was run for this measurement. The
review distinction was established by execution, not by assuming every name in
a selection used the same lookup rules.
