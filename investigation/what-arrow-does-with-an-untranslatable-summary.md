# What Arrow does with a summary it cannot evaluate

Investigated: 2026-08-25

Measured on R 4.6.1, arrow 25.0.1, dplyr 1.2.1, except where a version is
named. Version history was read from `apache/arrow` at its release tags.

## The failure

`summarize_with_margins()` on an Arrow table, given a summary expression Arrow
could not evaluate, aborted with `object of type 'special' is not subsettable`,
class `notSubsettableError/error/condition`. `dplyr::summarize()` on the same
table and the same expression returned the right answer.

`arrow:::try_arrow_dplyr()` runs
`try(evalq(call <- match.call(), parent), silent = TRUE)`. Reached through a
wrapper that forwards the caller's expressions in `...`, `match.call()` raises
`... used in a situation where it does not exist` — visible by setting
`options(arrow.debug = TRUE)`, which unsilences the `try()` — so `call` is
never bound. `arrow:::abandon_ship()` then reads it with
`get("call", envir = env)`, whose inheriting lookup finds `base::call`, and
`call$.data <- dplyr::collect(.data)` subsets an object of type `"special"`.

Tracing `arrow:::collect.ArrowTabular` showed it is called before the error:
R evaluates the right-hand side of the assignment first, so the whole input was
read on the failing path as well.

## What Arrow does instead of translating

Arrow warns and evaluates the expression in R. Two properties were measured
because the design turned on them.

**It collects the input as the verb received it.** Traced on a table with three
columns the summary did not mention, `collect.ArrowTabular` was called with
`k,v,junk1,junk2,junk3`. `abandon_ship()` reads `.data` from the verb's frame,
not the narrowed table `do_arrow_summarize()` built. The collect is not
ALTREP-deferred: 8M rows by 3 columns collected in 0.063s with non-ALTREP
columns.

**It happens only where no external system is involved.**
`arrow:::query_on_dataset()` answered `FALSE` for `Table`, `RecordBatch`, and an
`arrow_dplyr_query` over either, and `TRUE` for `Dataset` and a query over one.
The fallback fires on the `FALSE` set exactly; the `TRUE` set raises
`arrow_not_supported` instead, carrying `$call` (the blamed expression) and
`$body` ("Call collect() first to pull data into R.").

## Which expressions are absorbed

34 expressions were put to `dplyr::summarize()` on an Arrow table.

Translated: `sum` `mean` `min` `max` `dplyr::n()` `n_distinct` `sd` `var`
`median` `quantile` `any` `all` `sum(x, na.rm = TRUE)` `sum(x > 1)`,
`max()` of a character or date column, `n_distinct()` of a factor column, and
arithmetic composed on top of an aggregate — `sum(v)/dplyr::n()`,
`max(v) - min(v)`, `round(mean(v), 2)`, `as.integer(sum(v))`,
`mean(dplyr::if_else(...))`.

Absorbed: `dplyr::first` `dplyr::last` `dplyr::nth`, `length(unique(v))`,
`paste(collapse=)` `toString`, `sum(v[v > 1])` `mean(v[s == "a"])`,
`stats::cor` `stats::weighted.mean`.

Two findings bear on how the boundary can be described.

`sum(v[v > 1])` is absorbed although `sum` is at its head and `sum` is
translated; it is the `[` that fails. And a user-written closure is *not*
automatically absorbed: `myfun <- function(x) sum(x)/length(x)` gave `myfun(v)`
an `arrow_dplyr_query`, Arrow's mask evaluating the body symbolically. So
neither the head of a call nor who wrote the function decides it. Only the
composition of primitives does, and only Arrow can answer.

Translatability did not vary with the grouping set: 18 expressions were put to
each of `.by = character()`, one column, and two columns, with no expression
differing across the three.

## How an absorption can be recognised

The warning carries nothing but its text: class `rlang_warning/warning/condition`
with `$parent` `NULL`, `$call` `NULL`, and an empty `$footer`.

Its wording changed inside the range `DESCRIPTION` admits. `arrow (>= 13.0.0)`
is the floor, and at tags 13.0.0, 14.0.0, 15.0.0, and 16.0.0 `abandon_ship()`
lived in `r/R/dplyr.R` and wrote

```r
warning(msg, "; pulling data into R", immediate. = TRUE, call. = FALSE)
```

while from 17.0.0 it lives in `r/R/dplyr-eval.R` and writes an rlang warning
whose body ends `"Pulling data into R"`. What survived is the phrase and not its
capitalisation, which is why the marker in `R/grouping-adapter-union.R` is
matched case-insensitively.

Two further differences follow from the same rewrite. Through 16.0.0
`abandon_ship(call, .data, msg)` takes the originating call as an argument, so
the `match.call()` defect above does not arise there at all — the untyped error
is reachable only from 17.0.0. And the older wording names the offending
expression inside a sentence rather than on an `In <expr>: ` line, so the label
cannot be placed from it and the refusal names every summary argument.

No option controls any of this: all 23 `arrow.*` options referenced in the
arrow namespace were enumerated and none of them does, and arrow's `NEWS.md`
carries no entry for the behaviour.

## Refusing costs no read

A calling handler that stops when the warning is raised refuses with zero calls
to `collect.ArrowTabular`, in both the `...`-forwarding shape and one that
splices the caller's expressions at the call site, because Arrow warns before it
collects.

A zero-row probe on `head(.data, 0L)` also identifies an absorbed expression —
by the class of what `summarize()` returns rather than by any text — at a
constant ~14 ms independent of input size, and one probe over all the summaries
detects any absorbed one among them. It was not adopted: `head()` turns a
`Dataset` into a `Table`, so it would have to tell absorbing and refusing inputs
apart before probing, and for an `arrow_dplyr_query` that means reading Arrow's
internal structure.

The whole test suite was run twice against a rebuilt branch summary — once
refusing, once absorbing — and reported 4807 passing, 0 failing, 0 skipped, and
0 warnings both times. No test in the suite reaches Arrow's fallback, which is
what let the defect ship.

## What was decided from this

ADR 0025 refuses a summary an Absorbing backend would read the input to compute.
ADR 0020's amendment withdraws the claim that an in-memory Arrow table cannot be
told from a dataset in object storage. `CONTEXT.md` defines *Absorbing backend*.
Those are authoritative for the decision and for current state; this note is
authoritative only for what was measured on the date above.
