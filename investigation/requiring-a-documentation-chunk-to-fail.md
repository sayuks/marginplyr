# Requiring a documentation chunk to fail

Investigated: 2026-08-07

Searched while deciding how `vignettes/recipes.qmd` (#82) should show the
calls it says are rejected. The question was whether a chunk can be made to
*require* an error rather than merely tolerate one, so that prose asserting a
failure cannot outlive the failure. The evidence for what those calls do is in
`margin-order-and-plan-joins.md`; this note covers only the mechanism.

## Environment

| Component | Version |
|---|---|
| R | 4.6.1 |
| knitr | 1.51 |
| Quarto CLI | 1.9.38 |
| rlang | 1.3.0 |

## Nothing supplies the assertion

knitr 1.51 exposes one chunk option in this area, `error`. Enumerating
`knitr::opts_chunk$get(default = TRUE)` and matching names against
`err|cond|expect|assert|fail` returned `error` alone. Its `TRUE` value permits
an error without requiring one. `knitr::knit_hooks$get()` names twelve hooks —
`source`, `output`, `warning`, `message`, `error`, `plot`, `inline`, `chunk`,
`text`, `evaluate.inline`, `evaluate`, `document` — and the `error` hook is
called only when an error occurs, so its absence cannot be observed through
it.

Quarto 1.9.38 documents `error: true` with the same permissive meaning.

No package supplying the assertion was found. The knitr and rmarkdown issue
trackers discuss only the permissive direction (yihui/knitr#2366,
rstudio/rmarkdown#149).

## Two implementations were built and compared

**A helper that catches the condition.** Wrapping each call in a function that
`tryCatch`es and re-`stop()`s on success is the shorter form, and needs no
knitr internals. It renders the condition object rather than knitr's error
output:

```
<error/rlang_error>
Error in `left_join()`:
! `x` and `y` must share the same source.
---
Backtrace:
 1. ├─global must_fail(left_join(report, plan, by = c(set = "set_id")))
 2. │ └─base::tryCatch(...)
 3. │   └─base (local) tryCatchList(expr, classes, parentenv, handlers)
```

The header and the three frames through the helper are not what a reader would
see in their own console.

**A wrapper around knitr's `evaluate` hook.** The hook receives the evaluated
result objects, and `knitr::opts_current$get()` supplies the chunk's options
inside it, so one hook can both preserve knitr's own error rendering and
inspect what the chunk produced. Twenty lines against forty-one for a version
built from the `error` hook plus a chunk hook plus an environment to carry
state between them.

Measured behaviour of the `evaluate`-hook version, on a document with a chunk
marked to require an error:

| Case | Result |
|---|---|
| chunk raises an error | renders; knitr's error output appears in the HTML |
| chunk completes normally | `quarto render` exits 1, no HTML, message names the chunk |
| chunk withheld by `eval: false` | renders; no assertion |
| option set without `error: true` | cannot occur; an `opts_hooks` entry supplies it |

The third row is a property of knitr rather than of the implementation: knitr
does not call the `evaluate` hook for a chunk it does not evaluate. A version
built from the `error` hook had to test `options$eval` explicitly, and failed
the case before that test was added — a guarded chunk that never ran was
reported as a chunk that had stopped failing, which would break a
`_R_CHECK_DEPENDS_ONLY_` build.

`AGENTS.md` is authoritative for which version the repository uses and for the
rule about when to mark a chunk.
