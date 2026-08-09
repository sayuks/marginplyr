# Restoring knitr hooks a vignette installs

Investigated: 2026-08-10

Read while #116 moved the `must_error` chunk option out of
`vignettes/recipes.qmd` into one definition every vignette can source. Two of
that ticket's three defects were about state: the option left an `opts_hooks`
entry and an `evaluate` wrapper installed, and it asserted only that some error
occurred. The question here was which knitr mechanism can undo an installation
made *during* a render, and where the raised condition can be read from.
`requiring-a-documentation-chunk-to-fail.md` covers whether the assertion can
be expressed at all, and is not superseded by anything below.

## Environment

| Component | Version |
|---|---|
| R | 4.6.1 |
| knitr | 1.51 |
| Quarto CLI | 1.9.38 |
| rlang | 1.3.0 |

## knitr restores neither of the two things such an option installs

Read from `deparse(body(knitr::knit))` in knitr 1.51.

`opts_hooks` appears in no `on.exit()` at all. The four settings objects knitr
snapshots and restores are `opts_chunk`, `opts_current`, `knit_code`, and
`opts_knit`; `opts_hooks` is not among them, so an entry added by a setup chunk
outlives the render.

`knit_hooks` is restored, but conditionally and only to knitr's defaults:

```r
if (identical(knit_hooks$get(names(.default.hooks)), .default.hooks) &&
      !child_mode()) {
  getFromNamespace(paste("render", out_format(), sep = "_"), "knitr")()
  on.exit(knit_hooks$set(.default.hooks), add = TRUE)
}
```

The condition is evaluated before any chunk runs, so it says something about
the caller's state on entry and nothing about a hook installed while knitting.
A render started from customised hooks gets no restoration, and one started
from defaults gets the defaults back rather than whatever the definition
replaced.

## `after.knit` is the hook that runs on both paths

`knit()` registers `on.exit(run_hook("after.knit"), add = TRUE)`, and
`run_hook()` reads `knit_hooks$get(.name)` at exit time rather than at
registration time. A hook installed mid-render is therefore still called, and
called on the halted path as well as the completed one — which is what a render
stopped by the assertion needs.

Two orderings make this work, and both were checked rather than assumed:

- `after.knit` is not among the twelve names in `.default.hooks` (`source`,
  `output`, `warning`, `message`, `error`, `plot`, `inline`, `chunk`, `text`,
  `evaluate.inline`, `evaluate`, `document`). The `knit_hooks$set(.default.hooks)`
  exit handler above is registered first and so runs first, and it leaves
  `after.knit` in place.
- It also runs at the end of a *child* document's `knit()`, with
  `opts_knit$get("child")` still `TRUE` there, because `knit_child()` clears
  that flag from its own frame after `knit()` returns. A restoration that does
  not test it uninstalls the option while the parent still has chunks to run.

The `document` hook was the alternative considered. It is called at
`res = one_string((knit_hooks$get("document"))(res))`, inside the body, so it
does not run when a chunk halts the render — the case that matters most.

## The condition object is already in the `evaluate` results

`evaluate::evaluate()` returns the condition itself for a failing expression,
not its rendered text, so the `evaluate` hook the option already wraps can read
`class()` and rlang's `parent` chain from what it is handed. No second
mechanism is needed to assert a class, and none of the rendering behaviour
recorded in `requiring-a-documentation-chunk-to-fail.md` changes.

The chain matters because the errors a vignette shows are usually wrapped.
`retail_sales |> summarize_with_margins(...) |> filter(grouping_bit(channel) == 1L)`
raised `rlang_error/error/condition` at the top with `marginplyr_error` on its
`parent`; an assertion reading only the outermost class could say nothing about
which package refused the call.

## knitr is not optional during a vignette rebuild

Relevant because a definition sourced from a vignette calls `knitr::` at
top level, and `_R_CHECK_DEPENDS_ONLY_=true` is the authority on whether a
Suggest is optional. It is not reachable here: `DESCRIPTION` declares
`VignetteBuilder: quarto`, `R CMD check` makes the VignetteBuilder visible
while rebuilding vignettes even in that mode, and
`packageDescription("quarto")$Imports` names `rmarkdown`, whose `Imports` names
`knitr (>= 1.43)`. So knitr sits in the hard dependency closure of the builder
whenever a vignette is being built — the same shape as the `DBI = FALSE` entry
in `tests/testthat/helper-optional-backends.R`, and a guard written against
knitr in a vignette would never fire.

`AGENTS.md` is authoritative for what the option now does and for the rule
about when to mark a chunk.
