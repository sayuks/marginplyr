# What session state costs an R package, and what shape the last-call precedents take

Investigated: 2026-09-02

#318 justifies a `last_marginplyr_sql()` accessor on two claims: that
`rlang::last_error()` is the ecosystem precedent for this shape of problem, and
that marginplyr has no package-environment pattern of its own. #377 records
that the second is false. This note records what was read and measured about
the first, and about the four other questions #380 asks: what CRAN policy and
*Writing R Extensions* permit, what happens to a package environment under
fork, PSOCK, and callr, whether a user-written `withCallingHandlers()` is an
idiom the ecosystem actually expects, and what `R CMD check` does to state that
outlives a call.

It records evidence only. The verdict on whether marginplyr gains such an
accessor is #383's.

Everything below was read from package sources downloaded from CRAN
(`download.packages(type = "source")`) rather than from documentation, and
measured on macOS 15 (arm64), R 4.6.1 (2026-06-24), with rlang 1.3.0,
dplyr 1.2.1, dbplyr 2.6.0, testthat 3.3.2, httr2 1.3.0, lifecycle 1.0.5,
duckplyr 1.2.1, ggplot2 4.0.3.

## 1. What CRAN policy and WRE permit

### CRAN Repository Policy

Read at https://cran.r-project.org/web/packages/policies.html on 2026-09-02.
Three sentences bear on run-time state, and none of them reaches a package's
own environment:

> A package must not tamper with the code already loaded into R: any attempt to
> change code in the standard and recommended packages which ship with R is
> prohibited. Altering the namespace of another package should only be done
> with the agreement of the maintainer of that package.

> Packages should not modify the global environment (user's workspace).

> Packages should not write in the user's home filespace (including
> clipboards), nor anywhere else on the file system apart from the R session's
> temporary directory (or during installation in the location pointed to by
> `TMPDIR`: and such usage should be cleaned up).

The prohibitions name *another* package's namespace and the *global*
environment. The policy has no sentence about a package writing to an
environment it created itself. This is the finding: the permission is an
absence, not a grant.

The third sentence is quoted because it is the same paragraph and because it
does bind a different feature #318 bundles — an on-disk log at a caller-chosen
path. That is #382's question and is not pursued here.

### Writing R Extensions

Read at https://cran.r-project.org/doc/manuals/r-release/R-exts.html on
2026-09-02. Its statement on sealing, §1.5 *Package namespaces*:

> Namespaces are sealed once they are loaded. Sealing means that imports and
> exports cannot be changed and that internal variable bindings cannot be
> changed.

Sealing binds *bindings*, not the objects they point at. WRE's §1.5.3 *Load
hooks* says nothing about environments created in a hook, and a search of the
whole manual for "user's workspace", "package state", and "store … state"
returned nothing on the subject. So WRE neither permits nor forbids a
package-level environment; it describes the mechanism that makes one work, and
stops.

Measured, to confirm that reading (`/tmp/seal.R`):

```r
e <- asNamespace("rlang")
cat("environmentIsLocked(ns):", environmentIsLocked(e), "\n")
cat("bindingIsLocked('the', ns):", bindingIsLocked("the", e), "\n")
r1 <- tryCatch({assign("the", 1, envir = e); "ok"}, error = conditionMessage)
cat("assign('the', 1, envir = ns):", r1, "\n")
r2 <- tryCatch({assign("zzz_new", 1, envir = e); "ok"}, error = conditionMessage)
cat("assign('zzz_new', 1, envir = ns):", r2, "\n")
the <- get("the", envir = e)
cat("environmentIsLocked(the):", environmentIsLocked(the), "\n")
r3 <- tryCatch({the$zzz_probe <- 1; "ok"}, error = conditionMessage)
cat("the$zzz_probe <- 1:", r3, "-> value", the$zzz_probe, "\n")
```

```
environmentIsLocked(ns): TRUE
bindingIsLocked('the', ns): TRUE
assign('the', 1, envir = ns): cannot change value of locked binding for 'the'
assign('zzz_new', 1, envir = ns): cannot add bindings to a locked environment
environmentIsLocked(the): FALSE
the$zzz_probe <- 1: ok -> value 1
```

Writing to the namespace fails both ways — rebinding an existing name and
adding a new one. Writing *into* an environment the namespace binds succeeds,
because the binding is untouched. Every precedent in §2 uses that second form.

### What `R CMD check --as-cran` raises

Measured. A throwaway package `statepkg` was built holding one top-level
`the <- new.env(parent = emptyenv())`, one function that appends to it, one
that reads it back, and one that returns `Sys.getpid()`; with two testthat
files and one knitr vignette, all of which read and write it.

```sh
R CMD build statepkg
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran --no-manual statepkg_0.0.1.tar.gz
```

```
Status: 1 NOTE
* checking CRAN incoming feasibility ... [2s/14s] NOTE
Maintainer: ‘A B <ab@example.com>’

New submission
```

The only NOTE is the one every unpublished package gets. Nothing in
`--as-cran` — not the codetools pass, not the namespace check — reported the
environment or the writes to it.

### Where a top-level `new.env()` lives

`new.env()` at the top level of an `R/` file runs at *install* time and is
serialized into the package's lazy-load database, so each session deserializes
its own copy. Measured against the installed `statepkg`:

```
session A, log at start:  (empty)
session A, log at end:   written-in-session-A
session A, after unload+reload:  (empty)
session B, log:  (empty)
```

State does not survive `unloadNamespace()` + `library()` in the same session,
and does not reach a second session. It is session state, not persistent state.

## 2. The precedents' actual shape

A scan of all 293 installed packages' exports for `^last_|_last$` found seven
packages holding a genuine last-call accessor, eight accessors between them.
Excluded from the count after inspection: `last_col()`, a tidyselect helper
re-exported by seven packages; the `stri_*_last()` string helpers;
`gh::gh_last()`, which follows an HTTP pagination link; and `rhub::last_check()`,
whose body is `deprecated()`.

| accessor | held in | written | reset | shape | opt-in |
| --- | --- | --- | --- | --- | --- |
| `rlang::last_error()` | `the$last_error` | at unhandled-error time | never | one slot, overwritten | no |
| `rlang::last_warnings()` / `last_messages()` | `the$last_warnings` / `the$last_messages` | on each entraced condition | on first push of a new top-level command | list, appended | yes, `global_entrace()` |
| `dplyr::last_dplyr_warnings()` | `the$last_warnings` | after a verb finishes | on first push of a new top-level command | list, appended | no |
| `lifecycle::last_lifecycle_warnings()` | `warnings_env$warnings` | on each deprecation warning | on first push of a new top-level command | list, appended | no |
| `dbplyr::last_sql()` | `the$last_sql` | in `db_sql_render()` | never | one slot, overwritten | no |
| `httr2::last_request()` / `last_response()` | `the$last_request` / `the$last_response` | at the *start* of `req_perform1()` | request set, response set to `NULL`, both per call | one slot each, overwritten | no |
| `duckplyr::last_rel()` | `duckplyr_the$last_rel` | from a `duckdb.materialize_callback` hook | never | one slot, overwritten | no |
| `ggplot2::last_plot()` | closure over `.last_plot` in `.store` | on plot creation/print | never | one slot, overwritten | no |

Every one is held in a package-level container created at the top level of an
`R/` file. Six packages use `new.env(parent = emptyenv())` or rlang's
`new_environment()`; ggplot2 alone uses a closure.

### `rlang::last_error()` — the precedent #318 names

`R/aaa.R:1`:

```r
the <- new.env(parent = emptyenv())
```

`R/cnd-last.R`:

```r
last_error <- function() {
  err <- peek_last_error()

  if (is_null(err)) {
    local_options(rlang_backtrace_on_error = "none")
    stop(
      "Can't show last error because no error was recorded yet",
      call. = FALSE
    )
  }
  ...
}

peek_last_error <- function(cnd) {
  the$last_error
}
poke_last_error <- function(cnd) {
  the$last_error <- cnd
}
```

Three properties of this precedent differ from what #318 proposes for
`last_marginplyr_sql()`:

- It is written at *error* time, not at call start. `poke_last_error()` is
  called from five sites (`R/cnd-abort.R:896`, `R/cnd-entrace.R:146`, `:212`,
  `:379`, `R/cnd-message.R:341`); the comment at `cnd-abort.R:895` is
  "Save the unhandled error for `rlang::last_error()`". Nothing resets it.
- It holds one slot, overwritten. `last_error()` is not an accumulator and
  cannot report more than one error.
- It **errors** when nothing has been recorded. #318 specifies the opposite —
  "zero rows if nothing signaled … it never errors".

`last_warnings()` and `last_messages()` are the accumulators, and their reset
is not "at the start of a call". It is lazy, on write, keyed on the address of
the top-level frame (`R/cnd-last.R`):

```r
push_condition <- function(cnd, last) {
  top <- obj_address(cmd_frame())

  if (has_new_cmd_frame(top)) {
    the$last_top_frame <- top
    the[[last]] <- list(cnd)
    the$n_conditions <- 1L
  } else {
    the[[last]] <- c(the[[last]], list(cnd))
    ...
  }
}
```

rlang documents the heuristic's failure mode in a comment above it: if a GC
occurs between two commands and the new first frame reuses the old address,
"we'll wrongly keep collecting warnings instead of starting anew".

`cmd_frame()` carries a knitr special case — under `knitr_in_progress()` it
looks for a knitr frame rather than `sys.frame(1)` — which is direct evidence
that "one top-level command" is not a well-defined thing inside a rendered
document.

### `dplyr::last_dplyr_warnings()` and `lifecycle::last_lifecycle_warnings()`

Both copy rlang's heuristic. dplyr, `R/conditions.R:319`:

```r
# Flushes warnings if a new top-level command is detected
push_dplyr_warnings <- function(warnings) {
  last <- the$last_cmd_frame
  current <- obj_address(sys.frame(1))

  if (!identical(last, current)) {
    reset_dplyr_warnings()
    the$last_cmd_frame <- current
  }

  the$last_warnings <- c(the$last_warnings, warnings)
}

# Also used in tests
reset_dplyr_warnings <- function() {
  the$last_warnings <- list()
}
```

Two things this records. The reset is on write, not at call start — the state
of a finished command is still readable until the next command writes. And
`reset_dplyr_warnings()` exists partly for dplyr's own test suite; the comment
says so.

`last_dplyr_warnings()` is documented `@keywords internal`, so it is exported
but not indexed as part of dplyr's public surface. `last_error()`,
`last_sql()`, `last_response()`, `last_rel()`, and
`last_lifecycle_warnings()` are not marked internal. `ggplot2::last_plot()` is.

Both accumulators bound what they hold, because holding it is not free.
dplyr's handler (`R/conditions.R:301`):

```r
# Don't entrace more than 5 warnings because this is very costly
if (is_null(cnd$trace) && length(state$warnings) < 5) {
```

rlang counts pushed conditions against `max_entracing()`, default 20, citing
rlang#1473.

### `dbplyr::last_sql()` — the closest analogue, and it already exists

dbplyr 2.6.0 exports it. NEWS for 2.6.0: "`last_sql()` retrieves the most
recent SQL query generated by dbplyr, which is useful for debugging (#1471)."

`R/remote.R:127-142`:

```r
#' Retrieve the last SQL query generated
#'
#' This is a helper function that retrieves the most recent SQL query generated
#' by dbplyr, which can be useful for debugging.
#'
#' @return A SQL string, or `NULL` if no query has been generated yet.
#' @export
last_sql <- function() {
  the$last_sql
}
```

It is written in one place, `db_sql_render()` (`R/db.R:61-79`):

```r
  out <- db_sql_render_dispatch(con, sql, ..., sql_options = sql_options)
  the$last_sql <- out
  return(out)
```

Never reset. Not behind an option. Written on every render, whether or not
anyone will read it.

`db_sql_render()` is the funnel: `collect()` (`R/verb-collect.R:48`),
`compute()` (`R/verb-compute.R:57`), `collapse()` (`R/verb-collapse.R:17`),
`do()` (`R/verb-do.R:44`), and `remote_query()` (`R/remote.R:118`) all route
through it. So for a dbplyr-backed query, "the last SQL dbplyr rendered" is
already retrievable without any marginplyr code. What it cannot do is report
more than one — `the$last_sql` is a single slot, and a marginplyr call that
renders a probe, a schema proxy, a label check, and a result overwrites it four
times.

### `httr2::last_request()` / `last_response()` — the only reset-at-call-start

`R/req-perform.R:192`, the first two lines of `req_perform1()`:

```r
  the$last_request <- req
  the$last_response <- NULL
  signal(class = "httr2_perform")
```

and `create_response()` (`R/resp.R:138`) fills `the$last_response` when one
arrives.

This is the one precedent whose state is cleared at the start of the call, and
what it clears is one slot it is about to fill — not a log that then
accumulates within the call. It is also the one precedent that does both: it
holds the state *and* signals a condition (`httr2_perform`) at the same site.

Its accessors return `NULL` when nothing was recorded; the `_json()` variants
`cli_abort()`.

### `duckplyr::last_rel()` and `ggplot2::last_plot()`

duckplyr writes from a hook installed on load rather than from its own call
sites (`R/last.R`):

```r
last_rel <- function() {
  duckplyr_the$last_rel
}

# Ellipsis for future extensions
last_rel_store <- function(rel, ...) {
  duckplyr_the$last_rel <- rel
}

on_load({
  options(duckdb.materialize_callback = last_rel_store)
})
```

ggplot2 uses the pre-environment idiom, a closure (`R/plot-last.R`):

```r
.plot_store <- function() {
  .last_plot <- NULL

  list(
    get = function() .last_plot,
    set = function(value) .last_plot <<- value
  )
}
.store <- .plot_store()
```

### Answering #380's question directly

Of the eight accessors, exactly one (httr2) clears state at the start of a
call. Three
(rlang's warnings and messages, dplyr, lifecycle) append and flush lazily on
the first write of a new top-level command, which is not the same thing: the
previous command's record stays readable until something replaces it. Four
(rlang's error, dbplyr, duckplyr, ggplot2) hold a single slot and never reset
at all.

None of the eight is a per-call log that is emptied at call start and then
appended to several times within the call, which is the shape #318 specifies.

### marginplyr's own precedent

`R/share.R:2301`:

```r
share_dialect_verdicts <- new.env(parent = emptyenv())
```

It is the only *package-level* environment in `R/`. `R/conditions.R:95` also
calls `new.env(parent = emptyenv())`, but inside `new_branch_conditions()`,
so that one is created per call and dies with it — the two are not the same
pattern, and #377's reading of `share.R:2301` as the sole instance is right.

Written at `R/share.R:2275`, read at `:2254`, never reset — a memo, in the
shape of the four never-reset precedents above and the opposite of #318's.

The suite records what the pattern costs there. `tests/testthat/test-share-backends.R:1174-1185`
carries two helpers built only for it:

```r
empty_share_dialect_verdicts <- function() {
  rm(
    list = ls(share_dialect_verdicts, all.names = TRUE),
    envir = share_dialect_verdicts
  )
}

restore_share_dialect_verdicts <- function(saved) {
  empty_share_dialect_verdicts()
  list2env(saved, envir = share_dialect_verdicts)
  invisible(NULL)
}
```

with four `empty_share_dialect_verdicts()` calls and three
save-and-`on.exit(restore…)` pairs across the file. That is the measured price
of one package-level environment in this repository's own tests.

## 3. Concurrency

Measured against `dbplyr::last_sql()`, since it is a real last-call accessor
with no database required (`dbplyr::lazy_frame()` renders without a
connection). `/tmp/conc.R`, run under `Rscript`:

```r
suppressMessages({library(dplyr); library(dbplyr)})
lf <- lazy_frame(x = 1:3, .name = "df")

invisible(capture.output(lf |> filter(x > 1) |> show_query()))   # parent render

seen <- parallel::mclapply(1:2, function(i) {
  before <- as.character(dbplyr::last_sql())
  invisible(capture.output(lf |> mutate(child = x + i) |> show_query()))
  list(before = before, after = as.character(dbplyr::last_sql()))
}, mc.cores = 2)

cl <- parallel::makeCluster(2, type = "PSOCK")
parallel::clusterEvalQ(cl, suppressMessages({library(dplyr); library(dbplyr)}))
psock <- parallel::clusterApply(cl, 1:2, function(i) { ... })

out <- callr::r(function() { ... })
```

Output, abbreviated to the lines that matter:

```
00 parent, before anything:  NULL
01 parent, after PARENT render:  <SQL> SELECT *
FROM "df"
WHERE ("x" > 1.0)
10 fork child 1 saw BEFORE: SELECT *
FROM "df"
WHERE ("x" > 1.0)
11 fork child 1 saw AFTER:  SELECT *, "x" + 1 AS "child"
FROM "df"
12 parent after mclapply:   <SQL> SELECT *
FROM "df"
WHERE ("x" > 1.0)
20 PSOCK worker 1 saw BEFORE: NULL
21 PSOCK worker 1 saw AFTER:  SELECT *, "x" + 1 AS "child"
FROM "df"
22 parent after clusterApply: <SQL> SELECT *
FROM "df"
WHERE ("x" > 1.0)
30 callr saw BEFORE: NULL
31 callr saw AFTER:  SELECT *, "x" + 99.0 AS "child"
FROM "df"
32 parent after callr::r: <SQL> SELECT *
FROM "df"
WHERE ("x" > 1.0)
```

Three facts, one of them asymmetric:

- **Fork (`mclapply`).** The child *inherits* the parent's state at fork time —
  child 1 read back the parent's `WHERE ("x" > 1.0)`, a query the child never
  issued. Writes in the child do not reach the parent: after `mclapply()`
  returned, the parent still read its own pre-fork value.
- **PSOCK.** Workers start empty (`NULL`) because the package is loaded
  separately in each. Writes do not reach the parent.
- **callr.** Same as PSOCK: starts empty, writes do not reach the parent.

So a last-call accessor read after a parallel run does not report the calls
that ran in parallel. It silently reports the last call that ran *serially* in
the calling process — or, in the fork case inside the worker, a call from
before the fork. There is no error and no warning; the value is just stale.

**Do the precedents document a position on this?** No. Grep over
`man/last_sql.Rd`, `man/last_response.Rd`, `man/last_error.Rd`,
`man/last_warnings.Rd`, `man/last_dplyr_warnings.Rd`, and `man/last_rel.Rd`
for `parallel`, `fork`, `subprocess`, and `process` returned nothing. Six
accessors from five packages, and not one of them says what it means in a
session that ran calls in parallel.

## 4. Is a user-written `withCallingHandlers()` the idiom?

Two scans of the installed library (293 packages, R 4.6.1).

**Rd corpus.** Every Rd topic of every installed package, read with
`tools::Rd_db()` and searched for `withCallingHandlers`:

```
packages scanned: 293
Rd topics mentioning withCallingHandlers: 15
        pkg                         topic
       base             StackOverflows.Rd
       base                    browser.Rd
       base                 conditions.Rd
       base                       stop.Rd
   evaluate         new_output_handler.Rd
      httr2                  req_error.Rd
      lintr         nested_pipe_linter.Rd
      lintr unnecessary_nesting_linter.Rd
        pak                    sysreqs.Rd
 parallelly                   killNode.Rd
      purrr        purrr_error_indexed.Rd
      rlang               cnd_inherits.Rd
      rlang               is_installed.Rd
      rlang       topic-error-chaining.Rd
      rlang                  try_fetch.Rd
```

Four are base R's own condition documentation. Four are rlang's, documenting
the condition system itself. Two are lintr rules *about* the expression. The
remainder (`httr2::req_error`, `purrr::purrr_error_indexed`,
`pak::sysreqs`, `evaluate::new_output_handler`, `parallelly::killNode`) are
about handling *errors* or capturing output, not about observing a package's
ordinary activity.

**Vignette corpus.** All 1386 installed `*/doc/*` files (`.Rmd`, `.R`, `.md`,
`.html`):

```
installed vignette/doc files scanned: 1386
files hitting withCallingHandlers: 1
   ggplot2/doc/profiling.html
```

One hit in 1386, in a profiling vignette, incidental.

The finding is the asymmetry, not either number alone: seven installed
packages ship a stateful last-call accessor as the documented way to read back
what happened, and no installed package documents a user-written
`withCallingHandlers()` on a custom condition as the way to observe its
ordinary operation. The one package that signals a condition at exactly the
site this question is about — httr2, `signal(class = "httr2_perform")` — does
not document it, and ships `last_request()` / `last_response()` as the
user-facing route to the same information.

Nothing here was searched for and found *against* the accessor idiom. What was
searched for and not found is the counter-example: a package saying "wrap your
call in `withCallingHandlers()` to see what we did".

## 5. Interaction with `R CMD check`

Measured with the `statepkg` probe from §1. `state_log()`'s example runs first
(topics enter `pkg-Ex.R` alphabetically), pushes `"B"`; `state_push()`'s example
runs second and pushes `"A"`. From `statepkg.Rcheck/statepkg-Ex.Rout`:

```
> nameEx("state_log")
> cat("state_log() at start of example B:", state_log(), "\n")
state_log() at start of example B:
> state_push("B")
> state_log()
[1] "B"

> nameEx("state_push")
> state_push("A")
> state_log()
[1] "B" "A"
```

The second example read back state the *first* example wrote. WRE's promise
about examples is about the evaluation environment only:

> Each example is run in a 'clean' environment (so earlier examples cannot be
> assumed to have been run), and with the variables `T` and `F` redefined to
> generate an error unless they are set in the example

A package environment is not in that clean environment. All examples run in one
process, and package-level state carries between them, in alphabetical topic
order. So an example asserting that `last_x()` equals what *this* example
produced is order-dependent, and adding or renaming an alphabetically earlier
topic can change it.

Tests behave the same way. `statepkg.Rcheck/tests/testthat.Rout`:

```
TEST-A pid: 49052 log:
TEST-B pid: 49052 log: test-a
```

One process for the whole `testthat.R` run, and `test-b.R` read what
`test-a.R` wrote. That is what `dplyr`'s `reset_dplyr_warnings()` — "Also used
in tests" — and this repository's own `empty_share_dialect_verdicts()` exist
for.

Vignettes are the exception. The vignette ran in its own process and saw
nothing:

```
VIGNETTE pid: 48542 log:
```

Three further facts about flakiness:

- **testthat's parallel mode changes the picture, and marginplyr does not use
  it.** `testthat/R/parallel.R`'s header: "Subprocesses run `callr::r_session`
  R sessions. They are re-used, one R session can be used for several
  `test_file()` calls." So under `Config/testthat/parallel: true`, state does
  not reach the main process, but *can* still leak between whichever files
  happen to share a reused worker — which is not deterministic. marginplyr's
  `DESCRIPTION` sets `Config/testthat/edition: 3` and no `parallel` key, so
  today the suite is the single-process case above.
- **knitr breaks the top-level-command heuristic**, and rlang says so in code
  rather than prose: `cmd_frame()` (`rlang/R/cnd-last.R`) branches on
  `knitr_in_progress()` and looks for a knitr frame instead of `sys.frame(1)`.
- **lifecycle withholds its own example for this reason.** `R/warning.R:15`:

  ```r
  #' # These examples are not run because `last_lifecycle_warnings()` does not
  #' # work well within knitr and pkgdown
  #' \dontrun{
  ```

  A maintained tidyverse package concluded that its last-call accessor's
  example could not be run under check and pkgdown, and wrapped it in
  `\dontrun{}`.

For contrast, a single-slot never-reset accessor tests cleanly. dbplyr's whole
test for `last_sql()` (`tests/testthat/test-remote.R:101`):

```r
test_that("last_sql() retrieves the most recent query", {
  lf <- lazy_frame(x = 1:3, y = c("a", "b", "c"))

  capture.output(lf |> filter(x > 1) |> show_query())
  expect_match(last_sql(), "WHERE")

  capture.output(lf |> mutate(z = x + 1) |> show_query())
  expect_match(last_sql(), "\\+ 1")
})
```

It writes before every read and never asserts emptiness, so no earlier file's
state can reach it.

## What was searched for and not found

- Any sentence in the CRAN Repository Policy about a package writing to an
  environment it owns.
- Any sentence in *Writing R Extensions* about run-time package state, beyond
  the sealing rule that describes why the environment idiom works.
- Any `R CMD check --as-cran` diagnostic for a package-level environment
  written during a call.
- Any documentation, in six precedent accessors across five packages, of what
  the accessor means after a parallel run.
- Any installed package documenting a user-written `withCallingHandlers()` on
  its own condition class as the route to observing its ordinary operation.
- Any precedent whose state is emptied at the start of a call and then appended
  to more than once within it.
