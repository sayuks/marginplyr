# Finder brief

<!-- The coordinator copies this file to <SCRATCH>/brief-finder.md and
replaces every <UPPER-CASE> field. `design/agents/bug-hunt.md` is the
procedure it belongs to. -->

You are one of several independent investigators. Each has a different
starting point and none sees another's candidates. The subject is the R package
marginplyr at the pinned commit `<SHA>` (origin/main on <DATE>). The goal is
ordinary functional correctness: values, types, order, conditions, and side
effects (which queries are sent, and when). This is ordinary quality review of
an open-source R data-summary package.

`S` below is `<SCRATCH>`.

## Hard rules

- The current directory is a git worktree checked out at the pinned commit.
  It is read-only for you: do not edit, create or delete any file in it, do not
  run `git commit`, `git checkout`, `git stash` or anything that writes, and do
  not create, edit or comment on any GitHub issue or pull request. Read-only
  `git` and `gh` commands are fine.
- Every script you write goes under your lane directory `S/lanes/<lane>/`.
  Data files a script needs (CSV, JSON, Parquet, database files) go under
  `tempdir()`/`tempfile()` inside the script, so the script runs anywhere.
  Never write to `/tmp` directly.
- Observe the installed build, never the checkout. A candidate script loads
  marginplyr with **exactly one line**, marked with the trailing comment
  `# hunt-load`, and that line is the only place the private library appears:

  ```r
  .libPaths(c("<SCRATCH>/lib", .libPaths())); library(marginplyr) # hunt-load
  ```

  That library holds the pinned commit built outside the checkout. Do not call
  `pkgload::load_all()`, do not install or upgrade any package. Read the source
  in the checkout (`R/`, `tests/testthat/`, `man/`, `vignettes/`, `design/`,
  `CONTEXT.md`, `NEWS.md`, `investigation/`).
- A reproduction that needs several fresh processes does not write child
  scripts: the script re-invokes **itself** with an argument
  (`system2(file.path(R.home("bin"), "Rscript"), c(shQuote(<own path>), "<mode>"))`,
  own path from `commandArgs(FALSE)`'s `--file=`), and branches on
  `commandArgs(TRUE)`.
- Do not run the test suite, `tools/review-ready.R`, `R CMD check`,
  `devtools::test()` or `testthat::test_file()`. Small R scripts of your own are
  the method. The AGENTS.md "local checks" instructions do not apply to you:
  you change nothing and report only.
- Do not spawn subagents. Do the work yourself.
- The structured return is the whole report. The harness refuses a report file
  written by a subagent, so write only scripts.
- Sandbox facts: `python3` does not work (use R, jsonlite for JSON). Put R code
  in a script file and run `Rscript file.R`; `Rscript -e` with regular
  expressions breaks under zsh quoting. There is no TCP network; do not
  download, and do not run DuckDB `INSTALL`.
  Read an issue with `gh issue view <n> --json title,body,labels,comments`:
  `--comments` prints nothing here. Run `gh` from the checkout.
  Every observation you report must come from a fresh `Rscript` process.

## Versions and engines

<VERSIONS: R and each dependency, as measured for this hunt.>

<ENGINES: for each optional engine that answered -- an older DuckDB library,
the DuckDB json extension, a PostgreSQL socket -- the one line that loads or
connects to it, and the lanes that may use it. A lane using PostgreSQL
creates one schema named `hunt_<lane>_<suffix>`, keeps every object in it,
drops it on exit, and confirms before reporting that none of its schemas
remains. Nobody starts, stops or reconfigures the cluster.>

## Where the contract lives

The authority is `CONTEXT.md`, the ADRs under `design/adr/` (their amendment
sections included), the public help in `man/*.Rd` (generated from roxygen in
`R/`), the vignettes under `vignettes/`, and `README.md`. A test under
`tests/testthat/` that pins a behaviour, and a closed issue that decided one,
are evidence that the behaviour is intended; they are not themselves the
contract, and a behaviour neither covers is open.

## Method

1. For each probe, state beforehand the expected value, type, order, condition
   and side effect.
2. Use public entry points only. Start from a working call and change one
   variable at a time. Keep input generation and expectation separate: an
   expectation comes from a contract sentence or from an independent
   calculation — a plain `dplyr::summarise()` at each explicit grain bound in
   Grouping-plan order, a hand division for a share, a row-copy oracle for an
   expansion or a nested cell, native dbplyr on the same connection for a
   database. **Never take an expectation from marginplyr internals
   (`marginplyr:::`) or from two marginplyr paths agreeing** (local and DuckDB
   agreeing is a lead, not an oracle). `marginplyr:::` may be used to *trace*
   or to locate a cause, never as the expected value.
3. Classify each difference as one of: a contract violation, the dependency's
   own behaviour, an intended refusal, or a wrong expectation of yours. There
   is no quota. An empty report is acceptable and is better than a weak
   candidate. Do not stop at the first finding; return to its neighbours and
   to other causes. Record every hypothesis that did not reproduce in one line.
4. Keep confidence (how sure the reproduction and the contract reading are)
   and severity (what it costs a user) as two separate judgements.
5. For conditions, use marginplyr's own vocabulary and contract: *Package
   condition*, *External condition*, *Condition context*, *Repeated condition*
   in `CONTEXT.md`, ADRs 0015, 0021–0024 and `?marginplyr_conditions`.
6. Two candidates that touch one function are still two candidates unless the
   cause and the repair are the same; do not merge them yourself.

## The native-control lesson (apply before counting anything)

Previous hunts lost candidates because the comparison object was the wrong
kind. Before a difference counts:

- Run the native control **on the same backend**, and make it the same kind of
  object marginplyr builds. #1625 compared `compute()` of a sorted SQLite
  Margin summary with native `arrange() |> compute()`, which returns an
  unordered table and promises no order; the right control was a native
  `arrange(tbl, g)`, which had the same `NULL` `remote_name()` and the same
  `union_all()` refusal. Closed `wontfix`.
- #1768's first control (a per-grain `union_all()` then `arrange()`) and
  #1794's first control (native dtplyr's ungrouped zero-output `summarise()`)
  both stopped a step short of what marginplyr composes; the refuter rebuilt
  the same composition natively. The findings stood only because the caller
  had written none of the failing steps.
- If plain dplyr, dbplyr, dtplyr, arrow or the database itself does the same
  thing on the equivalent input, it is the dependency's behaviour and is not
  counted — **unless the caller did not write the step that fails**. #1622 and
  #1640 were counted although native `select()` fails the same way, because the
  failing `select()` was marginplyr's own and the caller wrote none. #1641 was
  counted although native `summarise() |> arrange()` fails, because the caller
  wrote `.sort`, not `arrange()`.
- If the call has no meaning and fails natively too, only the wording differs
  (#1621, `grouping_bit(g)(1)`, `wontfix`).
- Ask whether the expected value is the contract's or yours. Rejected before:
  a PostgreSQL sorted summary following the database collation (native
  `arrange()` does the same; #1325); `nest_by_with_margins()` of a data.table
  carrying `.internal.selfref`; an sf expansion's class (ADR 0044); a dtplyr
  label-conversion error (native dtplyr raises the same bare error); `ORDER BY`
  before `UNION ALL` on a `lazy_frame()` (a real table collected correctly);
  a list-typed dimension failing at `collect()` on dtplyr and Arrow (native
  `group_by()` on a list column fails identically); Arrow typing `int64` as
  integer in the schema proxy (arrow's own `select(where(is.integer))` agrees).

## Decided shapes

Read `tools/hunt/decided-shapes.md` in the checkout before you start. A shape
it lists is not a finding; its neighbour is fair game. A repaired issue that
reproduces again at the pinned commit **is** a finding: say which.

<NOTES: the hunt notes under `investigation/` this lane must read so that it
starts elsewhere.>

## What to report

For each candidate, write a script `S/lanes/<lane>/cand-<k>.R` that runs with
`Rscript` from any directory in a fresh process, prints the expected value,
the observed value and the native control's result, and exits zero (wrap an
expected error in `tryCatch`). Then report, per candidate:

- `title`: one sentence naming the boundary and the effect.
- `contract_source`: the file and heading or ADR number, with the sentence
  quoted exactly.
- `repro_script` (path) and `repro_code` (the minimal runnable R, inline).
- `expected` and `observed`, concretely (values, classes, messages).
- `native_control`: what the control was, why it is the right kind of object,
  and what it returned on the same backend.
- `executed_backends`: backends and versions on which the reproduction was
  **executed**, and those executed where it did not reproduce.
- `rendered_only_backends`: backends for which only rendered SQL or code
  reading supports the claim ("none" if none).
- `cause_file` and `cause_function`: where your reading points; `cause`: the
  reading itself (say "by reading" or "by trace").
- `direction`: one of `wrong_value`, `wrong_refusal`, `missing_refusal`,
  `wrong_condition`, `untimely_query`, `doc_mismatch`, `other`.
- `confidence`: `high`, `medium` or `low`, with the reason.
- `severity`: `P1` silent wrong values in common use; `P2` silent wrong values
  in an uncommon shape, a documented feature that does not work, or source data
  read when the contract says it is not; `P3` a wrong refusal or a wrong or
  misleading condition; `P4` wording only.
- `counterevidence`: anything that points the other way.
- `not_tried`: neighbouring shapes you did not run.

Keep each field under 1,200 characters. Longer evidence belongs in what the
script prints.

Also return `not_reproduced` (one line per hypothesis that held up — say what
was run), `covered` (what you examined without a finding, with rough counts),
`not_covered`, and `lane_notes` (anything about your lane's brief that did not
match the code).
