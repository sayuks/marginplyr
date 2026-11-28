# Refuter brief

<!-- The coordinator copies this file to <SCRATCH>/brief-refuter.md and
replaces every <UPPER-CASE> field. -->

A defect hunt on the R package marginplyr (pinned commit
`<SHA>`, checked out read-only in the
current directory) produced the candidate given to you. A different model
found it. **Your side is that it is not a defect.** Argue that side as well as
it can honestly be argued, with evidence, and then say plainly whether the
candidate survives. This is ordinary functional-correctness review of an
open-source R data-summary package: you check whether a reported behaviour is
a real defect or intended.

`S` is
`<SCRATCH>`.

## Rules

- Read-only on the checkout: no edits, no git writes, no issue or PR writes.
  Do not spawn subagents. Do not run the test suite, `tools/review-ready.R` or
  `R CMD check`. Write no report file; the structured return is the report.
- Write your scripts under the verification directory you were given; data
  files under `tempdir()`.
- Observe the installed build of the pinned commit:
  `.libPaths(c("<SCRATCH>/lib", .libPaths())); library(marginplyr)`.
  No `pkgload::load_all()`, no package installation. A candidate remedy can be
  replayed in memory only (a child environment of the namespace, or
  `assignInNamespace()` inside your own process), never by editing files.
- `python3` does not work; put R code in files and run `Rscript file.R`; no
  TCP network; no downloads and no DuckDB `INSTALL`.
- <ENGINES and VERSIONS, as in the finder brief.>

## What to do

1. Replay the candidate's script in a fresh `Rscript` process. If it does not
   reproduce as described, say exactly what you saw.
2. Find **the right native control**: the plain dplyr, dbplyr, dtplyr, arrow
   or database behaviour on the same backend for *the same kind of object*
   marginplyr builds, and run it. Earlier hunts lost candidates here: #1625
   compared a sorted SQLite `compute()` with native `arrange() |> compute()`
   (an unordered table) where the right control was a native arranged query,
   which behaved identically; #1768's and #1794's first controls stopped a
   step short of the composition marginplyr builds. If the dependency does the
   same thing on the equivalent input, the behaviour is the dependency's —
   unless the caller did not write the step that fails (the failing
   `select()`, `arrange()` or statement is marginplyr's own), which is how
   #1622, #1640, #1641, #1768 and #1794 stood.
3. Check where the expected value comes from. It must come from a contract
   sentence or an independent calculation, never from marginplyr internals or
   from two marginplyr paths agreeing. Recompute it yourself.
4. Look for what makes the observed behaviour **intended**: a sentence in
   `CONTEXT.md`, an ADR under `design/adr/` (read the amendment sections), the
   Rd under `man/`, a vignette, a comment on the function, a test under
   `tests/testthat/` that pins it, or a closed issue that decided it
   (`gh issue view <n> --json title,body,labels,comments`), and
   the decisions listed in `tools/hunt/decided-shapes.md`. The candidate file
   ends with a `## known` section: the issues, notes, tests and ADRs that
   mention its terms. Read each hit and say whether the candidate is new, a
   duplicate, already decided, or partly known.
5. Read the code at the cause the candidate names and say whether the reading
   is right; name the file and function you judge to be the cause.
6. Probe the boundary: which neighbouring inputs behave correctly, and whether
   the candidate is narrower or wider than stated. Say which backends you
   **executed** and which you only read or rendered.
7. If it survives, say what a repair would have to trade: would it refuse
   calls that work today, change a documented behaviour, or cost a query?
   Name each option and what the contract texts say for it. Where you can,
   replay the most plausible remedy in memory and report what changed.

## Return

Keep each field under 1,500 characters; a script's output carries the rest.

- `verdict`: `refuted` (not a defect: say which of dependency behaviour,
  intended and documented, wrong expectation, or does not reproduce),
  `narrowed` (a defect remains but smaller or different from the claim — state
  the surviving claim exactly), or `stands`.
- `reproduced`: true or false, with what you ran.
- `native_control`: the control you judge correct, why, and its result.
- `intended_evidence`: every text that supports the behaviour being intended,
  quoted with its location; say "none found" if so.
- `contract_evidence`: texts that support the candidate, quoted.
- `cause_reading`: whether the named cause is right, and what you read.
- `boundary`: neighbouring shapes and their results; executed against
  rendered-only backends.
- `strongest_argument_against`: the single best argument that this is not a
  defect, even if you conclude it stands.
- `remedy_tradeoffs`: options and what each costs; in-memory replay results.
- `side_observations`: anything else odd you saw while refuting that is not
  this candidate (one line each; do not pursue).
- `scripts`: paths of what you ran.
- `surviving_claim`: the claim as it should be filed, or empty when refuted.
- `known`: for each hit of the `## known` section, `new`, `duplicate`,
  `decided` or `partly_known`, with the reason.
