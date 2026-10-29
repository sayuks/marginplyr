# dtplyr summary and contextual-share special-name evidence

Investigated: 2026-10-29

This evidence was collected against main `4c63bb917ed7ed31ff6aadf060a5eae7a32cd004`,
with dplyr 1.2.1, dtplyr 1.3.3, and data.table 1.18.6.1. No package implementation
or package test was changed. The full R environment was recorded in
`session-info.txt`.

## Matrix and bounded negative results

`matrix.R` crossed 32 spellings, six roles (input measure, variable dimension,
fixed key, ordinary source-summary output, share output, and requested Grouping
set identifier), no share / Parent share / Total share, and direct / across
summaries. Share-output cases without a share were omitted. Every operation
used one fixed key, a rollup dimension, an identifier, and `.sort = "last"`.
Each candidate was compared with a local result and a safe-name dtplyr control;
row order was normalized with base R rather than asking a backend to read the
special output names again.

The 1,088 calls produced 973 matching results, 73 dtplyr errors, and 42 local
name refusals. There were no successful value mismatches in this first matrix.
The findings below were distinct from inherited upstream refusals: `.N` and
`.I` input measures already failed without a share; direct `sum(.)` retained
upstream dot semantics; `...` and `..1` had ordinary tidyselect / R dot-argument
limitations. The result inventory and exact diagnostics were retained in
`matrix.csv`.

All requested `.EACHI` and `.NATURAL` roles matched their controls. `.ROW`,
`.SDall`, `.xSD`, and `.iSD` variable dimensions, fixed keys, and ordinary
source-summary output names matched their controls. Ordinary argument-like
names and function names (`key`, `keep.rownames`, `check.names`,
`stringsAsFactors`, `i`, `j`, `by`, `with`, `on`, `:=`, `CJ`, `data.table`,
`.SDcols`, and `NROW`) added no finding in this matrix. The raw matrix also
included `.`, `..x`, `..g`, and `..value`.

## S1: source validation broke six optimized input-measure names

A two-row input had `g = c("a", "b")`, fixed `f = c("P", "P")`, and a numeric
measure `c(1, 3)`. Naming that measure `.SD`, `.GRP`, `.NGRP`, `.SDall`, `.xSD`,
or `.iSD` admitted an ordinary grouped dtplyr sum and a no-share Margin sum
with `.by = f` and `rollup(g)`. Adding a share of the ordinary output `value`
failed at collection with `cannot change value of locked binding` naming the
input measure. A safe measure name returned values `1, 3, 4` and double ratios
`0.25, 0.75, 1`.

Both share helpers and direct/across ordinary sources failed for all six
spellings: 24 failures. The `.BY` input-measure control passed all four
combinations. Every candidate left its caller-owned data.table unchanged.
`input-measures.csv` retained the ordinary, no-share, local, renamed-input,
and affected results; `query-input-SD.txt` retained the generated query.

This boundary depended on GForce optimization. With `datatable.optimize`
set to 2 or Inf, all seven ordinary/no-share controls passed. At 0 or 1,
they already failed upstream and therefore did not establish an added
Margin failure. Share validation failed for all six new names at every
optimization level; `.BY` shares passed at every level. These 28 optimization
controls were recorded in `optimization-controls.csv`.

The fixed key was material to the no-share control. Without it, the Grand
total branch was ungrouped and `.SD`, `.GRP`, `.NGRP`, and `.SDall` could
already fail before adding a share. The finding claimed only the working
ordinary/no-share shapes. The package-generated validator removed the optimized
aggregate path there. `protect_dtplyr_share_measure()` protected only `.BY`.

Closed #1022 repaired `.BY` input measures, #696 concerned special
source-summary *outputs*, and #948 concerned nesting's internal bindings.
None covered these six input-measure spellings in contextual-share validation.

## S2: generated writes rejected `.ROW` as share output or identifier

A rollup over two rows with values `1, 3` accepted `.ROW` as an ordinary
summary-output name, and a no-share Margin summary accepted `.id = ".ROW"`.
Both share helpers, through direct and across share calls, failed when `.ROW`
was either the share-output name or the requested identifier: eight failures
with `.ROW can only be used with := NULL to delete rows`.

Local calls and dtplyr calls changing only the output name succeeded. The
public names, values, and original caller-owned table were recorded in
`ROW-output.csv`; `ROW-output.log` included the ordinary/no-share controls.
The generated queries showed two concrete writes: `:= (.ROW = <ratio>)`
and `:= (.ROW = <private set identifier>)`. Reading and restoring an ordinary
summary named `.ROW` was already supported; writing these public targets was
the failing part.

Ordinary dtplyr `mutate(.ROW = x)` failed with the same upstream restriction.
The caller had requested a Margin summary, whose generated share/finalization
writes introduced that restricted operation. This did not claim ordinary
mutation support. The source/output distinction and generated writes were
retained in `query-ROW-share_output.txt` and `query-ROW-id.txt`.

## S3: string and indirect lexical assignments became input reads

An input held `g = c("a", "b")`, unused `.BY = c(1, 3)`, and `x = c(2, 6)`.
The independent ordinary summary
`local({".BY" <- c(5, 7); sum(.BY)})` evaluated to `12, 12, 12` locally.
With a dtplyr share of an unrelated `value = sum(x)`, it silently returned
`1, 3, 4`, reading the unused input column instead.

Six forms reproduced across both share helpers and direct/across ordinary
source summaries: a string left-hand side, its right-assignment spelling, a
string left-hand side inside an immediately invoked function, `do.call("<-", ...)`,
and computed `assign` callees obtained with `get()` and `match.fun()`.
These were 24 successful wrong results. Renaming only the unused input `.BY`
column to `measure` returned `12, 12, 12` for every candidate on the same lazy
backend. Ordinary dtplyr and the no-share Margin calls refused the affected
input rather than returning its payload values. Caller tables were unchanged.

The source probe showed the direct string assignment was considered not to
bind the measure, then rewritten as
`local({".BY" <- c(5, 7); sum(safe_alias)})`.
`lookup_assignment_names()` admitted symbol targets and statically named
assignment primitives; the missed forms fell outside that recognition.

Closed #1033 addressed lexical `.BY` ownership, including symbol assignment
and function formals. The positive controls for those forms still returned
12; a statically named `assign()` produced the intended targeted refusal.
This was an uncovered extension of the same contract after that fix, rather
than a request to reopen the closed issue or expand ordinary dtplyr's grammar.

## S4: explicit evaluation masks lost their `.BY` ownership

With the same input, `eval(quote(sum(.BY)), list(.BY = c(5, 7)))` also changed
from the local `12, 12, 12` to successful `1, 3, 4` results when a dtplyr share
was present. `evalq()` and `with()` equivalents reproduced. Both helpers and
both ordinary-source styles gave 12 successful wrong results. Renaming only
the unused input column restored `12, 12, 12`; ordinary/no-share calls refused
the original spelling. Every caller table remained unchanged.

The source probe showed a different ownership boundary from assignment:
`eval(quote(sum(.BY)), list(.BY = c(5, 7)))` became
`eval(quote(sum(safe_alias)), list(.BY = c(5, 7)))`.
No assignment had occurred. `rewrite_evaluated_call_parts()` opened evaluated
language while the alias rewrite did not retain the explicitly supplied
mask's ownership of `.BY`. The replacement bypassed that binding and reached
the outer input alias.

S3 and S4 were recommended as separate, independently verifiable tickets:
assignment-target recognition and explicit evaluation-scope ownership were
different boundaries. Both concerned ordinary lookup under ADR 0019, while
ADRs 0010/0017 governed the shares and ADR 0020 retained lazy construction.
The shared replay `lexical-ownership.R` recorded 36 wrong successful results
and 12 passing/refusing controls in `lexical-ownership.csv`. Its query files
and log retained the actual aliases and direct source probes.

## Replay

Run from the repository root:

```sh
Rscript investigation/2026-10-29-datatable-reserved-bug-hunt/summary/matrix.R
Rscript investigation/2026-10-29-datatable-reserved-bug-hunt/summary/input-measures.R --expect-bug
Rscript investigation/2026-10-29-datatable-reserved-bug-hunt/summary/optimization-controls.R
Rscript investigation/2026-10-29-datatable-reserved-bug-hunt/summary/ROW-output.R --expect-bug
Rscript investigation/2026-10-29-datatable-reserved-bug-hunt/summary/lexical-ownership.R --expect-bug
```

The three targeted scripts also accepted `--assert-correct`, which required
correct candidate results for source measures and `.ROW` outputs, and forbade
successful wrong values for lexical cases while allowing an appropriate
refusal. That mode exited 1 on the investigated implementation; the default
observed-bug mode exited 0. `assertion-modes.txt` recorded the checks. These
were evidence scripts, not package regression tests or an implementation.
