# Expansion and Grouping specification exhaustive bug hunt

Investigated: 2026-10-30
Baseline: `28c449aa03fd5289916f0673e41c848cc126999e` (`main`)
Scope: investigation, reproducible evidence, and implementation tickets; no fixes

A dedicated worktree was created from local `main`; fetching confirmed that
`origin/main` named the same commit. Four investigators divided local
expansion, Grouping specification evaluation, Arrow/dtplyr expansion, and
independent reproduction/SQL composition. The October 29
[expansion hunt](expand-grouping-bug-hunt-2026-10-29.md),
[follow-up](expand-grouping-followup-2026-10-29.md),
[deep hunt](expand-grouping-deep-hunt-2026-10-29.md), and
[systematic hunt](expand-grouping-systematic-hunt-2026-10-29.md), together with
recent nesting and summary investigations, supplied prior coverage and closed
Issue boundaries. Their measurements were not counted as new execution.

The user delegated ticket boundaries and publication without a quiz. The
scoped findings became two independent `bug`, `ready-for-agent` tickets through
the `to-tickets` workflow; an incidental publication-check defect was ticketed
separately. Each could be verified on its own; neither needed a blocker or a
prefactoring ticket. No package implementation, package regression tests,
generated documentation, dependency declarations, or build rules were changed.

## Findings and ticket boundaries

| Finding | Priority | Ticket | Independently verifiable behavior |
| --- | --- | --- | --- |
| L1 | P2 | [#1069](https://github.com/sayuks/marginplyr/issues/1069) | Validate typed-missing classed arrays and packed dimensions without recursion or false refusal |
| L2 | P2 | [#1071](https://github.com/sayuks/marginplyr/issues/1071) | Preserve the typed-missing prototype of a one-dimensional table dimension |

These findings reached separate stages. L1 had a correctly shaped prototype
but asked its class-specific equality proxy the wrong missingness question.
L2 lost the prototype's class and shape before that validation. Their ordinary
controls succeeded independently. The distinct prototype-construction and
validation paths required separate acceptance criteria.

## L1: class-specific missingness broke supported structured dimensions

A one-by-one Date matrix was placed in a tibble column named key.
Included-only expansion returned the original input. An unsorted rollup with
a NULL Margin label instead raised:

```text
expressionStackOverflowError
evaluation nested too deeply: infinite recursion / options(expressions=)?
```

Ordinary mutation using a row-sized `vctrs::vec_init()` value followed by
`dplyr::union_all()` returned the original row and a typed-missing row,
retaining Date and the two-by-one matrix shape. A separate investigator used
a different date and independently reproduced both results. A packed
one-column tibble containing a factor matrix also recursed, although a direct
factor matrix succeeded after the repair for closed #1040.

The ranked hypotheses were recursive representability validation, a malformed
prototype, and unsupported ordinary branch combination. Inspection of the
Date prototype found its original class and shape and all-missing components;
ordinary combination succeeded. Calling `vec_detect_missing()` on that
prototype reproduced the recursion. These controls supported the first
hypothesis and rejected the other two. The factor-only branch in
`validate_margin_label()` did not reach classed non-factor arrays or factor
arrays inside packed dimensions.

A different manifestation reached the same gate for two- and
three-dimensional table columns. Their omitted prototype retained its shape
and all-missing counts. The class-specific equality proxy expanded dimension
labels alongside counts, answered non-missing because labels such as A/B were
present, and could even return two answers for one row. Expansion therefore
raised a Package condition claiming that the typed-missing label could not be
represented. Ordinary typed-missing mutation/union preserved the missing
components and shape. This was included in L1 because it was the same
validation question over a supported structured prototype.

The focused matrix made 300 comparisons across nine families, zero/one/three
rows, matrix/array shapes, direct/packed dimensions, and NULL/NA labels. All
300 ordinary controls succeeded and all 300 included-only comparisons matched.
Rollup produced 126 matching results, 138 recursion failures, and 36 false
table-proxy refusals. The recursion counts were Date 36, POSIXct 36, difftime
36, Surv 12, and packed factor 18. Direct factor arrays passed their 18 cases;
integer, units, and integer64 direct/packed controls each passed 36.

A supplemental posterior rvar witness had all-missing draws but an equality
proxy containing non-missing chain metadata, and reached the same false
refusal. It was retained as evidence for the validation mechanism, without
requiring a new package dependency for an implementation ticket. The broader
rvar identical-only differences described below were a separate, excluded
observation.

The public *Display labels and grouping identity* contract admitted local
matrix/array dimensions with NULL/NA labels, including empty input, and
promised shape and typed-missing components. Its packed-dimension contract
retained inner types through dplyr/vctrs. These supplied the expected behavior
without adding arbitrary attribute restoration under ADR 0016. Ordinary
sorting/grouping also recursed for some classed arrays; L1 concerned the
independently supported unsorted row-copy boundary, not an upstream repair.
Closed #1040 explicitly covered direct factor arrays, not these additional
classes, packed factors, or false table-proxy refusals.

## L2: one-dimensional table prototypes became plain integers

A one-dimensional frequency table of observations a, b, b contained counts
1 and 2. It was placed in one tibble key column beside source-row identifiers.
Included-only expansion and ordinary dplyr grouping succeeded. An unsorted
rollup with a NULL Margin label failed with:

```text
vctrs_error_ptype2
Can't combine `..1$key` <table[1d]> and `..2$key` <integer>.
```

Ordinary mutation with a row-sized `vec_init()` value followed by union_all
returned four rows. The key retained its table class and one-dimensional
shape, with counts 1, 2, missing, missing; every source payload row appeared
once in each Grouping-set occurrence. A separate investigator reproduced the
same successful control and failure.

The ranked hypotheses were scalar-subsetting loss of the prototype's class,
an ordinary table-union limitation, and validation's class proxy. Direct
inspection showed that `key[NA_integer_]` was a plain integer with no dim,
while `vec_init(key, 1L)` remained a one-dimensional table. The ordinary union
control succeeded, and the expansion reached branch type combination rather
than failing missingness validation. These observations supported prototype
construction as the cause. `margin_column_info()` used row-shaped construction
for packed columns and arrays of more than one dimension, leaving a
one-dimensional table on the scalar-subsetting path.

The focused 216-case matrix crossed integer/double counts, zero/one/three
rows, one-dimensional table/plain-array controls, NULL/NA labels, included-
first/omitted-first/duplicate plans, and all three sort options. All 108 table
cases raised the same type-combination error. All 108 plain-array controls
matched the ordinary value/occurrence oracle. Every source remained unchanged.
The comparison aligned occurrence and source-row identifiers; it did not prove
Margin order for an input that could not reach ordering.

The general NULL typed-missing label contract and successful ordinary type
composition supplied the expectation. The explicit matrix paragraph alone
was not used to extend a promise to one-dimensional tables. L2 required the
prototype marginplyr constructed to obey its ordinary type rules, not a new
attribute-restoration mechanism. The separate prototype-construction path made L2 independent of L1's
validation repair.

## Additional executed coverage and excluded leads

| Probe | Executed scope | Result on the baseline |
| --- | --- | --- |
| Local additional types | 1,377 comparisons across additional class families, roles, labels, sorts, and sizes | 1,089 identical matches, 90 raw Package refusals, 162 External errors, 36 identical-only rvar differences; classified below |
| Closure selection differential | 180 expressions across 15 bodies, four closure shapes, and three head-parenthesis depths; local/dtplyr, fixed selection and five constructors: 2,160 comparisons | 1,824 successful answers, 144 matching External-condition classes, 48 required empty-dimension refusals, 144 required renaming refusals; no difference |
| Deep capture and namespace bindings | Both active namespace operators, depths 0/1/4/16/64, five parents, local/dtplyr, inspection/expansion: 200 comparisons | Every plan/value matched; one caller invocation and one operator read per case |
| Arrow filesystem types | 46 families × Parquet/IPC × empty/populated × three labels × three sorts: 1,656 comparisons | 1,510 value/schema matches, 130 shared errors, 16 ordinary-control-only errors; no package-only difference |
| dtplyr mutation metadata | Seed 301030, 1,600 attempted queries, six source families, zero/one/three rows, one to five updates | 695 admitted sources matched six typed selections; 665 documented metadata refusals; 39 ordinary construction failures and 201 ordinary collection failures |
| Lazy boundary controls | Four virtual Arrow scanner names, one caller copy binding, three dtplyr pronoun/dot-dot aliases | Eight asserted controls passed |
| SQL input composition | SQLite/DuckDB, 16 source derivations, five plans, NULL/text labels, three sorts: 960 comparisons | All public names, row bags, and requested fixed/dimension/occurrence key sequences matched the collected-input control |

The Arrow oracle compared row/occurrence identifiers, dimension values, and
physical dimension types against ordinary backend union, ignoring unspecified
order. It was not a complete Margin-order oracle and did not count warnings.
Its initial control used the pre-write Array type; the final run read the
opened source schema so Parquet dictionary/temporal normalization was not
misclassified. Numeric IPC dictionary text-label controls accounted for the
16 control-only failures. The dtplyr oracle inspected metadata selection over
collected controls, not full expanded values or order. The
[lazy track summary](2026-10-30-expand-grouping-hunt/lazy/README.md) retained
these bounds and the final classifications.

The additional local sweep's raw observations were not all independent bugs.
The 162 External errors included 144 manifestations of L1/L2 and 18 ordinary
unsupported rvar sorting cases. Its 90 Package refusals included 54
non-missing structured-label refusals, 18 unsupported raw-vector typed-missing
refusals, and 18 rvar proxy refusals examined as L1. The 36 rvar identical-only
differences were equal under `all.equal()` and involved environment attributes;
they were excluded under ADR 0016 rather than treated as value corruption.
The focused L1/L2 matrices supplied the asserted behavior for those tickets.

SQL derivations included filters, empty inputs, distinct, ordered/unordered
limits, cumulative windows, grouped windows, unions/intersection/difference,
joins, collapse, and explicit computation. The five plans covered included-
only, omitted-first, rollup, cube, and retained duplicate occurrences. The SQL
oracle normalized values to character for row bags; it was not a general
physical-type proof. A separate DuckDB probe retained physical type logs for
MAP, UUID, INTERVAL, ENUM, VARIANT, fixed-size arrays, and empty MAP values:
seven admitted families crossed empty/populated inputs and three sorts, or
42 cases. Collection also completed for those cases. UNION, BIT, BIGNUM, and
JSON fixtures failed at ordinary source-field acquisition and were excluded.
A SQLite NOCASE source probe retained its backend's ordinary value ordering
for both label forms and both requested Margin orders.

The Grouping specification differential initially mistook the expected
renaming diagnostic for a difference because its oracle searched for the
wrong fragment. The corrected oracle reran the complete matrix. Deep capture
controls deliberately put refusing namespace operators in outer environments,
so loss of the inner quosure environment would have failed. Prior malformed
non-quosure specification probes were not promoted into a new construction
promise. The detailed scope and exclusions were retained in the
[specification track summary](2026-10-30-expand-grouping-hunt/spec/summary.md).

## Incidental inherited publication-check failure

The document-reference verifier failed on the pinned baseline because the
focused-test recipe named an explicitly external generated script under
`focus_workspace`, but the scan required a tracked repository file. The
maintained instructions and verifier were unchanged by this investigation;
its dated evidence files were excluded by the verifier's document selector.
The run reported 257 paths across 368 maintained documents and one unresolved
reference. This separate Repository-only defect became
[#1075](https://github.com/sayuks/marginplyr/issues/1075), with no blockers.
No instruction or verifier fix was made.

## Replay, environment, and publication boundary

The [evidence bundle](2026-10-30-expand-grouping-hunt) retained scripts, logs,
classified CSV observations, environment versions, and replay outcomes. Run
from the repository root:

```sh
Rscript investigation/2026-10-30-expand-grouping-hunt/local/reproduce-classed-structured-missing.R --assert
Rscript investigation/2026-10-30-expand-grouping-hunt/local/reproduce-table-structured-missing.R --assert
Rscript investigation/2026-10-30-expand-grouping-hunt/local/reproduce-table-prototype.R --assert
```

Observation mode omitted `--assert`, required successful controls, and printed
the measured failures. Assertion mode required the intended public behavior
and intentionally exited nonzero on the baseline. Independent one-file
witnesses in the bundle's independent directory also supported `--assert`.
Matrix runners reported classified observations; an exit zero did not mean
that defects reported by those observations were fixed.

The environment was aarch64 macOS Tahoe 26.6.2, R 4.6.1, marginplyr 0.1.0 loaded
from the pinned checkout, dplyr 1.2.1, dbplyr 2.6.0, rlang 1.3.0,
tidyselect 1.2.1, vctrs 0.7.3, dtplyr 1.3.3, data.table 1.18.6.1,
Arrow 25.0.1, DuckDB 1.5.5, and RSQLite 3.53.3. Backend fixtures used disposable
local files and private in-memory databases. No external database service was
needed for a finding.

Five existing focused files for Grouping specifications, inspection,
expansion, and metamorphic semantics passed: 1,855 assertions, zero failures,
warnings, or skips. They did not exercise the new boundaries above.
Measurements did not enumerate arbitrary caller code, every class/proxy,
unbounded plans, every filesystem or SQL dialect, every dependency version,
or other operating systems. Successful comparisons established only the
measured inputs.

All additions were dated evidence under the existing source-package exclusion
for investigation notes, a Repository-only change under
[Local review-ready checks](../design/agents/local-checks.md). The session-level
common-CLI instruction was honored separately; its exact committed snapshot
and terminal outcome, final linters/verifiers, and Standards/Spec review record
belonged to the pull request. CI was not monitored.
