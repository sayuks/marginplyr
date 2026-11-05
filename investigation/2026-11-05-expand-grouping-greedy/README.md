# Expansion and Grouping specification greedy boundary hunt

Investigated: 2026-11-05
Baseline: `0403c9150394cbae21b59f34c75791d0873b8451` (fetched `main`)

A fresh worktree from main divided the investigation across local expansion,
Grouping specification algebra and selection, deferred dtplyr/Arrow inputs,
and live SQL execution. The [November 5 adversarial
hunt](../2026-11-05-expand-grouping-adversarial/README.md), [November 4
boundary hunt](../2026-11-04-expand-grouping/README.md), [invariants
hunt](../2026-11-04-expand-grouping-invariants/README.md), and earlier
structured-type and dispatch notes supplied prior witnesses and exclusions.
Their earlier measurements were not credited as executions in this hunt.

## Findings and ticket boundaries

| Finding | Disposition | Observed behavior |
| --- | --- | --- |
| S1 | New [#1349](https://github.com/sayuks/marginplyr/issues/1349), `bug`, `ready-for-agent` | Computing a derived SQLite expansion with a Boolean-word public column into a shadowing temporary name returned zero rows; an existing-input overwrite could erase that temporary input. |
| G1 | Existing [#1344](https://github.com/sayuks/marginplyr/issues/1344) | A caller's S3 pattern-length method saw name placeholders instead of typed selection metadata and caused a false pattern-length refusal. |

`to-tickets` produced one independent implementation slice for S1, with no
blockers or prerequisite refactoring ticket. New-destination loss and existing
temporary-input loss shared the same derived materialization boundary and
remained one ticket. The user delegated ticket granularity and publication,
so no breakdown approval was requested. No implementation was changed.

G1 was independently reproduced here, but a post-publication query found
#1344 from a parallel investigation on the same main SHA. The initial issue
snapshot preceded that publication. This hunt closed its own #1350 as a
duplicate, read #1344's complete body and comments, and retained the additional
evidence for that existing ticket.

### S1: derived Boolean-word materialization lost the source binding

[The primary replay](sql/sqlite-derived-destination.R) used persistent SQLite
table `facts`, two rows with grouping values `b` and `a`, and integer payloads
11 and 22. A rollup expansion requested a NULL Margin label, last Margin order
and Grouping set identifier `TRUE`. A positive-payload filter retained all four
expanded rows when collected. Computing that derived result to temporary
`facts` succeeded with zero rows. When the unqualified input already resolved
to a two-row temporary `facts`, overwrite succeeded with zero rows and replaced
the input with an empty table. The persistent namesake stayed intact.

The direct expansion copied four rows to a new shadowing name and refused the
unsafe existing-input overwrite while preserving its two rows. A different
destination also copied four rows. The [independent
challenge](sql/sqlite-derived-destination-independent.R) changed the input to
three rows, identifier to `FALSE`, destination to case-equivalent `FACTS`, and
used mutation, selection, arrangement and limiting. Each derived path returned
zero rather than six rows, or four for the limit. Each overwrite erased the
three-row temporary source. Qualified-source and ordinary-identifier controls
copied six rows. This was neither a filter-only failure nor an additional
promise about derived Margin order.

The package's SQLite materializer created a zero-row destination before
inserting the public query. Its direct-result guard delayed publication or
refused an unsafe overwrite. A derived Boolean-word result still used that
creation/insertion method, but skipped the guard because it was no longer a
direct result. The insertion could then resolve the source name to the newly
emptied destination. ADR 0031 owned that materialization/savepoint boundary;
its ordinary derived-query type/order semantics did not authorize losing rows.
Closed #1282/#1295 and PRs #1293/#1300 protected direct results. #1095/#1136
protected executable Boolean-word composition. None covered the remaining
derived materialization admission path.

[The 288-fixture matrix](sql/sqlite-destination-matrix.csv) crossed two public
spellings, identifier/payload roles, all three sort choices, direct/filter/head
results, four destination identities and absent/present temporary inputs. It
observed 108 successful-but-empty results, 150 correct copies and 30 safe
refusals. Every new-destination filter/head case with a same, case-equivalent
or explicit-temp source name returned zero rows. Existing-input behavior
varied by role and sort; these observations did not establish that every
Boolean-word role failed. All persistent sources remained unchanged. The
retained [matrix script](sql/sqlite-destination-matrix.R) and
[transcript](sql/sqlite-destination-matrix.log) recorded the finite scope.

### G1: pattern consumption dispatched in the wrong selection context

[The specification note](spec/README.md) and [minimal
replay](spec/pattern-dispatch.R) recorded a standard stringr regex object with
a caller-defined `length.stringr_regex()` method. The method inspected only
whether selection column `a` was character, returning one in the typed
context and two otherwise. Ordinary tidyselect selected `a`; so did the
typed-wrapper control `matches(identity(pattern))` in inspection and expansion.
Direct `matches(pattern)` instead raised `stringr patterns must be length 1`
in Grouping inspection, expansion and fixed-key inspection. The method had
seen the integer name proxy. No wrong-column result was established.

[The independent challenge](lazy/independent-pattern-dispatch.R) reduced the
input to one character column and one row. Thirty observations crossed
regex/fixed/coll patterns, exact or inherited S3 methods, and five public
surfaces. All used the wrong context. Twelve ordinary/typed-wrapper controls
and three method-free derived-dtplyr controls passed. An unrelated method ran
zero times. The custom methods lived only in fresh R processes. #995 supplied
the earlier dispatched-helper contract; #1255 repaired the reverse
misclassification of ordinary stringr patterns. The newer #1344 already owned
this exact consumption-method boundary, so no additional repair was split out.

## Additional executed coverage and exclusions

| Area | Result against the baseline |
| --- | --- |
| [Local row/type/order oracle](local/README.md) | 4,455 comparisons matched independent expected branches and physical Margin order across 19 types, zero/one/four rows, three column roles, three input classes, three families and three sort choices. Each checked input immutability. Eighteen structured data.table source-conversion combinations were excluded before expansion. |
| Local group-state and re-input | 54 cases matched: 27 admitted/re-expanded inputs and 27 documented row-wise or dropped-group refusals. |
| [Mixed-family algebra](spec/README.md#bounded-algebra-coverage) | Seed 41248 generated 2,000 shapes; 1,634 fixture-sized specifications matched the family/order/set-ID oracle under all duplicate policies, including 542 required duplicate refusals. It excluded 358 top-level selectors and eight oversized fixtures. |
| Composite dimensions | Seed 51796 generated 2,000 further shapes; 1,772 matched exact included-column order and set memberships under keep/drop. It excluded 190 top-level selectors and 38 oversized fixtures. |
| [Deferred and physical lazy inputs](lazy/README.md) | 552 exact Arrow physical-value comparisons, 120 volatile/nested-source comparisons, 48 once-per-collection dtplyr controls and 162 derived/cached dtplyr comparisons passed. |
| [SQL public-name oracle](sql/names-oracle.R) | 1,200 eager/live-SQLite/live-DuckDB comparisons crossed 25 spellings, four column roles, NULL/text labels and all sort choices. There were 1,188 agreements, including shared upstream refusals. Twelve public-identifier cases named `..1` reached ordinary tibble's reserved-name refusal; supported noncanonical dot-like spellings passed. |

The local note separated exploratory oracle failures, native source reshaping
and documented label/type refusals from package findings. The lazy note
excluded an Arrow int64 class-predicate lead: the available schema-to-R proxy
had the same representation as ordinary Arrow schema conversion, and #864
limited the documented promise to available metadata. Literal numeric/logical
dots forwarding remained within the existing #1280 investigation boundary;
this hunt established no separate ticket from it. The finite algebra controls
did not prove arbitrary plan resource bounds or early-refusal ordering for all
partially resolved products. Concurrent #1341/#1342 covered further full-join
metadata and duplicate-prefix preflight boundaries.

Live PostgreSQL, SQL Server, hosted stores, Polars, other operating systems and
other dependency versions were outside the measured scope. The source contract
was assessed against CONTEXT.md and the relevant ADRs; a successful finite
comparison was not treated as a proof of every backend or extension class.

## Replay, duplicate checking and publication

The baseline was fetched again before ticket publication and still named
`0403c9150394cbae21b59f34c75791d0873b8451`. The coordinator archived that SHA
to a separate disposable source directory and reran both primary
desired-contract assertions there. Both intentionally exited one after their
success controls. This confirmation was separate from the evidence worktree.

From an archive of that SHA, set `evidence` to the absolute path of this
directory and run the following from the archived package root, in fresh R
processes:

```sh
Rscript "$evidence/sql/sqlite-derived-destination.R"
Rscript "$evidence/sql/sqlite-derived-destination.R" --assert
Rscript "$evidence/spec/pattern-dispatch.R"
Rscript "$evidence/spec/pattern-dispatch.R" --assert
Rscript "$evidence/lazy/independent-pattern-dispatch.R" --assert
```

The commands without `--assert` checked successful separating controls and
printed observations. The assertion modes expressed the desired contract and
failed on this baseline; their failure was evidence of the reported defects,
not a passing package test. Subdirectory notes gave the broader replay commands,
counts and environment. The SQL matrix accepted an output CSV path as its
first argument. The SQL-name oracle wrote its detailed failure object to an R
temporary file; its transcript retained the count and representative exclusions.
Stored text transcripts had trailing spaces and final blank lines removed.

Duplicate checking used an all-state issue snapshot and 250 recent all-state
PRs, searched by title and body for shadowing, overwrite, derived Boolean
materialization, stringr patterns, S3 dispatch, pattern length and `matches()`.
Live all-state queries included `SQLite derived compute`, `stringr length`,
`SQLite compute` PRs and `stringr selection` PRs. The complete bodies/comments
of #1282, #1295, #1095, #1136, #1179, #1255 and #1344 were read, together with
PR #1293's discussion. The publication refresh also checked the recent issue
list and identified the concurrent #1341/#1342 reports. The initial snapshot
missed the newly published #1344; the subsequent live query supplied the
duplicate disposition recorded above.

The environment was arm64 macOS with R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
rlang 1.3.0, tidyselect 1.2.1, stringr 1.6.0, vctrs 0.7.3, RSQLite 3.53.3,
DuckDB 1.5.5, Arrow 25.0.1, dtplyr 1.3.3 and data.table 1.18.6.1.

This submission added only dated notes and standalone evidence under the
existing investigation build exclusion. Package code, package tests,
dependencies, generated documentation, installed files and verification tools
were unchanged. Its Review-ready scope was `source`: the evidence documented
observed defects without changing package execution or its check inputs.
Package build/check, the complete suite and snapshots were omitted; the PR's
verification record owns the pinned submission result. No CI monitoring was
performed.
