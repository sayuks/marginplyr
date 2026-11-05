# Lazy expansion probes and independent selection challenge

Investigated: 2026-11-05
Baseline: `0403c9150394cbae21b59f34c75791d0873b8451`

The lazy-backend investigation targeted the deferred source mechanisms added
for #1271 and #1272, the derived-source metadata boundary, repeated collection,
and Arrow physical values. It read the November 4 expansion boundary and
invariants notes and the November 5 adversarial note before selecting probes.
The relevant contracts were ADR 0002, ADR 0016, ADR 0020 and ADR 0029.
The source-sharing and physical-value fixtures established no new confirmed
defect. An independent selection-dispatch challenge confirmed the issue
described below.

## Executed coverage

| Replay | Observed result |
| --- | --- |
| [Arrow physical values](arrow-physical.R) | The 48 existing physical-value controls passed. A further 504 exact Arrow array comparisons passed: 24 physical types, seven source shapes and three consumption routes. The sources were collapsed, filtered, empty, arranged, projected, mutated and unioned; consumption used direct collection, compute and a RecordBatchReader round trip. |
| [Deferred source invariants](deferred-invariants.R) | 120 Arrow comparisons passed across the three Margin orders, direct collection, compute, reader consumption, nested expansion and expansion of a union containing an earlier expansion. Each outer grouping-set occurrence retained the same volatile payload multiset. Another 48 dtplyr collections each evaluated the predicate source once, including cached-source and all three sort controls. Construction evaluated it zero times. |
| [Derived and cached dtplyr inputs](dtplyr-derived.R) | 162 comparisons matched the materialized eager input: nine source transformations, zero/one/three source rows, NULL/string labels, and all three Margin orders. The sorted cases also matched the exact eager row sequence. |

The physical-value oracle compared Arrow column arrays with `Equals()`, without
converting wide integers, decimals, timestamps or nested values to R. Its types
included uint64 beyond signed integer range; decimal32/64/128/256; fixed and
variable binary; map and fixed-size-list; durations; times and zoned timestamps;
floating-point types; large strings; date64; and null. The first 48 comparisons
replayed the earlier fixture against the new baseline; the 504 comparisons
extended it to transformed sources and the new deferred consumption paths.

The Arrow volatile oracle used native `arrow_random()`. It compared the two
outer occurrences after removing the rewritten dimension and occurrence
identifier, retaining the source row identifier and payload. For the union
case it compared multisets, since the source legitimately held repeated rows.
The dtplyr predicate incremented a lexical counter and returned a stable true
vector; each individual collection had to increment it exactly once.

Initial direct Arrow controls also exercised post-expansion filter, projection,
join, re-expansion and compute, and renamed hms, difftime, ordered factor and
zoned POSIXct payloads. No difference from the corresponding Arrow control was
established. These exploratory controls were narrower than the retained
matrices.

## Excluded leads

Arrow's schema-to-R proxy represented an int64 field as `integer(0)`, although
collecting the nonempty wide-value source produced an `integer64` column.
Consequently `where(bit64::is.integer64)` selected no dimension in inspection
or expansion. Ordinary `as.data.frame(source$schema)` gave the same proxy.
The public *Fixed columns and grouping dimensions* section limits lazy
predicates to available schema/zero-row metadata, and #864 removed the former
blanket equivalence claim. This observation did not establish a violation of
that documented boundary and was not proposed as a new repair ticket. It does
not establish general class-predicate equivalence for Arrow.

The known dtplyr root-head and derived-bracket metadata findings in #1303 and
#1326 were identified from the November 5 nesting note and were not counted as
new findings. This investigation did not execute the package suite or claim
coverage beyond the fixtures above.

## Independent challenge: stringr pattern S3 dispatch

[The independent replay](independent-pattern-dispatch.R) reduced the original
Grouping investigator's witness to one row and one character column. The
pattern was an ordinary `stringr::regex("a")`, `stringr::fixed("a")` or
`stringr::coll("a")` value. A caller-defined `length` method for its exact
class, or for the inherited `stringr_pattern` class, returned one when the
active selection metadata held a character column `a`, and two otherwise.
It read no source values.

All 30 combinations of three pattern kinds, exact/inherited dispatch, and
five public surfaces used the wrong name-only proxy and raised
`stringr patterns must be length 1.` The five surfaces were local inspection,
local expansion, fixed-key inspection, dtplyr-root inspection and derived
dtplyr inspection. Every method invocation in these cases observed a
noncharacter `a`. The observed error originated from ordinary tidyselect,
but marginplyr supplied the context that made that error false.

Twelve ordinary tidyselect and `matches(identity(pattern))` inspection
controls selected `a`, with every method invocation seeing character
metadata. Three standard-pattern controls with no relevant method resolved
the derived dtplyr input from names, preserving the behavior #1255 required.
An installed method for an unrelated class received zero calls. The harness
restored all method bindings after each complete run.

The helper-value classifier admitted the exact standard pattern class stack
as names-only without accounting for S3 methods executed by the helper.
Tidyselect's `matches()` invoked `length()` on the pattern before string
matching. This was a reachable implicit callback even though the written
selection contained no explicit `peek_data()` call. #995 established the
precedent for ordinary typed dispatch in Grouping selections. #1255 repaired
the opposite error for standard patterns and required genuine typed
selectors to retain their metadata boundary; its standard-pattern controls
did not exercise a caller-defined pattern method. These two issue bodies and
comments were read in full for this challenge. The parent investigation owned
additional duplicate checking and ticket publication.

The observation run exited zero after confirming the 30 mismatches and all
controls. A separate `--assert` run exited one at the desired-contract check
`all(r$desired)`. Removing the method restored ordinary pattern handling;
wrapping the same value restored typed handling. The environment additionally
used stringr 1.6.0 and tidyselect 1.2.1.

```sh
TESTTHAT_PARALLEL=false Rscript investigation/2026-11-05-expand-grouping-greedy/lazy/independent-pattern-dispatch.R --assert
```

## Replay and environment

From the package root containing this evidence, run each script in a fresh R
process:

```sh
TESTTHAT_PARALLEL=false Rscript investigation/2026-11-05-expand-grouping-greedy/lazy/arrow-physical.R
TESTTHAT_PARALLEL=false Rscript investigation/2026-11-05-expand-grouping-greedy/lazy/deferred-invariants.R
TESTTHAT_PARALLEL=false Rscript investigation/2026-11-05-expand-grouping-greedy/lazy/dtplyr-derived.R
```

The source-sharing and physical-value matrix runs ended with exit zero. The retained physical and deferred
logs record their counts. The derived dtplyr run printed
`dtplyr derived/cached eager comparisons: 162 passed` and exited zero.

The environment was arm64 macOS, R 4.6.1, dplyr 1.2.1, Arrow 25.0.1,
dtplyr 1.3.3, data.table 1.18.6.1 and bit64 4.8.6. The probes used local
in-memory sources. Hosted stores, Polars and noninstalled backend versions
were outside this measured scope.
