# Nesting and Grouping specification follow-up bug hunt

Investigated: 2026-11-04
Baseline: `ca50a281e0707897ff5768b4c20f533cf7194c4c` (fetched `origin/main`)

Four investigators used a dedicated worktree on
`investigation/nesting-grouping-20261104` and disposable source copies of the
baseline. The scope included both nesting verbs, all five Grouping specification
constructors, `inspect_grouping()`, and their shared preparation and backend
boundaries. The [November 3 hunt](../2026-11-03-nest-spec-greedy/README.md) and
the two November 4 investigations
([nesting and Grouping](../2026-11-04-nest-grouping-bug-hunt/README.md),
[specification and factor reconstruction](../2026-11-04-nest-spec-greedy/README.md))
supplied the prior findings and search boundaries. The earlier factor,
numbered-dot forwarding, and caller-promoted warning reproductions passed when
loaded against this baseline. The ordinary-column numbered-dot case below
exercised a different path from forwarding.

| Finding | Observed failure | Repair ticket |
| --- | --- | --- |
| Binary subtraction | A wrapper picked the wrong key and collapsed two cells into one. A mixed specification ran caller code against the discarded proxy. | [#1233](https://github.com/sayuks/marginplyr/issues/1233) |
| Numbered-dot capture | A previously selectable column errored. Wrapping a tagged missing dot changed the public diagnostic. | [#1234](https://github.com/sayuks/marginplyr/issues/1234) |
| Empty row-wise nest | Admitted payload names caused errors. A dtplyr graph that produced no rows ran twice. | [#1235](https://github.com/sayuks/marginplyr/issues/1235) |
| AsIs POSIXlt | A typed-missing rollup failed to combine the included and omitted branches. | [#1236](https://github.com/sayuks/marginplyr/issues/1236) |
| dtplyr payload construction | A caller binding silently replaced original values inside cells. | [#1238](https://github.com/sayuks/marginplyr/issues/1238) |

All five issues were published in English with `bug` and `ready-for-agent`
through `to-tickets`. Each is independently verifiable and has no blockers.
The empty-name and source-replay failures share the same empty row-wise
finalization path and were grouped. The two numbered-dot symptoms share the
capture boundary and were grouped. The user delegated those decisions and
publication. Remote main still matched the baseline at publication.

## Binary subtraction selected the wrong column

The [public reproduction](binary-minus.R) used a character column `a` with
two equal values and integer `b = 1:2`. A default argument named `a` read
the active tidyselect data and returned 3 on character data or 2 otherwise.
A second default supplied 1 for `b`. Direct tidyselect evaluated `a - b`
as an ordinary R call and selected position 2, column `b`. Both nesting
verbs instead returned one cell under column `a`. A literal `b` plan
returned two cells, and unary `-a` still selected the complement.

To separate a classification error from a wrapper capture error or a bad
typed snapshot, the [matrix](binary-minus-matrix.R) used the same promises
with a qualified binary head. That control selected `b` in all 36 calls.
All 108 bare, wrapped, and selection-walk variants selected `a`. Each
promise was forced once across local and immutable dtplyr input, fixed keys,
all five constructors, inspection, and both nesting verbs.

Tidyselect's `eval_minus()` walks a unary complement but sends binary
subtraction to its ordinary evaluation context. The Grouping classifier
walked the operands as column spellings in both cases. The name proxy
presented integer `a`, and R retained the wrong promise value into the
canonical pass. An active-binding control for a mixed specification read
`integer` and then `character` while the final plan happened to be correct.
That isolated the speculative evaluation from the separate promise-caching
symptom. ADR 0008's leading-selection amendment excludes such caller
evaluation from the discarded pass.

All-state Issue and PR searches included `"bare minus" selection`,
`"binary" "subtraction" grouping`, and `"arithmetic" "selection"`.
Closed #868 and #889 addressed qualified or parenthesized operator heads.
Closed #1043 concerned arithmetic in the dtplyr input graph, a separate
evaluation context. The qualified binary control passed here.

## Numbered dots lost the column and empty-argument readings

The [numbered-dot probe](numbered-dots.R) used the parenthesized expression
`` (`..01`) `` in a Grouping specification with no variadic context. Direct
tidyselect and explicit `all_of("..01")` selected the column. Nested capture
attempted a forwarded-dot lookup and raised
`'...' used in an incorrect context`. All 500 wrapped comparisons
across ten aliases, five constructors, five public consumers and two
parenthesis depths regressed. All 500 matched the explicit control on
`f5a8d7d1b18c981f044102fd56d92606679c7c52`, the exact predecessor of the
fix. The separate 250 bare cases already failed at upstream capture on
that predecessor.

A wrapper with an explicitly missing first dot and a populated second dot
also made a tagged first argument receive different refusals. Bare forwarding
said that argument 1 was empty. Wrapped forwarding reported that the
constructor took no named arguments. The literal tagged empty and wrapped
untagged controls both reported the empty argument. The named-argument check
read the original quosure before numbered-dot capture resolved it.
The diagnostic matrix had 100 wrapped-tagged-empty mismatches and 500
matches. All 750 genuine populated forwards matched the literal plan,
including same-spelled source columns. Nested lexical forwarding, a
previously forced dot, and an ordinary forwarded selector also passed.

All-state searches included `"numbered dots"`, `"empty" "numbered dots"`,
`"named empty argument"`, and `"incorrect context"`. The full body and
comments of #1214 and PR #1223 established the populated-forwarding contract
and the intended missing-argument diagnostic. The predecessor comparison
separated the newly rejected ordinary column from the upstream bare-capture
restriction. The positive forwarding controls preserved the repaired case.

## Empty keyless row-wise finalization lost payloads or reran the source

The [local reproduction](empty-payload.R) used a zero-row tibble with one
integer payload named `..0`. Native `dplyr::nest_by()` returned one row
with that empty payload. The Margin row-wise verb raised
`indexing '...' with non-positive index 0`. Renaming the payload, supplying
one source row, or calling the ungrouped nesting verb reached successful
controls. The [name matrix](empty-payload-matrix.R) recorded 108 failures
and 396 matching controls across local and dtplyr input, 14 names, absent
and public identifiers, and all Margin orders. The failure also reached
`..01`, `..+1`, and `..-1`.

The [dtplyr reproduction](empty-dtplyr-replay.R) used a two-row integer
source filtered to empty. Its predicate recorded the number of input rows
and raised an error at its second invocation. Native collection and
explicit collection of a deferred `nest_with_margins()` each invoked it
once. Constructing the deferred result invoked it zero times. The row-wise
verb reached the predicate twice with two rows each and propagated the
second error. In the [execution matrix](empty-dtplyr-replay-matrix.R), the
24 keyless zero-result row-wise cases executed twice. The 72 other row-wise
cases, 96 deferred nesting cases, and 12 native collects each executed once.

The finalizer reached both failures after collecting a zero-row result with
no visible keys. It collected `head(0L)` of the original input, which ran
the upstream dtplyr filter over all rows, and built a payload expression
using the raw source names. The ordinary fold used safe column aliases.
The row-wise reference documents one typed empty cell in this case.
ADR 0020's dtplyr amendment distinguishes returning zero rows from
processing the source rows.

All-state searches included `"empty" "nest_by_with_margins"`, `"reexecute"`,
`"second evaluation"`, and `"..01" "nest"`. Closed #997 concerned scalar
identifier attributes in the same finalizer. Closed #950 and #964
concerned output list-column names. The `..0` witness was an admitted
payload. Source spellings refused by the ordinary selection boundary were
excluded from this finding.

## AsIs POSIXlt branches used incompatible representations

Both nesting verbs failed when a local `I(as.POSIXlt(...))` dimension
appeared in an included and an omitted grouping set with a typed-missing
label. The [native comparison](asis-posixlt.R) used ordinary dplyr nesting,
vctrs slicing and a typed-missing grand-total key. That composition retained
the two detail cells and the grand-total cell. Applying the Margin verbs to
the already sliced representation also succeeded.

The included branch kept the original AsIs POSIXlt object while the omitted
branch held the native packed data-frame representation. The common-type
error arose at union. A [matrix](asis-posixlt-matrix.R) recorded 64 errors
with NULL or NA_character_ labels, 64 included-only matches, 64 normalized
source matches, and 32 documented refusals for literal labels on packed
dimensions. It crossed two source classes, populated and missing timestamps,
both set orders, and both keep choices. A one-row witness also reproduced
the error through expansion. Summary and ordinary POSIXlt were controls.
ADR 0016 delegates the result type to ordinary dplyr and vctrs composition.

All-state `"POSIXlt" "AsIs"` and `"POSIXlt" "nest"` searches led to closed
#1093 and merged PR #1130. Their scalar repair covered dist and logLik class
loss on the omitted side. The probe here already had a usable omitted value
and isolated the incompatibility to the representations across both branches.

## A caller list binding changed dtplyr payload values

The [selective binding reproduction](dtplyr-payload-list.R) created an
immutable dtplyr source with one row and payload `v = 1L`. A function
named `list` delegated to base and replaced only a single named argument
`v` with 999L. A deferred Margin nest built before installing that binding
collected 999L with it and 1L after removal. The row-wise Margin nest
returned 999L. Native tidyr nesting, grouped dtplyr counts, plain collection,
local Margin nesting, and a native explicit base-list cell all retained
their expected values under the same binding.

The generated cell constructor used an unqualified `list()` that dtplyr
resolved from the source environment. Timing the binding change after
query construction distinguished deferred helper lookup from source
mutation. An earlier blanket mask also failed ordinary native operations
and was excluded. The selective control passed those operations.
All-state searches included `"nesting" "root environment"`,
`"nesting" "helper" "payload"`, and `"list" "nesting" "mask"`.
Closed #1143 and #1089 addressed generated label and order helpers.

## Additional coverage and replay

| Boundary | Completed evidence |
| --- | --- |
| [Backend name frontier](backend-names.R) | 1,220 comparisons across 61 names, five roles, both verbs and both keep choices. 1,190 matched exactly and 30 agreed on refusal. No source mutation or one-sided warning was observed. |
| [Empty backend matrix](empty-backend-matrix.R) | 560 comparisons of empty and populated inputs, identifier and cell names, and no, fixed or dimension keys agreed. |
| [Inspection identity](identity-boundaries.R) | 198 comparisons across 0 to 40 dimensions including the 31-bit boundary, duplicate policies, two formats, and grouped, local and simulated PostgreSQL inputs matched the closed-form oracle or required refusal. |
| [Prior family algebra](../2026-11-04-nest-spec-greedy/spec/family-algebra.R) | 1,080 comparisons on 180 deterministic trees matched on the pinned follow-up baseline. |
| [Remote admission replay](remote-boundaries.R) | SQLite sent zero statements in 36 calls. DuckDB sent 12 zero-row statements in 36 calls. Twenty-four Arrow calls left all 18 readers and their 36 rows available. |

The first binary-minus matrix collected local results but attempted to read
dtplyr output names from the step object. The final matrix collected
both kinds of result and passed its controls. A stateful helper inside an
otherwise names-only specification also ran once per resolution pass in a
separate probe. ADR 0008 records that budget for ordinary column selections.
The binary-subtraction ticket uses the distinct incorrect proxy classification
and the mixed-pass restriction on executing caller code.

The recorded runs used R 4.6.1 on macOS arm64 with dplyr 1.2.1, tidyr 1.3.2,
rlang 1.3.0, tidyselect 1.2.1, vctrs 0.7.3, tibble 3.3.1, dtplyr 1.3.3,
data.table 1.18.6.1, DBI 1.3.0, dbplyr 2.6.0, RSQLite 3.53.3, DuckDB
1.5.5 and Arrow 25.0.1. Run the scripts with the investigated source as the
working directory. Six probes accept `--assert` and intentionally exit 1
on the baseline after their controls pass.

```sh
Rscript /path/to/evidence/binary-minus.R --assert
Rscript /path/to/evidence/numbered-dots.R --assert
Rscript /path/to/evidence/empty-payload.R --assert
Rscript /path/to/evidence/empty-dtplyr-replay.R --assert
Rscript /path/to/evidence/asis-posixlt.R --assert
Rscript /path/to/evidence/dtplyr-payload-list.R --assert
```

The final replay loaded a disposable archive of the pinned baseline.
Terminal session `29933` exited 0 after collecting six observation runs
at exit 0, six desired-behavior runs at exit 1, and eight additional
matrix runs at exit 0. All probe processes had exited when the session
completed. Session `79617` repeated the two probes after generating the
redundant parentheses and empty argument slots explicitly. It exited 0
with the same counts and numbered-dot assertion failure.

The finite checks cover the named inputs and dispatch boundaries. Hosted
databases, arbitrary class methods, other R and dependency versions, and
resource exhaustion were outside the measurement. The committed files are
dated evidence under the existing source-package exclusion. The PR records
the `source` Review-ready result and the separate review disposition.
