# Backend nesting and typed-metadata investigation

Investigated: 2026-10-29
Baseline: `4c63bb917ed7ed31ff6aadf060a5eae7a32cd004` (`main`)

## Finding: an active arithmetic binding escaped metadata admission

The public reproduction in `reproduce-active-arithmetic.R` established the
wrong Grouping selection and nested row membership tracked by
[#1043](https://github.com/sayuks/marginplyr/issues/1043).

An immutable dtplyr root contained one integer column `value = 1:2`; a normal
mutation added `derived = value + 1L`. The caller's environment had an active
binding for `+`. Its first two lookups returned the base operator, its third
returned a function that produced character on empty input and ordinary
addition on populated input, and subsequent lookups returned the base
operator. Every operator it returned agreed on the actual populated source.
Five sequential ordinary collections, including the special third lookup,
all returned integer `derived = 2:3`. Constructing the dtplyr mutation itself
performed no operator lookup.

After a fresh fixture, inspection with `.by = where(is.character)` selected
`derived` as a fixed key. Inspection of the explicitly collected input
selected no fixed key. Both nesting verbs repeated this wrong selection:
they returned two one-row cells and outer columns `derived`, `data`, instead
of one two-row cell containing both `value` and `derived` and no outer key.
The lazy verb retained a deferred result; the callback involved in its
construction received a length-zero vector, not source rows.

The operator lookup was read twice by the arithmetic metadata guard, first
for admissibility and then for scalar tracking. Both reads returned the base
operator. The isolated zero-row proxy evaluated the expression afterwards,
read the same active binding for a third time, and ran its nonstandard
function. The guard therefore had not established that the function which
actually produced its proxy was base arithmetic. This was unsafe admission,
not a claim that any particular arithmetic binding read count was promised.

A fixed base binding produced the correct inspection and both nesting
results, including cell contents and memberships. A fixed nonstandard
binding received the existing `marginplyr_error` unsafe-metadata refusal
before its implementation ran. Names-only inspection read the active operator
zero times. Those controls distinguished the guard defect from ordinary
variable source results, unsupported lexical arithmetic, and an unavoidable
source-row read needed by the selector.

With an additional character `g` column, the same false metadata reached
`rollup(where(is.character))`. Explicit collection followed by either nesting
verb returned the expected two outer rows. The lazy-source call instead
failed in the branch union with `dt_join_type_mismatch_error`, because the
actual included `derived` values were integer while omitted values were
character. The retained reproduction covered this additional manifestation.

The ordinary observation mode exited zero. Its `--assert` mode exited one on
the baseline with the intended-behavior error shown in the adjacent output.
The assertion accepted either faithful accepted behavior or an existing-class
Package condition raised before the changing implementation ran; it did not
require future acceptance of this metadata-unsafe derivation.

### Duplicate check and contract

The investigation read closed #782, #1017, and #1024, including their complete
bodies and empty comment lists. They established the faithful metadata or
pre-evaluation refusal requirement but concerned scalar overwrites, repeated
updates, and shared cached graphs, respectively. It also searched all issues
for active bindings and read #873, #889, and #629. Those concerned selection
pass evaluation budgets, qualified selection syntax, and summary-output
prediction. None covered the arithmetic guard's active function binding.

The relevant contract was the typed-metadata boundary in `inspect_grouping()`
and the nesting functions, ADR 0020's caller-row-read policy, and ADR 0029's
separate immutable-input boundary. No new syntax or source materialization
was needed to reproduce the problem. The proposed repair boundary was one
independent ticket: establish faithful arithmetic metadata or refuse the
unsafe binding before executing it, through inspection and both nesting
verbs. This investigation made no implementation or package-test change.

## Negative search coverage

`cached-dag-differential.R` completed 9,720 graph cases. It varied three
source sizes (0, 1, 5), ordinary/special-name/NA-level-factor roots, immutable
roots and caches, filters with counting callbacks, arithmetic and copied
columns, repeated cached roots in joins and set operations, and downstream
ordering, relocation, grouping, filtering, and caching. It selected one
nesting combination per admitted graph, alternating both verbs, `.keep`,
and Margin order. It compared complete outer keys and normalized inner-row
multisets against nesting of the explicitly collected source.

Its 5,400 admitted nesting comparisons all matched. The other 4,320 graph
inspections were refused: this counter included both intentional conservative
metadata refusal and invalid typed Grouping selections, such as a character
predicate resolving to no dimension on a factor-only graph or selecting a
fixed key already supplied by grouped input. The script checked that these
were Package conditions but did not fully compare the refusal categories
against the collected-input control; those cases were not counted as
successful nesting comparisons. No source mutation or construction-time
row-predicate callback was observed, and all 9,720 ordinary collection
controls succeeded.

`metadata-boundaries.R` attempted 208 derived metadata cases across 13 source
column types, empty/populated roots, projection/rename, cached relocation and
ordering, column copies, cached joins, and cached unions. Four ordinary
backend collection controls failed and were excluded. Of the remaining 204,
164 proxy prototypes exactly matched their explicitly collected sources,
40 received the documented conservative metadata refusal, and none differed.
The types included NA-level and ordered factors, dates, zoned timestamps,
difftime, integer64 beyond double precision, hms, AsIs, lists, raw, and
complex. Classed join roots were not promoted to findings merely because
the metadata guard conservatively refused them.

That script also compared 32 nesting results with explicitly collected-source
controls. The upstream filter used a lexical callback closing over a caller
binding named `.BY`, `.SD`, `.I`, `.N`, `.GRP`, `.ROW`, `.xSD`, or `.iSD`, while
the source had a same-named grouping column. Cached filters and downstream
ordering preserved the lexical callback and source-row memberships for both
verbs and both `.keep` values. No premature callback was observed during
construction of the lazy result.

## Replay and limits

Run from the investigated checkout root:

```sh
Rscript investigation/2026-10-29-nesting-grouping-deep-hunt/backends/reproduce-active-arithmetic.R
Rscript investigation/2026-10-29-nesting-grouping-deep-hunt/backends/reproduce-active-arithmetic.R --assert
Rscript investigation/2026-10-29-nesting-grouping-deep-hunt/backends/cached-dag-differential.R
Rscript investigation/2026-10-29-nesting-grouping-deep-hunt/backends/metadata-boundaries.R
```

The second command was intentionally red on the baseline. The other commands
were observation scripts and their zero exits did not indicate a repair.
Finite comparisons on this R/dependency snapshot established no universal
absence of other defects. SQL and Arrow nesting were outside the documented
nesting backend set; no unsupported-backend refusal was presented as a bug.
The retained fresh backend probes focused on dtplyr because recent
investigations had already recorded broad SQL/Arrow inspection coverage.
No claim was made for another dependency version, operating system, hosted
backend, unbounded graph depth, or every stateful caller binding.
