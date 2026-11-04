# Follow-up bug hunt for summaries, Grouping specifications, and contextual shares

Investigated: 2026-11-04
Repository snapshot: `ca50a281e0707897ff5768b4c20f533cf7194c4c`

The investigation ran in a new worktree from main, on branch
`investigation/summary-spec-total-share-20261104`. The remote main ref agreed
with the tested snapshot during publication. The
[November 2](summary-grouping-share-bughunt-2026-11-02.md),
[November 3](summary-grouping-share-bughunt-2026-11-03.md) and
[earlier November 4](summary-grouping-share-bughunt-2026-11-04.md) hunts supplied
the comparison cases. The Grouping lane also consulted the recent expansion
and nesting hunts. Merged repairs supplied controls, including #1211/#1214 on
numbered dots, #1212 on dtplyr-generated calls, #1213 on formal defaults, and
#1224 on directly invoked nested functions.

Three parallel lanes examined ordinary summary evaluation, Grouping
specifications, and share-source runtime validation. The coordinator examined
the lexical dependency analysis used by both contextual helpers. The split
followed independent public failure modes. The request delegated ticket
granularity, so the interactive `to-tickets` checkpoint was omitted. Each
ticket could start immediately and received `bug` and `ready-for-agent`.

| Finding | Ticket | Observed effect |
| --- | --- | --- |
| G1 | [#1227](https://github.com/sayuks/marginplyr/issues/1227) | A legal dots-shaped grouping column was refused outside a dots frame |
| G2 | [#1228](https://github.com/sayuks/marginplyr/issues/1228) | A trailing typed selector triggered metadata before an established duplicate |
| G3 | [#1234](https://github.com/sayuks/marginplyr/issues/1234) (existing) | Parentheses changed the diagnostic for a named, missing numbered dot |
| S1 | [#1229](https://github.com/sayuks/marginplyr/issues/1229) | DuckDB returned shares for ineligible INTERVAL sources |
| S2 | [#1230](https://github.com/sayuks/marginplyr/issues/1230) | Static enclosing-frame removals exposed a forbidden dependency |
| S3 | [#1231](https://github.com/sayuks/marginplyr/issues/1231) | Demanding a numbered dot exposed a forbidden dependency |
| S4 | [#1232](https://github.com/sayuks/marginplyr/issues/1232) | Inline literal actuals and defaults changed function lookup without updating the dependency decision |
| M1 | [#1237](https://github.com/sayuks/marginplyr/issues/1237) | A nested dplyr verb changed caller control flow, assignment, and Arrow diagnostics |

## Grouping specifications

### G1. Dots-shaped column selection outside a dots frame

A data frame held a legal column named `..01`. Inspection of
`grouping_set((..01))` raised `'...' used in an incorrect context` when
ordinary tidyselect, the parenthesized fixed-key selection, and
`all_of("..01")` selected it. An injected symbol reproduced the refusal.
The predicate in `nested_arg_quosure()` accepted a numbered spelling as a
dots reference without establishing a dots frame. The same parenthesized
selection worked on the archived parent of the #1214 change. A real supplied
dot in the presence of a same-spelled column still forwarded the specification
as #1214 intended. The unparenthesized literal outside dots can fail in rlang
capture before reaching this boundary. [Grouping lane evidence](2026-11-04-summary-spec-total-share-followup/grouping-findings.md#g1-legal-dot-shaped-columns-were-read-as-dots-outside-a-dots-frame).

### G2. Duplicate Grouping sets before a typed sibling

Two leading occurrences of `a` in `grouping_sets(a, a, where(predicate))`
were already duplicate sets under the default duplicate policy. DuckDB
invoked the traced DBI metadata method once and the typed predicate twice
before reporting positions 1 and 2. A names-only control reported those
positions without either action. A derived dtplyr step returned the unsafe
metadata refusal instead of the duplicate. The mixed leading-selection pass
retained dimensions for fixed-key overlap but discarded completed occurrences
for duplicate validation. ADR 0005 established the ordering. The trace
counted DBI method entry and supplied no source-row scan claim.
[Grouping lane evidence](2026-11-04-summary-spec-total-share-followup/grouping-findings.md#g2-a-typed-sibling-hid-already-established-duplicate-grouping-sets).

### G3. Parenthesized named empty dots lost the position diagnostic

A variadic wrapper passed an explicitly missing first dot to
`grouping_set(alias = (..1), a)`. Inspection diagnosed a forbidden named
argument. Removing only the name or only the parentheses diagnosed the
empty argument at position 1. A populated named dot still received the
named-argument refusal. The early name check exempted missing quosures
before numbered-dot resolution exposed the missing payload.

The constructor's redundant-parentheses contract and the test named
`a name on an empty argument is refused by position` established the
expected precedence. Searches for `"numbered dots" "empty" "named"`
and `"empty argument" "named" "grouping"` found #1214 and #261 for
the earlier contracts, plus #925 for an omitted embrace. A final
all-state comparison found the exact witness in open #1234, so that
ticket covered G3. It also overlapped G1, which had already been filed
as #1227. The final comparison found separate closed overlap reports
#1239 for G1 and #1246 for G2.
[Executable diagnostic replay](2026-11-04-summary-spec-total-share-followup/grouping-named-empty-dots.R).

## Contextual shares

### S1. DuckDB duration summaries passed the eligible-source rule

In a live DuckDB table, `max(duration)` summarized two INTERVAL values of
one and two days. Without a Grouping specification or fixed keys, collecting
its Total share returned a 172800-second `difftime` and a double share of one.
The ordinary summary returned that duration, and the numeric share control
returned one. The local share call refused the same duration. With
`rollup(g)`, a nonmissing interval failed during collection, while an
all-missing typed INTERVAL collected missing shares on detail rows and ones on
the Grand total set for both helpers.

ADRs 0010 and 0017 and the eligible-source section of the share reference
excluded durations. The constant-ratio path bound the source using
multiplication by one, and DuckDB reported INTERVAL as the type before and
after multiplication. In the all-missing detail case, CASE branches returned
the missing or own-denominator values without reaching the incompatible
arithmetic. These observations distinguished source eligibility from
denominator values and from failure of the ordinary duration aggregation.

All-state issues and PRs were searched for `"ineligible source"`,
`interval OR duration OR difftime`, `share INTERVAL`,
`grand-total-only source`, and `duration share source`. Closed #429 and
#446 and merged #448 covered the character witness. #446's triage established
that the type rule applies even if every row is its own denominator.
The interval witness remained after that repair. The exploratory 18-fixture DuckDB
sweep found four successful numeric controls, ten expected refusals, two
interval cases and two unsupported fixture setups (BIGNUM and BIT). The
retained replay focused on the interval and its paired controls.
[Executable interval replay](2026-11-04-summary-spec-total-share-followup/duckdb-interval.R).

### S2. A removal targeted an enclosing frame through another static form

The scope fixture used groups a/b, measure x of 1/3, and an input payload
`prior` of 100/300. It first defined a share named `prior`. A later ordinary
summary called an inline function whose formal `prior` equaled one. Its
nested invocation removed that formal with
`rm("prior", envir = sys.frame(sys.parent()))`. The enclosing function then
summed `prior` and the Margin call returned 100/300/400 beside displayed shares
.25/.75/1. Using the equivalent immediate `parent.frame()` produced the
required refusal. A depth of two through two direct invocations and a
literal integer `inherits = 1L` also escaped the guard. The logical
`inherits = TRUE` and local-only removal controls established the distinction.
The reverse case, in which a share source resolved an earlier ordinary
alias, also succeeded when it should have been refused.

`scope_after_removal_call()` handled a recognized immediate parent and a
literal logical true, and otherwise retained the removed binding in these
enclosing frames. The replay bounded the finding to directly visible frames
and literal operands. Local aliases for frame objects and dynamically
computed inheritance were exploratory witnesses and were outside the ticket.

### S3. A demanded numbered dot removed a binding in the caller

Using the same fixture, an inner inline dots function read `..1` then summed
`prior`. Its supplied dot removed the enclosing formal `prior` in its
argument evaluation frame. Both forbidden dependency directions succeeded
with the same wrong or invalid result as S2. A matched named formal
produced the required refusal. The second dot, a zero-padded spelling, and
`force(..1)` also reproduced the bypass. An undemanded dot retained the
formal and returned one in every row.

`function_scope_frame()` kept unmatched dots separately, while the numbered
reference never reached the pending-promise effect recorded for a matched
formal. The following read therefore still appeared bound. This boundary
concerned evaluation of supplied arguments after capture, separate from
#1211/#1214's forwarding repairs.

### S4. Literal argument values changed function lookup

An enclosing inline function bound `prior` to one and had a default for
`f` that removed `prior` and returned `identity`. An inner inline function
took `f` equal to one, then called `f(0)`. R skipped the non-function
binding and forced the enclosing default. The Margin verb accepted the
subsequent forbidden read. It refused the same read when the inner binding
came from a local assignment of one. This difference held when the literal
came from a default and when the supplied literal had already been forced.

In the other direction, a literal no-op closure supplied to a formal named
`rm` correctly left `prior` bound in ordinary evaluation. The Margin verb
treated the call as a possible removal and incorrectly refused. Assigning
the same closure locally produced the expected value of one per grouping row.
The scope walk recorded known value or function kinds for local assignments
and lacked equivalent information for these supplied or default literals.
This one ticket covered both observable consequences of that distinction.

The two dependency rules came from the share reference and ADR 0010 as
inherited by ADR 0017. The scope matrix covered 19 syntax shapes across
data.frame, tibble and data.table, both helpers, and both directions.
It found 36 mismatches for enclosing targets, 48 for numbered dots,
and 60 for callable-kind handling. The other 84 comparisons agreed.
Each comparison also checked its no-share ordinary result and serialized
input. The searches across open and closed issues and PRs covered
`parent.frame`, `sys.parent`, `scope removal`, `inherits rm`,
`binding removal`, `lexical scope`, `share dots promise`,
`share non-function binding` and `share numeric inherits`. Closed #1042
covered removals within nested expressions, #1213 covered formal defaults,
and #1224 and merged #1226 covered the named-formal and immediate-frame
controls. The compared reports did not cover the three reproduced slices.
[Executable scope matrix](2026-11-04-summary-spec-total-share-followup/share-scopes.R).

## Margin summary evaluation

### M1. A nested verb's input ran in an introduced function scope

An inline function passed `return(sum(x))` as a nested mutate's input
argument, then had the literal 99 after that call. With x equal to 1/2,
ordinary dplyr returned 3 and the Margin summary returned 99. An assignment
inside the nested verb's input likewise left the surrounding variable at 99
in the Margin result. The same result occurred with a legitimate `pick()`
inside the nested mask. A local assignment before the nested verb preserved
ordinary evaluation.

The independent-mask rewrite in `rewrite_summary_selections()` enclosed the
whole call, including the input argument. #781 and #795 established the
nested mask's ownership of its selectors. An additional scope around the
caller-owned input accounted for the assignment and return differences.
For Arrow inputs, the wrapper also changed the untranslatable expression
passed into Arrow. Ordinary Arrow Table evaluation warned and collected for
the nested group_by, mutate and select forms. The Margin verb raised
`simpleError: bad value` with zero traced collection entries, where
ADR 0025 called for a Package condition with the original expression and
projection/collection guidance. Arrow Dataset inputs lost their original
`arrow_not_supported` refusal.

The eager matrix produced 27 wrong values and 18 matching controls across
three input classes and three grains. All 15 Arrow cases lost the applicable
diagnostic across Table, RecordBatch, a query over Table, Dataset and a query
over Dataset. Native sums and a conventional unsupported expression passed
10 Arrow controls. All-state issue and PR searches used
`"lexical" "mutate" in:title`, `"bad value" "Arrow"` and
`"independent" "mask"`. #781/#795 covered inner mask ownership, and #254
covered fallback call capture. The conventional Arrow refusal in the matrix
preserved that earlier behavior. [Executable summary and Arrow matrix](2026-11-04-summary-spec-total-share-followup/nested-summary-mask.R).

An independent ordinary-dplyr oracle matched 96 summary, Grouping
identifier and Margin order comparisons over eager, dtplyr, Arrow,
SQLite, and DuckDB native and portable paths. The fixtures varied
empty input, fixed keys, typed-missing versus string labels, and Margin
order. In four empty SQLite results, ordinary summary columns were
logical and were normalized for the empty comparison. The package-owned
identifiers remained numeric, within ADR 0033's guarantee.
[Executable aggregate matrix](2026-11-04-summary-spec-total-share-followup/summary-aggregate-matrix.R).

## Replay and limits

The [evidence directory](2026-11-04-summary-spec-total-share-followup) held
standalone R scripts. Run them with Rscript from the investigated source
root. The coordinator ran each replay in a fresh R process sequentially.
Observation mode checked the controls and reported the observed outcomes. The
`--assert` mode required the desired contract and deliberately exited one
on the pinned snapshot. The Grouping and aggregate matrices used observation mode only.
[Replay exit statuses](2026-11-04-summary-spec-total-share-followup/replay-results.csv).

The environment was R 4.6.1 on arm64 macOS (Darwin 25.6.0), pkgload 1.5.3,
dplyr 1.2.1, dbplyr 2.6.0, rlang 1.3.0, tidyselect 1.2.1, vctrs 0.7.3,
tibble 3.3.1, dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1,
DuckDB 1.5.5, RSQLite 3.53.3 and DBI 1.3.0. The DuckDB fixtures used private in-memory
connections and disconnected after the probes. An attempted disposable
PostgreSQL setup could not acquire shared memory under the sandbox, so the
interval evidence was specific to DuckDB. The matrices covered finite
fixtures and the named versions. They established no result for other
versions, operating systems, hosted backends, arbitrary caller callbacks,
concurrency, or interruption.

Only dated evidence and its standalone scripts changed. The evidence was
outside the package build and introduced no inputs to package runtime,
regression tests, docs generation, examples or vignettes. That effect
justified Review-ready `source`, with package build/check, complete suite
and snapshots omitted. The PR was to record the exact submitted SHA and
terminal checks. CI monitoring was outside the request.
