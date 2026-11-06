# Constructor capture and qualified specification boundaries

Investigated: 2026-11-06
Tested package SHA: `9ffb24614a71512170298a3827bc11c80354e742`

This lane investigated Grouping specification constructor capture, selection
correspondence, nested-argument forwarding and caller namespace operations.
It used a disposable `git archive` of the tested revision at
`/private/tmp/marginplyr-constructor-hunt-20261106/source`. Every reproduction
set `TESTTHAT_PARALLEL=false`; none changed package implementation or tests.

## Confirmed: a qualified wrapper's answer disappeared before nested admission

[#1396](https://github.com/sayuks/marginplyr/issues/1396) records one repair
slice with no blockers. The [minimal replay](qualified-spec-minimal.R) used
three source columns: `a` separated two rows, `b` held one shared value, and
`value` identified source rows. A caller bound `s` to `grouping_set(a)` and
bound its own namespace operator to return a function selecting `b` for the
explicit `base::` parenthesis expression around `s`.

Ordinary typed tidyselect selected `b` after one caller namespace lookup.
The same captured expression inside `grouping_sets()` instead included `a`
after zero caller namespace lookups. Both Nest verbs consequently returned
two cells instead of the intended one. Genuine base-qualified identity
parentheses retained the bound specification, and an unwrapped
constructor-shaped call ran the caller's actual selector; both controls
succeeded.

The observed minimal output was:

```text
direct typed selection: b
Grouping dimension: a
caller namespace lookups: 0
actual nesting rows: 2 ; expected: 1
actual nesting rows: 2 ; expected: 1
```

The separating hypotheses were a lost forwarded environment, a premature
names-only selection classification, and loss of the caller-authored wrapper
while reading the nested argument. The direct unforwarded witness ruled out
forwarding alone. The callback was never entered, ruling out the types it
would see during selection. Public versus private namespace controls
separated normalization of the qualified spelling from ordinary evaluation:
only the public-qualified expression lost the callback.

`nested_arg_expr()` read through the whole qualified wrapper before
`read_grouping_arg()` evaluated the nested spelling. Forwarded arguments also
passed through `capture_nested_arg()`. `restore_nested_arg()` restored recorded
wrappers on the selection branch, whereas a reading that found a nested
specification retained that specification directly. Thus a different answer
from the caller-authored outer call never reached nested admission. This
also produced a false nesting refusal when the parent was `grouping_set()`:
the returned ordinary selection should have been valid there.

ADR 0019's *Amendment: isolate generated namespace operations* retained
ordinary captured-environment evaluation for caller-authored namespace
expressions. ADR 0008's constructor-shaped selection amendments retained
first values and one-read budgets. Syntactic parentheses retained their
separate static reading; the witness concerned an explicit qualified call,
not a request to change that reading.

## Measured scope

[The qualified-boundary matrix](qualified-spec-matrix.R) executed 300 public
comparisons: all five constructors, local and immutable dtplyr-root inputs,
public/private namespace operators, five payload or forwarding forms, and
inspection plus both Nest verbs. The five forms were a bound specification,
a constructor call, direct quosure injection, embracing and a numbered dot.
Every direct typed reference selected `b` after one lookup. Materialized Nest
results were compared with literal `b` specifications.

| Operator | Exact matches | Wrong results | False refusals | Caller lookups per call |
| --- | ---: | ---: | ---: | ---: |
| Public `::` | 0 | 120 | 30 | 0 |
| Private `:::` | 150 | 0 | 0 | 1 |

The 30 false refusals all used a `grouping_set()` parent. These were different
public manifestations of the same discarded wrapper, not separate tickets.
The complete rows are in [qualified-spec-results.csv](qualified-spec-results.csv).

[The ordinary-selection matrix](selection-matrix.R) compared 23 selectors
through six wrapper forms, all five constructors and the same two input
classes: 1,380 comparisons. Its reference selected against an independent
zero-row typed frame, then compared the complete public inspection tibble
with the same constructor over a literal selection. It recorded 1,136 exact
result matches and 244 paired refusals with identical condition classes and
messages. There were zero differences. This was a selection-correspondence
oracle, not an independent enumeration of grouping-family algebra.
Selectors covered names, strings, positions, NULL, combining calls, lexical
vectors and pronouns, ranges, Boolean selection, type predicates, direct
selection-context names, qualified parentheses and a non-syntactic name.
[selection-results.csv](selection-results.csv) retains every observation.

An initial ordinary-selection fixture used a reserved numbered-dot column
name. Its name-validation refusals made all comparisons uninformative; that
run was excluded, the fixture was corrected and the complete matrix reran.
An initial qualified matrix compared lazy objects directly; the final matrix
materialized both Nest results before comparison. Only the complete corrected
runs supplied the counts above.

## Duplicate disposition

The lane exported all-state tracker snapshots with `gh issue list --state all
--limit 2000 --json number,title,body,url` and the equivalent `gh pr list`:
702 issues and 691 pull requests. Local title/body searches combined
`qualified|parenthes|namespace` with `bound|nested|constructor`; the relevant
records were #1288, #1315, #1322, #1383, #1304, #1240 and #1039.

The first four issue bodies and complete comments were read, along with the
complete PR #1395 body/comments; the remaining three issue bodies were read.
#1288 concerned retaining a constructor-shaped selector's first answer and
callee lookups. #1315/#1322/#1304/#1039 concerned metadata classification of
ordinary qualified selections. #1383 and merged PR #1395 restored a forwarded
selection's qualified boundary after normalization. This witness instead
found a nested specification while the caller-authored qualified call would
have answered differently, so the restoration branch was bypassed. #1240
preserved static ambiguity checks through syntactic parentheses with a
masked parenthesis operator; the explicit qualified ordinary-call boundary
here was distinct. No equivalent concrete report was found in those
snapshots. The coordinator owned the publication-time main refresh and issue
publication.

## Replay, execution and limits

Run from an archive of the tested revision, with the evidence scripts reached
by absolute path. `MARGINPLYR_HUNT_SOURCE` optionally overrides the package
source; `MARGINPLYR_HUNT_OUTPUT` optionally overrides the two CSV destinations,
which otherwise use R's temporary directory.

```sh
Rscript /path/to/constructors/qualified-spec-minimal.R
Rscript /path/to/constructors/qualified-spec-minimal.R --assert
Rscript /path/to/constructors/qualified-spec-matrix.R
Rscript /path/to/constructors/selection-matrix.R
```

Observation mode exited zero; desired-contract `--assert` exited one with
`identical(actual, list(expected)) is not TRUE`. The collected combined
minimal execution used terminal session `59907`; portable-script replay `91446`
collected observation and the desired-contract assertion through exit one.
The final qualified matrix used session `40891`, exit zero; the corrected and
tightened ordinary matrix used session `98954`, exit zero.
Every process was collected before the next reproduction; no owned subprocess,
database connection or server remained.

R was 4.6.1 on aarch64 macOS; dplyr 1.2.1, tidyselect 1.2.1, rlang 1.3.0,
dtplyr 1.3.3, data.table 1.18.6.1 and pkgload 1.5.3. The
[environment record](environment.txt) records the R platform and versions.
No complete suite, snapshot suite, package build/check, render or hosted
backend ran in this lane. Arbitrary caller functions, dispatch methods,
other package versions and every possible forwarding composition remained
outside these finite observations. No post-fix result was claimed.
