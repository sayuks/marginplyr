# Nesting and Grouping specification boundary investigation

Investigated: 2026-11-06
Tested package SHA: `547d77095c2923573216d002f50bef21778247f9`
Environment: R 4.6.1, aarch64-apple-darwin23

Four investigators examined `nest_with_margins()`, `nest_by_with_margins()`,
the five Grouping specification constructors and `inspect_grouping()` in a
new worktree from fetched main. The [earlier November 6 hunt], [November 5
boundary hunt] and [adversarial follow-up] supplied completed repairs,
counterexamples and prior finite coverage. The new worktree was
`/private/tmp/marginplyr-nest-spec-exhaustive-20261106`, on branch
`investigation/nest-spec-exhaustive-20261106`.

## Finding and ticket

The user delegated ticket granularity and publication. `to-tickets` produced
one English ticket with `bug` and `ready-for-agent`, independently verifiable
acceptance criteria and no blocking edges. The same root-isolation boundary
caused wrong partitioning and exposed caller-owned data, so those symptoms
belonged to one repair. No package implementation or permanent regression
test was changed.

| Issue | Observed violation | Evidence |
| --- | --- | --- |
| [#1382] | Root subclass truncation ran before dtplyr isolation, silently dropping a real Grouping dimension or fixed key; a reference-writing method also changed the original source during inspection. | [Backend investigation](backend/README.md) |

A two-row data.table subclass retained character key `g = c("a", "b")`
through native collection and native local/dtplyr grouping. A `head()` method
delegated normally but changed `g` to integer on zero-row output. Typed
inspection passed the populated root to that method and selected no key.
Both Nest verbs consequently returned one keyless cell instead of two keyed
cells. A minimal subclass bracket method reached the same boundary without
a custom head method. Plain data.table and name-only controls retained `g`.

The 36-case matrix crossed direct, arithmetic and filtered inputs, typed
dimension/fixed-key selection, three consumers and subclass presence.
Eighteen subclass cases failed and eighteen plain-class controls matched.
Every native collection retained the expected values; retyping preserved the
source. A separate reference-writing head method changed the caller's
original values to `changed,changed` during typed inspection of an immutable
input. The Mutable-step Nest refusal invoked no method.

ADR 0020's dtplyr metadata amendment required faithful metadata without
processing source rows, or safe refusal. ADR 0029 required inspection to
protect every contributing root. The qualification of the head generic did
not protect the subclass dispatch that occurred before isolation.
Closed #1303/#1333 protected caller-bound generic lookup; #1359/#1371
protected generated data.table bracket lookup. Neither repaired this root
subclass boundary. The [backend note](backend/README.md) retained full
Issue/PR retrievals and duplicate dispositions.

## Completed finite coverage

| Boundary | Dated observation |
| --- | --- |
| [Local special names](local/README.md) | 612 comparisons matched public names, source membership, cell projections, rowwise identifiers and unchanged sources across simultaneous special-name payloads. |
| [Local shapes](local/README.md) | 768 comparisons matched for higher-dimensional arrays, zero-width columns, deeply packed keys, both Nest verbs, keep/sort options and empty/populated sources. |
| [Upstream name boundary](local/README.md) | Eight refusal comparisons and eight renamed controls agreed with native tidyr/dplyr; no additional package defect was established. |
| [Grouping selections](spec/README.md) | 1,664 comparisons matched plans independently constructed from direct typed tidyselect answers across 52 selectors, all five constructors, three duplicate policies and empty/populated sources. |
| [Capture and forwarding](spec/README.md) | 37 comparisons matched one-reading and numbered-dot forwarding expectations. |
| [Variable-size product suffix](spec/README.md) | 24 observations retained a boundary related to #1342; unknown occurrence positions and unresolved suffix validity prevented a new contract-violation claim. |
| [Backend root isolation](backend/README.md) | 18 failures and 18 matching controls; minimal head/bracket, source-mutation and independent native-grouping controls isolated the new defect. |
| [Deferred source sharing](backend/README.md) | Four uncached/caller-cached volatile-input controls passed for the two Nest verbs. |
| [Remote boundaries](backend/README.md) | 96 calls: SQLite 36, DuckDB 36 and Arrow 24. SQLite sent no statement, DuckDB sent six typed-inspection `LIMIT 0` statements, and all 18 original Arrow readers retained their 36 rows. |

The local oracles grouped original keys with vctrs and compared cell contents
against source slices. Sort options were exercised for interference; these
matrices did not independently prove every Margin-order term. The selection
oracle used tidyselect directly and independently built expected families,
Grouping bits, identifiers and duplicate outcomes. Its finite selected
families did not prove every possible nested tree.

The backend remote replay initially stopped at an obsolete expectation that
name-only DuckDB inspection sent a metadata statement. A six-graph probe
established zero name-only statements and one per typed graph. The copied
harness corrected that expectation and passed; no former note was amended,
and the mismatch was not promoted to a defect.

## Independent confirmation and duplicate searches

The coordinator extracted an immutable Git archive of the tested SHA to a
separate disposable directory. The independent [public-seam replay]
confirmed normal collection and name-only inspection with zero callbacks,
then observed wrong typed selection after callbacks received two and zero
rows. A separate mode changed the original source. Its `--assert` invocation
completed with exit 1 at the desired `(g)` assertion, after those controls
passed; [the log](coordinator/independent-head.log) captured the result.
The local investigator separately compared native local and dtplyr grouping
with typed inspection using a fresh subclass and method.

The coordinator also replayed the prior classed `.key`, `.id`, Option
argument and logical collision-check desired-behavior assertions in the
archive. Both invocations exited 0, as captured in
[scalar controls](coordinator/prior-scalars.log) and
[logical controls](coordinator/prior-logical.log). Those repaired reports
were not filed again.

All-state Issue and PR searches covered head/subclass dispatch, root isolation,
head metadata and subclass brackets. Exact coordinator queries and results
were retained beside the independent replay; the backend track recorded
its separate queries and nearest-report dispositions. Fetching main again
immediately before publication returned the unchanged tested SHA. No parent
issue was modified or closed.

## Replay and limits

Run one command at a time from the tested package root. Track READMEs give
additional controls and output-directory options. The three `--assert`
commands below deliberately require desired behavior and fail on the
investigated source; observation modes establish the failure and controls.

```sh
p=investigation/2026-11-06-nest-spec-exhaustive
Rscript "$p/backend/root-subclass-minimal.R" --bracket --assert
Rscript "$p/backend/root-subclass-head.R" --assert
Rscript "$p/coordinator/independent-head.R" --assert
Rscript "$p/local/name-crossings.R"
Rscript "$p/local/shape-crossings.R"
Rscript "$p/spec/selectors-oracle.R"
Rscript "$p/spec/capture-controls.R"
Rscript "$p/backend/remote-frontier-controls.R"
```

`MANIFEST.sha256` identifies the retained evidence. Dependency environments
and execution outcomes accompany each track. Every investigation execution
reached terminal completion; no owned child work remained. No fix or post-fix
success was claimed. The finite investigation did not establish arbitrary
classes or expression/query graphs, hosted backends, other dependency/OS
versions, unbounded resource behavior or interruption.

Only dated evidence under the existing investigation exclusion was submitted.
Package sources, tests/helpers, generated documentation, installed files,
dependencies, build exclusions and check machinery retained their baseline
bytes. The scope decision, submitted SHA, Review-ready outcome and fetched-main
compatibility assessment belong to the PR verification record.

[earlier November 6 hunt]: ../2026-11-06-nest-grouping-greedy/README.md
[November 5 boundary hunt]: ../2026-11-05-nesting-grouping-spec-hunt/README.md
[adversarial follow-up]: ../2026-11-05-nest-grouping-adversarial-followup/README.md
[#1382]: https://github.com/sayuks/marginplyr/issues/1382
[public-seam replay]: coordinator/independent-head.R
