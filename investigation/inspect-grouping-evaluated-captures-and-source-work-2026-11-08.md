# Grouping inspection: evaluated captures and source-work boundaries

Investigated: 2026-11-08
Tested main: `d6f1f45824ddc04fd25fcd67860a016b2f9b6ea0`

A new worktree started from freshly fetched main. Three fresh-context agents
independently generated candidates from implementation, public contracts and
processing-path consistency before exchanging initial reports. All used archives
of the same SHA. The parent separately prepared reference calculations and small
input generation. Three independently repairable defects were confirmed and
filed as [#1514][capture], [#1516][json] and [#1518][numeric]. No package
implementation or package test was changed.

## Contracts and preparation

The [constructor][constructors], [inspection][inspection] and
[identity-helper][identity] references supplied the public contracts. Exploration
followed public capture, argument evaluation, nested-name admission, selection,
metadata acquisition, family expansion, normalization, duplicates, identifiers,
formatting and Margin-operation correspondence, rather than only recent edits.
The agents read `CLAUDE.md`, `AGENTS.md`, `CONTEXT.md`, local-check instructions
and relevant ADRs, including 0002, 0005, 0007, 0008, 0009, 0013, 0020 and 0026;
backend investigation also applied metadata, source-ownership and recording rules.

The recent [capture/CSV][prior-csv], [deep inspection][prior-deep],
[systematic inspection][prior-systematic] and [metadata inspection][prior-metadata]
notes and relevant implementation/fix history were examined. The starting main
included PR #1511's capture and CSV repairs. Historical counts in those notes
were not counted as executions in this investigation.

## Confirmed: executed capture operands lost empty positions

The constructor contract refused an authored empty argument, apart from a genuine
trailing comma. The top-level specification accepted any expression returning a
specification. This literal call nevertheless returned an empty-set plan:

```r
library(marginplyr)
d <- data.frame(a = 1L)
s <- base::bquote(.(grouping_set(, !!!list())))
inspect_grouping(d, .grouping = s)
# Correct: Argument 1 of `grouping_set()` is empty.
inspect_grouping(d, .grouping = base::bquote(.(grouping_set(, !!!list()))))
# Actual: included = "()", grouping_id = 0L.
```

Public capture skipped the entire call when part of it was classified as quoted
language. That also skipped bquote's executed `.()` substitution and `where`,
and substitute's executed environment. The directly spelled constructor then
lost the authored empty before its empty splice. No constructor alias was needed.

An isolated before comparison at `9d5e9e89efc9056b6c0388447a35c32994930fef`
correctly refused substitute-environment, bquote-where and masked-quote witnesses.
The same desired-contract replay exited 0 before PR #1511 and 1 on the tested
main. The parent independently reproduced the minimal literal failure. A's
125 stored/inline pairs crossed five constructors, five public consumers and
five wrappers; all stored forms refused and all inline forms accepted. A nested
constructor also silently introduced a valid set. Genuine quoted-factory,
trailing-comma and single-splice-evaluation controls remained correct.

Existing quoted-factory tests covered nonexecuted language and immediate function
bodies, but did not execute a constructor inside these capture operands. Direct
constructor tests did not exercise this boundary. [#1514][capture] recorded this
regression separately from #1489's previously reported alias/factory residual and
#832's Grouping-helper rewrites inside summaries.

## Confirmed: JSON metadata inference sampled source values

With DuckDB's JSON extension loaded, a lazy input captured only the name `k` from
`SELECT * FROM read_json(path)`. Name-only inspection succeeded before the
synthetic file existed and recorded no SQL. Changing only one source value then
changed the same numeric selection during typed inspection:

```r
writeLines('{"k":101}', path)
inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included
# "(k)"
writeLines('{"k":"source_tail"}', path)
inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included
# "()"
```

Both observations preceded explicit collection. Each typed call recorded one
returned `selection_proxy` wrapping a `LIMIT 0` statement. That count and outcome
did not establish absence of source work. [DuckDB's JSON documentation][json-doc]
described type detection over sampled objects and a full-file sampling option.
Default sampling and `sample_size=-1` reproduced; `read_json_auto`, `read_ndjson`,
`read_ndjson_auto` and typed `.by` reached the same cause.

Native zero-row collection also inferred character from the text value. DuckDB
answered its requested inference correctly; marginplyr requested it under a
stronger no-source-read contract. An explicit integer schema with auto-detection
disabled retained numeric metadata despite an incompatible text value; explicit
collection then raised a conversion error. No remote cost or large-file timing
was measured.

An independent archive replay completed 16 reader/value/selection comparisons
and four nonexistent-source name-only controls; input serialization stayed
unchanged. The parent separately confirmed default/full sampling and controls.
Existing CSV tests and the guard covered explicit CSV readers, not JSON readers.
Counting one admitted zero-row collection would miss this violation.
[#1516][json] retained the self-contained reproduction, extension prerequisite,
source-work-sensitive acceptance criteria and separation from #1506.

## Confirmed: metadata admission dispatched on full S4 columns

Derived dtplyr metadata admission classified root columns with `is.numeric(x)`
before excluding objects. An S4 numeric column's method therefore received the
populated source even when inspection selected only character columns:

```r
methods::setClass("inspection_numeric_witness", contains = "numeric")
seen <- numeric()
methods::setMethod("is.numeric", "inspection_numeric_witness", function(x) {
  seen <<- c(seen, sum(as(x, "numeric")))
  TRUE
})
d <- data.table::data.table(
  a = methods::new("inspection_numeric_witness", c(13, 29)),
  n = 1:2, label = c("x", "y")
)
x <- dtplyr::lazy_dt(d) |> dplyr::mutate(copy = n)
seen <- numeric()
inspect_grouping(x, .grouping = grouping_set(where(is.character)))
seen # 42
```

Changing only the values to 101 and 307 produced 408. The method received length
two rather than a zero-row prototype. The character predicate did not request
numeric dispatch, and the returned `(label)` plan was correct, so a plan-only
assertion missed the callback. The parent independently reproduced both totals.

Twenty derivation/value/ownership controls found callbacks for alias and
arithmetic mutation, and none for direct roots, filtering or renaming. Source
serialization stayed unchanged and counted row-filter callbacks stayed uncalled.
The confirmed impact was unrequested full-vector method execution, not observed
source mutation. It happened before the protected zero-row operations.

[#1518][numeric] distinguished this classification generic from #1382's root
subclass truncation and #1397/#1418's registered table-operation dispatch.
Existing tests covered those operations and upstream source callbacks, but did
not observe S4 numeric dispatch on an unrelated column during metadata admission.

## Existing causes: additional evidence, not new tickets

The [CSV report received implicit-reader evidence][csv-comment]. SQL string
paths, quoted-identifier paths and `query_table(path)` bypassed a guard looking
only for explicit reader-call tokens. An independent matrix completed 192 typed
executions over CSV/TSV and compressed files, source spellings, derivations,
audit settings and both selection positions, plus 24 nonexistent-source
name-only controls. A right-key-coalesced join did not change result type and
was excluded as source-work evidence; the other five source forms did. The
parent independently reproduced the minimal implicit CSV witness.

The [quoted-factory report received an immediate-body residual][body-comment].
An immediately invoked function comparing its own body with an external template
selected `b` inline instead of `a`; stored and plain rlang-capture controls selected
`a`. No injection was involved. A dead constructor acquired a marker before the
caller observed its body. This was the language-mutation mechanism already
reported in #1505. Neither closed report was reopened or otherwise changed in
state; neither residual was counted as another independent new cause.

## Executed coverage and hypothesis dispositions

Counts below describe different comparisons and are not combined into a score.
Each row records hypothesis, evidence, result/disposition and next evidence.

| Hypothesis and evidence | Execution result / disposition | Next evidence |
| --- | --- | --- |
| Ordered algebra, overlapping/composite dimensions and duplicate optimization could change plans. | Parent: 222 generated models, 1,124 exact full-plan comparisons and 104 expected duplicate refusals. B: 80 independent families, 768 exact matches and 192 expected refusals across 960 calls. No discrepancy. | A distinct algebraic boundary or dependency version. |
| Inspection could disagree with public operation identity or lazy carriers. | Parent: 30 local operation pairs; three hand-enumerated composite rollup/cube/product plans; 288 exact dtplyr/Arrow/SQLite/DuckDB plan comparisons and 12 lazy operation pairs. All matched. | Other backend/version evidence. Shared-path operation matches were separate from independent oracle arithmetic. |
| Width, fixed keys, unusual names or diagnostic thresholds could corrupt IDs/types/display. | B: 126 width/fixed-key comparisons at 0/1/30/31/32/64/127 dimensions and 0/1/40 fixed keys, seven helper boundaries, 90 format/policy comparisons over 15 unusual-name families, 15 operation-ID controls and nine 255/256/257-occurrence threshold checks matched. | Other encodings or distinct threshold families. |
| Qualified capture, environment overlap or nested masking could change binding counts/readings. | A: four retention controls, 30 direct empty/splice/trailing comparisons and 11 environment/nesting assertions matched. Executed-capture loss was isolated separately. | A distinct evaluation boundary. |
| Derived types or Mutable roots could change metadata or source ownership. | C: 2,304 dtplyr typed-plan checks over 12 types, eight derivations, 12 predicates and two selection positions matched; 96 Mutable sources stayed byte-identical with no counted row callbacks. The S4 classification callback required separate observation. | Other constructible types or dispatched operations. |
| Repeated calls, errors or recording changes could leak state. | C's bounded recording controls covered audit enable/disable, reentry and typed-refusal followed by name-only success; records and input ownership matched. | Other control-transfer/backend conditions. |

The parent enumerated the three small plans by hand, then used simple R ordered
families and weighted bits for a separate reference evaluator. Input syntax
generation was separate; no internal compiler supplied expected values. List
output included exact vectors, column types and ordinary tibble classes. Text
was checked as display, retaining its documented ambiguity for separators.
Operation `.id` and bare `grouping_id()` checks exercised shared paths and were
not presented as independent proof of the expansion algorithm.

Harness corrections were excluded as findings. The parent's hand-plan assertion
first distinguished `NULL` from `character()` in its own empty-family carrier;
normalizing that reference carrier allowed the three manual assertions and lazy
comparisons to finish. Earlier full-plan expectations already produced identical
output for both carriers. B excluded 12 `..2`/`...` fixtures after native
tidyselect rejected them with the same name condition. A corrected an overly
specific diagnostic-word expectation and moved its plain-capture control outside
public capture. JSON's first attempt stopped for a missing extension; only the
later temporary-extension runs established JSON behavior. None of these stopped
or faulty harnesses was described as an overall pass.

An additional `names.data.table` method could observe populated roots through
ordinary name extraction and inside native `head(..., 0L)`. Its faithful name
answer left plans unchanged. This was not filed as another defect: obtaining
column names is an admitted metadata question, and the evidence did not settle
whether the public contract forbids a caller's own accessor from inspecting the
object while answering it. The S4 numeric witness differed because the caller
asked only for character selection and the internal classifier dispatched on a
column it then excluded as an object. A stronger explicit accessor-boundary
contract, or a metadata mismatch/side effect attributable to bypassing an
established guard, would be needed to promote the names observation.

## Provenance, duplicate checks and limits

Before publication, fetching main again returned the tested SHA. The parent's
current-main replay confirmed capture, JSON and implicit CSV behavior; its
separate S4 replay confirmed both source totals. All-state Issue/PR API searches
included `read_json`, `ndjson`, `"bquote" "empty"`, `"substitute" "capture"`,
`"implicit" "CSV"`, `"S4" "dtplyr"`, `"is.numeric" "metadata"` and
`"metadata" "dispatch"`. A local export contained 777 Issues with comments and
400 recent PR bodies; older PRs were covered by targeted server searches.
Full related reports and comments distinguished #1489, #1505, #1506, #832,
#1382 and #1418. An initial `gh search` query-encoding error was replaced by
successful `gh api` searches. New tickets used `bug` and `ready-for-agent`, with
no blockers or established prerequisite refactoring, under the requested
`to-tickets` workflow.

Temporary scripts, logs and short hypothesis/evidence/result/disposition/next
records stayed outside the submitting worktree. Parent evidence was under
`/private/tmp/marginplyr-inspect-root-evidence1108b/`; agent evidence was under
`/private/tmp/marginplyr-agent-implementation-1108/`,
`/private/tmp/marginplyr-contracts-1108b/` and
`/private/tmp/marginplyr-backends-*`. The Issues retained minimal public replays.
All executions were collected to terminal completion, including parent sessions
66207 (oracle), 70534 (lazy), 57822 (JSON), and B's 82226/96600. Each source copy
ran at most one reproduction at a time with testthat parallel execution disabled.
No complete suite, package build or rendering was used for exploratory feedback.

Environment: arm64 macOS Tahoe 26.6.2, R 4.6.1, marginplyr 0.1.0 from the tested
SHA; dplyr 1.2.1, dbplyr 2.6.0, tidyselect 1.2.1, rlang 1.3.0, tibble 3.3.1,
vctrs 0.7.3, dtplyr 1.3.3, data.table 1.18.6.1, DBI 1.3.0, DuckDB 1.5.5,
RSQLite 3.53.3 and Arrow 25.0.1. The official JSON extension was downloaded and
loaded only from temporary investigation storage. Databases and data were
locally owned and synthetic; connections were closed.

The cross-lane check included both formats, local/lazy and derived inputs,
successful/rejected calls, name/type/value controls, source work distinct from
metadata-call counts, input immutability and repeated recorded state. Each
confirmed cause was followed into adjacent paths before returning to other
hypotheses. No concrete executable promising hypothesis remained in these finite
lanes. Other OS/dependency versions, live PostgreSQL/SQL Server, remote sources,
other extension inference, arbitrary dispatched methods and unbounded expression
graphs were not established. They require their own fixtures and independently
observable source-work controls; finite matching comparisons do not establish
universal correctness.

## Submission effects

Only this dated note was added. Its consumers were investigation readers; it
changed no package implementation, tests, dependencies, generation source,
installed file, build behavior or verification tooling. The cumulative baseline
was the merge-base with freshly fetched main. These effects justified Review-ready
`source`, with package build/check, complete suite and snapshots omitted. No NEWS
entry was needed for this research record. Revision-specific verification,
current-main compatibility and separate Standards/Spec review records belonged
on the PR.

[capture]: https://github.com/sayuks/marginplyr/issues/1514
[json]: https://github.com/sayuks/marginplyr/issues/1516
[numeric]: https://github.com/sayuks/marginplyr/issues/1518
[csv-comment]: https://github.com/sayuks/marginplyr/issues/1506#issuecomment-6721240758
[body-comment]: https://github.com/sayuks/marginplyr/issues/1505#issuecomment-6721241028
[json-doc]: https://duckdb.org/docs/1.5/data/json/loading_json
[constructors]: ../R/grouping-spec.R
[inspection]: ../R/inspect-grouping.R
[identity]: ../R/grouping-context.R
[prior-csv]: inspect-grouping-capture-and-csv-boundaries-2026-11-08.md
[prior-deep]: inspect-grouping-deep-bughunt-2026-11-08.md
[prior-systematic]: inspect-grouping-systematic-2026-11-07.md
[prior-metadata]: inspect-grouping-bughunt-2026-11-07.md
