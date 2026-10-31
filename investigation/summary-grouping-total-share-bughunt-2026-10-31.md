# Bug hunt: summaries, Grouping specifications, and Total shares

Investigated: 2026-10-31
Repository snapshot: `339315c387f31deace77566f5d3294b15b323865`

The investigation began in a new worktree created from fetched `main`, on
branch `investigate/summary-grouping-share-2026-10-31`. Local `main`,
`origin/main`, and the initial worktree HEAD agreed on the snapshot above.
Four investigators divided Grouping specifications, summary evaluation,
contextual shares, and derived backend inputs. The coordinator independently
replayed each minimized finding, including both scope-mutation families.

The investigations from [October 28](summarize-grouping-share-bughunt-2026-10-28.md),
[October 29](summary-spec-share-bughunt-2026-10-29.md), its
[follow-up](summary-grouping-share-followup-bughunt-2026-10-29.md), and
[October 30](summary-spec-total-share-bughunt-2026-10-30.md) guided the remaining
boundaries. Their dated findings were not rewritten.

The requested scope was investigation and ticket publication. No package
implementation, package tests, dependency metadata, generated documentation,
or build exclusions were changed. The user delegated ticket boundaries and
publication, so the interactive breakdown checkpoint in `to-tickets` was
omitted. Three independent `bug`, `ready-for-agent` tickets were published;
none blocked another. The two scope-mutation variants shared one ticket
because both needed the existing callee-resolution boundary applied to scope
effects; each retained its own reproduction and acceptance criteria.

| Finding | Ticket | Priority | Established effect |
| --- | --- | --- | --- |
| F1 | [#1090](https://github.com/sayuks/marginplyr/issues/1090) | P2 | A later typed selection caused metadata acquisition before an independently invalid leading selection |
| F2 | [#1091](https://github.com/sayuks/marginplyr/issues/1091) | P2 | Indirectly called scope mutations bypassed contextual-share dependency refusals |
| F3 | [#1092](https://github.com/sayuks/marginplyr/issues/1092) | P3 | The first local summary read a bare external selector twice and silently summarized another column |

## F1: a typed sibling disabled leading names-only error preflight

```r
inspect_grouping(
  remote,
  .grouping = grouping_set(absent, where(is.numeric))
)
```

`remote` was a live DuckDB table with one integer column x. Selecting absent
was already invalid from the locally available column names. With x as the
second selection, inspection raised the missing-column External condition
and sent no query. Replacing only that second selection with the type
predicate caused a `SELECT * FROM grouping_preflight LIMIT 0` invocation
before the identical condition. A temporary trace of the actual DBI query
method confirmed the invocation; a construction-time Sent query record alone
was not treated as proof of invocation. No claim of a source-row scan was
made: the unwanted work was the zero-row metadata query itself.

On a derived dtplyr input with metadata-unsafe character conversion, the
names-only control raised the missing-column condition. The mixed call
instead raised the unsafe-metadata Package condition and recommended
collection. Collecting could not repair the nonexistent column. A counter
in the later predicate remained zero; a valid typed-selection control
incremented it, separating metadata acquisition from predicate execution.

The ranked hypotheses were an all-or-nothing names-only preflight, early
execution of the later predicate, and independent backend metadata work.
The zero predicate count, names-only control, and actual DBI trace separated
the first from the others. `prepare_grouping_plan()` compiled against the
names proxy only when the entire preflight was names-only. A typed sibling
made that property false and removed the earlier selection's preflight too.
ADR 0005 required a locally decidable error to precede backend metadata.

The retained matrix covered five constructors, five consumers, and three
leading invalid selections: absent, `all_of()` of absent, and an invalid
multiplication selection. Its 75 cases each executed five local/backend
controls or investigated calls. All 45 mixed DuckDB cases for inspection,
summary, and expansion invoked the extra query; their names-only controls
invoked none. SQL nesting was already unsupported, so its 30 matrix entries
were excluded from that query claim. All 75 dtplyr mixed cases masked the
leading selection condition. Later predicates were never evaluated in those
failures. Closed #342 addressed classification of entirely names-only
selections, rather than this independent mixed-sibling boundary.

Replay: `grouping-mixed-preflight.R` reported the minimized controls and
exited 0. Its `--assert` mode exited 1 against the desired zero-query and
original-error behavior. The matrix script retained its row-level CSV.

## F2: resolved mutation callees did not carry their scope effects

```r
data <- tibble::tibble(g = c("a", "b"), x = c(1, 3))
summarize_with_margins(
  data,
  prior = sum(x),
  value = { prior <- 2; (get("rm"))(prior); sum(prior) },
  fraction = share_of_total(value),
  .grouping = rollup(g)
)
```

The call was accepted with value one, three, four and fraction one-quarter,
three-quarters, one. Replacing the indirect removal with direct `rm()` raised
the intended Package condition: the source depended on earlier ordinary
summary alias prior. Removing the temporary lexical binding exposed that
alias to the later read, whichever spelling invoked the removal.

In the opposite dependency direction, a later ordinary summary returned
input payload values 100, 300 and 400 while its displayed earlier share alias
contained one-quarter, three-quarters and one. The documented ordinary-to-share
dependency refusal was absent. The issue concerned admitting a forbidden
dependency, not the division of otherwise eligible numeric summaries.

A second scope effect had the same boundary. An existing environment began
with the base environment as its parent. A directly named parent setter
changing it to the summary evaluation environment correctly invalidated its
external-lookup proof and refused the alias-dependent share source. Calling
that setter through a literal-name resolver or `do.call()` retained the old
proof and accepted the dependency. The later-ordinary direction also
reproduced the payload/share disagreement.

The ranked hypotheses were missing scope effects for resolved callees,
capture-boundary treatment of indirect-call arguments, and a changed runtime
summary scope. Canonical/indirect spelling pairs, no-share controls, and
independent lexical or non-inheriting lookup controls separated the first
from the others. `nested_removal_retained_bound()` and
`lookup_reparented_environments()` read direct call names. Other dependency
walks already used `resolved_callee_name()` and explicitly recognized the
literal naming primitives. This was not a request to infer arbitrary opaque
callback effects or to execute caller expressions while planning.

Across data.frame, tibble and data.table inputs, both helpers and both
dependency directions, the removal script established 84 missing refusals
and 144 successful controls. The reparenting script established 48 missing
refusals and 120 successful controls. The controls included canonical
mutations, no-share ordinary evaluation, retained lexical bindings and
non-inheriting reads. ADRs 0010/0017 and the share reference supplied the
self-contained-source and later-ordinary dependency contracts.

Closed #1042 and #1045 addressed nested removals and visible reparenting;
their complete bodies and comments did not cover these indirect callee
forms. Their fixes were present in the investigated snapshot. The new ticket
covered the remaining callee-resolution boundary across both effects.

Replay: `share-indirect-removal.R` and `share-indirect-reparenting.R` each
exited 0 in report mode and 1 with `--assert`, which required the missing
Package refusals. These were investigation signals, not failing package
regression tests.

## F3: a bare external selector was read once more during preflight

An active binding named cols returned x on its first read and y on subsequent
reads. The input held grouping values a, a, b, x values one, two, three and y
values five, six, seven.

```r
dplyr::summarise(data, dplyr::across(cols, sum), .by = g)
# One selector read; x = 3, 3.
summarize_with_margins(data, dplyr::across(cols, sum), .by = g)
# Two selector reads; y = 11, 7.
```

Both calls accepted the selection. Bare external-vector selection was
deprecated, so the reproduction explicitly quieted that ordinary deprecation
and the ticket used P3. The finding did not request promoting this spelling
over `all_of()`. A binding that errored on a second read instead made only
the Margin call fail.

The ranked hypotheses were an extra first-dot preflight read, name prediction
reading the binding, and ordinary per-group selector evaluation. Captured
call stacks contained `preflight_local_selection()` only on the first Margin
read. A preceding independent summary removed the extra read; `all_of(cols)`,
parenthesized cols and both environment-pronoun forms also restored ordinary
single-read behavior. These controls separated the first hypothesis.

The first bare symbol was sent through `resolve_summary_selection()` during
preflight and again during actual summary expansion. It could name an
external binding rather than only an input column. Closed #1070 repaired
function-local selection scope; this was an extra read before execution.

The retained matrix covered three local input classes, across/pick, five
selection spellings, named/unnamed output and first/preceded position: 120
cases. Six first-bare-across cases changed the selected column; the other
114 agreed with ordinary dplyr, including pick and selector-read counts.

Replay: `summary-selector-reproduction.R` reported both result sets, the
preflight stack distinction, successful controls and the read-once failure.
Report mode exited 0; `--assert` intentionally exited 1 on ordinary-result
equality.

## Exploration and bounded negative results

The retained scripts and logs were saved under
[`2026-10-31-summary-grouping-share-hunt`](2026-10-31-summary-grouping-share-hunt).
Counts below described separate bounded loops, not an exhaustive input space
or disjoint sets of all API calls.

| Loop | Executed scope | Established result |
| --- | --- | --- |
| Grouping selection differential | Seed 20261031; 288 selections, three injected-quosure depths, five constructors and `.by`, three unusual-name layouts | 5,184 comparisons agreed on selected values or External error classes |
| Mixed Grouping preflight | Five constructors × five consumers × three invalid prefixes, five controls/investigated calls each | 45 extra DuckDB queries; 75 masked dtplyr errors; later predicate effects zero |
| Scope-mutation share checks | Three eager input classes × Parent/Total × both dependency directions × indirect/canonical/control forms | 132 missing refusals; 264 successful controls |
| First-selector matrix | Three local classes × across/pick × five spellings × named/unnamed × first/preceded | Six F3 divergences; 114 matching controls |
| Local selection scope | 27 inline-function/local/evaluated-language/quosure/independent-mask forms | No additional difference after the recent lexical-scope fix |
| Across argument effects | `.fns`, `.names`, `.unpack`; active bindings and function calls; direct/head/whole/nested forms; named/unnamed and first/preceded | 96 comparisons agreed on results and evaluation counts |
| Local result shapes | 19 shapes × 19 shapes × named/unnamed | 722 comparisons agreed on values/attributes or paired refusal |
| Derived backend summaries | 23 source transformations × local/dtplyr/Arrow/SQLite/DuckDB | 103 matched results, five documented metadata refusals, one documented empty-input difference, four ordinary invalid-sort failures avoided by Margin construction, two ordinary Arrow input-operation refusals; zero unexplained differences |

The Grouping differential compared error classes, not full diagnostics or
warning behavior. Its selections included ranges, logical/arithmetic/set
operators, data/environment pronouns, selection helpers, unusual names,
constructor shapes and injected environments. The result-shape differential
included POSIXlt, duration, integer64, AsIs, list_of, data.table, sf/sfc and
record values alongside frame and scalar shapes. Paired errors did not
establish identical error diagnostics.

The backend loop included filters, key/measure mutation, renaming,
arrangements, distinct, union, intersection/difference, six join forms,
grouped mutation, upstream summaries and window expressions. Its oracle
executed ordinary backend summaries for each grain, so SQL empty-sum rules
were not assumed equal to local R's. The dtplyr empty control produced no
ordinary grand-total row while the Margin result retained the documented
scalar-summary grand-total row. Unsafe dtplyr metadata refusals were required
by the query policy. Arrow's ordinary intersect/setdiff input construction
failed before a Margin call; arranged Arrow and DuckDB ordinary summaries
retained invalid sort references while Margin summaries succeeded. None of
these raw differences established another package defect.

Additional minimized Arrow output-name/absorption controls found no new
failure. A reparented environment with its own alias-named binding was not
promoted to another issue: no new precision promise beyond the documented
conservative dependency analysis was established. Existing successful
reparenting tests covered safe parents, other targets or non-inheriting
reads, rather than that exact inheriting lookup. SQL NaN/infinity portability
was outside ADR 0010's value guarantee and was not treated as a new finding.

The negative results were limited to the tested grammar, fixtures, classes,
backend shapes and dependency versions. They did not establish live
PostgreSQL/hosted-service behavior, another dependency version, arbitrary
extension classes or callbacks, unbounded plans, concurrency, or interruption
behavior. No new numeric denominator-selection defect was established.

## Replay and publication

Run the retained R scripts from the repository root; each loads that checkout
with pkgload. All DuckDB drivers use `shared_home = FALSE`. Report modes assert
controls and record the investigated bad behavior. The four `--assert` modes
deliberately fail on the investigated snapshot:

```sh
Rscript investigation/2026-10-31-summary-grouping-share-hunt/grouping-mixed-preflight.R --assert
Rscript investigation/2026-10-31-summary-grouping-share-hunt/share-indirect-removal.R --assert
Rscript investigation/2026-10-31-summary-grouping-share-hunt/share-indirect-reparenting.R --assert
Rscript investigation/2026-10-31-summary-grouping-share-hunt/summary-selector-reproduction.R --assert
```

The other retained scripts replay the coverage loops named in the table.
Adjacent logs record their outcomes; `replay-status.json` records terminal
exit codes and `manifest.json` records retained evidence hashes. The matrix
script regenerates its CSV when replayed. These investigation scripts were
outside the source package and were not added to its regression suite.

`environment.txt` recorded R 4.6.1 (2026-06-24), aarch64 macOS; dplyr 1.2.1,
dbplyr 2.6.0, dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1,
RSQLite 3.53.3, DuckDB 1.5.5, rlang 1.3.0, tidyselect 1.2.1,
pkgload 1.5.3, sf 1.1.2 and bit64 4.8.6. The tested implementation came from
the checkout snapshot, rather than an installed package of the same version.

This was a Repository-only change under the local-check policy:
`investigation/` was excluded from the package and drove no generation. The
common `/Users/sayuks/.local/bin/marginplyr-review-ready` entry point was
nevertheless invoked from this worktree as instructed. Its exact submitted
SHA and terminal outcome, publication checks and the review record were
recorded on the pull request. CI monitoring was outside the requested scope.
