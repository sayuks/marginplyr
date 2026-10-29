# Nesting and Grouping specification shared-cache bug hunt

Investigated: 2026-10-29
Baseline: `ee4321789ca60b6259c53df2852f1574aaede584` (`main`)

## Outcome and ticket boundary

This follow-up investigated `nest_with_margins()`, `nest_by_with_margins()`,
the five Grouping specification constructors, and `inspect_grouping()` after
the preceding nesting, dispatch, special-name, sf, and cached-input repairs
had landed. The new managed worktree and branch started from `main`;
`origin`'s main reference matched the audited SHA when checked during the run.

One independently reproduced cause was published through `to-tickets`:

| Finding | Observed effect | Ticket | Blocking edges |
| --- | --- | --- | --- |
| Shared cached dtplyr branches contaminated the schema-only graph | Wrong column types dropped selected fixed keys, refused a valid rollup, and manufactured copy warnings or strict-warning metadata refusals | [#1024](https://github.com/sayuks/marginplyr/issues/1024) | None |

The ticket was labelled `bug` and `ready-for-agent`. The type error and the
artificial warning shared the same missing branch-isolation boundary; separate
tickets could have allowed warning suppression to leave the wrong selections
intact. The user delegated granularity and publication, so the `to-tickets`
breakdown interview was omitted. No parent issue was modified.

Only investigation evidence was added. No package implementation, permanent
regression test, generated document, build exclusion, or public contract was
changed. The fix and post-fix phases of `diagnosing-bugs` were outside the
requested scope; the desired-behavior reproductions deliberately remained red.

## F1: cached branches changed another branch's metadata

A default immutable dtplyr root had two integer columns. Two cached derivations
added the same new column with different types:

```r
root <- dtplyr::lazy_dt(data.frame(g = 1:2, v = 3:4))
left <- dplyr::compute(dplyr::mutate(root, new = v + 1L))
right <- dplyr::compute(dplyr::mutate(root, new = "a"))
input <- dplyr::inner_join(left, right, by = "g")
inspect_grouping(input, .by = where(is.character), .format = "list")
```

Ordinary collection of `input` emitted no warning and returned character
`new.y`. The inspected plan's `fixed[[1]]` was instead `character(0)`.
Inspection of the explicitly collected input selected `"new.y"`. Both nesting
verbs dropped that outer key, returning only the list column, while their
collected-input controls returned `new.y` and the list column. A rollup over
`where(is.character)` was refused with:

```text
`rollup()` requires at least one dimension.
```

The proxy had classified `new.y` as integer. This was a wrong Grouping plan,
rather than a backend difference in how an already correct plan was executed.
The coordinating investigator and a separate validator independently reproduced
the plan and nesting symptoms.

The same graph boundary manufactured data.table's shallow-copy warning. A
cached arithmetic derivation unioned with itself collected without a warning;
typed inspection and both nesting verbs emitted the warning while acquiring
metadata. With `warn = 2`, those calls instead raised the package's unsafe
metadata refusal, although ordinary source collection still succeeded.
Name-only inspection was a successful control because it did not need the
schema-only graph.

The experiments separated an ordinary upstream-input limitation, a damaged
empty-root table, and loss of cached-branch isolation. The isolated roots had
valid self-reference metadata before dtplyr merged same-name source entries.
The package's schema rewrite removed cached assignments and local bindings,
but retained the multi-input graph's assumption that the cached branches did
not need copying. Its first expanded mutation could then write to a shared
zero-row root before another branch used it. With integer arithmetic first,
the later scalar character assignment inherited the already-created empty
integer column. Retaining the cache bindings or restoring the copy requirement
on an experimental graph recovered the character type and removed the warning.
Those graph experiments were diagnostic controls, not an implementation patch.

[The backend record](backend/README.md) retained the public reproductions,
graph query, self-reference observations, copy controls, row-size tracing, and
full matrices. A cached rename reused in a set/semi-join graph also collected successfully
but failed only during schema acquisition: after cache expansion, the first
rename changed the shared empty root and the next could not find the old
column name. Twelve broad-sweep calls reproduced that F1 refusal.

The type matrix crossed four scalar assignment types on each
side, four joins, four type predicates, and three consumers. Of 768 calls,
288 selected the wrong keys or returned the wrong outer columns; 480 matched
their selection oracle, and all 768 emitted an artificial warning. The
arithmetic/character minimal example's left join succeeded, but that was a
local control: constant-assignment cases also contaminated left joins.

The metadata evaluations traced in the reproductions used zero-row roots.
Caller source snapshots stayed unchanged, and this evidence did not establish
any full source-row read or source destruction. [ADR 0002](../../design/adr/0002-acquire-typed-metadata-once.md)
owned the faithful input-schema boundary, and
[ADR 0020](../../design/adr/0020-ask-before-reading-a-lazy-input.md) owned the
no-unrequested-row-processing boundary. The supported repair had to retain
both; merely suppressing the warning would not repair the wrong schema.

All-state issue searches and the bodies and comments of #996, #1003, and #451
were checked before publication. #996 concerned saved row predicates;
#1003 concerned shared cached aliases in executable result branches;
#451 concerned explicitly mutable roots. Their already-landed repairs did not
prevent this default-immutable schema-only failure. Open #468 was the separate
upstream-report task for mutable roots. No duplicate ticket was published for
any preceding finding.

## Bounded search coverage

The tracks used separate oracles and their counts were not combined into a
single assertion total. A matching documented refusal was distinguished from
a successfully nested result.

| Track | Retained observations |
| --- | --- |
| [Local source-row and projection](local/README.md) | 27 scalar/structured/list/record shapes; 3,888 calls and 14,118 independent membership/projection comparisons, including 1,296 grouped-input/explicit-`.by` pairs |
| Local Margin order and empty payload | 1,728 independently ordered calls; 1,998 no-payload calls and 7,356 zero-column cells with independently checked source-group cardinalities |
| Direct data.table inputs | 4,320 calls: 1,992 successful tibble-control pairs and 168 matching documented refusal pairs; serialized sources unchanged |
| [Specification algebra](specification/README.md) | Seed 291029701; 5,000 generated trees: 4,607 exact legal expansions, 389 expected empty-unit refusals, four probe-size exclusions |
| Reentrant selections | 192 public-result comparisons, 384 callbacks, and 768 context-restoration assertions matched; callback budgets and observed options restored after nested success or caught failure |
| Remaining implicit dispatch | 243 non-dispatch observations over 27 candidate operations and nine implicit classes, calibrated by 27 observed-dispatch controls |
| [dtplyr execution graphs](backend/README.md) | 2,592 calls: 2,364 value/cell matches, 216 conservative head safety-gate refusals, and 12 F1 cached-rename metadata refusals; cached-mutate warnings also led to F1 |
| Backend cache and permission boundaries | 192 grouped-cache/order calls matched; 128 cache/Mutable-step boundary checks and 20 unsafe-schema callback checks passed |
| Remote inspection/refusal boundaries | 48 Arrow calls preserved 12 readers; 28 SQLite calls sent zero SQL statements; 28 DuckDB calls sent 14 statements, all `LIMIT 0` |
| Direct and repeated materialization | `coordinator/materialization.R`: 552 nesting configurations and 2,208 comparisons across direct/repeated collection and compute matched separately built local results |

The materialization matrix included empty and populated inputs, both `.keep`
values, first/last Margin order, a two-dimensional cube, and dot-dot,
data.table-special, and internal-prefix output names. dtplyr results with
dot-dot names were read through `as_tibble(.name_repair = "minimal")`:
ordinary dtplyr `collect()` revalidated those names and refused them too.
That independent collection limitation was not a new package finding.

The local track excluded POSIXlt-to-POSIXct differences reproduced by ordinary
union, and distinguished raw typed-missing omission refusals from accepted
complete single-set raw keys. The remote track excluded a rename graph whose
ordinary source collection already failed SQL translation. The 216 head-graph refusals were existing conservative safety-gate outcomes;
no metadata was acquired in those cases. The probe did not adjudicate complete
support for every schema-preserving dtplyr operation and did not count those
refusals as successful nesting or establish that source rows were necessary.
Unsafe derived metadata, explicit Mutable steps, unsupported nesting backends, output-name
collisions, and `.rows` rowwise-key restrictions retained their documented
boundaries. The dedicated records stated their controls and probe limits.

The preceding records read included the
[2026-10-28 nesting/specification audit](../2026-10-28-nest-spec-bug-hunt/README.md),
[2026-10-29 nesting audit](../2026-10-29-nesting-grouping-bug-hunt/README.md), and
[selector-dispatch follow-up](../2026-10-29-nest-spec-dispatch-followup/README.md).
Their observations were historical inputs, not additional measurements of this
baseline. No earlier investigation note was rewritten.

## Replay and validation

Run the commands from the investigated repository root. Every retained R
entry point loaded that checkout with `pkgload::load_all()`; adjacent logs and
CSV files captured the observations. Trailing horizontal whitespace was
removed from text logs for Git; diagnostic wording and measured values were
retained. The backend record identified the
expected-red `--assert` commands, which asserted faithful types/selections or
absence of the artificial warning. Their exit-one results were evidence that
the finding remained unfixed on the audited SHA. Ordinary observation modes
and the other sweeps exited zero after checking their stated controls; a zero
exit from an observation mode did not mean F1 had been repaired.

The focused existing suite completed with terminal exit zero for 21 files:

```sh
NOT_CRAN=true Rscript -e 'testthat::test_local(".", filter="^(nest-operation|nest-rows|dtplyr-special-nesting|dtplyr-asis-nesting|dot-dot-nesting|sf-nesting|grouping-plan|grouping-interface|inspect-grouping|grouping-selection-budget|selection-dispatch|selection-helper-arguments|selection-arithmetic|selection-operators|data-subscript-selections|injected-nested-grouping|cyclic-grouping|packed-grouping|matrix-grouping|captured-grouping-helpers|qualified-selection-helpers)$", reporter="summary", stop_on_failure=TRUE)'
```

`coordinator/focused-suite.log` retained that run. The existing `.Rbuildignore`
excluded the entire investigation directory, and no package-check input was
changed. This was Repository-only under
[the local-check classification](../../design/agents/local-checks.md), so the
Package-affecting Review-ready check was not required. Final linter,
context-budget, document-reference, whitespace, and review outcomes were
recorded on the pull request. `MANIFEST.sha256` identified the retained files.

[environment.txt](environment.txt) recorded the local R/macOS and dependency
versions. These finite probes did not establish behavior for arbitrary custom
S3/S4 methods, every subclass, unbounded plans, resource exhaustion, another
R/dependency version or operating system, a live PostgreSQL service, or a
hosted database. The remote boundary checks did not establish nesting support
on SQL or Arrow. No universal absence-of-bugs claim followed from the passing
comparisons. CI monitoring was outside the request.
