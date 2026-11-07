# Grouping inspection metadata and plan bug hunt

Investigated: 2026-11-07
Tested main: `0d99de31aa3c23ac36df363e2a655d1c12c0de89`

The worktree started from freshly fetched main. The hunt exercised the public
`inspect_grouping()` entry point, including the shared Grouping specification
compiler and the inspection-only paths for lazy metadata, ordinary tibble
output and Sent queries. The final replay used a separate source archive and
independent expectations for ordered sets, membership, identifiers and
backend activity.

The prior [identity-boundary investigation][identity], [receiver-dispatch
hunt][receiver-prior], [nesting boundary investigation][nest-prior] and
[deep nesting investigation][deep-prior] supplied case families and
separating controls. Their counts are outside the measurements below.

## Confirmed defects

| ID | Observed consequence | Repair ticket |
| --- | --- | --- |
| D1 | Typed DuckDB inspection prepared a dynamic PIVOT and evaluated three source rows despite the zero-row result. | [#1461][pivot] |
| D2 | DuckDB typed metadata invented a dimension, exposed a fixed-key invariant, and missed a genuine overlap. | [#1462][proxy] |
| G1 | Arrow, DuckDB and RSQLite inspection bypassed a tibble names method and chose the wrong fixed key or refused a valid dimension. | [#1463][dispatch] |
| G2 | The default duplicate policy exhausted a 96 MiB vector heap before rejecting a repeated-unit cube or Cartesian product. | [#1464][budget] |
| A1 | Direct Arrow reader normalization displaced both invalid Option argument conditions with an Arrow schema error. | [#1465][reader] |

Each ticket covers an independently verifiable user-visible behavior, with
`bug`, `ready-for-agent` and no blockers. The user delegated the split and
publication. This branch records the evidence and its replay commands.

### D1. Dynamic DuckDB pivot preparation evaluated source rows

A DuckDB table held three rows keyed east, west and west. The dynamic PIVOT
discovered output names from its pivot expression. The caller supplied east
and west as the lazy input's captured variables. The pivot expression
advanced a private sequence for each source row evaluated.

Name-only inspection used the captured variables, recorded zero Sent
queries and left the sequence unset. Typed inspection of the same query
with an explicit static IN list returned both dimensions from one metadata
statement and also left the sequence unset. Typed inspection of the dynamic
query returned the same dimensions and set the sequence to 3. Its only
owned statement was a `selection_proxy` ending in `LIMIT 0`, with status
`returned`. An additional probe put an error sentinel in the dynamic pivot
key and reproduced its execution in DuckDB's preparation phase.

The sequence distinguishes value-dependent binding from normal type
discovery and makes the read observable even on a successful inspection.
ADR 0020's exemption requires no source-row processing. The evidence covers
this dynamic pivot shape on DuckDB 1.5.5.

### D2. DuckDB's metadata wrapper changed captured column identities

A raw query exposed integer `region` and character `REGION` with both names
explicitly captured by the lazy input. Top-level native collection and
name-only inspection retained both spellings. The zero-row metadata
statement nested the raw query, which exposed the second field as
`REGION_1`. A character predicate then reported `REGION_1` as the included
dimension. In the fixed-key argument the same predicate failed with
`simpleError` and `Unknown .by column` for the invented name. Fixing on
`REGION` and selecting character dimensions accepted the two spellings
as distinct. The literal equivalent raised the expected overlap condition.

The contrast separates dbplyr's schema discovery from marginplyr's
consistency requirement. Without explicit variables, dbplyr captured
`REGION_1` itself in the measured case. The report concerns the mismatch
between the declared input and its selection metadata. Closed #697
addressed equivalent identifiers at Margin-result and private-alias
boundaries.

### G1. A names-only proof omitted the tibble receiver's method

A scoped `names.tbl_df` method returned the stored names outside selection
and reversed the two names when its receiver was the active selection data.
Native tidyselect selected b from the first active name. On Arrow, DuckDB
and RSQLite, the direct selector returned a as the fixed key. In a Grouping
specification beside fixed a it falsely refused overlap. An identity wrapper
around the same expression selected b in both roles on all three backends.
Without the method, native tidyselect selected a.

The direct and wrapped calls isolate the names-only proof. The real
selection proxies in these cases were tibbles. The frame classifier checked
`names.data.frame` and `names.default`, leaving out the tibble receiver.
Closed #1434 and #1439 established related contracts for local subclasses and
the dtplyr data.table receiver. The current-main replay used Arrow, DuckDB
and RSQLite proxies after those fixes.

### G2. Duplicate rejection depended on expanding an exponential family

A one-row frame held only k. A cube repeated that dimension 20 times. A
Cartesian product repeated the family containing k and the empty set
20 times. Each specification described 1,048,576 occurrences with only
two distinct grouping sets. Under a 96 MiB R vector heap, the default
duplicate policy hit the vector limit for both families. The same
specifications with `"drop"` returned two rows with included k then the empty
set, set IDs 1 and 2, and Grouping identifiers 0 and 1.

Three-unit controls retained eight occurrences under `"keep"` and reported
duplicate positions 1 through 7 under `"error"`. The error path builds the
occurrence family and constructs position diagnostics from it. Closed
#1245 and #1314 optimized drop. Closed #1246, #1228 and #1342 addressed the
precedence of known duplicates over a later typed selection. The new
witnesses are fully resolved and need no metadata.

### A1. Direct reader normalization ran before Option arguments

An Arrow Table and a direct RecordBatchReader each held two fields named a.
An invalid `.format` or `.duplicates` produced the argument-specific
Package condition on the Table. The reader path produced Arrow's field
uniqueness error for both calls because normalization performed a schema
selection before checking the Option arguments. Reading the original
reader after either rejection still yielded its two rows.

Closed #510 established direct reader admission without consuming a batch.
The ordering difference was observed before Grouping plan preparation.
ADR 0005 supplies the locally decidable error precedence used in the ticket.

## Finite coverage and limits

The [matrix][matrix] generated 96 seeded ordered-set specifications
combining literal sets, grouping-set unions, composite rollups, composite
cubes and nested Cartesian products. It used an independent set algebra
oracle for both output formats, three duplicate policies, non-syntactic
and non-ASCII names, and optional fixed keys. All six carriers completed
the same 576 comparisons each.

| Carrier | Exact full plans | Expected duplicate refusals |
| --- | ---: | ---: |
| Populated tibble | 434 | 142 |
| Empty tibble | 434 | 142 |
| Derived Mutable dtplyr step | 434 | 142 |
| Arrow Table | 434 | 142 |
| Live DuckDB | 434 | 142 |
| Live RSQLite | 434 | 142 |
| Total | 2,604 | 852 |

The matrix checked all six output columns, the ordinary tibble class,
occurrence order and zero recorded SQL on each name-only case. Sixteen
additional comparisons covered 0, 1, 2, 30, 31, 32, 64 and 96 dimensions
with a fixed key. The Arrow reader control consumed one batch explicitly
before inspection, inspected three times, then read the same remaining batch
and reached EOF. Typed controls matched on tibble, empty tibble, dtplyr,
Arrow and DuckDB. RSQLite refused the predicate without an owned statement.
The Mutable dtplyr source retained its serialized bytes, and its upstream
predicate saw zero elements throughout.

The exploratory controls distinguished native backend behavior from a
package defect. Stock dtplyr wrappers for renaming, relocation and set union
ignored unrelated caller functions with matching names and retained their
normal types. Several projections involving data.table engine-variable
names also differed from captured metadata in native dtplyr collection.
They were kept outside the findings because the native projection itself
renamed or refused the fields. The prior Arrow right-join schema discrepancy
was likewise outside the findings because native typed Arrow selection
already used that declared schema. An apparent mismatch in the reader matrix
came from comparing the outer classes of a tibble and an Arrow-converted
data frame. The final assertion compared the named columns and values.

No hosted database, live PostgreSQL, other OS or dependency version was
measured in this run. The finite matrix does not cover arbitrary dispatched
methods, arbitrary SQL preparation effects, unbounded expression graphs or
general memory ceilings.

## Reproduction and publication

Run from an archive of the tested package source. The three scripts are
standalone research probes.

```sh
Rscript /path/to/evidence/matrix.R
Rscript /path/to/evidence/reproduce.R observed
Rscript /path/to/evidence/reproduce.R contract
R_MAX_VSIZE=96M Rscript /path/to/evidence/duplicate-budget.R observed
R_MAX_VSIZE=96M Rscript /path/to/evidence/duplicate-budget.R contract
```

The [public repro][repro] and [bounded allocation repro][allocation] have
two modes. The observation mode asserts the dated failures and runs the
passing controls. The contract mode asserts the desired behavior and exits
1 on this baseline. The matrix exits 0 with the comparison counts above.

The environment was arm64 macOS, R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
tidyselect 1.2.1, rlang 1.3.0, tibble 3.3.1, vctrs 0.7.3, dtplyr 1.3.3,
data.table 1.18.6.1, DBI 1.3.0, DuckDB 1.5.5, RSQLite 3.53.3 and Arrow
25.0.1. Main was fetched again before ticket publication and still named
the tested SHA. Searches across open and closed Issues and PRs included
`"dynamic PIVOT" "DuckDB"`, `"REGION_1" "metadata"`, `"names.tbl_df"`,
`"duplicates" "vector memory limit"` and `"Field names must be unique"
"reader"`. The duplicate-budget search found the drop-policy tickets above.
The full related reports also distinguished the source-name, receiver and
reader boundaries. The five new Issues were read back as open with both labels.

## Submission effects

The cumulative branch diff adds this dated note and three replay scripts.
They are human-invoked evidence in the existing source-package exclusion.
Package code and tests, generated documentation, examples, vignettes,
dependencies, build rules and verification tooling are unchanged. The
Review-ready scope is `source` on that basis. The submitted SHA, compatibility
assessment and terminal verification outcome belong on the PR.

[identity]: grouping-plan-identity-boundaries-2026-10-01.md
[receiver-prior]: expand-grouping-receiver-dispatch-bughunt-2026-11-07.md
[nest-prior]: nest-grouping-boundaries-2026-11-07.md
[deep-prior]: nest-spec-deep-bughunt-2026-11-07.md
[matrix]: 2026-11-07-inspect-grouping-bughunt/matrix.R
[repro]: 2026-11-07-inspect-grouping-bughunt/reproduce.R
[allocation]: 2026-11-07-inspect-grouping-bughunt/duplicate-budget.R
[pivot]: https://github.com/sayuks/marginplyr/issues/1461
[proxy]: https://github.com/sayuks/marginplyr/issues/1462
[dispatch]: https://github.com/sayuks/marginplyr/issues/1463
[budget]: https://github.com/sayuks/marginplyr/issues/1464
[reader]: https://github.com/sayuks/marginplyr/issues/1465
