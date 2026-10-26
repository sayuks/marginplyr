# Nesting and typed Grouping specification follow-up

Investigated: 2026-10-26
Revised: 2026-10-26 — constructor-depth measurement in `investigation/2026-10-26-nesting-specification-followup/algebra-depth.R`

## Snapshot and outcome

This investigation examined `main` at
[`bfcdaf76a36d102132cd039c68ba8bc05a4fd465`](https://github.com/sayuks/marginplyr/commit/bfcdaf76a36d102132cd039c68ba8bc05a4fd465)
in a new worktree on `codex/nest-spec-bug-hunt`. The scope was
`nest_with_margins()`, `nest_by_with_margins()`, the five Grouping specification
constructors, Grouping-plan inspection, and the associated Grouping identity
helpers. Three defects were reproduced through public calls and independently
replayed by the coordinating investigator.

| Finding | Observed impact | Ticket |
| --- | --- | --- |
| F1: qualified selection operators forced a promise against the wrong proxy | Incorrect fixed keys or dimensions, incorrect nested membership, or premature duplicate-set refusal | [#889](https://github.com/sayuks/marginplyr/issues/889) |
| F2: omitted AsIs identity keys lost their class | Both nesting verbs failed on a supported dtplyr dimension | [#890](https://github.com/sayuks/marginplyr/issues/890) |
| F3: ordering a derived dtplyr step was refused as unsafe metadata | Valid nesting and typed inspection required unnecessary eager collection | [#892](https://github.com/sayuks/marginplyr/issues/892) |

The tickets were published through the `to-tickets` workflow, with `bug` and
`ready-for-agent` labels. Each represented an independently verifiable behavior
and had no blockers. The user delegated granularity and publication, so no
approval interview was needed. Existing open issues and related closed
#825, #837, #860, #868, and #873 were checked to distinguish the defects from
previously repaired routes. No parent issue was modified.

No implementation, package test, generated documentation, public contract, or
configuration was changed. The preserved scripts were investigation evidence,
excluded from the source package by its existing build exclusions.

## F1: an ordinary wrapper selected the wrong grouping columns

A normal wrapper default read numeric column names from the active tidyselect
data. Its input held a character column named `cols` and a numeric column
`key` whose two values were equal:

```r
x <- tibble::tibble(cols = c("x", "y"), key = c(1, 1))
numeric_columns <- function() {
  data <- tidyselect::peek_data()
  names(data)[vapply(data, is.numeric, logical(1))]
}
selection <- function(data, cols = numeric_columns()) {
  dplyr::select(data, base::c(cols))
}
fixed <- function(data, cols = numeric_columns()) {
  inspect_grouping(data, .by = base::c(cols), .format = "list")
}
grouped <- function(data, cols = numeric_columns()) {
  inspect_grouping(data,
    .grouping = grouping_set(base::c(cols)), .format = "list")
}
```

The direct dplyr selection returned only `key`. The inspected fixed keys and
included dimensions instead contained both `cols` and `key`, for local and
dtplyr inputs. Both nesting verbs split the source into two one-row cells;
the expected result had one key with both source rows in its cell. A second,
smaller wrapper passed an explicit promise that selected `a` if the selection
proxy's `a` was numeric and `b` otherwise. With character `a` and numeric `b`,
dplyr selected `b`, while inspection selected `a`.

Neither reproduction needed an active binding, a forged specification, or an
unsupported input. A separate changing-binding control caused
`grouping_sets(base::c(a), a)` to report duplicate sets prematurely; the same
selection written with `all_of(a)` resolved the distinct sets successfully.

### Cause and controls

The ranked explanations were an incorrect operator classification, retention
of the first fixed-key answer or promise value, and different upstream
selection semantics. Direct tidyselect controls settled the last hypothesis:
qualified and parenthesized operator calls used ordinary R evaluation, while
a bare tidyselect operator used selection syntax.

`is_name_only_expr()` in `R/grouping-plan.R` walked these ordinary calls as
selection operators. A symbol matching an input column therefore appeared
settled by names even though the call read the wrapper's lexical binding.
`resolve_fixed_keys()` retained that names-only answer. In a Grouping
specification, the discarded names-only compilation forced the promise against
`grouping_name_proxy()`, whose column values were integers; R retained that
value for the later typed compilation. Typed canonical compilation could not
repair an already forced promise.

The public constructor and inspection help delegated column selection to
tidyselect. ADR 0005 governed the early-error ordering, ADR 0008 the compilation
and evaluation boundaries, and ADR 0013 the inspection result. This was a
misclassification of a typed selection, beyond the refused-arithmetic branch
repaired by #868 and the separate binding-read budget repaired by #873.

## F2: dtplyr nesting lost the class of an omitted identity key

The minimal reproduction had one row and one column:

```r
x <- tibble::tibble(g = I(1))
lazy <- dtplyr::lazy_dt(x)
dplyr::collect(nest_with_margins(lazy, .grouping = rollup(g)))
```

It failed with:

```text
Item 2 of x is 'AsIs' but the corresponding item of y is 'numeric'.
```

`nest_by_with_margins()` failed at its collection boundary too. Local rollup,
a dtplyr single grouping set, the same column as a fixed key, and an ordinary
dtplyr union using an explicitly class-preserving missing value all succeeded.
The same failure was observed with `.keep` both ways and with `"Total"`,
`NULL`, and `NA_character_` labels. Plain unclassed columns supplied successful
controls.

The hypotheses separated the generated identity column, the public dimension,
and an upstream inability to combine the source class. The class-preserving
union ruled out the latter. The public dimension's typed-missing expansion and
summary also succeeded. Nesting alone added a typed identity key to prevent
source values with the same display from merging.

`execute_margin_nest()` in `R/nest_with_margins.R` supplied the identity's
missing prototype. `expand_margin_union()` in `R/grouping-adapter-union.R`
repeated that prototype for the omitted branch. For base atomic AsIs vectors,
`rep()` removed the AsIs class, leaving the included and omitted identity
columns incompatible under the backend's union. The public dimension followed
a different replacement route and retained its supported class.

The nesting help promised a cell for each group in each grouping set and
original key values under `.keep`. ADR 0016 delegated backend result classes
without authorizing incompatible internal keys. This atomic identity-key
route was distinct from the classed-list omission repaired by #860.

## F3: a metadata-preserving ordering composition was refused

An immutable dtplyr root could be arranged and nested successfully. Adding a
row-only filter before the same ordering changed the outcome:

```r
x <- dtplyr::lazy_dt(tibble::tibble(g = c("b", "a"), v = 1:2))
nest_with_margins(dplyr::arrange(x, g), .grouping = rollup(g))
nest_with_margins(
  dplyr::arrange(dplyr::filter(x, v > 0), g),
  .grouping = rollup(g)
)
```

The first call succeeded. The second was refused with a Package condition:

```text
Can't safely determine column metadata for `.data` from this dtplyr step.
```

Both nesting verbs and typed inspection were affected; name-only inspection
succeeded. Ordering after selection, rename, relocation, safe mutation, and a
supported join reached the same representation gap.

The hypotheses were an omitted representation in metadata admission, an
actual change in types or factor levels, and a need to evaluate the filter.
The controls used a counting row predicate, copied caller tables, and a schema
comparison over integer, Date, and factor columns including unused levels.
Inspection and the isolated zero-row proxy ran no source-row predicate and
left the caller tables unchanged. Explicit collection alone visited the two
source rows and produced the same schema as that proxy.

The composition produced a `dtplyr_step_call` with `fun = "setorder"`.
`dtplyr_metadata_safe_step()` in `R/backend-metadata.R` admitted the relevant
column-name/reordering call forms but omitted this row-ordering form. Root
ordering used a different representation and passed. Inspection of a Mutable
step could reach the same rejection even when ordering the root directly.

ADR 0020 required faithful metadata without source-row processing; the controls
established both for these calls. ADR 0029 allowed isolated inspection of
Mutable steps and continued to require their refusal by Margin verbs. This
finding did not request relaxing that refusal or admitting arbitrary
transformations. It extended the metadata-preserving composition missed by
the grouping-wrapper repair in #837.

## Search coverage and rejected candidates

Three parallel tracks investigated nesting, constructor syntax/algebra, and
plan/backend preparation. The coordinating track added combined-name,
structured-membership, and Grouping identity checks. Existing dated nesting
investigations from 2026-10-25 and 2026-10-26 were read as historical evidence;
their successful comparisons were not counted as new measurements.

The preserved harnesses are under
`investigation/2026-10-26-nesting-specification-followup/`.

| Harness | New coverage and oracle | Observed outcome |
| --- | --- | --- |
| `typed-selection-algebra.R` | Seed 104728; 2,000 ASTs, depth at most three; five constructor kinds, composite/overlapping/empty selections, 34 selection spellings, twelve columns spanning eight types and non-syntactic names; independent mathematical expansion plus direct tidyselect leaf evaluation | 3,120 keep/drop inspections matched; 440 expected grammar refusals |
| `extended-names.R` | Five name roles, both verbs, local/dtplyr, `.keep`, empty/nonempty inputs; control characters, Unicode, data.table symbols, pronouns, constructor-like names, and private-name prefixes | 4,664 public calls matched outer typed values and cell row multisets |
| `name-interactions.R` | Seed 264013; 150 random assignments of seven distinct special names across simultaneous fixed/dimension/payload/identifier/list-column roles; both backends and `.keep`/Margin-order variants | 290 result comparisons matched a safely named local control |
| `structured-membership.R` | Seed 264012; eleven structured/scalar types, 30 iterations, 0–8 rows, both verbs, fixed keys, `.keep`, cube and explicit sets; independent `vec_group_loc()` membership | 2,310 grouping-set comparisons matched; retained original keys matched source slices |
| `grouping-identity.R` | Five plan shapes including repeated sets, two duplicate policies, fixed/no-fixed keys, empty/nonempty inputs, local/dtplyr/Arrow/live SQLite/live DuckDB | 200 results and 1,770 rows agreed with inspection for three bits, default/reversed masks, and fixed-key bits; declared helper types held |
| `asis-controls.R` | Eleven plain base/classed types plus AsIs wrappers over four atomic types, both verbs, two `.keep` values, three labels, empty/one-row inputs | 96 dated defect observations; 264 successful dtplyr rollups and 1,080 local/single-set/fixed-key controls |
| `dtplyr-metadata-sweep.R` | 28 derived steps and eight multi-input graphs, five type predicates; copied source tables and counting filters | 150 faithful typed-proxy comparisons; six refusals comprised two ordering defects and four intentionally unsafe derivations; no inspection source-row work or caller mutation |
| `operator-scope.R` | Eight operator forms with qualified/parenthesized heads, compared with direct tidyselect | Eight wrong selections for `c`, binary minus, range, and division; eight matching upstream refusals |
| `qualified-operator-coverage.R` | Both nesting verbs and inspection, fixed/dimension positions, local/dtplyr/Arrow/DuckDB; all five constructors on local/dtplyr; distinct-set control | All saved F1 symptoms reproduced |
| `sorted-metadata-controls.R` | Immutable/Mutable root, filter and ordering graphs; isolated zero-row metadata compared with explicit collection | Ordering refusal isolated without source-row work or caller mutation; explicit collection visited two rows |
| `upstream-controls.R` | Direct ordinary dtplyr conversions, grouping and unions for the rejected candidates | External limitations reproduced independently |

Additional failure-focused matrices and metadata probes were described by their
saved output. These observations deliberately recognize the unfixed defects;
they do not substitute for the red-capable acceptance reproductions.

Excluded candidates included:

- A shadowed `everything()` helper returned the same selection through direct
  tidyselect, whose helper context supplied its own binding.
- Packed columns flattened by data.table before reaching marginplyr, raw or
  complex dtplyr set-operation limits, and unsupported list grouping were
  attributed to controls outside the package's generated identity route.
- The first combined-name harness accidentally passed a named vector to
  `all_of()`, asking to rename the fixed key. Removing those unintended names
  repaired the harness and produced the 290 successful comparisons above.
- A vctrs fixture conversion changed a combined AsIs/Date class before the
  target call. The decisive AsIs finding used the original base atomic vector.
  Compound AsIs classes were not generalized into its acceptance boundary.
- Empty hms conversion produced `logical(0)` through its own `as.character()`
  method and failed an ordinary union as well; it was not filed as a separate
  marginplyr defect.

## Reproduction and local verification

Each script accepts a repository root as its first argument, defaulting to
`.`. Run from the investigated checkout, for example:

```sh
Rscript investigation/2026-10-26-nesting-specification-followup/reproduce-typed-selection.R
Rscript investigation/2026-10-26-nesting-specification-followup/reproduce-asis-identity.R
Rscript investigation/2026-10-26-nesting-specification-followup/reproduce-sorted-metadata.R
```

Each command intentionally exited 1 on the investigated snapshot after its
working control succeeded. A failure of the asserted public behavior was the
signal; the error alone was not considered sufficient evidence. The fuller `default-promise-selection.R` reproduction also intentionally
exited 1. The other harnesses exited 0 when their controls and dated
observations agreed.
`replay-results.txt` preserved the final runs and exit statuses;
`environment.txt` recorded dependency versions.

The existing focused suite completed without failures:

```r
testthat::test_local(
  ".",
  filter = paste(
    "nest-operation|nest-rows|grouping-plan|inspect-grouping",
    "selection-helper-arguments|grouping-interface|cyclic-grouping",
    "dtplyr-special-nesting", sep = "|"
  ),
  reporter = "summary"
)
```

The committed change was Repository-only under
`design/agents/local-checks.md`, so it did not require the package-affecting
Review-ready check. Both linters and the context-budget and document-reference
verifiers were run before pushing; their terminal outcomes were recorded in
the pull request. No CI monitoring was performed.

## Environment and limits

Measurements used R 4.6.1 (2026-06-24), dplyr 1.2.1, tidyr 1.3.2, dtplyr 1.3.3,
data.table 1.18.6.1, dbplyr 2.6.0, Arrow 25.0.1, RSQLite 3.53.3, DuckDB 1.5.5,
rlang 1.3.0, tidyselect 1.2.1, tibble 3.3.1, and vctrs 0.7.3 on macOS.

This finite investigation did not prove every tidyselect program or backend
correct. It did not exercise a live PostgreSQL server, other operating systems,
older dependencies, unlimited specification depth, or large performance stress
cases. Internal metadata probes were diagnostic controls for the stated causes,
not additional public compatibility requirements. No repair was attempted.

## Revisions (2026-10-26)

The coverage table's "depth at most three" described the generator's recursion
parameter rather than the depth of its resulting constructor tree. A separate
replay of the same seed and 2,000 generated ASTs measured a maximum of five
constructor nodes along a path, or depth four when the root is zero. A rollup
or cube at generator recursion depth three could add a composite grouping-set
child. `algebra-depth.R` preserved that measurement; the original comparison
counts, defect reproductions, and ticket boundaries were unchanged.

The default-promise reproduction's direct optional-dependency guard was also
removed. Like the other saved backend evidence, that script required its
stated installed dependencies and ran the dtplyr comparison unconditionally.
Its expected exit remained 1 at the incorrect fixed-key assertion.
