# Grouping specification and Margin-summary bug hunt

Investigated: 2026-10-25
Baseline: b6f9faba946521aeb92fcf2c5655a2c3bf608ec8

## Scope and outcome

The investigation used a new worktree created from the fetched `origin/main`
commit above. It examined `summarize_with_margins()` and its synonym, Grouping
specification constructors, plan compilation and inspection, contextual
Grouping helpers, and Parent/Total shares. No implementation or package test
was changed.

Six independent defects were reproduced and published as agent-ready tickets.
None required another ticket to start. The P1 classification below reflected
silent cross-partition denominator mixing; the P2 findings covered a wrong
partition or a reproducible failure of a documented public call.

| ID | Finding | Ticket |
| --- | --- | --- |
| F1 | dtplyr whitespace-bearing fixed keys mixed share partitions | [#824](https://github.com/sayuks/marginplyr/issues/824) |
| F2 | Type-dependent helper arguments were resolved on a name-only proxy | [#825](https://github.com/sayuks/marginplyr/issues/825) |
| F3 | SQLite intermediate queries lost Boolean-word public names | [#826](https://github.com/sayuks/marginplyr/issues/826) |
| F4 | Valid dplyr dot-dot summary names were rejected after evaluation | [#827](https://github.com/sayuks/marginplyr/issues/827) |
| F5 | Cyclic nested-name specifications exhausted R's stack | [#828](https://github.com/sayuks/marginplyr/issues/828) |
| F6 | dtplyr private share keys collapsed an NA factor level into missing | [#829](https://github.com/sayuks/marginplyr/issues/829) |

## Replay and environment

From the repository root, run:

```sh
Rscript investigation/2026-10-25-grouping-summary-bug-hunt/reproduce.R
```

[reproduce.R](reproduce.R) contains one public-contract assertion per finding
and reports all six independently. On the baseline it exited 1 with six
contract failures; [reproduce-output.txt](reproduce-output.txt) preserves that
output. An independent second investigator reran the same script unchanged
and reproduced all six. The replay requires the investigation's local
packages; it is an investigation artifact outside the source package and is
not part of the package test suite.

The measured environment was R 4.6.1 (2026-06-24),
`aarch64-apple-darwin23`, Darwin 25.6.0, with:

| Package | Version |
| --- | --- |
| dplyr | 1.2.1 |
| dbplyr | 2.6.0 |
| tidyselect | 1.2.1 |
| rlang | 1.3.0 |
| dtplyr | 1.3.3 |
| data.table | 1.18.6.1 |
| RSQLite | 3.53.3 |
| duckdb | 1.5.5 |
| duckplyr | 1.2.1 |
| arrow | 25.0.1 |
| testthat | 3.3.2 |
| pkgload | 1.5.3 |

## F1: whitespace-bearing fixed keys mixed dtplyr share partitions

The minimized input had two rows, fixed keys ` a ` = F/G and `a` = X/X,
variable dimension `g` = x/x, and values 2/3. A rollup with a scalar sum and
one share should have returned four rows, each share equal to one. The local
report did so. The dtplyr report returned six Parent-share rows or eight
Total-share rows, including 1.5 and 2/3, and leaked columns named `a.1` and
`i. a `. A key without its trimmed-name counterpart raised a missing-column
error instead. Implicit fixed groups reproduced the eight-row Total-share
failure too.

The ranked hypotheses were a parser-sensitive join name, a name-restoration
error, and a collision with the trimmed counterpart causing a wrong match.
The targeted controls established the first mechanism and the third symptom:

- A share-free Margin summary and an ordinary grouped summary succeeded.
- Renaming the fixed keys to safe names before the report restored the shares.
- Inspection of the installed data.table `.parse_on()` showed `trimws()` over
  unquoted join terms. The ordinary dtplyr join reproduced that upstream
  limitation as well.
- `dtplyr_join_names()` protected backticks and comparison operators but
  returned no replacement for the whitespace-bearing name.

The internally introduced share join therefore matched ` a ` as `a`.
Leading/trailing spaces, tabs, and newlines reproduced; internal whitespace
and NBSP boundaries did not. A whitespace-bearing public `.id` succeeded.

The relevant package sites were `dtplyr_join_names()`,
`build_lazy_parent_mapping()`, and the lazy share denominator joins in
`R/share.R`. The Parent/Total share definitions in `CONTEXT.md`,
[ADR 0010](../../design/adr/0010-compute-parent-shares-as-a-contextual-summary.md),
and [ADR 0017](../../design/adr/0017-calculate-total-shares-against-the-grand-total-set.md)
placed each denominator inside its fixed-key partition. Existing special-name
coverage did not include this whitespace boundary.

## F2: type-dependent helper arguments used a name-only proxy

With character `a` = x/y and numeric `b` = 1/1, the selection in the replay
used `all_of()` over the numeric names discovered through
`tidyselect::peek_data()`. Direct dplyr selection chose only `b` and its grouped
count returned one row, `b = 1, n = 2`. The Margin summary chose both columns
and returned two count-one rows. `inspect_grouping()` reported the same wrong
fixed keys. Used as a dimension selection alongside fixed `a`, the expression
was falsely refused for overlapping `a`.

The hypotheses were premature classification of a leaf helper, an incorrect
typed snapshot, and incorrect fixed/dimension overlap normalization. Two
controls isolated classification: a user wrapper around the same selection
succeeded, and adding a never-selecting `where()` sibling to stop the name-only
pass also succeeded. Neither changed the input's types or desired columns.

`is_name_only_expr()` returned true for its leaf-helper branch without
accounting for the helper argument. `resolve_fixed_keys()` consequently
selected against `grouping_name_proxy()`, whose columns were all integer
indices. The resulting fixed keys reached the canonical plan unchanged.

The public `.by` and constructor tidy-select contracts supplied the oracle.
Issues [#342](https://github.com/sayuks/marginplyr/issues/342) and
[#346](https://github.com/sayuks/marginplyr/issues/346), including their comments,
were read: they established the names-alone invariant but addressed different
syntax. Existing tests named `the name-only reader answers what the names
settle`, `selectors and fixed .by columns are resolved once`, and `fixed keys
settled by name alone need no typed metadata` passed without covering this
helper-argument dependency.

## F3: SQLite intermediate queries lost Boolean-word public names

A live SQLite source column named `TRUE` worked with direct dbplyr grouping
and zero-row metadata collection. The Margin report failed at collection with
`no such column: TRUE`. Single-set and rollup reports, all three sort modes,
`NULL`, `NA_character_`, and string labels reproduced. `FALSE`, lower-case,
and mixed-case variants reproduced too. Separate probes reproduced summary
output, fixed-key, share-source, and `.id` roles.

The hypotheses were SQLite subquery alias retention, package name restoration,
and loss of the source column. Raw SQL isolated the first:

```sql
SELECT NULL AS `TRUE`;
-- column name: TRUE
SELECT * FROM (SELECT NULL AS `TRUE`);
-- column name: column1
SELECT `TRUE` FROM (SELECT NULL AS `TRUE`);
-- no such column: TRUE
```

The source table and its direct grouped aggregate retained the requested
column, excluding a source-schema failure. Margin SQL introduced intermediate
public aliases and nested projections that exposed the backend behavior.
`NULL` and `CURRENT_DATE` column names were successful controls. No claim was
made about SQLite releases other than the installed RSQLite build.

The affected construction crossed the portable summary adapter, omitted-key
labeling, and SQLite typed/order wrappers. The public interface accepted these
names, while [ADR 0031](../../design/adr/0031-preserve-sqlite-typed-dimensions-under-margin-order.md)
and [ADR 0033](../../design/adr/0033-preserve-declared-types-in-empty-margin-results.md)
defined the SQLite result's public-schema boundary.

## F4: valid dot-dot summary names were rejected after evaluation

A one-row input with value `1L`, summarized into an explicit output named
`..1`, returned the expected integer column under dplyr but raised
`vctrs_error_names_cannot_be_dot_dot` under the Margin verb. No grouping,
splicing, or branch union was needed. `...` and `..2` also reproduced on plain
data frames and tibbles; `..0`, `..01`, `..a`, and `a...1` succeeded.

The hypotheses were extra validation during schema inspection, branch-union
name repair, and dot capture. Probes found two independent obstacles on the
same output-name path: `summarize_margin_branch()` and `names()` returned the
correct `..1` output, but `get_col_names()` called `dplyr::select()` and refused
it; `bind_margin_branches()` independently used a `check_unique` repair that
also refused this spelling. Capture preserved the requested name.

The public dplyr-summary semantics and
[ADR 0016](../../design/adr/0016-delegate-result-class-and-attributes-to-dplyr.md)
supplied the comparison. This finding concerned newly produced summary names;
tidyselect's own rejection of some source-key names was a separate, excluded
case. `test-branch-names.R` covered suffixes such as `sales...1`, not a whole
name of the dot-dot-number form.

## F5: cyclic nested-name specifications exhausted the stack

The minimized declaration was `spec <- grouping_sets(spec)`, followed by
inspection or a Margin summary using `spec`. It raised
`nodeStackOverflowError` / `stackOverflowError`, with `node stack overflow`.
A same-name extension of an existing rollup and a mutual-reference pair
reproduced the same failure.

The hypotheses were a repeated ancestor, invalid nesting grammar, and ordinary
finite nesting exhausting the stack. `!!spec` injection of the previous value,
reuse of one specification in independent branches, and 25 layers of finite
nesting all succeeded. The repeated ancestor in `preflight_grouping_spec()`
was reached through `grouping_arg_spec()` repeatedly resolving the same bound
name, without a cycle boundary.

[ADR 0015](../../design/adr/0015-separate-package-conditions-from-internal-invariants.md)
and [ADR 0026](../../design/adr/0026-refuse-a-nested-name-two-readings-claim.md)
provided the Package-condition and nested-name contracts. This finding was
limited to cyclic references in captured Grouping specifications; it did not
require catching arbitrary caller functions' own nontermination. Tests of
nesting grammar, recognized spelling/name forms, and one evaluation per
recognized nested argument passed but did not exercise a cycle.

## F6: private dtplyr share keys collapsed an NA factor level

The factor in the replay had codes `1, NA_integer_` and one declared
`NA_character_` level. These represented distinct groups. Scalar sums of
2/3 produced three local rollup rows with shares 0.4, 0.6, and one. Both dtplyr
share kinds instead raised `marginplyr_share_cardinality_error`, saying
another ordinary summary had expanded the grouped result. The only ordinary
summary was a scalar sum. `NULL` and a permitted non-missing label reproduced
for both helpers. Ordered-factor probes reached the same refusal.

The hypotheses were loss of private factor-key identity, a scalar-validator
wrapper changing the groups, and a denominator join causing duplicates.
Runtime tracing found the cardinality guard ran before the join and received
occurrence identifiers `1, 1, 2` and a private factor with zero levels and all
missing codes. A direct `data.table::rbindlist()` control reproduced loss of
the declared NA level. The original factor had no duplicate values according
to vctrs. The no-share Margin report and ordinary dtplyr grouping preserved
the two identities; a fixed-factor-only share control succeeded.

`summarize_margin_union()` copied the factor into `parent_key_names` without
the public dimension's encode/rebuild protection. The branch union erased the
private identity before `assert_dtplyr_grouped_keys()` checked cardinality.
Removing that guard alone would leave the denominator identity broken.
Temporary tracing was removed after the probes.

[ADR 0012](../../design/adr/0012-distinguish-factor-na-levels-from-missing-margin-values.md)
required the distinction between a declared NA level and a missing code.
Issues [#408](https://github.com/sayuks/marginplyr/issues/408) and
[#415](https://github.com/sayuks/marginplyr/issues/415), including their comments,
were read; their public-factor fixes did not cover these private share keys.

## Breadth, negative results, and limits

| Exploration | Measured coverage and outcome |
| --- | --- |
| Grouping-plan property harness | Seed 20261025; 1,500 recursive specifications and 6,000 public inspections. Five constructors, composite/overlapping dimensions, empty identities, unions/products, keep/drop/error, and fixed-key presence. Independent comparisons of ordered included columns, set identifiers, bits, integer Grouping identifiers, and tibble class passed. |
| Local dplyr differential probes | 68 individual summaries and 13 summary sequences passed: scalar/frame/list/matrix/Date/factor results, named/unnamed outputs, pick/across/if_any, function/local/block forms, NULL, overwrite, changed types, later selections, and dynamic unpacking. |
| Local key-shape probes | 18 key shapes across three sort modes passed total/row-count checks, including complex/raw/list keys, zero-column matrix/frame keys, and a 3D array. |
| Manual share oracle | Seed 4176; 8 random fixtures, empty/nonempty, and local/dtplyr/SQLite/DuckDB gave 64 passing comparisons. Fixed/dimension NA, negative/zero denominators, counts, bits/identifiers, and both shares were included. SQL source checks were explicitly disabled in this value-semantic oracle. |
| Source-key name matrix | 42 names, five backends including Arrow, one-set/rollup, and three sort modes: 1,260 executions. 1,134 succeeded, 120 hit source-name restrictions also owned by tidyselect/dplyr, and six exposed F3. |
| Share-name matrix | 42 names across fixed/dimension/share-source roles and dtplyr/SQLite/DuckDB: 378 executions. 374 passed; one exposed F1 and three exposed F3. |
| Expression boundaries | 43 ordinary-expression comparisons and 18 additional capture/evaluation/nested-mask probes produced no further finding. Differences for returned functions/formulas and unrecognized capture spellings followed the static-rewrite boundary in ADR 0019. |

SQL all-missing `SUM()` versus R `sum(..., na.rm = TRUE)`, Arrow's documented
share refusal, and the declared-NA-level collision for an `NA_character_`
label were excluded as contract-consistent behavior. Noncyclic constructor
reuse and explicit injection were successful controls for F5. Search of open
issues and targeted historical issue searches found no open duplicate for
these six findings. Historical issues were read where their prior decisions
bore on classification.

The randomized constructor oracle used four ordinary dimension names; it did
not independently fuzz every non-syntactic name, option, print form, or quosure
environment. Existing tests covered those areas, and their success was
separate evidence. Live PostgreSQL and other external database services were
not exercised. Arbitrary data-frame subclasses, arbitrary caller
nontermination, large-plan performance, and every possible R expression were
not exhaustively tested. The passing comparisons established the listed cases
on the pinned environment, not a proof of absence of other defects.

## Existing checks

The following existing-test commands completed with exit 0, with no reported
failures, warnings, or skips:

```sh
Rscript -e 'testthat::test_local(filter = "^(grouping-plan|inspect-grouping)$", reporter = "summary")'
Rscript -e 'testthat::test_local(filter = "^(summarize-operation|grouping-interface|contextual-helpers|nested-data-masks|share|share-backends|margin-label|margin-order|followup-summary|metamorphic-margin-semantics)$", reporter = "summary")'
```

The second command's first sandboxed attempt stopped because testthat could
not write its snapshot file in the managed worktree. The same command was
then run with worktree write permission and completed successfully; no tracked
snapshot changed. The earlier sandbox attempt also emitted Arrow CPU-cache
`sysctlbyname` permission diagnostics. Those were environment diagnostics,
not new package findings.

The committed artifacts were confined to this investigation directory, which
`.Rbuildignore` excluded and no generation or package check consumed. They
changed no installed behavior or package-check input, so the full
Package-affecting Review-ready check did not apply under
`design/agents/local-checks.md`. Lint and repository verification outcomes
were recorded in the pull request. CI monitoring was outside this request.
