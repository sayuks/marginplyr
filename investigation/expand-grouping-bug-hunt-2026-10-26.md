# Expansion and Grouping specification bug hunt

Investigated: 2026-10-26
Baseline: `bfcdaf76a36d102132cd039c68ba8bc05a4fd465` (local `main`)

This investigation exercised `expand_with_margins()`, the five Grouping
specification constructors, `inspect_grouping()`, and their shared preparation
and finalization boundaries. Four independently verifiable P2 repair tickets
were published. None required another ticket to start. The requested work
stopped at investigation and ticketing: no package implementation, package
test, generated documentation, dependency, or build configuration was changed.

## Environment and method

The measured environment was macOS arm64, R 4.6.1, marginplyr 0.1.0 loaded
from the checkout through `pkgload::load_all()`, dplyr 1.2.1, dbplyr 2.6.0,
rlang 1.3.0, tidyselect 1.2.1, vctrs 0.7.3, tibble 3.3.1, pkgload 1.5.3,
Arrow 25.0.1, dtplyr 1.3.3, data.table 1.18.6.1, RSQLite/SQLite 3.53.3,
and DuckDB 1.5.5. SQLite and DuckDB probes used live in-process connections.
No external warehouse, PostgreSQL server, alternative dependency version,
or other operating system was exercised.

Three parallel investigations separated local expansion, Grouping
specification algebra/selection, and lazy-backend behavior. Independent
follow-up probes checked the confirmed failures. The hunt used public-call
reproductions, an independently generated set-algebra model, row-copy
fixtures, direct dplyr/vctrs and decoded-Arrow controls, seeded random input,
and explicit backend boundaries. A successful control established the
particular expected behavior; an arbitrary upstream failure alone was not
counted as a package defect.

Contract sources included `CONTEXT.md`, the public Grouping specification,
inspection, expansion and Margin-order references, and ADRs 0002, 0005, 0008,
0009, 0011, 0012, 0013, 0015, 0016, 0018, 0020, 0026, 0032 and 0033.
The existing open issues and relevant closed work were checked before
publication, including #452, #541, #599, #624 and #861. The previous dated
expansion investigations supplied leads; they did not override the checked
baseline's code or maintained decisions.

## Findings and tickets

| Finding | Priority | Ticket | Blocking edges |
| --- | --- | --- | --- |
| Non-string Arrow dictionary dimensions failed during preparation | P2 | [#893](https://github.com/sayuks/marginplyr/issues/893) | None |
| Numeric Arrow dictionary keys were ordered as strings | P2 | [#894](https://github.com/sayuks/marginplyr/issues/894) | None |
| Re-expanding a sorted Arrow Margin result produced incompatible branch schemas | P2 | [#895](https://github.com/sayuks/marginplyr/issues/895) | None |
| Plain-list NULL keys were placed before nonmissing list values | P2 | [#896](https://github.com/sayuks/marginplyr/issues/896) | None |

### Non-string Arrow dictionary preparation

A two-row Arrow Table was constructed with `g` holding the dictionary encoding
of integer values 1 and 2. The source collected, and name-based inspection and
`where(is.factor)` inspection returned the same rollup. However,
`expand_with_margins()` with `grouping_set(g)`, a missing label and an occurrence
identifier failed during construction:

```text
NotImplemented: Extend
```

This included-only plan omitted nothing. The same preparation failure also
reached `summarize_with_margins()`. Ordinary Arrow mutation/union using a
missing scalar obtained from a dictionary Array returned four rows, including
two missing keys. Ordinary character conversion/union supported a text label.
Keeping `g` as a fixed key or a carried payload also succeeded.

`margin_column_info()` reconstructed a missing prototype from the schema
proxy's R factor using `arrow::Scalar$create()` with the original non-string
dictionary type. The proxy was adequate for selection, but not for rebuilding
that physical scalar. The failure therefore preceded both branch generation
and any need to omit the dimension. #624 and #861 had repaired other physical
prototype boundaries; this dictionary case remained independently observable.

`arrow-dictionary-matrix.R` exercised six dictionary value types across Table,
empty query and Dataset inputs, included-only/rollup plans, and NULL/text
labels. All 60 cases for integer, double, logical, Date and timestamp values
failed; all 12 text-dictionary cases passed. Its 90 inspection, fixed-key,
payload and ordinary missing/text-union controls passed.

### Numeric dictionary value order

A numeric dictionary used only as a fixed key avoided the preparation failure
above, making this a separate silent error:

```r
encoded <- arrow::call_function(
  "dictionary_encode", arrow::Array$create(c(9L, 10L))
)
input <- arrow::Table$create(g = encoded, payload = 1:2)
dplyr::collect(expand_with_margins(input, .by = g, .sort = "last"))
```

The output held keys 10, 9 and payloads 2, 1. Decoding `g` to integer in Arrow
before the same call returned keys 9, 10 and payloads 1, 2. Integer keys
`9, 10, NA, -2, 2` returned `-2, 10, 2, 9, NA`; the numeric control returned
`-2, 2, 9, 10, NA`. Doubles exhibited the corresponding lexical order.

`margin_dictionary_sort_columns()` selected every dictionary field for the
character cast produced by `margin_sort_value_expr()`, whose assumption that
dictionary values were strings did not hold for these inputs. ADR 0018 chose
sorting an Arrow dictionary by its values; the character cast changed numeric
value order. Both Margin positions and both expansion and summary reproduced
the defect. Missing keys stayed last, and character dictionaries passed.
An independent ordinary rollup reproduced the wrong fixed-partition order
while its margin placement remained correct.

`arrow-dictionary-order.R` asserted eight numeric failures and four successful
character controls. It compared payload order against Arrow-side decoded
controls and R's explicit ascending order, rather than interpreting collected
factor codes as the original numeric values. Arrow's own warnings about
coercing dictionary values to character factor levels were observed separately.

### Sorted Arrow results at a new expansion boundary

Expanding a two-row Arrow Table across `rollup(g)` with a missing label and
`.sort = "last"` produced a lazy result that collected to four valid public
rows. Re-expanding that result across `rollup(f)` should have copied all four
rows into each occurrence. Instead, collection raised:

```text
Invalid: UnionNode input schemas must all match
```

The diagnostic showed generated arrange expressions and the private
`..marginplyr_sort_1` field in only one operand's physical schema. The
conflicting fields were not public columns of the first collected result.
An included-only second plan succeeded, as did a first operation without
sorting.

`dplyr::collapse(first)` supplied a decisive lazy control: expansion after that
boundary returned the expected eight rows and values. The inspected Arrow
method wrapped the query and recorded its implicit schema without collecting
source rows. Thus materialization was not necessary to express the result.
Ordinary union over an Arrow query ordered by computed expressions reproduced
the underlying engine invariant too. This was retained as a Margin boundary
finding because the failing source was a valid result generated by the package,
and a schema-preserving lazy boundary made its expansion work. It was not
classified merely from that ordinary union error.

`arrow-reinput-matrix.R` covered first expansion/summary, Table/Dataset,
empty/populated input, an absent/present first occurrence identifier, all three
first sort choices, and all three second sort choices. Of 144 re-expansions,
96 failed, all following a first-operation Margin order. The 48 unsorted-first
cases passed. All 240 direct-first, included-only-second and lazy-collapse
controls passed; collapse controls agreed with the explicit local result's
value multiset. The multiset comparison did not claim an order within ties.

### Missing plain-list keys

The smallest local counterexample needed no variable dimension or union:

```r
input <- tibble::tibble(g = list(NA_integer_, NULL), payload = 1:2)
expand_with_margins(input, .by = g, .sort = "last")
```

The result held payloads 2, 1, putting NULL first. Ordinary `dplyr::arrange()`
held payloads 1, 2. `vctrs::vec_detect_missing(input$g)` returned FALSE, TRUE:
a list element containing a scalar NA was a nonmissing list value, while NULL
was the missing list element. `is.na(input$g)` returned the opposite flags.

The shared `margin_missing_last_expr()` used the scalar predicate for a plain
list, ahead of otherwise-correct list-value ordering. A `vctrs::list_of()`
control used vctrs missingness in its own method and sorted correctly. This
separated the cause from branch order, occurrence allocation and dplyr's
ordinary list-value comparison.

Both Margin positions reproduced the wrong order through all four Margin
verbs. A rollup's detail rows reproduced it for a list Grouping dimension too;
typed-missing omitted list values were themselves NULL. Counts and copied
payloads remained intact. `local-list-order.R` reported ten desired-contract
failures and asserted the successful direct-arrange and classed-list controls.
The ticket included making the owning Margin-order decision/reference explicit
about list row-vector missingness where necessary; this note made no new
maintained ordering decision.

## Replaying the failures

All companion scripts were saved under
`investigation/2026-10-26-expand-grouping-bug-hunt/` and loaded the checkout from
the repository root. Each backend runner required its referenced optional
packages to be installed. The following commands were run against the baseline
and returned exit status 1 for the specific desired-contract failures:

```sh
Rscript investigation/2026-10-26-expand-grouping-bug-hunt/arrow-boundaries.R
Rscript investigation/2026-10-26-expand-grouping-bug-hunt/arrow-dictionary-order.R --assert
Rscript investigation/2026-10-26-expand-grouping-bug-hunt/local-list-order.R --assert
Rscript investigation/2026-10-26-expand-grouping-bug-hunt/arrow-dictionary-matrix.R --assert
Rscript investigation/2026-10-26-expand-grouping-bug-hunt/arrow-reinput-matrix.R --assert
```

`arrow-boundaries.R` ran both minimized backend failures even after the first
one failed; passing `dictionary` or `sorted_reinput` selected just one. The
other four runners printed their observations without demanding a green
contract unless `--assert` was supplied. Their failing exits were retained
investigation evidence, not passing package tests or attempted fixes.

## Breadth and limits of negative evidence

| Companion script | Measured scope and result |
| --- | --- |
| `grouping-algebra.R` | Seed 20261026, 500 recursive public specifications to depth three, all five constructors and keep/drop/error: 1,500 inspections agreed with independent generated set algebra on membership/order, fixed keys, occurrences, bits and identifiers. |
| `grouping-selectors.R` | 51 selections across five constructors: 255 comparisons agreed with ordinary dplyr/tidyselect on selection membership, constructor cardinality and external error classes. Ranges, set operators, empty/numeric/external selections, predicates, typed custom helpers, pronouns, parentheses and internal empty arguments were covered. |
| `local-types.R` | 459 type/label/size/sort cases checked row counts, included/omitted values, payload copies and type retention or expected refusal across 17 ordinary/classed/structured types. Its 252 special-name role cases produced only nine dot-dot input-name refusals also reached by ordinary tidyselect. |
| `local-row-copies.R` | Seed 10830, 200 random fixtures crossed with three sort modes: 600 comparisons agreed with literal-set row-copy and ordering fixtures across nine key types, fixed keys, per-dimension labels and retained duplicates. |
| `local-dtplyr-names.R` | 906 local/dtplyr calls crossed dimension names, occurrence names and all sort modes; explicit name selections preserved counts, payloads, identifiers and labels. |
| `sql-names.R` | 816 live SQLite/DuckDB calls crossed quoted, reserved, Unicode and control-character names with occurrence names and sort choices; the checked counts, payloads, identifiers and labels agreed. |

The random local ordering fixture followed the documented scalar `is.na()`
key for plain lists and did not generate an NA-valued list element alongside
NULL. Its agreement therefore did not rule out the missing-list finding.
The name matrices checked admission and copied values, not all Margin-order
relations. None of these finite grammars or type matrices proved correctness
for every R expression, Arrow type, SQL dialect or dependency version.

No new constructor/inspection correctness defect was confirmed. Focused
existing tests for grouping plans, the grouping interface, inspection,
expansion, metamorphic semantics, cycles and selection-budget behavior passed.
The broad package Review-ready check was not required for these
repository-only investigation artifacts under `design/agents/local-checks.md`.
The two linters (`jarl check .` and package-aware `lintr::lint_package()` after
`load_all()`), context-budget verifier, document-reference verifier and whitespace
check passed before publication. The initial jarl findings in investigation
fixtures were resolved by representing intentionally redundant/empty selection
syntax as data and making a fixture assignment explicit; no suppression or
package implementation change was introduced. CI was not monitored.

## Excluded leads

- A forged specification with non-quosure argument fields escaped into an
  rlang quosure error. The documented constructors could not produce it;
  ADR 0015's considered options expressly distinguished forged objects. This
  was not promoted into a promised malformed-object validation contract.
- A shadowed selection-helper hypothesis did not survive ordinary tidyselect's
  own helper mask. Shared tidyselect refusals for dot-dot input names and bare
  pronoun spellings were not package findings; explicit name selection supplied
  the latter's control.
- Raw vectors had no missing value: vctrs's own initialization yielded `00`.
  The evidence did not establish a package-specific violation to ticket.
- Unsupported Arrow structured-to-text casts and list sorting were not counted
  from their upstream rejection alone. Non-string dictionary reconstruction had
  explicit successful scalar/union controls and a no-omission counterexample;
  sorted-result re-expansion had a successful lazy-collapse control.

The companion files recorded the investigation on this date. The implementation
tickets owned the follow-up repairs; maintained code, ADRs and public references
remained authoritative for their respective contracts.
