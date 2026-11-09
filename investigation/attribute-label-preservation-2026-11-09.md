# Input attributes and labelled metadata across the public API

Investigated: 2026-11-09
Examined source: `cadf57fdf7b1c0b9f149736a49e6edf829ded317`

## Findings

There was no general attribute-preservation guarantee. At the examined source,
the public *Result class and attributes* section in
`R/summarize_with_margins.R` and
[ADR 0016](../design/adr/0016-delegate-result-class-and-attributes-to-dplyr.md)
delegated arbitrary input attributes to the actual dplyr/vctrs/backend
composition. A label surviving one call did not establish that it survived
another plan, backend, summary expression, or dependency version.

The empirical distinction that mattered most was between an ordinary vector
carrying `label` and a `haven_labelled` vector carrying both `label` and
`labels`. `labelled::var_label()` alone did not create a `haven_labelled`
class. In the local multi-set probes, ordinary column labels were lost,
including labels inside nested cells. The class-owned metadata of unchanged
`haven_labelled` columns survived. A string Margin label converted the grouping
dimension to character and lost that dimension's labelled metadata.

The recommendation from this investigation is to retain ADR 0016's boundary.
Keeping valid descriptive labels is useful, but unconditional restoration of
arbitrary attributes is unsafe. A separate opt-in metadata policy could be
designed if stronger preservation is required; this note does not adopt one.

## What “label” meant

| Metadata | Representation | Meaning |
| --- | --- | --- |
| Dataset description | Outer data frame `label` attribute | Description of the source table |
| Variable label | Column `label`, including `labelled::var_label()` | Human-readable variable description; no special class required |
| Value labels | `labels` on `haven_labelled` | Mapping from stored codes to display descriptions |
| SPSS user missing values | `na_values` / `na_range` on `haven_labelled_spss` | Which otherwise ordinary stored values count as missing |
| Tagged missing values | Tag encoded in an NA double | Missing-value payload; not an ordinary attribute |
| Margin label | `.margin_label`, default `"Total"` | Synthetic display value for an omitted grouping dimension |

`labelled::var_label(data_frame)` addressed the columns, whereas
`labelled::label_attribute(data_frame)` addressed the outer object. Thus a
dataset label and its column labels were different preservation questions.
The [variable-label reference][variable-labels] and
[labelled introduction][labelled-intro] owned these distinctions. The
[missing-value vignette][missing-values] and
[haven SPSS reference][spss] distinguished missing declarations from tagged NAs.

## Measurement and reproducibility

The companion [R probe](attribute-label-preservation-2026-11-09.R) recorded
observations, rather than adding package tests or changing the public contract.
It used `pkgload::load_all()` on the examined worktree. Run from that root:

```sh
R_LIBS=/private/tmp/marginplyr-attribute-label-library Rscript investigation/attribute-label-preservation-2026-11-09.R /private/tmp/marginplyr-attribute-label-results-verified
```

`labelled` 2.16.1 was installed from CRAN into that temporary library for the
investigation; it was not added to package dependencies. Other used packages
were already installed. The environment was macOS arm64, R 4.6.1, dplyr 1.2.1,
vctrs 0.7.3, tibble 3.3.1, tidyr 1.3.2, haven 2.5.5, data.table 1.18.6.1,
dtplyr 1.3.3, Arrow 25.0.1, DBI 1.3.0, RSQLite 3.53.3 and DuckDB 1.5.5.

The local matrix varied:

- `data.frame`, tibble, an otherwise unimplemented data-frame subclass and
  direct `data.table` inputs;
- bare numeric, `haven_labelled`, `haven_labelled_spss`, factor/ordered factor
  and Date columns;
- nonempty and zero-row inputs;
- one `grouping_set(g)` versus `rollup(g)` (two grouping sets);
- `"Total"` versus `NULL` as the Margin label;
- all four Margin verbs, with both `.keep` settings for nesting.

Every fixture carried an outer `label` and arbitrary `audit_note`, plus a
distinct column `label` and `audit_note`. SPSS fixtures additionally carried
unused missing-value declarations, so their retention could be inspected
independently of arithmetic over missing codes. A fixed `.by = f` key separated
unchanged keys from the grouping dimension `g`; `x` was a payload column.

The final run produced 8,192 object/column observations in `attributes.csv`,
with five attribute comparisons per observation. The run returned exit 0.
`errors.csv` recorded four unsupported Arrow labelled-key aggregation cases;
they were not counted as successful preservation observations. The same
Arrow limitation was probed directly through dplyr in `arrow-upstream.csv`.
`arithmetic.csv`, `inspection-helpers.rds`, `tagged-expanded.rds` and
`session-info.txt` retained the additional evidence in the output directory.
The script regenerated these artifacts; the tables below recorded the relevant
findings without committing the large raw matrix.

The exploratory calls also exposed that `first()` was unsupported in these
remote aggregate contexts. The final remote summary probes used supported
numeric sum/count expressions instead; local and dtplyr probes additionally
selected `dplyr::first(x)` to distinguish retaining an existing scalar from
creating an arithmetic result.

## Local results by API

The following table described nonempty local tibbles with `.by = f`,
`.grouping = rollup(g)` and `.id = "set"`. The same main retention pattern was
observed for plain data frames, the subclass and direct data.table inputs in
this multi-set case. “Labelled” here meant `haven_labelled` or
`haven_labelled_spss`, not a bare vector with a variable label.

| API | Outer source attributes | Ordinary column `label` | Labelled column metadata |
| --- | --- | --- | --- |
| `summarize_with_margins()` / `summarise_with_margins()` | Lost | Fixed and grouping keys lost labels across branch combination | Fixed keys retained metadata; grouping keys retained it with `NULL`, lost it with `"Total"`; `first(x)` retained it |
| `expand_with_margins()` | Lost | Fixed keys, grouping keys and carried payload lost labels | Fixed keys and payload retained metadata; grouping keys retained it with `NULL`, lost it with `"Total"` |
| `nest_with_margins()` | Lost on outer and inner frames | Outer keys and inner payload lost labels | Fixed keys and inner payload retained metadata; outer grouping keys retained it with `NULL`, lost it with `"Total"` |
| `nest_by_with_margins()` | Same attribute findings as nesting; rowwise outer result | Same as nesting | Same as nesting |
| `inspect_grouping()` | Source attributes not copied | Output described column names and plan, not source vectors | No source value dictionary or variable label copied |

With `.keep = TRUE`, the nested keys held their original pre-margin values.
For labelled inputs their inner variable labels, value labels and SPSS missing
declarations survived, even when the outer grouping dimension was character
because its display contained `"Total"`. `.keep` was not an attribute-
preservation switch: bare labels were still lost in the multi-set local cells.

The two summarize spellings were aliases in the examined source. No separate
alias semantics were inferred.

### Important qualifications

- For plain data frames and tibbles, a one-set expansion retained the outer
  dataset attributes. A subclass with no reconstruction method lost them.
  Multi-set expansion lost them in the measured local matrix. The old measured
  table in ADR 0016 was not a blanket promise for all plans.
- One-set expansion retained a bare payload label; the two-set expansion lost
  it. Many one-set keys/cell labels survived too. One-set nesting's outer `g`
  label could already be lost even with `NULL`, so neither a one-set plan nor
  `NULL` established universal preservation.
- Factor levels/order and Date class survived where delegated or explicitly
  reconstructed, while extra `label` / `audit_note` attributes were lost in
  these multi-set probes. A class surviving did not imply all its attributes
  survived. Factor reconstruction in `R/factor.R` rebuilt levels and ordered
  status, not arbitrary labels.
- The zero-row probes established observations only for empty vectors and
  result shapes actually returned. No inner-cell retention claim was inferred
  from a result with no cells.
- `sum(x)` and `mean(x)` returned ordinary numeric columns without `label`
  or `labels` for bare, haven and SPSS fixtures. `first(x)` returning a labelled
  scalar was therefore not evidence for arbitrary aggregate preservation.

### The other exported functions

`grouping_set()`, `grouping_sets()`, `rollup()`, `cube()` and `grouping_spec()`
constructed Grouping specifications from selections/specifications rather than
transforming an input dataset; source-attribute round-tripping was inapplicable.
`grouping_bit()`, `grouping_id()`, `share_of_parent()` and `share_of_total()`
were Contextual helpers constructing new values. The helper probe returned
integer bits/masks and double shares without source variable/value labels;
the unchanged labelled fixed/grouping columns remained separately labelled.
`last_sent_queries()` returned query diagnostics, not source rows, so no input
dataset or column-label preservation applied to it.

## Backend differences

Backend probes included a plain create/collect round trip before calling a
Margin verb. This located losses that had already happened before marginplyr
could see the input. The probes covered dtplyr, an in-memory Arrow Table,
SQLite and DuckDB. They did not cover Arrow Dataset/RecordBatch inputs,
Parquet interchange, every SQL driver, or every remote execution path.

| Backend | Before a Margin operation | Observed multi-set behavior |
| --- | --- | --- |
| dtplyr | Ordinary and labelled column metadata survived round trip | Bare column labels survived in untouched keys/payload/cells in these probes, unlike eager local combination; `"Total"` removed metadata from transformed dimensions; labelled metadata survived with `NULL` |
| Arrow Table | R round trip retained ordinary labels and haven metadata | Expansion retained untouched metadata, including the source outer label; `"Total"` removed transformed-key metadata; bare-key aggregation retained column labels but lost outer attributes |
| SQLite | R object/column labels and haven/SPSS dictionaries were already absent after upload/collect | Margin results did not recover them |
| DuckDB | R object/column labels and haven/SPSS dictionaries were already absent after upload/collect | Margin results did not recover them; factor/ENUM handling was a separate type contract |

Arrow aggregation with a labelled fixed key failed with `NotImplemented: Keys
of type <labelled<double>[0]>` or the SPSS equivalent, for both Margin-label
settings. Direct dplyr aggregation over the same fixed key also failed. Thus
Arrow's labelled round-trip/expansion success did not establish support for
labelled aggregation. Both nesting APIs admitted local and dtplyr inputs,
not arbitrary Arrow/SQL tables; unsupported nesting was not tested as though
it were supported.

One supplementary local expansion duplicated an `haven::tagged_na("a")`
payload along with its source rows, retaining two occurrences of that tag.
This was distinct from restoring SPSS missing declarations. No general tagged
NA propagation guarantee for summaries or other backends was inferred.

## Why the observations differed

`bind_margin_branches()` in `R/grouping-adapter-union.R` used
`vctrs::vec_rbind()` for eager local branches. A bare vector's arbitrary
attributes did not define its common type. Haven vectors supplied explicit
common-type/casting methods for their labelled class, so the value dictionary
and variable label could survive that route. The lazy backends used their own
combination and collection methods. The public delegation contract did not
promise that all routes gave the same arbitrary metadata.

The upstream references reinforced this boundary:

- [dplyr summarise][summarise] documented creating a new frame without
  retaining input frame attributes. [mutate][mutate] documented preservation
  for its own operation; that was not a guarantee for an entire multi-verb
  pipeline. [nest_by][nest-by] likewise did not retain input frame attributes.
- [dplyr extension guidance][extending] assigned data-dependent attributes
  and subclass reconstruction to class authors. A source-row provenance
  attribute could become false after expansion or aggregation.
- [vctrs combination][vec-c] derived a common type, and
  [proxy/restoration][vec-proxy] assigned class-specific restoration to class
  methods. “A class can restore metadata” did not imply that arbitrary bare
  attributes were preserved.
- The [haven 2.5.5 labelled source][haven-source] implemented explicit
  `vec_ptype2()` and cast methods, combining value-label dictionaries and
  choosing a non-null variable label. This was a class-owned policy, not
  marginplyr copying attributes from its source.

## Desirable preservation and recommendations

These are recommendations from the evidence, not new accepted API promises.

1. **Do not promise arbitrary attributes.** Stable descriptions and properties
   dependent on the original rows cannot safely be distinguished by their
   presence. Restoring every attribute after upstream processing could override
   a class author's intentional invalidation. Stripping valid metadata just
   to make all verbs alike would also discard useful information.
2. **Unchanged columns can use upstream metadata retention.** An untouched
   payload's variable description and value dictionary remain useful; nested
   detail cells are a particularly useful place for them. Stronger guarantees
   would need explicit tests and a deliberately narrower metadata contract,
   including empty inputs, all supported backends and conflicts.
3. **New summaries should get newly chosen labels.** Copying “Individual
   income” onto a count, or a 1/2 response dictionary onto a mean/share, would
   misdescribe the output. The sum/mean probes left those attributes absent.
   Callers should label resulting variables after deciding what each means.
4. **Treat labelled grouping dimensions as a value-domain choice.** Adding
   `"Total"` to numeric codes requires conversion or a new code/dictionary
   policy, including collisions and SPSS user missings. Blindly reattaching the
   old `labels` cannot solve it. `NULL` avoids the string conversion and kept
   haven metadata locally in these probes, but does not distinguish source
   missing keys from Margin rows by itself; `.id` or a Grouping bit can do so.
5. **Normalize import representations before analysis when appropriate.**
   [labelled's introduction][labelled-intro] and
   [haven's conversion semantics][semantics] recommended factors or ordinary
   vectors for analysis. For categorical keys, converting value labels to
   factor levels before applying margins lets the factor-level contract
   represent `"Total"`. Numeric measures and user missing codes require a
   separate analysis decision; automatic factor conversion is inappropriate
   for every labelled variable.

For example, this expressed a categorical interpretation before the operation:

```r
input$g <- haven::as_factor(input$g)
result <- summarize_with_margins(
  input, amount = sum(x), .grouping = rollup(g)
)
labelled::var_label(result$amount) <- "Total amount"
```

That preserved the categorical display through factor levels, not through a
promise to retain an arbitrary factor `label` attribute. For SPSS inputs,
missing codes should first be normalized according to the analysis intent;
[haven semantics][semantics] warned that unaware operations could count them
as ordinary values. Reattaching metadata after a calculation could not repair
an already incorrect calculation.

Implementing general restoration would conflict with ADR 0016. Supporting an
explicit labelled-key contract would require a separately accepted design;
no package behavior or ADR was changed in this investigation.

[variable-labels]: https://larmarange.github.io/labelled/reference/var_label.html
[labelled-intro]: https://larmarange.github.io/labelled/articles/labelled.html
[missing-values]: https://larmarange.github.io/labelled/articles/missing_values.html
[spss]: https://haven.tidyverse.org/reference/labelled_spss.html
[summarise]: https://dplyr.tidyverse.org/reference/summarise.html
[mutate]: https://dplyr.tidyverse.org/reference/mutate.html
[nest-by]: https://dplyr.tidyverse.org/reference/nest_by.html
[extending]: https://dplyr.tidyverse.org/reference/dplyr_extending.html
[vec-c]: https://vctrs.r-lib.org/reference/vec_c.html
[vec-proxy]: https://vctrs.r-lib.org/reference/vec_proxy.html
[haven-source]: https://raw.githubusercontent.com/tidyverse/haven/v2.5.5/R/labelled.R
[semantics]: https://haven.tidyverse.org/articles/semantics.html
