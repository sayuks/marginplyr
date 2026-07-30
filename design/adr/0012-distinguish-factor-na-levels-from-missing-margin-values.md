# Distinguish factor NA levels from missing Margin values

Margin label validation and factor restoration will distinguish an `NA`
factor level from a missing factor value. `NA_character_` and `NULL` do not
create synthetic factor levels: an omitted dimension is represented by a
typed missing value. A non-missing Margin label remains a synthetic factor
level placed last by default and may be placed first explicitly.

## Decision

For this decision, an NA level and an actual missing value are tested
independently:

```r
has_na_level <- anyNA(levels(x))
has_missing_value <- any(is.na(x))
```

An observation that uses an NA factor level can print as `<NA>` while
`is.na()` is false. A factor whose code itself is missing has `is.na()` equal
to true.

With `.check_margin_label = TRUE`, the eight combinations have these results:

| Margin label | NA level | Missing value | Result |
|---|---:|---:|---|
| `NA_character_` | yes | yes | Error: NA is already a factor level |
| `NA_character_` | yes | no | Error: NA is already a factor level |
| `NA_character_` | no | yes | Error: the label collides with a value |
| `NA_character_` | no | no | Allowed; use typed missing |
| `NULL` | yes | yes | Allowed; missing source values and margins require structural identity |
| `NULL` | yes | no | Allowed; preserve the NA level and use typed missing for margins |
| `NULL` | no | yes | Allowed; source missing values and margins require structural identity |
| `NULL` | no | no | Allowed; use typed missing |

An NA factor level is part of the column's domain even when no observation
uses it. Consequently, the first two combinations remain errors when
`.check_margin_label = FALSE`; the error is structural rather than an
optional value-collision check. Disabling the check changes only the third
combination, which becomes allowed but ambiguous without a Grouping set
identifier or Grouping identifier.

`NULL` requests typed missing values and therefore bypasses Margin label
collision checks. It preserves existing factor levels, including an NA
level. When the source also contains missing factor codes, those values and
omitted-dimension margins cannot be distinguished from the grouping column
alone. The operation's internal grouping-set metadata remains authoritative,
and user-facing examples will retain `.id`, `grouping_id()`, or
`grouping_bit()` when the distinction matters.

`.margin_label_position` has no effect for `NA_character_` or `NULL`, because
neither creates a synthetic level. For a non-missing label,
`.check_margin_label = TRUE` checks the complete factor domain, not only
observed values. An unused factor level equal to the proposed label is
therefore a collision. When the check is explicitly disabled, the existing
level may be reused and is moved to the requested first or last position.

The rules apply per column when `.margin_label` is a named vector. One
dimension may use a non-missing label while another uses `NA_character_`;
factor validation, typed-missing construction, and level positioning are
performed independently for each dimension.

## Implementation consequences

Factor metadata validation must run before backend execution. It must inspect
both factor levels and actual missing values without conflating them through
`as.character()`. Named Margin labels require column-specific validation and
restoration rather than one scalar reconstruction path.

Backend adapters must represent `NA_character_` for a factor dimension as a
typed missing value, not as an NA enum label. In particular, the DuckDB path
must not attempt to add `NA` to an enum merely because `NA_character_` was
selected for that dimension. Native grouping metadata or internal masks,
rather than displayed missing values, distinguish omitted dimensions.

Tests will cover all eight combinations with `.check_margin_label = TRUE`,
the one changed case with the check disabled, unused non-missing factor-level
collisions, ordered factors, named per-column labels, first and last label
positions, and local and factor-preserving lazy adapters where supported.
Assertions will cover levels, underlying missingness through `is.na()`,
Grouping identifiers, displayed values, and error calls.

## Documentation consequences

Implementation must document these rules in the places where users encounter
them:

- the `.margin_label`, `.check_margin_label`, and
  `.margin_label_position` argument documentation;
- the shared display-label section inherited by all Margin verbs;
- a Get Started example contrasting an NA level, a missing factor value, and
  an omitted dimension;
- the `.id`, `grouping_bit()`, and `grouping_id()` comparison showing how
  structural identity resolves ambiguous display values; and
- backend notes for factor-preserving adapters, including typed missing
  handling in DuckDB.

The reference documentation will include the complete eight-case table rather
than relying only on prose. Examples that use `NULL` or `NA_character_` will
show the relevant `levels()` and `is.na()` results.

The dedicated `vignettes/grouping_identity.qmd` article will carry the
cross-cutting identity comparison and link to this detailed eight-case factor
table. It will explicitly distinguish a source missing value, a factor NA
level, and a typed-missing Margin label without duplicating the full
factor-validation reference.
