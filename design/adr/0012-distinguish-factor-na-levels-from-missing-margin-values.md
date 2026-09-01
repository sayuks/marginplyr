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

## Amendment: a typed-missing Margin label is not an observed collision

Two rows of the table above described the same displayed result and disagreed
about it. `NA_character_` with no NA level and a missing value present was an
error; `NULL` against the same column was allowed. Both produce a typed missing
Margin value, so on a column that already holds missing values both produce a
margin row and a source row that display identically. The refusal rejected one
of two spellings of a result this ADR permits by the other, and it reached the
caller who had picked the spelling the documentation calls the checked one
rather than the caller whose result was ambiguous.

The table's third row therefore reads *Allowed; source missing values and
margins require structural identity*, matching its seventh. `NA_character_` no
longer participates in the observed half of the collision check: it is excluded
before the check selects any column, so a call whose every label is
typed-missing sends no query, as `NULL` already did (ADR 0020). Nothing is
reported in its place — a message on one spelling and silence on the other
would move the asymmetry into the diagnostics rather than remove it, and the
structural identity that resolves the ambiguity is documented where the
argument is.

The declared half is unchanged, and the first two rows still read *Error*. An
NA factor level is part of the column's domain whether or not an observation
uses it, and `levels()` records it, so `NA_character_` proposed against one
conflicts with something a caller can observe. That is the whole of what now
separates the two spellings, which is why it is the difference kept.

`CONTEXT.md` narrows *Margin label collision* to a non-missing label to match.
The code already drew that line for the declared half: the NA-level refusal in
`validate_margin_label()` is a block of its own above
`check_declared_label_collision()`, which fires only for a non-missing label.

## Amendment: `.margin_label` accepts a named list

"The rules apply per column when `.margin_label` is a named vector" above could
not be exercised for `NULL`. A named character vector cannot carry a `NULL`
element — `c(region = "All", store = NULL)` drops it, and the call then fails
the requirement to name every Margin dimension — so `NULL` was reachable only
as an unnamed scalar applying to every dimension at once. Both halves of the
collision check rested on a per-dimension `NULL` that could not be written, and
the declared half's refusal named it as the remedy.

`.margin_label` therefore also accepts a named list whose elements are
character scalars or `NULL`: `list(region = "All", store = NULL)`. A named
character vector remains accepted as the shorthand for a list with no `NULL`
element, since that is the ordinary case and the list brackets buy it nothing.
The requirement to name every Margin dimension is unchanged: an unnamed
dimension stays a refusal rather than becoming a silent `NULL`, because the
alternative makes a misspelled name turn every dimension typed-missing with no
diagnostic.

The declared refusal's remedy names the list spelling rather than the bare
`NULL` it named before. That `NULL` is the whole of `.margin_label`, so it was
a remedy only where there was one dimension; a caller with two who followed it
literally reached a second refusal. `R/margin-label.R` states the rule this
broke, beside the same pair of refusals: a remedy is offered only where it is
one.
