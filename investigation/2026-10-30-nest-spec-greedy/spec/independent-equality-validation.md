# Independent validation of nesting equality versus display

Investigated: 2026-10-30
Baseline: `28c449aa03fd5289916f0673e41c848cc126999e` (`main`)

The independent replay ran both
[`local/reproduce-equality.R`](../local/reproduce-equality.R) and
[`coordinator/equality-independent.R`](../coordinator/equality-independent.R).
The case-insensitive character key and integer-bucket numeric key each formed
one two-row group under their vctrs equality proxies, ordinary dplyr nesting,
fixed-key nesting, typed-missing-label nesting, and a Margin summary. Both
nesting verbs with a non-missing label split that group into two one-row cells.
The character-key script's `--assert` invocation exited 1 on the correct
membership assertion.

## Base-R fixture and scope

A further independent fixture required no custom class or method:
`package_version(c("1.0", "1.0.0"))`. Base comparison considered its two
values equal; `as.character()` returned different strings. The executable
[`base-version-equality.R`](base-version-equality.R) asserted that vctrs,
`dplyr::nest_by()` and `tidyr::nest()` placed both source rows in one cell.
It then measured the wrong split in all 96 combinations of plain frame/tibble,
both nesting verbs, both `.keep` settings, three `.sort` settings,
`grouping_set()`/`rollup()`, and payload/no-payload inputs. Each combination's
`NULL`-label, `NA_character_`-label, fixed-key nesting, and Margin-summary
controls retained one two-row detail group. A rollup with the non-missing
label returned three outer rows instead of two; a single Grouping set returned
two instead of one. The default observing run exited 0; `--assert` demanded
the correct membership and exited 1. Adjacent logs retained those outcomes.

## Contract and mechanism

The opening contract of [`nest_with_margins()`](../../../R/nest_with_margins.R)
promised one nested frame per group per grouping set. Its inherited
[*Display labels and grouping identity*](../../../R/summarize_with_margins.R)
section assigned a label a display role. The finding concerned that group
membership: it required neither preservation of the outer key's class nor
restoration of arbitrary attributes, which
[ADR 0016](../../../design/adr/0016-delegate-result-class-and-attributes-to-dplyr.md)
declined to promise.

`execute_margin_nest()` copied the original dimension to an identity column;
`nest_expanded_margins()` grouped on both that identity and the displayed
column. Different display strings therefore refined a single original equality
group. A direct dplyr control in the base-version script produced one group
when grouped on the original identity and two when the display column was
added, without changing the original column's class or attributes.

The full body and comments of closed [#620](https://github.com/sayuks/marginplyr/issues/620)
were read; it had no comments. It addressed distinct original groups merging
when their displays collided. This was the opposite unhandled boundary:
originally equal keys split when their displays differed. The existing hidden
identity protected the earlier separation case but did not prevent this
additional display partition. The new fixture thus supported a separate
membership correction rather than a new attribute-restoration guarantee.
This validation established the local-input scope; it made no equality-proxy
promise for dtplyr/data.table or other backends.
