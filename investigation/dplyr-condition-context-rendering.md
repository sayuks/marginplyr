# How dplyr renders the context of a condition from a summary expression

Investigated: 2026-08-18

Measured while implementing #199, which asked for a Condition context quoting
the caller's own spelling of a summary argument. ADR 0022 records the decision
that came out of this; what follows is the evidence it rests on, including the
parts that ruled options out and the two measurements that belong to ADR 0021's
subject rather than to #199's.

Everything below was run against dplyr 1.2.1 and rlang 1.3.0 on R 4.6.1.

## What a condition carries, by kind

An error from a caller's summary expression arrives with its context in
separately addressable fields. Measured on
`summarize_with_margins(data, total = sum(nonexistent_fn(units)), .grouping = rollup(region))`:

```
class    : "rlang_error" "error" "condition"
names    : message, trace, parent, body, rlang, call, use_cli_format
$message : c(i = "In argument: `total = sum(nonexistent_fn(units))`.")
$body    : c(i = "In group 1: `region = \"East\"`.")
$call    : the Margin verb, once ADR 0021's restatement has assigned it
```

`$message` is the argument bullet and nothing else, and the `i` is the *name*
of the vector element rather than part of its text. The caller's own condition
is `$parent`.

A warning arrives as one condition per branch whose `$message` is a single
pre-rendered string, with `$parent` `NULL` and no `$body` — the shape ADR 0021
already records. The argument bullet is one line inside that string, carrying
cli's `i` glyph.

**No field names the argument.** Neither kind carries the index or the name of
the dot the condition was attributed to; the only statement of it is the
backticked span inside the rendered bullet. That is what forced the
identification in ADR 0022 to be a text comparison at all, and it was checked by
listing the fields above rather than inferred.

## How dplyr renders the label

`dplyr:::error_label_named()` and `dplyr:::expr_as_label()`, both unexported:

```r
error_label_named(name, expr):
  if (is_null(name) || !nzchar(name)) expr_as_label(expr)
  else paste0(name, " = ", expr_as_label(expr))

expr_as_label(expr):
  if (is_data_pronoun(expr)) deparse(expr)[[1]]
  else with_options(`rlang:::use_as_label_infix` = FALSE, as_label(expr))
```

So the `name = expr` convention is dplyr's, and reproducible without reading
anything undocumented. The two divergences from a plain `rlang::as_label()` are:

- **Long infix expressions.** dplyr suppresses rlang's infix labelling through
  an option named with an internal `:::` spelling. Measured on
  `total = sum(as.numeric(grade)) + 0 * (sum(units) + sum(units) + sum(units) + sum(units) + sum(units))`,
  dplyr rendered `` `total = +...` `` where `rlang::as_label()` answered
  `sum(as.numeric(grade)) + ...`. Plain dplyr, outside marginplyr, rendered the
  same `+...`, so the abbreviation is dplyr's and not a marginplyr artefact.
- **Data pronouns.** `is_data_pronoun()` is `FALSE` for `.data` alone and `TRUE`
  for `.data$x` and `.data[["x"]]`, where `as_label()` answers `x`, `x`, and
  `<unknown>`.

Both divergences run in dplyr's direction only: `as_label()` emits neither
`+...` nor `.data$x`. That asymmetry is what establishes that dplyr's label of
one argument can never equal marginplyr's label of another, which is the
property #199's hard constraint needed.

## What separates the caller's spelling from what dplyr quotes

Four rewrites, none of them the one #199's body named:

| Rewrite | Where |
|---|---|
| a selection becomes `dplyr::all_of()` over resolved source names | `summary_all_of_expr()`, via `resolve_summary_selections()` |
| `across()`'s `.names` is evaluated, and its arguments normalised for dtplyr | `rewrite_across_selection()` |
| a share's source expressions are wrapped | `wrap_share_sources()` |
| `grouping_bit()` / `grouping_id()` become the branch's own constant | `rewrite_grouping_dots()` |

#199's body attributed the `all_of()` rewrite to `rewrite_grouping_dots()`. It
is `plan_summary_expressions()`, which runs before either adapter — so the
rewrite is not the `UNION ALL` adapter's, and ADR 0021's reason for confining
itself to that adapter does not transfer. A correction was posted to the ticket
on this date.

The first three rewrites are shared by every branch; the fourth is not, and that
is what made the argument bullet differ *between* grouping sets.

## Positions do not survive planning

`plan_share_expressions()` drops a share dot to `NULL` and expands a placeholder
into one dot per output before flattening, so the dots handed to an adapter are
neither the same length as the caller's nor in correspondence with them by
index. Any per-dot value carried alongside has to be remapped where that
function already remaps its cardinality positions.

## Two measurements about ADR 0021's identity

Both were taken on `main` — that is, they describe the deduplication ADR 0021
shipped, before any of #199's work — and both are properties of reading a
*rendered* message.

**Console width.** ADR 0021's identity reads a message as the lines it was
*written* as, and holds at any width. A reading of the lines cli *rendered* it
onto does not: implementing #199 against rendered lines reported the
`grouping_bit()` reproduction once at 80 columns and twice at 40.

**Colour.** With `cli.num_colors` above 1 — an ordinary interactive session —
cli styles the markers, so every pattern matched against a message misses.
Measured on `main` with the ticket's own `cube(region, grade)` reproduction:

| session | reports |
|---|---|
| `cli.num_colors = 1` | 1 |
| `cli.num_colors = 256` | 4 |

The contract ADR 0021 states — one report, however many grouping sets raised it
— therefore did not hold in a colour session at all. Nothing in #199's change
introduced this and nothing in it fixes it; the ADR 0022 work inherits the same
bound, since it reads the same rendered text. Filed as #217, which is
authoritative for what to do about it; this note is authoritative only for the
measurement above.

**An aggregation without a cause line.** dplyr's aggregated warning normally
introduces the caller's diagnostic with `Caused by`, and both the identity and
#199's restatement use that line as the boundary of dplyr's own region. A
caller whose diagnostic renders empty is aggregated without one:

```
There were 2 warnings in `dplyr::summarize()`.
The first warning was:
i In argument: `z = { ... }`.
i In group 1: `region = "East"`.
i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
```

Measured on `main`, that plan reported twice. So a message with no `Caused by`
line is not evidence that dplyr did not aggregate it — only that the boundary
cannot be found in it.

## Searched for and not found

- No exported dplyr function renders the argument label; `dplyr::as_label` is
  a re-export of rlang's.
- No field, attribute, or condition class carries the failing dot's index or
  name (see the field listing above).
- `dplyr::last_dplyr_warnings()` holds the structured per-branch conditions the
  flat message lacks, but it is documented as a debugging aid and is reset per
  `summarize()` call, so by the end of a branch loop it holds only the last
  branch's warnings. ADR 0021 records the same finding.
