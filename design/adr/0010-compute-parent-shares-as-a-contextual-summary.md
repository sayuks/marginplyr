# Compute Parent shares as a contextual summary

`share_of_parent()` will be an exported contextual helper used only inside
`summarize_with_margins()`. It will take one previously defined, named numeric
scalar summary and return its ratio to the corresponding value in the
immediately less detailed grouping set. It will be supported only when
`.grouping` is a `rollup()`, including rollups with composite dimensions.
Fixed `.by` columns partition the calculation.

Calling `share_of_parent()` directly, from an ordinary `dplyr::summarise()`,
or from `dplyr::mutate()` is an error. The error will explain that the helper
requires the Grouping plan and staged Margin result owned by
`summarize_with_margins()`, and will point callers to a following
`dplyr::mutate()` when they intend to derive a value from an existing Parent
share.

## Decision

The root row has a Parent share of `1`. A missing numerator, zero denominator,
or missing denominator produces `NA_real_`; every other result is an
unclamped double. When duplicate grouping-set occurrences are retained, they
remain separate result rows but are skipped while finding the next strictly
less detailed parent set.

An eligible source is one scalar integer or double per grouping row, including
the corresponding integer, floating-point, and decimal numeric types on lazy
database backends. The result is always cast to a double, including ratios
whose source is an integer count. Logical, complex, date-time, duration,
character, factor, list, data-frame, and other semantic classes are not
implicitly converted. When the summary-result type is available from the
backend's existing metadata seam, a type error identifies the Parent share
output, the source summary, and its detected type; callers make any
intentional numeric conversion in the ordinary summary.

Scalar means exactly one value for each grouping row. A zero-length or
multi-value result is not eligible because it creates missing or multiple
rows within one grouping-set key and leaves no key for matching each child
statistic to the corresponding parent statistic. Multiple statistics are
defined as separate scalar summary columns and receive separate
`share_of_parent()` calls.

General dbplyr backends cannot reliably report an arbitrary summary
expression's result type without executing or probing the staged query.
marginplyr will not issue an additional schema query or implicitly collect
the result solely to improve this error. It will preserve laziness, generate
an explicit double cast and division, and allow an incompatible type to fail
with the database's error when the query is executed. Syntax, source-name,
dependency, and `across()` validation remains local and targeted on every
backend.

The same laziness boundary applies to cardinality that cannot be determined
statically. Backends with an available materialized result report a targeted
error naming the Parent share and source summary. A general lazy SQL query is
not executed early solely to prove scalar cardinality and may therefore fail
when the query is executed.

The portable value contract covers finite numbers, R missing values, SQL
`NULL`, and zero denominators. Local `NaN` is treated as missing. Infinite
values and database-specific `NaN` representations are outside the
cross-backend value guarantee because supported fallback SQL dialects do not
share a portable finite-value predicate or representation. The operation does
not collect a lazy result to validate them. Callers that can produce
non-finite summaries normalize them explicitly in the ordinary summary using
operations supported by their backend.

Parent-share calculation never synthesizes result rows. On an empty input
without fixed `.by` groups, the existing Margin-summary contract produces the
root grand-total row only; its Parent share is `1` even when the source
summary is zero or missing. On an empty input with `.by`, no fixed group
exists, so the result has zero rows and the Parent share column has the
declared double type. Missing detail or subtotal rows are not completed.

The source summary must precede the helper call. Its argument must be the bare
name of that ordinary summary; an aggregate expression, calculated expression,
string, or name that has not yet been defined is an error. When an ordinary
summary has the same name as a source-data column, the helper refers to the
preceding summary output, not the source-data column.

An eligible ordinary summary is either a top-level explicitly named summary
or a statically named column produced by a preceding `across()`. A column
expanded from an unnamed data-frame-valued summary is not eligible, although
such summaries otherwise retain their existing margin-summary behavior. The
expanded column's provenance, type, and expression position are not
consistently available for static dependency validation across local and lazy
backends. The error shows how to rewrite it as top-level named summaries or a
preceding `across()`.

The eligible source name must be defined exactly once in the call before the
Parent share expression. Redefining a summary name makes its provenance and
dependency order backend-sensitive, even when local dplyr can evaluate it.
Such a name is not eligible as a Parent-share source. The error identifies the
duplicate source name and Parent-share output and recommends combining the
calculation into one uniquely named ordinary summary.

The eligible source expression must also be self-contained: it may aggregate
source-data columns, but it may not refer to another summary alias created
earlier in the same call. Although local dplyr can evaluate some sequential
summary aliases, many database backends cannot reuse an alias in the same
`SELECT`. This restriction applies only when a summary is used as a
Parent-share source; it does not redefine the package's general summary
expression contract. The error identifies the Parent share, its source, and
the earlier alias dependency, and recommends combining the calculation into
one ordinary summary expression.

A direct call must have an explicit output name distinct from its source
summary. `across()` is supported when its selection resolves to preceding
ordinary summaries. It must supply `.names` explicitly, and the resulting
names must be distinct:

```r
summarize_with_margins(
  across(c(revenue, units), sum),
  across(
    c(revenue, units),
    share_of_parent,
    .names = "{.col}_share"
  ),
  .grouping = rollup(region, store, product)
)
```

For an `across()` whose function is `share_of_parent`, `.cols` is evaluated
against only the ordinary summaries defined before that expression. Source
columns, fixed and variable grouping keys, and previously created Parent
shares are not in that selection context. Name-based tidyselect helpers such
as `ends_with()` and `everything()` therefore select only eligible ordinary
summaries. Explicitly requesting an ineligible or unknown column produces a
targeted error.

Only name-based tidyselect is accepted in this context. Direct names, ranges
and Boolean combinations, positive and negative selections, `all_of()`,
`any_of()`, `everything()`, and name-pattern helpers are supported.
Type- or value-predicate selection such as `where(is.numeric)` is rejected
because arbitrary summary-result prototypes are not reliably available before
executing every lazy backend. The error recommends an explicit name-based
selection.

The `.fns` argument must be the bare helper `share_of_parent` or its explicit
namespace-qualified form `marginplyr::share_of_parent`. Formulas, anonymous
functions, and function lists are not accepted, even when a list contains
only this helper. In particular, one `across()` cannot combine an ordinary
aggregate and `share_of_parent()` in a function list. Those forms conceal the
helper from the expression planner or make the dependency between the
ordinary summary and the later Parent share ambiguous. Callers instead use
two ordered `across()` expressions and control the Parent share names with
`.names`.

No additional arguments are accepted for the Parent-share function in
`across()`. Missing-value handling belongs to the preceding ordinary
aggregate. `.unpack` may be omitted or explicitly `FALSE`; `TRUE` is rejected
because each Parent share is already one scalar double column. An error names
each unsupported argument and shows the plain direct-function form.

Requiring `.names` avoids the ordinary `across()` default `{.col}`, which
would overwrite each source summary. The package does not silently add a
`_share` suffix because that would introduce a contextual naming convention
that differs from `across()`. Empty names, duplicate generated names, and
collisions with fixed or variable grouping keys, ordinary summaries, `.id`,
or other Parent shares are detected before query execution and reported with
the conflicting name.

The initial interface accepts `share_of_parent()` only as the complete
right-hand side of a named summary or as the function supplied directly to
`across()`. It does not accept arithmetic, conditionals, anonymous functions,
or other calls wrapped around the helper:

```r
# Supported
revenue_share = share_of_parent(revenue)

# Not supported
revenue_percent = 100 * share_of_parent(revenue)
revenue_share = share_of_parent(sum(revenue))
revenue_share = share_of_parent(revenue + tax)
revenue_share = share_of_parent("revenue")
```

```r
# Not a Parent-share source
summarize_with_margins(
  sales,
  tibble::tibble(
    revenue_total = sum(revenue),
    units_total = sum(units)
  ),
  revenue_share = share_of_parent(revenue_total),
  .grouping = rollup(region, store)
)
```

A later summary expression cannot refer to a Parent share created earlier in
the same `summarize_with_margins()` call, and a Parent share cannot be the
source of another `share_of_parent()` call. Parent shares are calculated after
ordinary summaries, so permitting those expressions would require another
ordered expression stage and would differ from ordinary dbplyr summary-alias
rules. Callers will use a following `mutate()` for scaling, rounding,
formatting, conditionals, and other derived values.

An independent ordinary summary may appear after a Parent share expression.
The implementation may evaluate all ordinary summaries in its first query
stage, but dependency validation follows the expressions' written order: a
Parent share can reference only an eligible ordinary summary written before
it. Final columns retain the user's expression order, including the expansion
position of an `across()` call; internal query staging does not reorder the
public result.

The helper is not available for `grouping_sets()`, `cube()`, or
`grouping_spec()`, because those specifications do not define one unambiguous
parent chain. It does not accept a subtotal-display option: every non-root row
uses its immediate parent.

## Execution consequence

Parent shares cannot be evaluated by an ordinary summary expression because
the denominator belongs to another grouping-set row. The executor will first
materialize the ordinary Margin summaries, then calculate all requested
Parent shares together using one shared parent mapping. This staging also
avoids relying on backend-specific support for referring to a summary alias
from the same `summarise()` call.

The staged calculation must preserve lazy execution and backend-independent
results. Internal grouping-set metadata, rather than displayed Margin labels,
will identify levels and parent rows. Genuine missing grouping values and
Margin labels therefore cannot be confused while matching parents.

Parent lookup uses missing-safe key identity. Missing values in fixed `.by`
columns belong to one fixed partition, and missing values in included
grouping dimensions match the corresponding missing-valued parent key.
Implementations must not rely on ordinary SQL `NULL = NULL`, nor use a
displayed Margin label as a join key. Internal grouping metadata distinguishes
a dimension omitted for a margin from an included dimension whose source
value is missing. Local, live DuckDB, and portable-fallback tests cover both
fixed and variable missing keys.

Validation will identify the unsupported output expression and the Parent
share column it references. Errors will state the supported replacement
rather than exposing the internal staging:

```text
`share_of_parent()` can only be used inside
`summarize_with_margins()`.
```

```text
`revenue_percent` can't use Parent share `revenue_share`
inside the same `summarize_with_margins()` call.
Create it in a following `dplyr::mutate()`.
```

```text
`revenue_percent` must call `share_of_parent()` directly.
Create scaled, rounded, conditional, or formatted values in a
following `dplyr::mutate()`.
```

```text
`revenue_share` must give `share_of_parent()` the bare name of a
previously defined ordinary summary.
```

```text
`revenue_share` can't use `revenue_total` because it was expanded from
a data-frame-valued summary.
Define `revenue_total` as a top-level named summary or with a preceding
`across()`.
```

```text
`revenue_share` can't use `revenue_total` because that summary name is
defined more than once.
Define it once with a complete ordinary summary expression before
calling `share_of_parent(revenue_total)`.
```

```text
`net_share` can't use ordinary summary `net` because it depends on
earlier summary `gross`.
Make `net` a self-contained ordinary summary, for example by combining
the aggregate expressions.
```

```text
`flag_share` can't use ordinary summary `flag_total` because it has
type <logical>; `share_of_parent()` requires a numeric scalar summary.
Convert it explicitly in the ordinary summary.
```

```text
`quantile_share` can't use ordinary summary `revenue_quantile`
because it returns more than one value per grouping row.
Create separate scalar summary columns and calculate a Parent share
for each one.
```

```text
`revenue` can't be selected for `share_of_parent()` because it is not
a previously defined ordinary summary.
```

```text
`share_of_parent()` must be supplied directly as `.fns`.
Use a separate `across(..., share_of_parent, .names = ...)`
after the ordinary summaries.
```

```text
`na.rm` can't be supplied to `share_of_parent()` through `across()`.
Handle missing values in the preceding ordinary summary.
```

```text
`.unpack = TRUE` can't be used with `share_of_parent()` because it
returns one double column per selected summary.
```

```text
`.names` is required when `across()` uses `share_of_parent()`.
Choose names that do not overwrite the source summaries, for example
`.names = "{.col}_share"`.
```

```text
`where()` can't be used to select summaries for `share_of_parent()`
because summary-result types are not portable before lazy execution.
Select previously defined summaries by name, for example with
`all_of()`.
```

The helper reference will document the direct-call grammar, numeric,
missing-value, and non-finite portability rules, root behavior, duplicate-set
behavior, empty-input behavior, expression and output ordering, and the
restriction to `rollup()`. Its edge-case table will distinguish an empty
ungrouped input from an empty input partitioned by `.by`, and will state that
the helper does not perform key completion. It will also show a missing fixed
key and a missing included dimension reaching their correct parents without
being confused with a Margin label.

The numeric-source section will define scalar cardinality, reject zero-length
and multi-value summaries with a quantile example, and show separate scalar
summary columns as the supported replacement.

The direct-call section will distinguish eligible top-level and `across()`
summaries from data-frame-valued summaries, define the latter in plain
language, include `.data` explicitly in every non-pipe example, and show the
supported rewrite next to the rejection. It will also explain that a
Parent-share source name must be unique within the call and show how to
combine an overwritten calculation into one ordinary summary expression. It
will define the self-contained-source rule, explain the SQL alias portability
reason, and show the combined-expression rewrite.

Its `across()` section is a required constraints section, not only an example.
It will:

- show the two ordered `across()` expressions;
- define the preceding-ordinary-summary-only selection context;
- enumerate supported name-based tidyselect forms, including negative
  selection and `all_of()`/`any_of()`;
- reject `where()` and other type- or value-predicate selectors, explain the
  lazy-type limitation, and show an explicit-name rewrite;
- list the bare and namespace-qualified direct `.fns` forms;
- reject formulas, anonymous functions, and function lists;
- explain why aggregation and Parent share cannot share one function list;
- reject additional function arguments and direct missing-value handling;
- allow only omitted or false `.unpack`;
- require an explicit `.names`;
- enumerate empty, duplicate, grouping-key, ordinary-summary, `.id`, and
  Parent-share name collisions; and
- give a supported replacement next to every rejected form.

The `summarize_with_margins()` reference and Get Started guide will show one
direct call, one `across()` call, and one post-summary `mutate()` example.
The summary reference's empty-input section will include the Parent share
column and its type for both fixed-key cases.
The grouping-identity article will explain that Parent lookup uses internal
Grouping set identity and bits rather than displayed Margin labels, and link
back to the helper's full value and expression contract.
Database documentation will explain that the ordinary summaries and Parent
shares are separate lazy query stages without requiring callers to understand
their join implementation. It will also distinguish the guaranteed
finite/`NULL`/zero behavior from backend-native `NaN` and infinity semantics.
It will state that general dbplyr sources remain lazy and receive no extra
schema or cardinality probe, so an incompatible summary-result type or
non-scalar result may surface as a database error at execution even though all
statically detectable helper errors are targeted before execution.

## Amendment: where each backend proves the source contract

This decision divides backends into ones whose summary-result type and
cardinality are available before execution and ones whose are not, and it
assumed every backend falls on one side or the other of that line. Two do not,
and the implementation settled both. The value contract above is unchanged;
only where each backend enforces it is amended.

**Arrow rejects Parent shares entirely.** Arrow's schema seam can reject some
semantic types but cannot prove scalar cardinality, and the mechanisms that
would run validation inside the query — native UDFs, batch hooks, query
wrappers — either erase the Package condition class and the caller's call or
are not preserved by query-rebuilding verbs. Both alternatives to rejecting
were therefore unavailable: enforcing the contract weakly would let an
ineligible source through, and enforcing it by collecting would collect behind
the caller's back. A Parent-share request on an Arrow backend raises a Package
condition before ordinary summaries are staged, so no Arrow query is
constructed. Ordinary Arrow Margin summaries, expansions, and nesting are
unaffected. ADR 0005 records why that admission point is allowed to run after
the typed metadata snapshot.

**dtplyr validates at explicit execution.** dtplyr has no materialized result
to inspect while the query is built, but unlike a general SQL backend it
translates R expressions, so the check can be placed inside the ordinary
summary rather than beside it. Each referenced source summary is wrapped in a
validator that becomes part of the translated data.table expression and runs
only when the caller collects. It costs no validation-only query, keeps the
result a native lazy step, and raises the same Package condition — including
`marginplyr_parent_cardinality_error` for a non-scalar source — naming the
Parent share, the source summary, and the original public call. An invalid
source therefore fails at collection rather than emitting a wrong row.

Local execution uses the same wrapper for the same reason: validating inside
the ordinary summary is what removes the full input rescan per Grouping set
that a separate cardinality query would need. General dbplyr keeps the
relaxation this decision already granted it: no extra schema or cardinality
query, no implicit collection, and an incompatible type or non-scalar result
surfacing as a database condition at execution.

## Considered options

`rollup_share()` was rejected because it names the input structure rather than
the result's meaning. A `.subtotals = "one"` mode was rejected because forcing
subtotal rows to `1` is a presentation rule and obscures the immediate-parent
definition, especially in rollups with three or more dimensions.

A post-summary `add_rollup_share()` verb was rejected because the grouping
plan and parent mapping are already available inside the Margin operation.
Allowing `cube()` or arbitrary grouping sets was deferred until an explicit
parent-selection model exists.
