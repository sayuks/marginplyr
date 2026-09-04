# What truncates an argument label on the native grouping-sets adapter

Investigated: 2026-09-04

ADR 0022 confines the Condition-context restatement to `summarize_margin_union()`
and rejects extending it to the native grouping-sets adapter "as covering nothing
today, since both backends holding the capability are lazy". Its own *Scope*
section names an exception to that reasoning — an error dbplyr raises while it
translates the rewritten expression does arrive while the verb runs — and leaves
it quoting the rewrite. This note measures that exception on the date above:
what the caller was shown, why the label is unreadable, and whether the
mechanism ADR 0022 already has would restore it if applied there. It was
prompted by #410.

Everything below was run against marginplyr at `4f367b4` with R 4.6.1,
dplyr 1.2.1, dbplyr 2.6.0, rlang 1.3.0, and cli 3.6.6, on a live DuckDB
connection (duckdb 1.5.5) and on `dbplyr::simulate_postgres()`, which agreed.

## Method, and which measurements are simulations

Nothing under `R/` was modified. The SQL-rewritten dots the native adapter hands
dplyr were captured during execution with
`trace(rewrite_grouping_dots, where = asNamespace("marginplyr"))`, and the
package's own `summary_argument_labels()`, `branch_argument_map()`, and
`restate_condition_arguments()` (all `R/conditions.R`) were then applied to the
real condition objects.

Two measurements are therefore **simulations**, and are marked as such below: the
native adapter does not call that machinery on this date, so the package's real
functions were applied to the real condition after the fact rather than observed
firing in place. Everything else is an observation of an unmodified call.

## The bullet a native-path translation error carries

Measured on
`summarize_with_margins(remote, total = sum(value) + grouping_bit(a) + no_such_column, .grouping = rollup(a))`:

```
ℹ In argument: `total = +...`
Caused by error:
! Object `no_such_column` not found.
```

The condition chain has the shape ADR 0022 assumes for an error — the argument
bullet alone, the marker being the element's name rather than part of its text,
and the caller's diagnostic in `$parent`:

```
-- chain[[1]] class: rlang_error/error/condition
   message[1] (name="i"): "In argument: `total = +...`"
-- chain[[2]] class: rlang_error/error/condition
   message[1] (name=NULL): "Object `no_such_column` not found."
```

The same call on a local input, where the restatement applies, renders:

```
ℹ In argument: `total = sum(value) + grouping_bit(a) + no_such_column`.
ℹ In group 1: `a = "x"`.
Caused by error:
! object 'no_such_column' not found
```

## What truncates the label

Not the length of the caller's expression. Three readings settle it.

Handing dbplyr's lazy `summarize()` an expression of the caller's own shape
produces an untruncated bullet:

```
ℹ In argument: `total = sum(value) + 0L + no_such_column`
Caused by error:
! Object `no_such_column` not found.
```

A shorter caller expression is truncated all the same:
`total = grouping_bit(a) + no_such_column` renders `` `total = +...` ``.

And `rlang::as_label()` over the rewrite itself, in isolation:

| expression | `as_label()` |
|---|---|
| a bare SQL literal — what a dot that is `grouping_bit(a)` alone rewrites to | `<sql>` |
| `<sql literal> + 1` | `+...` |
| `sum(value) + <sql literal>` | `+...` |

So what truncates the label is that the rewrite splices in a SQL literal whose
deparse is multi-line. The consequence is that native-path translation errors are
unreadable across the board rather than in a long-expression corner: any dot
combining a helper with anything else labels as `+...`, and the one case that
escapes — a dot that is the helper alone — labels as `<sql>`, which names the
caller's expression no better.

## The span, marginplyr's own label, and uniqueness

`summary_argument_labels()` over the captured rewritten dots answers
`total = +...`, identical to the span dplyr put in the bullet. Read with the
trailing period made optional (see below):

```
relaxed span: "total = +..." 
span == marginplyr's rewritten label: TRUE 
span matches exactly one label: TRUE 
```

`rlang::as_label()` of the caller's own spelling is
`total = sum(value) + grouping_bit(a) + no_such_column`, with no `...` in it, so
what a substitution would put back is readable.

`branch_argument_map()` over the captured dots and the caller's labels builds

```
                                           total = +... 
"total = sum(value) + grouping_bit(a) + no_such_column" 
```

## One byte separates the two bullet formats

The bullets differ in exactly one character:

```
native bullet: "In argument: `total = +...`"                                      ends with '.': FALSE
local  bullet: "In argument: `total = sum(value) + grouping_bit(a) + no_such_column`."  ends with '.': TRUE
```

`restate_argument_bullet()` (`R/conditions.R:506-519`) matches
`` "^([iℹ] )?In argument: `(.*)`\\.$" ``, which requires the period. Programmatic
extraction of the native bullet with that pattern returns `character(0)`: the
pattern does not match, so no span is read and the map is never consulted, even
though its key already equals the span.

The pattern was written against eager dplyr's bullet, which carries the period.
dbplyr's does not.

## End to end (simulation)

**(a) With the mechanism exactly as it stands.**
`restate_condition_arguments(err, map)` returns the message unchanged, and so
does wrapping the failing `summarize()` in `with_branch_conditions()`. The cause
is the period alone.

**(b) With the trailing period made optional in the bullet reader**, everything
else being the package's own map and run reading:

```
ℹ In argument: `total = sum(value) + grouping_bit(a) + no_such_column`
Caused by error:
! Object `no_such_column` not found.
```

The restoration is a string-level substitution performed on a message that
already exists, so there is no stage at which the substituted caller label could
be truncated a second time.

## The uniqueness boundary

Two *unnamed* dots that both collapse to `+...` share a label. Where the callers
spelled them differently, `branch_argument_map()` finds no single candidate and
drops the entry — measured on two such dots:

```
labels of two unnamed rewritten dots:
[1] "+..." "+..."
map built from them:
named character(0)
```

so dplyr's own quotation stands, which is the degradation ADR 0022 documents.
Where the callers spelled them alike the entry is kept, the restoration being
unique whichever dot dplyr meant — the rule the ADR body already states, read
here off `R/conditions.R:390-397` rather than measured. A named dot carries
`name = ` into its label, so it shares a label with neither an unnamed dot nor a
differently-named one.

## Searched for and not found

- No condition field naming the failing dot on the native path either — the
  chain carries the same two links and nothing else, so the identification is a
  text comparison here for the same reason the 2026-08-18 note gives.
- No dbplyr option, connection field, or `simulate_*` variant that changes the
  bullet's trailing punctuation; the two spellings come from dplyr's eager path
  and dbplyr's respectively.
- No translation *warning* was produced by any expression tried, so whether the
  warning shape reaches this path is unestablished rather than ruled out.
