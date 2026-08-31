# What the `.data` pronoun settles in a tidyselect selection

Investigated: 2026-08-31

#346 asked for `is_name_only_expr()` to settle `.data$x` from the column names
and drew a boundary at the pronoun's other half:

> It is narrower than it looks: `.data$x` with a literal name is settled, while
> `.data[[var]]` is not, so whatever reads this has to tell a symbol
> right-hand side from an expression one.

That premise is what this note measures. It does not hold: every spelling under
the pronoun is settled by the names, so the reader the issue asked for would
have declined half of its own subject.

Measured against tidyselect 1.2.1, dplyr 1.2.1, R 4.6.1, through
`tidyselect::eval_select()` on `grouping_name_proxy(c("region", "area",
"value"))` — the list of positions marginplyr resolves a name-only selection
against — with the subscript variable `v <- "region"` bound in the quosure's
environment.

| selection | result |
|---|---|
| `.data$region` | selects `region` |
| `.data[["region"]]` | selects `region` |
| `.data[[v]]` | error: The subscript of `.data[[subscript]]` must be a constant. |
| `.data[[toupper(v)]]` | error: same |
| `.data[[c("a", "b")]]` | error: same |
| `.data[[1]]` | error: Must subset the data pronoun with a string, not the number 1 |
| `.data[[NULL]]` | error: Must subset the data pronoun with a string, not `NULL` |
| `` `$`(.data, "region") `` | error: The RHS of `.data$rhs` must be a symbol. |
| `(.data)$region` | error: object '.data' not found |
| `.data$region$sub` | error: object '.data' not found |

So the subscript never reaches the data. A constant string is a name lookup; a
subscript that is not a constant, or is no string, is refused before anything
is walked, exactly as `*` and `^` are. Both are answers the column names
decide, which is the question `is_name_only_expr()` asks.

Two of the errors are about the pronoun's *operand* rather than its subscript,
and they are why the reader compares that operand with the `.data` symbol as
written rather than through a redundant pair or a second `$`.

## What differs by position, and what does not

`dplyr::select(d, .data[[v]])` selects `region` while
`tidyselect::eval_select(quo(.data[[v]]), d)` raises the constant-subscript
error, which reads like a positional difference in the pronoun. It is not one:
`select()` wraps its arguments as `c(!!!quos)`, so what tidyselect walks is a
`c()` call holding a *quosure*, and a quosure node is evaluated whole rather
than walked as a `[[` node. Confirmed by writing the same shape without
`select()`: `c(.data[[v]])` with `.data[[v]]` written as a bare call raises the
constant-subscript error, so it is the quosure and not the `c()` that changes
the answer.

marginplyr passes each argument's own quosure to `eval_select()`, so the
pronoun is the quosure's expression and is walked as a `[[` node. The
`select()` reading is not reachable from there, and an injected quosure is
answered `FALSE` by `is_name_only_expr()` for its own reason.

## Where this landed

`is_data_pronoun()` in `R/grouping-plan.R` takes both halves, and the header
there is authoritative for the decision. This note is the evidence and is not
maintained: it says what tidyselect 1.2.1 did on the date above.
