# An empty argument under a selection walk

Investigated: 2026-09-01

Measured while deciding what `is_name_only_expr()` should answer for R's empty
argument (#351). That reader read a name off every symbol, and the empty
argument is a symbol whose name is `""`, so `rlang::env_has()` raised `attempt
to use zero-length variable name` for any selection carrying one. Answering it
as tidyselect answers it required knowing what tidyselect does with such a
part, which is not documented in `language.Rd` or anywhere else read here.

## Environment

| Component | Version |
|---|---|
| R | 4.6.1 |
| tidyselect | 1.2.1 |
| rlang | 1.3.0 |
| vctrs | 0.7.3 |
| dplyr | 1.2.1 |

## What tidyselect does, per operator

`tidyselect::eval_select()` over a two-column frame, one empty operand per
call. The operators are those `selection_walk_operators()` names plus `(`,
and two leaf helpers for contrast.

| expression | result |
|---|---|
| `c(, region)` | selects `region` |
| `c(region, )` | selects `region` |
| `c(region, , grade)` | selects `region`, `grade` |
| `-( )` | `simpleError`: attempt to use zero-length variable name |
| `!( )` | `simpleError`: attempt to use zero-length variable name |
| `\|(, region)` | `simpleError`: attempt to use zero-length variable name |
| `/(, region)` | `simpleError`: attempt to use zero-length variable name |
| `( )` | `simpleError`: attempt to use zero-length variable name |
| `:(, region)` | `missingArgError`: argument "expr" is missing |
| `&(region, )` | `missingArgError`: argument "y" is missing |
| `all_of( )` | `rlang_error`: argument "x" is missing |
| `starts_with( )` | `rlang_error`: argument "match" is missing |

`c()` is the only operator that drops the part. Under every other one
tidyselect fails, and under five of them with the same untyped message
marginplyr was producing on its own account — so a caller who reached that
message through `-( )` was seeing tidyselect's failure either way, while one
who reached it through `c(, region)` was seeing a selection tidyselect would
have accepted.

Both halves are settled by the column names: no branch above reads a column's
data, so a reader answering `TRUE` for the part hands tidyselect a question
the names decide, which is the contract `is_name_only_expr()` states.

Only the three `c()` rows are spellings R parses. The rest were built with
`as.call()`, and the empty argument was written as `rlang::missing_arg()` at
each use rather than bound to a local, because reading such a local raises
`missingArgError` naming the local (#168, #174) — which happened once while
building this table.

## The same measurement through `dplyr::summarise()`

`dplyr::summarise(d, n = n(), .by = <expr>)` was measured for the `c()` rows
and agrees with `eval_select()`: `c(, region)`, `c(region, )`, and
`c(region, , grade)` group by the named columns. `dplyr::select()` reproduces
the failures for the rest, so the non-`c()` rows are not a marginplyr
divergence to close.

## What was not measured

Why `c()` differs was not traced into tidyselect's `eval_c()`. The finding
here is the behaviour rather than its mechanism, and the behaviour is what the
reader's answer rests on.
