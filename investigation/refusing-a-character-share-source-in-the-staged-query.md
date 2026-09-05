# Refusing a character share source from the staged query itself

Investigated: 2026-09-05
Revised: 2026-09-05 — `R/share.R`
Revised: 2026-09-05 (#446) — `R/share.R`

`investigation/share-source-eligibility-on-coercing-dialects.md` (2026-08-16)
established that a dialect can be asked whether it converts a non-numeric value
to a number, and that the answer is a property of the dialect. #429 then found
that the question that probe asks and the operations the staged share performs
are two different dialect behaviours, so DuckDB calculated a Parent share from
a character source while classified as refusing.

This note measures #429's second option, *Change the query*: making the staged
share apply an operation the refusing dialects reject on a character source. It
establishes that such an expression exists, that it substitutes for the two
casts rather than adding a term beside them, and what it cost the suite on the
date below.

Measured on 2026-09-05 against R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0, duckdb
1.5.5, RPostgres 1.4.10 with PostgreSQL 17.11 (Homebrew), RSQLite 3.53.3, and
bit64 4.8.6, with marginplyr loaded from the working tree at `1c126b2` via
`pkgload::load_all()`. arrow, data.table, dtplyr, DBI, tibble, and tidyr were
all installed. Scripts were throwaway; commands are quoted inline.

## What the staged share applied to its source

The expression built for a Parent share divided two `as.double()` casts, and
guarded that division with `is.na()` on the source and on the denominator and
with an equality against `0` on the denominator. dbplyr rendered it on DuckDB
as:

```sql
CASE WHEN (... OR "..marginplyr_denominator_of_total_1" = 0.0) THEN NULL
     ELSE (TRY_CAST(total AS DOUBLE) / TRY_CAST("...denominator..." AS DOUBLE)) END
```

Both of the operations that can reject a value were measured against DuckDB
directly:

| SQL | Result |
|---|---|
| `TRY_CAST('x' AS DOUBLE)` | `NA`, no error |
| `TRY_CAST('1' AS DOUBLE)` | `1` |
| `'x' = 0.0` | Conversion Error |
| `'1' = 0.0` | `FALSE` |
| `SUM('x')` | Binder Error |
| `SUM('1')` | Binder Error |

`TRY_CAST` raised for nothing, so the error a non-numeric source produced came
entirely from the equality. `SUM`'s refusal held whatever the string contained;
the equality's refusal did not. The probe measured a value-independent
behaviour while the staged query depended on a value-dependent one.

## Which operations a dialect refused for a character column

Twenty-four expressions were run over three one-column tables — a `DOUBLE`, a
`VARCHAR` holding `'1','2','3'`, and a `VARCHAR` holding `'n','m','o'` — on
DuckDB and on PostgreSQL. The wanted shape is: the numeric column answers, and
both character columns raise.

| SQL | DuckDB | PostgreSQL |
|---|---|---|
| `c + 0`, `c * 1`, `c / 1`, `c - c`, `-c` | refused character | refused character |
| `c + 0.0`, `c + CAST(0 AS DOUBLE PRECISION)` | refused character | refused character |
| `abs(c)`, `sqrt(c)`, `round(c)`, `floor(c)`, `ceil(c)`, `ln(c)`, `exp(c)`, `sign(c)` | refused character | refused character |
| `greatest(c, 0)`, `least(c, 0)`, `coalesce(c, 0)` | refused character | refused character |
| `c > 0` | refused character | refused character |
| `CASE WHEN c > 0 THEN 1 ELSE 0 END` | refused character | refused character |
| `c = 0` | **accepted `'1'`**, raised on `'n'` | refused character |
| `nullif(c, 0)`, `c IS NOT DISTINCT FROM 0` | **accepted `'1'`**, raised on `'n'` | refused character |
| `CAST(c AS DOUBLE PRECISION)` | **accepted `'1'`**, raised on `'n'` | **accepted `'1'`**, raised on `'n'` |

DuckDB accepted a numeric-looking `VARCHAR` in four of the twenty-four: the
equality, `nullif`, `IS NOT DISTINCT FROM`, and the cast. The other twenty
refused a `VARCHAR` argument at binding, whatever it held. The staged share's
two rejecting operations — the equality against the denominator, and the cast
applied to both sides — were among those four. PostgreSQL accepted only the
cast.

## A substitution, not an addition

Five dplyr-level forms were run through dbplyr on DuckDB, PostgreSQL, and
RSQLite, over a `DOUBLE` pair, an `INTEGER` pair, and the two `VARCHAR` pairs.
`x` is the numerator and `y` the denominator; the wanted result is `0.5` for
both numeric pairs and an error for both character pairs.

| form | DuckDB | PostgreSQL | RSQLite |
|---|---|---|---|
| `as.double(x) / as.double(y)` | character passed | character passed | character passed |
| `(x * 1) / (y * 1)` | wanted | wanted | character passed |
| `(x + 0) / (y + 0)` | wanted | wanted | character passed |
| `as.double(x * 1) / as.double(y * 1)` | wanted | wanted | character passed |
| `x / y` | wanted | integer pair gave `0` | integer pair gave `0` |

dbplyr rendered `x * 1` as `x * 1.0` on all three dialects, which is what forced
the numeric binding. The bare division is not usable: PostgreSQL and RSQLite
performed integer division on the `INTEGER` pair, which is what the casts were
there for.

RSQLite let a character source through under every form. `?share_of_parent` and
`check_dialect_share_sources()` are authoritative for what happens to a share
over a converting dialect; this note records only that no form measured here
changed RSQLite's answer.

`(x * 1) / (y * 1)` is the same length as the two casts it stands in for.
#429's second option was described as costing "an expression to every share on
every dialect"; what was measured is a substitution.

## End to end, with the substitution applied

The share expression in `R/share.R` was edited to the `mul1` form and the
working tree reloaded. Each call used `.check_share_source = FALSE`, so the
dialect check was bypassed and what executed was the database's own answer.

| case | DuckDB | PostgreSQL |
|---|---|---|
| character source, `'1','2','3'`, Parent share | raised | raised |
| character source, `'n','m','o'`, Parent share | raised | raised |
| `INTEGER` source, Parent share | `0.5, 0.5, 1` | `0.5, 0.5, 1` |
| `INTEGER` source, Total share | `0.5, 0.5, 1` | `0.5, 0.5, 1` |

A local data frame under the same edit returned `0.5, 0.5, 1` for an integer
source. The local, dtplyr, and `other` backend kinds reach the same expression
builder as the SQL kinds — `share_adapter()` routes them to
`execute_row_matched_shares()`, and both adapters call `apply_joined_shares()`,
which is where the ratio is built — so the edit reached them too, and the suite
run below is what exercised them.

## What the substitution cost the suite

`testthat::test_local()` was run over the whole suite with `NOT_CRAN=true` and
`MARGINPLYR_REQUIRED_SUGGESTS=""`, with the edit in place, and with every
optional package named in the environment block above installed, so no test
skipped for an absent Suggest. One assertion failed:

```
test-share-backends.R — fallback simulators render portable staged Parent-share SQL
expect_match(sql, "(CAST|CDBL)\\(", info = simulator)
```

That assertion matches the rendered SQL against a cast, which the substitution
removes. What it stands for was not re-established: the test renders for the
fallback simulators without executing, and the `x / y` row above shows integer
division defeating a cast-free form on two dialects that were executed. Whether
`x * 1.0` guards those simulators' dialects against integer division was
therefore not measured here.

No other test in the suite changed result. The working tree was restored
afterwards; nothing here was committed as a change to `R/`.

## What this establishes

An expression exists that made both DuckDB and PostgreSQL refuse a character
share source from the staged query itself, value-independently, while leaving
integer and double sources calculating as before. It is a substitution for the
two casts, it cost one assertion in the suite as it stood on this date, and it
was reached without changing any dialect's verdict.

## What was not measured

- Any dialect other than DuckDB, PostgreSQL, and SQLite. The fallback
  simulators were rendered but not executed, and the generic `sql` kind was
  not run.
- What `x * 1` does to the type a backend returns for a source of a type other
  than the integer and double pairs run here — a `NUMERIC` or `DECIMAL` column
  in particular.
- What dtplyr translates `(x * 1) / (y * 1)` to in data.table, beyond the suite
  passing with the edit in place.
- Whether the equality against the denominator should change too. Under the
  substitution the division refused at binding, so the statement failed before
  that equality executed; whether it should refuse on its own was not
  established.
- Arrow inputs, which `abort_arrow_shares()` is reached for rather than the
  ratio; no Arrow case was run under the edit beyond the suite.

## Revisions (2026-09-05)

Two items *What was not measured* left open were measured on this date while
#429 was implemented, and a third question — what the widening those two found
costs at a declared type's maximum — came out of a review of that
implementation. Together they rule out every form scored above:
`as.double(x) / as.double(y)` computes a character share, and every form
carrying `* 1` carries `* 1.0`, since R's `1` is a double. `R/share.R` is
authoritative for what the package sends.

**What `x * 1` does to a source that is neither an integer nor a double.** On
duckdb 1.5.5, `typeof()` was read for each step over a `DECIMAL(18,3)` pair and
a `BIGINT` pair:

| expression | `DECIMAL(18,3)` source | `BIGINT` source |
|---|---|---|
| `x` | `DECIMAL(18,3)` | `BIGINT` |
| `x * 1.0` | `DECIMAL(18,4)` | `DECIMAL(21,1)` |
| `TRY_CAST(x * 1.0 AS DOUBLE)` | `DOUBLE` | `DOUBLE` |

So the multiplication did not by itself make its operand a double, and the bare
`(x * 1) / (y * 1)` left what the driver returned to the dialect's decimal
handling. That is a second reason to keep the cast, beside the integer division
the `x / y` row of that table recorded.

**What the widening does at the declared type's maximum.** The row above was
read for its type alone, and what that type change costs was not asked until a
review of #429's implementation asked it. In each `DECIMAL` case measured the
scale grew by one while the width did not, so a value the declared type holds
no longer fits. Measured on duckdb 1.5.5 over a column holding its declared
type's maximum:

| source type | value | `x` | `x * 1.0` | `x * 1` |
|---|---|---|---|---|
| `DECIMAL(18,2)` | `9999999999999999.99` | answered | `DECIMAL(18,3)`, raised | `DECIMAL(18,2)`, answered |
| `DECIMAL(38,10)` | `9999999999999999999999999999.9999999999` | answered | `DECIMAL(38,11)`, came back missing | `DECIMAL(38,10)`, answered |

The `DECIMAL(18,2)` refusal was

```
Out of Range Error: Overflow in multiplication of DECIMAL(18)
(999999999999999999 * 10).
```

and the `DECIMAL(38,10)` case raised nothing at all — the column came back
`NA`, which is what the share would have reported.

The literal's own type is what decides this, and R's `1` is a double, which
dbplyr renders as `1.0`. An integer literal changed no type: `HUGEINT` stayed
`HUGEINT` under `* 1` where `* 1.0` made it `DECIMAL(21,1)`, and `x + 0` and
`x - 0` also left `DECIMAL(18,2)` alone while `x / 1` gave `DOUBLE`. All six
forms that refused a `VARCHAR` at binding still refused one with an integer
literal, reporting `'*(VARCHAR, INTEGER_LITERAL)'` in place of
`'*(VARCHAR, DECIMAL(2,1))'`.

**What the fallback simulators render.** *What the substitution cost the suite*
left unmeasured whether `x * 1.0` guards those dialects against integer
division. Keeping the casts does not put that question to them: all fifteen
simulators that section's test names rendered both the multiplication and the
dialect's own cast, so the assertion that failed under the bare form did not
fail under a form that keeps them. Nothing here was executed, which is the whole
of what a simulator can establish.

**The multiplication on the denominator refuses nothing extra.** The
denominator is the source summary carried through a join, so it holds the
source's type, and a dialect that refuses one refuses the other. Dropping it
was measured to change no result and no refusal: a `VARCHAR` source was still
refused at binding with only the numerator multiplied, and the
`DECIMAL(18,2)` maximum answered `5e+15` for `x * 1 / y`, identical to
`x / y`. It halves how many operands the widening above can reach.

Nothing above is overturned. Which operations each dialect refused for a
character column stands, and every form measured made DuckDB and PostgreSQL
refuse a character source value-independently; what these measurements settled
is which of them is safe for the numeric sources that must keep working.

## Revisions (2026-09-05, #446)

Everything above measures the *ratio*. #446 found the path that builds no
ratio: when a Grouping plan gives every occurrence its own denominator, share
construction returns before any denominator is mapped or joined, and nothing in
the staged query then referenced the source at all. A refusing dialect was
never asked to bind it, so DuckDB answered `1` for a character source that the
local path refused. This section measures that path and the expression put in
it on this date; nothing above is overturned, because no measurement above
reaches a share with no denominator. `R/share.R` is authoritative for what the
package sends.

Measured on 2026-09-05 against R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0, duckdb
1.5.5, RSQLite 3.53.3, dtplyr 1.3.3, and data.table 1.18.6.1, with marginplyr
loaded from the working tree via `pkgload::load_all()`.

**Which plans reach it.** A Total share reaches the branch whenever every
occurrence's variable part is empty. All six combinations of `.grouping` absent,
`grouping_sets(grouping_set())`, and two empty sets under
`.duplicates = "keep"`, each with and without `.by`, reached it. A **Parent
share cannot**: `share_of_parent()` requires `.grouping` to be one pure
`rollup()`, every rollup has at least one dimension, and every such plan gives
some occurrence a parent. Both empty-set spellings were refused by that
grouping check before any share was built.

**Which backends had the gap.** The local path and dtplyr both refused a
character source with the eligible-type diagnostic, from the result and before
any share expression. RSQLite refused the call whatever the source held —
including an eligible one — because the dialect verdict is what refuses it
there. The gap was general dbplyr on a refusing dialect alone.

**What binds the source without changing the value.** Three forms were run on
DuckDB over a `VARCHAR` holding `'1','2','3'` and one holding `'1','2','n'`:

| SQL | numeric-looking | non-numeric |
|---|---|---|
| `CASE WHEN (c*1) IS NULL THEN 1.0 ELSE 1.0 END` | Binder Error | Binder Error |
| `1.0 + 0 * (c*1)` | Binder Error | Binder Error |
| `MAX(c*1) IS NULL` | Binder Error | Binder Error |

Each refused at binding, value-independently, reporting
`'*(VARCHAR, INTEGER_LITERAL)'` — the same operand pair *A substitution, not an
addition* recorded, so this rests on the same 20-of-24 result and adds no
dialect behaviour to it. The first is what `dplyr::if_else()` rendered, and
dbplyr rendered it without an `ELSE`, as
`CASE WHEN ((total * 1) IS NULL) THEN 1.0 WHEN NOT ((total * 1) IS NULL) THEN 1.0 END`.
So what kept the value at exactly `1.0` was that the condition is an `IS NULL`,
which SQL never answers with `NULL`: neither arm could be skipped, and no third
value could come from the missing `ELSE`.

Over eligible sources on DuckDB — a double, a zero, an all-`NA` double, and an
integer — every share was `1` of type double.

**`.check_share_source = FALSE` relaxed nothing here.** With the flag set, the
local path still refused a character source — its type check did not read the
argument — and DuckDB still failed at collection. The flag suppressed the
converting-dialect verdict refusal alone, which is what it already did for the
ratio.
