# Refusing a character share source from the staged query itself

Investigated: 2026-09-05

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
