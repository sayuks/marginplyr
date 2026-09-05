# Refusing a character share source from the staged query itself

Investigated: 2026-09-05

`investigation/share-source-eligibility-on-coercing-dialects.md` (2026-08-16)
established that a dialect can be asked whether it converts a non-numeric value
to a number, and that the answer is a property of the dialect. #429 then found
that the question that probe asks and the operations the staged share performs
are two different dialect behaviours, so DuckDB calculated a Parent share from
a character source while classified as refusing.

This note measures the third option #429 enumerated: changing the staged share
so that a refusing dialect produces the refusal the contract promises. It
establishes that such an expression exists, that it is one substitution rather
than an addition, and what it costs the existing suite.

Measured on 2026-09-05 against R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0, duckdb
1.5.5, RPostgres 1.4.10 with PostgreSQL 17.11 (Homebrew), RSQLite 3.53.3, and
bit64 4.8.6, with marginplyr loaded from the working tree at `1c126b2` via
`pkgload::load_all()`. Scripts were throwaway; commands are quoted inline.

## What the staged share applied to its source

The expression built for a lazy Parent share divided two `as.double()` casts
and compared the denominator with `0`. dbplyr rendered that on DuckDB as:

```sql
CASE WHEN (... OR "..marginplyr_denominator_of_total_1" = 0.0) THEN NULL
     ELSE (TRY_CAST(total AS DOUBLE) / TRY_CAST("...denominator..." AS DOUBLE)) END
```

Both operations were measured against DuckDB directly:

| SQL | Result |
|---|---|
| `TRY_CAST('x' AS DOUBLE)` | `NA`, no error |
| `TRY_CAST('1' AS DOUBLE)` | `1` |
| `'x' = 0.0` | Conversion Error |
| `'1' = 0.0` | `FALSE` |
| `SUM('x')` | Binder Error |
| `SUM('1')` | Binder Error |

`TRY_CAST` raised for nothing, so the error a non-numeric source produced came
entirely from the `= 0.0` comparison. `SUM`'s refusal held whatever the string
contained; the comparison's refusal did not. The probe therefore measured a
value-independent behaviour while the staged query depended on a
value-dependent one.

## Which operations a dialect refuses for a character column

Each expression was run over three one-column tables — a `DOUBLE`, a `VARCHAR`
holding `'1','2','3'`, and a `VARCHAR` holding `'n','m','o'` — on DuckDB and on
PostgreSQL. The wanted shape is: the numeric column answers, and both character
columns raise.

| SQL | DuckDB | PostgreSQL |
|---|---|---|
| `c + 0`, `c * 1`, `c / 1`, `c - c`, `-c` | refuses character | refuses character |
| `abs(c)`, `sqrt(c)`, `round(c)`, `floor(c)`, `ceil(c)`, `ln(c)`, `exp(c)`, `sign(c)` | refuses character | refuses character |
| `greatest(c, 0)`, `least(c, 0)`, `coalesce(c, 0)` | refuses character | refuses character |
| `c > 0` | refuses character | refuses character |
| `CASE WHEN c > 0 THEN 1 ELSE 0 END` | refuses character | refuses character |
| `c = 0` | **accepts `'1'`**, raises on `'n'` | refuses character |
| `nullif(c, 0)`, `c IS NOT DISTINCT FROM 0` | **accepts `'1'`**, raises on `'n'` | refuses character |
| `CAST(c AS DOUBLE PRECISION)` | **accepts `'1'`**, raises on `'n'` | **accepts `'1'`**, raises on `'n'` |

The two operations the staged share used were the only two of the twenty-four
measured that DuckDB accepted for a numeric-looking character column. Arithmetic
and scalar-function binding on DuckDB refused a `VARCHAR` argument whatever it
held; only the equality operator and the cast coerced it.

## A substitution, not an addition

Four dplyr-level forms were run through dbplyr on DuckDB, PostgreSQL, and
RSQLite, over a `DOUBLE` pair, an `INTEGER` pair, and the two `VARCHAR` pairs.
`x` is the numerator and `y` the denominator; the wanted result is `0.5` for
both numeric pairs and an error for both character pairs.

| form | DuckDB | PostgreSQL | RSQLite |
|---|---|---|---|
| `as.double(x) / as.double(y)` | character passes | character passes | character passes |
| `(x * 1) / (y * 1)` | wanted | wanted | character passes |
| `(x + 0) / (y + 0)` | wanted | wanted | character passes |
| `as.double(x * 1) / as.double(y * 1)` | wanted | wanted | character passes |
| `x / y` | wanted | integer pair gave `0` | integer pair gave `0` |

dbplyr rendered `x * 1` as `x * 1.0` on all three dialects, which is what
forced the numeric binding. The bare division is not usable: PostgreSQL and
RSQLite performed integer division on the `INTEGER` pair, which is why the
casts were there.

RSQLite let a character source through under every form, which is the expected
result rather than a gap: SQLite is a converting dialect, so a share over one is
refused before any query is staged.

`(x * 1) / (y * 1)` therefore replaces the two casts rather than adding a term
beside them. #429's third option was described as costing "an expression to
every share on every dialect"; what was measured is a substitution of equal
length.

## End to end, with the substitution applied

`R/share.R`'s share expression was edited to the `mul1` form and the working
tree reloaded. Each call used `.check_share_source = FALSE`, so the dialect
check was bypassed and what executed was the database's own answer.

| case | DuckDB | PostgreSQL |
|---|---|---|
| character source, `'1','2','3'`, Parent share | raised | raised |
| character source, `'n','m','o'`, Parent share | raised | raised |
| `INTEGER` source, Parent share | `0.5, 0.5, 1` | `0.5, 0.5, 1` |
| `INTEGER` source, Total share | `0.5, 0.5, 1` | `0.5, 0.5, 1` |

The local data-frame path was unchanged, as expected: it reaches the rule
through `wrap_share_sources()` rather than through the staged expression.

## What the substitution cost the suite

`testthat::test_local()` was run over the whole suite with `NOT_CRAN=true` and
the edit in place. One assertion failed:

```
test-share-backends.R:836 — fallback simulators render portable share SQL
expect_match(sql, "(CAST|CDBL)\\(", info = simulator)
```

That assertion reads the rendered SQL for a cast, which the substitution
removes. It is a statement about the implementation rather than about a
behaviour, and no other test in the suite changed result. The working tree was
restored afterwards; nothing here was committed as a change to `R/`.

## What this establishes

An expression exists that makes both DuckDB and PostgreSQL refuse a character
share source from the staged query itself, value-independently, while leaving
integer and double sources calculating as before. It is a substitution for the
two casts, it costs one assertion in the existing suite, and it does not
require reclassifying any dialect.

## What was not measured

- Any dialect other than DuckDB, PostgreSQL, and SQLite. The generic `sql`
  kind, and the simulators the fallback test iterates, were rendered but not
  executed.
- Whether `x * 1` changes the result type a backend returns for a source of a
  type other than the integer and double pairs run here — a `NUMERIC`/`DECIMAL`
  column in particular.
- Whether the `= 0` comparison should change too. Under the substitution the
  division refuses at binding, so the whole statement fails before the
  comparison executes; whether the comparison should refuse on its own was not
  established.
- Arrow, dtplyr, and data.table inputs, which do not reach the staged
  expression at all.
