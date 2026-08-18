# How marginplyr's diagnostics render under rlang and cli

Investigated: 2026-08-19

#223 decided to re-author every shipped diagnostic in the cli idiom and asked
ADR 0023 to settle three things it left open: cli's vector-formatting defaults,
the inline style each subject gets, and where the short-main-plus-bullets rule
applies. Those are decisions, and the ADR owns them. This note records the
measurements they were taken against, because every one of them is a fact about
rlang, cli, testthat, or the rendered site rather than about marginplyr, and a
fact of that kind ages when the outside world moves rather than when `R/` does.

The measurement that reshaped the ticket is the first one below. #223 describes
the migration as a re-formatting whose cost is retrieval-time wrapping. It is
more than that: the diagnostics marginplyr shipped as of f724ea0 were not
wrapped at all, so the migration introduces wrapping where none existed, into
rendered vignettes and into byte-exact test pins alike.

Environment: R 4.6.1 (2026-06-24), cli 3.6.6, rlang 1.3.0, testthat 3.3.2, on
macOS. Every figure below was produced in that session, against the working
tree at f724ea0.

## rlang does not wrap a plain message; cli does

The subject is the duplicate-grouping-set refusal exactly as
`R/grouping-plan.R` built it, 96 characters:

```text
Duplicate grouping sets were produced at positions 3, 6. Use `.duplicates = "drop"` or `"keep"`.
```

At `width = 80`:

| call | newlines in `conditionMessage()` |
| --- | --- |
| `rlang::abort(msg)` | 0 |
| `cli::cli_abort(msg)` | 1 |

cli breaks it after `Use`:

```text
Duplicate grouping sets were produced at positions 3, 6. Use
`.duplicates = "drop"` or `"keep"`.
```

`abort_marginplyr()` reached `rlang::abort()` at f724ea0, so the left-hand
column is what every one of the 83 sites produced.

Bullets behave the same way, so the difference is not specific to a message
vector's unnamed first element. At `width = 80`, a main line of 30 words and an
`i` bullet of 30 words came back from `cli_abort()` as four rendered lines — the
main line broken once, the bullet broken once with its continuation indented by
two spaces. The same message vector through `rlang::abort()` came back as two
lines: the main line and the bullet, each whole, the only newline being the one
between them. rlang wraps nothing.

## What the rendered site held

`docs/vignettes/get_started.html`, as built before f724ea0, carried that same
96-character message on one line:

```text
Duplicate grouping sets were produced at positions 3, 6. Use `.duplicates = "drop"` or `"keep"`.
```

No ANSI escapes, and no HTML elements inside the sentence. That is what lets
`.github/scripts/verify-site.R` match
`"Duplicate grouping sets were produced at positions"` as a `fixed = TRUE`
substring.

Its marker table was read for how many such markers there are. Seven of
`get_started.html`'s quote a sentence this package authors. Two more quote the
`Error in ...:` header, which rlang builds from the condition's `call` rather
than from its message. `recipes.html`'s three quote errors raised by dplyr,
purrr, and a join, which are External conditions and not this package's text at
all.

Two things were looked for and not found: any `<span>` inside a rendered
diagnostic, and any escape sequence. So at that date the marker matching was
insensitive to cli's styling because there was no styling to be insensitive to,
not because the matching accounts for it.

## testthat fixes the width and the colours

Inside `test_that()`, `local_reproducible_output()` sets `getOption("width")` to
80 and `cli::num_ansi_colors()` to 1. `getOption("cli.width")` was `NULL`, so
cli falls back to `width`.

The consequence is the one #223's re-pinning strategy turns on:
`conditionMessage()` of a `cli_abort()` condition is deterministic inside the
test suite. Retrieval-time formatting varies with the session, but not between
two runs of the same test.

## cli's inline vector defaults

`cli::format_inline()`, cli 3.6.6, no options set:

| template | result |
| --- | --- |
| `{.code {v}}`, 2 elements | `` `a` and `b` `` |
| `{.code {v}}`, 3 elements | `` `a`, `b`, and `c` `` |
| `{.val {v}}`, 3 elements | `"a", "b", and "c"` |
| `{.var {v}}`, 3 elements | `` `a`, `b`, and `c` `` |
| `{.arg {v}}`, 3 elements | `` `a`, `b`, and `c` `` |
| `{.fun {v}}`, 3 elements | `` `a()`, `b()`, and `c()` `` |
| `{.or {.code {v}}}`, 3 elements | `` `a`, `b`, or `c` `` |
| `{.code {v}}`, 25 elements | 18 elements, `…`, then the last two |

`{.var}`, `{.arg}`, and `{.code}` produced identical bytes for a character
vector. They differ in what they mean to a reader of the source and in what a
future cli could style differently, not in what a caller saw on 2026-08-19.

The 25-element row is `vec_trunc`, whose default is 20: cli shows twenty of the
twenty-five, as eighteen from the front and two from the back.

Pluralization was measured on the same call:
`"column{?s} {.code {v}} {?is/are} gone"` gave
``column `a` is gone`` for one element and ``columns `a` and `b` are gone`` for
two.

## Neither `format_inline()` nor `pluralize()` wraps

At `width = 40` and `cli.width = 40`, `cli::format_inline()` returned a
30-word string with 0 newlines. `cli::pluralize()` behaved the same way. Only
the block-level entry points — `cli_abort()` among them — consult the width.

## `pluralize()` reproduces the shipped branch byte for byte

`report_branch_warnings()` (`R/conditions.R`) built its count line at f724ea0
with `sprintf()` and an `if`:

```r
sprintf(
  "%d further grouping %s raised this warning.",
  entry$count - 1L,
  if (entry$count == 2L) "set" else "sets"
)
```

`cli::pluralize("{n} further grouping set{?s} raised this warning.")` was
measured against both arms:

```text
n = 3  ->  3 further grouping sets raised this warning.
n = 1  ->  1 further grouping set raised this warning.
```

Identical to what the `sprintf()` form produces for the same counts. The two
were measured to agree at `n = 0` as well — both give `sets` — although the
shipped branch reaches that only through `entry$count == 1L`, which
`report_branch_warnings()` never emits.

## cli feature versions, and the floors already in the closure

From cli's own `NEWS.md`, as installed at 3.6.6:

- `format_inline()` and `cli_abort()` — cli 3.0.0.
- the `vec_sep` / `vec_last` / `vec_trunc` spellings — cli 3.4.0. The entry
  records that the older names still work.
- `{.var}` — present by cli 2.5.0, which is where `NEWS.md` first shows it in
  an example.

Declared floors read from the installed packages' own `DESCRIPTION` on
2026-08-19:

| package | its cli floor |
| --- | --- |
| dplyr | `cli (>= 3.6.2)` |
| dbplyr | `cli (>= 3.6.1)` |
| tidyselect | `cli (>= 3.3.0)` |

dplyr and tidyselect are marginplyr Imports, so the highest of these was
already in the hard dependency closure. The highest version any feature above
needs is 3.4.0.

## The corpus these apply to

Counted over `R/` at f724ea0: 83 `abort_marginplyr()` call sites, in eight
files, led by `share.R` (33), `grouping-plan.R` (14), and `margin-label.R` (10).
That matches #223's own inventory.

Three different conventions for joining a vector into a sentence were in use at
once:

- a `paste0()` wrapping each element in backticks with `collapse = ", "` — a
  bare comma, at more than twenty sites;
- `format_grouping_constructors()` in `R/grouping-plan.R` — a serial `", or "`;
- the `.duplicates` alternatives in the same file — `" or "` with no comma.

`R/conditions.R`'s `report_branch_warnings()` is a ninth diagnostic that
pluralizes a noun by suffixing it, beyond the eight
`tests/testthat/test-diagnostic-pluralization.R` records under #224. Its two
arms were pinned only as `expect_match(..., fixed = TRUE)` phrases in
`test-execution-conditions.R` and as one line of
`_snaps/execution-conditions.md`.

`cli` was absent from `optional_suggest_spec()` in
`tests/testthat/helper-optional-backends.R`, whose six entries were arrow,
duckdb, dtplyr, data.table, RSQLite, and DBI. Nothing in the test suite's
optional-package registry named it.

## What was not established

Whether a `cli_abort()` diagnostic rendered into a vignette acquires HTML
markup inside the sentence, and at what width quarto renders it. The site
measurement above establishes only what an *rlang* diagnostic produced, since
that is what f724ea0 shipped. The first phase-3 pull request that re-authors a
diagnostic quoted by a `verify-site.R` marker is where this gets answered; ADR
0023's rule that a marker is chosen from a run of uninterpolated prose is
written to survive either answer.
