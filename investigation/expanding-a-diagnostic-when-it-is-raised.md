# Expanding a cli template when the condition is raised

Investigated: 2026-08-19
Revised: 2026-08-19 — investigation/a-no-break-space-inside-the-assertion-itself.md

`investigation/retrieval-time-formatting-of-a-cli-diagnostic.md` established
that `cli::cli_abort()`'s retrieval-time formatting collapses a run of
whitespace inside an interpolated value, so a column the caller named `a  b` is
named `a b` in the refusal, and left the question of what to do about it to
#230. This note is the measuring that question asked for: whether any cli style
avoids it, whether `cli_abort()` can be told not to format at retrieval, and
what an alternative would cost.

Environment: R 4.6.1 (2026-06-24), cli 3.6.6, rlang 1.3.0, testthat 3.3.2, on
macOS.

## No inline style survives the retrieval pass

A `.margin_label` dimension named `bad  name`, through `cli_abort()`, in every
style ADR 0023's table names and several it does not:

| template | stored `$message` | `conditionMessage()` |
| --- | --- | --- |
| `{x}`, `{.field {x}}`, `{.emph {x}}` | `bad  name` | `bad name` |
| `{.var {x}}`, `{.code {x}}`, `{.arg {x}}` | `` `bad  name` `` | `` `bad name` `` |
| `{.val {x}}`, `{.str {x}}`, `{.q {x}}` | `"bad  name"` | `"bad name"` |
| `{.path {x}}`, `{.file {x}}` | `'bad  name'` | `'bad name'` |

Ten styles, one answer. The spelling reaches the condition intact in every case
and is rewritten on the way out, so the choice of style cannot avoid it.

## `cli_abort()` cannot be told to skip the retrieval pass

`cli::cli_abort(msg, use_cli_format = FALSE)` fails with

```text
formal argument "use_cli_format" matched by multiple actual arguments
```

because `cli_abort()` passes that argument to `rlang::abort()` itself.

`cli::format_message()` was measured as a pre-rendering alternative and does not
help: it is the block-level formatter, so it collapses exactly as retrieval
does. Only `cli::format_inline()` preserves the spelling, and it consults no
width.

## What expanding at raise time keeps and loses

The shape measured is `vapply(message, cli::format_inline, .envir = <caller>)`
followed by `rlang::abort()`, against a three-element message: a main line with
`{length(cols)}` and `{?s}`, an `i` bullet interpolating a 12-element column
vector, and an `i` bullet naming `bad  name`.

Kept: `{.arg}` and `{.var}` styling, `{?s}` agreeing with the quantity, cli's
serial `and` between vector elements, the `i` bullet markers, the
`marginplyr_error` class, and the blamed call. Rendered `Error in ...:` header
and `!` prefix are byte-identical to `cli_abort()`'s.

Lost: wrapping. The same condition read at `width = 80` and at `width = 40`
carries 2 newlines both times, and both are bullet boundaries. Under
`cli_abort()` the same message carries 3 newlines at 80 and more at 40.

A frame is gained in the backtrace unless `.frame` is passed: `rlang::abort()`'s
default resolves to one frame deeper than `cli_abort(.envir =)` did, which
leaves the raising call visible.

## Styling is fixed by the raising session

| `cli.num_colors` | escapes in stored `$message` | escapes in `conditionMessage()` |
| --- | --- | --- |
| raise-time expansion, 1 | no | no |
| raise-time expansion, 256 | **yes** | yes |
| `cli_abort()`, 1 | no | no |
| `cli_abort()`, 256 | no | yes |

`num_ansi_colors()` was 1 in a plain `Rscript` run, which is what `R CMD check`,
CI, and a rendered vignette get.

Which styles emit an escape at 256 colours is narrower than the table suggests.
Of ADR 0023's five, only `{.val}` does; `{.var}`, `{.arg}`, `{.code}`, and
`{.fun}` emit none, and neither does `{.str}`. `{.field}` and `{.emph}`, which
that table does not name, do.

## Which spellings survive, to the codepoint

Through the raise-time expansion, a `.margin_label` dimension name:

| spelling | result |
| --- | --- |
| two spaces, tab, carriage return | preserved |
| leading space, trailing space | preserved |
| ideographic space U+3000, thin space U+2009, zero-width space U+200B | preserved |
| brace, backtick, emoji | preserved |
| **newline U+000A** | **shown as U+0020** |
| **no-break space U+00A0** | **shown as U+0020** |

The two rewritten ones go in cli's glue pass, before any marginplyr code sees
the result. Every row is a codepoint dump of the quoted name rather than a
visual comparison: an intermediate reading of the no-break space during this
session reported it preserved, because the literal reached R as an ordinary
space through the shell, and only dumping the codepoints showed it.

## How long an unwrapped bullet gets

`cli::format_inline("Remove {.var {cols}} from it.")`, which is the shape of the
longest vector-bearing bullet in this package's corpus:

| elements | characters, short names | characters, `column_N` names |
| --- | --- | --- |
| 3 | 42 | 50 |
| 8 | 82 | 118 |
| 20 | 189 | 269 |
| 100 | — | 273 |
| 1000 | — | 275 |

`vec-trunc` is 20 by cli's default, which ADR 0023 adopted, so the length stops
tracking the caller's vector at twenty elements. It is not a ceiling: cli
appends `and N more`, and that count widens, so the same bullet grows about six
characters between 20 elements and 1000. What is bounded is the growth, not the
length.

## What the rendered site holds after the move

The site was rebuilt with `altdoc::render_docs(parallel = FALSE, freeze = FALSE)`
against the installed working tree. `docs/vignettes/get_started.html` carries
seven rendered `Error in ...:` blocks and **no wrap in any of them**, where the
same page built through `cli_abort()` wrapped all seven with two-space
continuations. The 96-character duplicate-grouping-set refusal is back on one
line.

The two markers `.github/scripts/verify-site.R` could only match with whitespace
normalized —
`` Add an empty `grouping_set()` to the `grouping_sets()` specification `` and
`which can be nested: data.frame, dtplyr_step` — match as raw `fixed = TRUE`
substrings again.

## An expectation that passed for a reason not established

While pinning the two rewritten spellings, an assertion written as

```r
expect_true(grepl(gsub("[\n ]", " ", name), message, fixed = TRUE))
```

passed inside `test_that()` for the no-break-space case although the same
expression, evaluated by `cat()` on the line above it in the same loop,
returned `FALSE`; a deliberately failing expectation added to the same file was
reported normally, so failures were not being swallowed. The cause was not
found. The assertion was rewritten to build the expected string with `chartr()`
over codepoints given as escapes, which is unambiguous, and both directions of
the pin were then mutation-checked: moving a preserved spelling into the
rewritten set fails the rewritten test, and moving a rewritten spelling into the
preserved set fails the preserved test.

## Revisions (2026-08-19)

The cause the section above could not find was established later the same day
in `investigation/a-no-break-space-inside-the-assertion-itself.md`. The two
evaluations were not of the same expression: the assertion's `gsub()` character
class in the test file held a raw U+00A0 — bytes `5B 5C 6E C2 A0 5D` — so the
`gsub()` rewrote the no-break space to an ordinary space and the expectation
passed because it was true, while the `cat()` one line above had been typed
with an ordinary space in its class and was evaluating a different expression.
The code block above cannot show this, because quoting the assertion is what
normalized the byte away; the spelling it renders, a newline and an ordinary
space, is true of the quotation and was false of the file. The successor note
holds the byte-level evidence and flips the anomaly in both directions on that
one byte pair. Nothing about the `chartr()` rewrite changes: it was adopted for
being unambiguous and is mutation-checked, and the finding vindicates rather
than replaces it.
