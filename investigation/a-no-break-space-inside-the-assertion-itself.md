# A no-break space inside the assertion itself

Investigated: 2026-08-19

`investigation/expanding-a-diagnostic-when-it-is-raised.md` closed with an
expectation that passed for a reason not established: inside `test_that()`,

```r
expect_true(grepl(gsub("[\n ]", " ", name), message, fixed = TRUE))
```

passed for a `name` holding a literal U+00A0, although `gsub()` was read as
leaving U+00A0 alone — and although a `cat()` evaluating what looked like the
same expression, one line above it in the same loop iteration, printed `FALSE`.
This note establishes the cause. Every mechanism the handoff suspected —
`quasi_label()`'s capture, the C collation `local_reproducible_output()` sets, a
local `message` binding — turned out to be uninvolved. The two expressions were
not the same expression.

This note never spells the decisive codepoint as a raw byte, because the
finding is that a raw U+00A0 does not survive being quoted, displayed, or
re-typed. Every load-bearing string appears here as a hex dump or as a `\u`
escape; the one place a snippet above shows the pattern as `[\n ]`, that
spelling is itself the illusion under investigation.

Environment: R 4.6.1 (2026-06-24), testthat 3.3.2, cli 3.6.6, rlang 1.3.0,
macOS, C.UTF-8 locale, repository at `25827da`. The two files the anomaly was
preserved in sat at `/tmp/marginplyr-handoff-230/repro/` on this date; the
decisive bytes are quoted below so nothing here depends on that directory
outliving the day.

## The two expressions differed by one byte pair

Hex dumps of the preserved instrumented file, which is the file the anomaly was
observed in. The `cat()` line, 309, spelled its `gsub()` character class as
`5B 5C 6E 20 5D` — `[`, `\`, `n`, U+0020, `]`:

```text
00000080  3d 22 2c 20 67 72 65 70  6c 28 67 73 75 62 28 22  |=", grepl(gsub("|
00000090  5b 5c 6e 20 5d 22 2c 20  22 20 22 2c 20 6e 61 6d  |[\n ]", " ", nam|
```

The `expect_true()` line, 311, spelled the same-looking class as
`5B 5C 6E C2 A0 5D` — `[`, `\`, `n`, **U+00A0**, `]`:

```text
00000010  67 72 65 70 6c 28 67 73  75 62 28 22 5b 5c 6e c2  |grepl(gsub("[\n.|
00000020  a0 5d 22 2c 20 22 20 22  2c 20 6e 61 6d 65 29 2c  |.]", " ", name),|
```

The pre-instrumentation file — the version whose assertion passed before any
`cat()` was added — carried the same `5B 5C 6E C2 A0 5D` at its line 310. So
the byte was in the assertion as first written, not introduced by the
instrumentation; how it got into the class was not established, though the
`name` literal five lines above it held the same raw pair and is the obvious
neighbour to have been copied from.

## The expectation was true

With U+00A0 inside the class, `gsub()` rewrote the no-break space to an
ordinary space, so the searched-for string matched the message exactly as the
test intended. Verified at the top level with every codepoint given as an
escape:

```r
name <- "a\u00a0no-break space"
msg <- "quotes \"a no-break space\" here"                 # U+0020 throughout
grepl(gsub("[\n\u00a0]", " ", name), msg, fixed = TRUE)   # TRUE
grepl(gsub("[\n ]", " ", name), msg, fixed = TRUE)        # FALSE
sprintf("%X", utf8ToInt(gsub("[\n\u00a0]", " ", name)))
# "61" "20" "6E" "6F" "2D" "62" "72" "65" "61" "6B" "20" "73" "70" "61" "63" "65"
```

The newline iteration was unaffected either way, since both spellings of the
class contain `\n`. Nothing was wrong and nothing was swallowed: the
expectation passed because it was true, and the `cat()` printed `FALSE` because
it evaluated a different expression whose class held U+0020.

## Both directions flipped on that byte, with no marginplyr involved

A minimal file — `test_that()`, the two-name loop, a hard-coded `message`, no
package code — behaved as the bytes say and nothing else:

| assertion class | `cat()` class | `cat()` printed | suite |
| --- | --- | --- | --- |
| U+0020 | U+0020 | `FALSE` | FAIL 1 at the `expect_true()` |
| U+00A0 | U+0020 | `FALSE` | PASS — the anomaly, in full |
| U+00A0 | U+00A0 | `TRUE` | PASS — agreement again |

And in the original: re-running the preserved instrumented file against
`25827da` reproduced the anomaly exactly (`gsub= FALSE`, then
`FAIL 0 | PASS 36`), and changing only line 311's `C2 A0` to `20` — one byte
pair, nothing else — made it fail at line 311 with `FAIL 1 | PASS 35`. Every
element of the mystery is load-bearing on that byte and on nothing besides it.

The first minimal attempt is itself part of the evidence. Re-typing the file
from the rendered text produced the U+0020/U+0020 row — a file in which the
anomaly does not exist — because re-typing is exactly the normalization that
manufactured the contradiction in the first place. The anomaly reproduces
outside the original file if and only if the assertion's actual bytes are
copied, not its appearance.

## How the illusion propagated

Every rendering of the assertion normalized the byte to an ordinary space, so
every reading of "the same expression" was a reading of a different one:

- an editor and a terminal display U+00A0 as an indistinguishable blank;
- the `cat()` instrumentation was typed from that display, so its class came
  out U+0020 — one expression became two;
- the handoff quoting the assertion carried `5B 5C 6E 20 5D` at the quoted
  line, so its premise "the class holds a newline and an ordinary space,
  nothing else" was true of the quotation and false of the file;
- the predecessor note's own code block quoting the assertion carried
  `5B 5C 6E 20 5D` the same way.

The session that hit this had itself already recorded the hazard from the other
side: the predecessor note's codepoint-survival table notes that an
intermediate reading reported U+00A0 preserved because the literal reached R as
an ordinary space *through the shell*. The same normalization, applied to the
assertion's source instead of to a value, is the whole of this anomaly. The
runtime codepoint dumps that session took were of `name` and `message` — the
data — and never of the pattern literal, which is where the byte sat.

## What this leaves standing

The `gsub()` form asserted exactly the right thing all along — both rewritten
spellings mapped to the space the refusal shows — but it asserted it through a
byte that no display, quotation, or re-typing preserves, which is how it read
as an assertion that could not pass. The `chartr()` form that replaced it
builds the same expected string from codepoints spelled as escapes, and it was
mutation-checked in both directions; it stands on those merits, and this
finding is its vindication, not a reason to touch it. The shipped suite and
`R/`, `tests/`, `vignettes/`, `man/`, and `design/` held no raw `C2 A0` on
this date, so the escape-only idiom the shipped test uses is already the
practice.
