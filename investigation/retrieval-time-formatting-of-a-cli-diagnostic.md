# What happens to a cli diagnostic between raising it and reading it

Investigated: 2026-08-19

`investigation/diagnostic-wrapping-under-rlang-and-cli.md` established, earlier
the same day, that `rlang::abort()` wraps nothing and `cli::cli_abort()` wraps
at retrieval. It read `width` and `cli.width` for the width a condition is
rendered at, and left one question open: what a `cli_abort()` diagnostic looks
like once it reaches a rendered vignette.

This note was taken while #223's phase 2b moved all 83 `abort_marginplyr()`
sites onto `cli_abort()` at once, which is what made both answerable against
real diagnostics rather than against a constructed message. It corrects the
first reading and answers the second, and it records a third thing neither note
looked for: wrapping is not all that retrieval-time formatting does.

Environment: R 4.6.1 (2026-06-24), cli 3.6.6, rlang 1.3.0, testthat 3.3.2,
quarto 1.9.38, altdoc 0.7.3, on macOS.

## `cli.condition_width` is what governs a condition, not `width`

`testthat::local_reproducible_output()` sets `cli.condition_width` to `Inf`, and
rlang consults it ahead of `cli.width` and `width`. So inside `test_that()` a
condition is not wrapped at all, whatever `width` says.

The correction was already written down in this repository before it was
measured: `collect_warnings_rendered()` in
`tests/testthat/test-execution-conditions.R` states it, and sets both options
together for exactly that reason.

Measured through a two-frame helper matching `abort_marginplyr()`'s
`.envir = rlang::caller_env()`, against the 140-character NA-level refusal that
`tests/testthat/test-diagnostic-pluralization.R` pins:

| context | newlines in `conditionMessage()` |
| --- | --- |
| top level, `width = 80` | 1 |
| inside `test_that()` | 0 |

A message vector was measured beside it — a main line plus one `i` bullet
carrying twelve column names — and behaved the same way: 3 newlines at the top
level, 1 inside `test_that()`. That one is the break between the main line and
the bullet rather than a wrap.

So the earlier note's conclusion — that `conditionMessage()` is deterministic
inside the suite — survives, and is stronger than it was stated. The suite does
not see a narrower deterministic wrap. It sees no wrap.

Two consequences were observed rather than argued. All 83 sites moved to
`cli_abort()` in one commit and the suite stayed green with no pin edited,
including the 26 byte-exact ones. And a phrase-level `expect_error()` or
`expect_match()` pin cannot be split by a wrap that does not happen — the one
newline such a pin can still meet is the structural main-line/bullet break,
which normalizing would not rescue either, since the bullet marker sits in the
gap: `"A refusal.\ni A bullet."` normalizes to `"A refusal. i A bullet."`, and a
pin reading `"A refusal. A bullet."` misses whichever way it is matched.

## Retrieval also collapses whitespace inside an interpolated value

| stage | `"a\tb\nc  d"` interpolated as a value |
| --- | --- |
| `cli::format_inline("{x}")` | double space preserved |
| the condition's stored `$message` | `a\tb c  d` |
| `conditionMessage()` | `a b c d` |

A tab and a newline go at glue time; a run of spaces goes at retrieval. Against
the shipped unknown-dimension refusal, a `.margin_label` dimension named
`` `bad  name` `` is stored with its two spaces and shown with one.
`rlang::abort()` showed the caller's bytes.

`cli::format_inline()` takes `keep_whitespace`. `cli::cli_abort()` was checked
for an equivalent and has none — and the collapsing happens in rlang's
retrieval formatting rather than in the interpolation, so that argument would
not reach it in any case.

## cli breaks at whitespace, not inside a token

At `width = 30`, a 68-character absolute path inside a diagnostic stayed whole
on a line of its own; a 68-character identifier with no spaces in it did the
same. Only the space before them moved.

This was looked for on behalf of `.github/scripts/verify-site.R`, whose
build-machine-path and forbidden-string scans read the page unnormalized. Every
pattern written out in that script is whitespace-free, so a wrap cannot split
one. The `\Q<home>\E` pattern it builds from the checking machine's own home
directory is the exception, and carries a space only on a machine whose home
directory does.

## What the rendered site holds once every site signals through cli

The site was rebuilt with `altdoc::render_docs(parallel = FALSE, freeze = FALSE)`
against the installed working tree. `docs/vignettes/get_started.html` read back:

- Seven rendered `Error in ...:` blocks, all of them wrapped, with each
  continuation indented by two spaces. The 96-character duplicate-grouping-set
  refusal reached the page as two lines, where the build measured before the
  switch carried it as one.
- No `<span>` inside any of the seven, and no escape sequence anywhere on the
  page. So the styling question the earlier note left open stays moot for the
  reason it was already moot: there is no styling, rather than matching that
  accounts for one.

Two of the fourteen `verify-site.R` markers quoting a diagnostic stopped
matching as raw `fixed = TRUE` substrings, and match only with whitespace
normalized on both sides:

```text
Add an empty `grouping_set()` to the `grouping_sets()` specification
which can be nested: data.frame, dtplyr_step
```

Both span a break in the rebuilt page. Without the normalization the `altdoc`
job fails on those two.
