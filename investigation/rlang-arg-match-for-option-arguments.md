# What rlang::arg_match() would change about an Option argument

Investigated: 2026-08-17

`match_margin_choice()` validates every Option argument in the package by hand:
an `identical()` test for the untouched formal, a membership test for a string,
and `abort_marginplyr()` for everything else. `rlang::arg_match()` and
`rlang::arg_match0()` solve the same problem and are what a tidyverse package
would ordinarily reach for, so the question was whether the helper should be
built on one of them.

This note records what each of the three accepts and refuses, measured rather
than read, and what a migration would have to carry. It does not record the
decision; the comment above `match_margin_choice()` in `R/margin-operation.R`
is authoritative for that.

Measured on R 4.6.1 (2026-06-24) with rlang 1.3.0. `DESCRIPTION` is
authoritative for the version constraint this package places on rlang; the
behaviour below was not re-measured against the floor it names, so a migration
would have to establish that the floor produces these diagnostics and not
merely that it exports the functions.

## What each one accepts

Vocabulary `c("none", "last", "first")` throughout, as `.sort`. `arg_match()`
was called on a formal whose default is that vector; `arg_match0()` and
`match_margin_choice()` were passed the vocabulary explicitly, which is the
shape this package needs, since validation happens in a helper several frames
below the verb whose formal it is.

| Input | `arg_match()` | `arg_match0()` | `match_margin_choice()` |
|---|---|---|---|
| untouched formal | `"none"` | `"none"` | `"none"` |
| `"last"` | `"last"` | `"last"` | `"last"` |
| `"f"` | refused | refused | refused |
| `"zzz"` | refused | refused | refused |
| `NULL` | refused | refused | refused |
| `NA_character_` | refused | refused | refused |
| `1L` | refused | refused | refused |
| `c("none", "last", "first")` written out | `"none"` | `"none"` | `"none"` |
| `c("first", "last", "none")` | `"first"` | `"first"` | **refused** |
| `c("last", "first")` | refused | refused | refused |
| `c("aaa", "bbb", "ccc")` | refused | refused | refused |

The three agree on every row but one. `arg_match()` and `arg_match0()` accept
any **permutation** of the vocabulary and return its first element;
`match_margin_choice()`'s `identical()` requires the order too, so it refuses a
permutation. Both rlang functions are looser than the helper on exactly the
input #210 was opened about.

## The diagnostics

`arg_match0()`, given an invalid string:

```text
`.sort` must be one of "none", "last", or "first", not "zzz".
```

and, where a listed value is close enough, a second line:

```text
`.sort` must be one of "none", "last", or "first", not "frist".
ℹ Did you mean "first"?
```

The suggestion fires for a one-character abbreviation too — `"f"` produces
`ℹ Did you mean "first"?` — which is a better answer to the input #110 removed
than naming the vocabulary alone.

`match_margin_choice()` answered every one of those with the same sentence:

```text
`.sort` must be one of "none", "last", "first".
```

Three differences follow from that, and none of them is about which sentence
reads better.

**The `NULL` diagnostic stops naming the vocabulary.** `arg_match0(NULL, ...)`
reports a type, not a vocabulary:

```text
`.sort` must be a string or character vector.
```

`arg_match()` words the same case as `` `.sort` must be a character vector, not
`NULL`. `` Either is a defensible diagnostic, and arguably more actionable for
a caller whose variable held nothing. Both contradict the sentence #144 put on
five help pages, which says an abbreviation and a `NULL` are *both* "errors
naming the values the argument accepts". A migration would have to special-case
`NULL` ahead of the rlang call or rewrite that documentation.

**A non-string reports through an internal name.** `arg_match0()` given a
same-length vector that is not a permutation:

```text
`arg` must be length 1 or a permutation of `c("none", "last", "first")`.
```

`arg_nm` is not applied on that path, so the caller is told about `arg`, which
is not a name in their call.

**The wording moves upstream.** `test-verb-argument-admission.R` builds the
expected message from the vocabulary and compares whole sentences, and the
comment there records why: a message enumerating one value more or fewer is a
different string, which is what holds each verb to its own vocabulary. Under
rlang that assertion compares against rlang's wording, so the test's precision
becomes a dependency on a package whose diagnostics are free to change between
releases.

## Keeping the Package condition

`arg_match0()` raises `rlang_error`, and `CONTEXT.md`'s *Package condition*
together with ADR 0015 require an error this package raises to carry
`marginplyr_error`. Re-raising works, and was measured:

```r
rlang::try_fetch(
  rlang::arg_match0(value, choices, arg_nm = arg_name, error_call = call),
  error = function(cnd) abort_marginplyr(c(cnd$message, cnd$body), call = call)
)
```

This produced `marginplyr_error/rlang_error/error/condition`, the verb as the
blamed call, and the `ℹ Did you mean` line intact. The class chain is unchanged
from what `abort_marginplyr()` already produces, because it calls
`rlang::abort()` itself.

No exported way was found to obtain the suggestion without raising: the
"Did you mean" line is built inside rlang's `stop_arg_match()`, which is not
exported, and no rlang function was found that returns a candidate for a
misspelled string. A package wanting the suggestion under its own message has
to raise, catch, and re-word, or compute the candidate itself — `utils::adist()`
being in base R and already available here.

## Blast radius inside this repository

Measured on 2026-08-17 against the tree at `14b668d`, the merge of #209:

- 11 lines across 7 test files assert the sentence, but the central one is
  `expected_vocabulary_message()` in `test-verb-argument-admission.R`, which
  every case in that file derives from.
- `grep` for `must be one of` found no occurrence under `tests/testthat/_snaps/`
  and none under `vignettes/`, so no snapshot and no rendered vignette carries
  the wording.
- `R CMD check`, the site build, and the release matrix reach it only through
  those tests.

The migration is therefore small to perform and cheap to revert. What it is not
is free: it trades a sentence this package owns for one it does not, and it
widens what an Option argument accepts, on the one input already being tracked
as a question of its own.
