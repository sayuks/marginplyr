# Spell a caller's subject as the caller wrote it in a Package condition

A Package condition names a column, a value, or an argument the caller supplied
using the caller's own spelling. `abort_marginplyr()` therefore expands its cli
template when the condition is *raised*, through `cli::format_inline()` and
`rlang::abort()`, rather than leaving `cli::cli_abort()` to expand it when the
condition is *read*. The idiom every template is authored in does not change:
ADR 0023's style table, its `{?}` rule, its vector defaults, and its rule that
caller-derived text is an interpolated value all stand, and cli remains an
Import.

Two spellings survive neither route, and `?marginplyr` states them rather than
promising more than this keeps: a line break and a no-break space inside a name
are both shown as an ordinary space, because cli's glue pass rewrites them
before any marginplyr code sees the result.

## What retrieval-time formatting was doing

`cli_abort()` sets `use_cli_format`, so `conditionMessage()` formats the stored
message when it is read. That formatting collapses every run of whitespace —
and it does not distinguish the template from the values interpolated into it.
A `.margin_label` dimension the caller named `` `bad  name` `` was stored with
its two spaces and named `` `bad name` `` in the refusal.

This is the failure a refusal exists to prevent, run backwards: the reader is
sent looking for a column that does not exist in their data, by the sentence
telling them the column does not exist. `rlang::abort()`, which
`abort_marginplyr()` used before ADR 0023, showed the caller's bytes.

`CONTEXT.md` already held the value this violates, in *Condition context*: a
Margin verb owes the caller a context "written in the names and the expressions
the caller can act on", so "an argument is quoted as the caller spelled it".
That term scopes the value to an External condition's context because that is
where the problem was first noticed (ADR 0021, ADR 0022), not because a Package
condition was considered and excluded. The glossary now states it of *Package
condition* too, in that term rather than by widening *Condition context*, which
names a precise thing — the lines an External condition carries — and would be
blurred by carrying a second subject.

## Why the fix is a move in time rather than a change of style

Ten inline styles were measured against a two-space name. Every one of them —
`{.var}`, `{.code}`, `{.val}`, `{.arg}`, `{.field}`, `{.str}`, `{.q}`,
`{.path}`, `{.file}`, `{.emph}` — stored the spelling and showed it collapsed.
There is no style that survives, so nothing about ADR 0023's style table could
have been chosen differently to avoid this.

`cli_abort(use_cli_format = FALSE)` is not available either: `cli_abort()`
passes that argument itself, so supplying it is a duplicate-argument error. The
one remaining place to intervene is where the expansion happens, and
`cli::format_inline()` is the inline half of cli — it expands a template and
consults no width. Expanding there and handing the result to `rlang::abort()`
keeps the whole authoring idiom, because the template is the same template.

## What this costs

**A diagnostic no longer wraps to the reader's width.** The message is fixed at
the width it was written, which is no width at all: each element is the one line
it was authored as. This is what marginplyr shipped before ADR 0023 —
`rlang::abort()` wraps nothing — so it is a return rather than a regression, and
how long such a line can get is bounded by a decision ADR 0023 already made.
`vec-trunc` stays at cli's default of 20, so the length stops tracking the
caller's vector there: the longest vector-bearing bullet in this corpus measures
269 characters at twenty `column_N` names and 275 at a thousand, because what
grows past the truncation is cli's `and N more` count.

**Styling is fixed by the raising session rather than the reading one, for the
styles that emit any.** With `cli_abort()` the stored message is escape-free
and colour is applied on the way out; here a session with colour bakes the
escapes into `$message`, so a condition captured in a terminal and written to a
file later carries them.

The exposure is narrower than that reads, twice over. `num_ansi_colors()` is 1
in every non-interactive run, so nothing is frozen under `R CMD check`, in CI,
or in a rendered vignette. And of the styles the table above names, only
`{.val}` emits an escape at 256 colours at all — `{.var}`, `{.arg}`, `{.code}`,
and `{.fun}` emit none. It is accepted because the alternative is the sentence
in *What retrieval-time formatting was doing* above, and because this package
already treats a frozen escape as something to strip rather than something that
cannot happen (ADR 0022, #217).

## Consequences

ADR 0023's line conditions were written against wrapping that no longer
happens, and its own amendment records which of them go. One survives whole —
a part whose length the caller decides goes alone in an `i` bullet — and it
matters more here than it did there: without wrapping, that is the only thing
standing between a long vector and a very long line. So does the second
sentence of its third condition, that a marker is chosen from a run of
uninterpolated prose, which was never about wrapping: a marker quoting an
interpolated part varies with the data whatever the width.

`.github/scripts/verify-site.R` loses the whitespace normalization added with
ADR 0023's adoption. The two markers that needed it were both marginplyr's own
diagnostics, and neither wraps now. The External conditions the site also
renders do still wrap, but the markers quoting them are `Error in ...:` headers,
which cannot span a break.

The promise is pinned in both directions in `test-diagnostic-authoring.R`: the
spellings a refusal keeps — a run of spaces, a tab, a carriage return, leading
and trailing space, an ideographic and a thin space, a brace, a backtick — and
the two it cannot. Only the first list would pass a package that had quietly
stopped expanding at raise time; only the second would pass one that had lost
the promise entirely. A spelling moving between the lists is a change to
`?marginplyr` and fails here first.

Nothing about how a diagnostic is authored changes, so #223's phase 3 is
unaffected except in what it may cite: its per-pull-request criteria name ADR
0023's third condition, of which only the second sentence survives.

`abort_marginplyr_flat()` is untouched. The injection rule it exists for is a
property of the template, which is still glue-interpreted, so an assembled
string still has to reach cli as a value. Its invariant — one unnamed string —
holds for the same reason it did: `format_inline()` joins a longer vector with a
serial `and` and drops the names bullets are carried in.

The measurements are in
`investigation/expanding-a-diagnostic-when-it-is-raised.md`.

What this decision is about is the bytes of a subject surviving from the raise
to the read. It is not about an expression a static reading has already
normalized before any condition is built: ADR 0019's third amendment decides
that a nested position refuses `(f(region))` in the words `f(region)`, and
scopes this decision there rather than here, where the subject is a value cli
would otherwise collapse.

## Considered Options

**Accept the collapsing and document it** — the option #230 opened with, and the
cheap one. Rejected because the documentation it needs is a sentence widening
`?marginplyr`'s existing disclaimer from "the wording of any message" to the
names the caller passed in. That disclaimer exists so a caller matches on class
instead of grepping prose; stretching it to cover their own data says something
quite different, and nothing about a refusal is improved by being allowed to
misname its subject.

**Quote the subject through a style that survives** — #230's second option,
withdrawn on measurement. All ten styles collapse.

**Pre-render the whole message with `cli::format_message()`** — rejected. It is
the block-level formatter, so it collapses exactly as retrieval does; it only
moves *when* the wrap is chosen, not whether the spelling survives.

**Call `cli::format_inline()` at each of the eighty-three sites** — the option
ADR 0023 rejected, and still rejected, for the reason it gave: it is the same
boilerplate written eighty-three times and it removes the single point where the
injection rule can be gated. Doing it once inside the constructor is a different
shape, and is what this decides.
