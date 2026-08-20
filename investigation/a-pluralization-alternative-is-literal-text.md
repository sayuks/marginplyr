# A pluralization alternative is literal text

Investigated: 2026-08-20

`{?a/b}` picks between two spellings, and #223's phase 3 needed to know whether
the spelling it picks is re-read as a cli template before it could decide the
shape of `R/margin-label.R`'s collision refusal. That refusal has two arms —
one naming the single colliding label, which has to be interpolated, and one
naming the words `Margin labels`, which does not — chosen by a count. If an
alternative were re-read, the two arms would be one template with a `{?}` in
it, and the R branch that chooses between them could go, which is what ADR
0023's rule that no R branch spells a noun would prefer.

An alternative is not re-read. It is emitted as the bytes it was written as, so
no interpolation can happen inside the arm `{?}` picks, and a subject that
differs between arms cannot be written in one template.

Environment: R 4.6.1 (2026-06-24), cli 3.6.6, rlang 1.3.0, macOS, C.UTF-8
locale, repository at `5e29006`.

## What was measured

Each row is `cli::format_inline(<template>)` with `lab <- "All"` bound in the
evaluation frame, at both quantities. The first two rows are the question; the
last two are the controls, showing that the same call interpolates and
inflects normally everywhere else in the template.

| template | `n = 1` | `n = 2` |
| --- | --- | --- |
| `{cli::qty(n)}{?{.val {lab}}/Margin labels}` | `{.val {lab}}` | `Margin labels` |
| `{cli::qty(n)}{?{lab}/Margin labels}` | `{lab}` | `Margin labels` |
| `{cli::qty(n)}{?is/are} already` | `is already` | `are already` |
| `{cli::qty(n)}column{?s}` | `column` | `columns` |

The singular arm comes back with its braces intact in both of the first two
rows. Nothing is raised — no `Could not evaluate cli {} expression`, no warning
— so a template written this way would ship the brace text to a reader rather
than failing where an author could see it. `cli::pluralize()` was measured on
the second row and answered `{lab}` as well, so this is the pluralization pass
rather than something `format_inline()` does on top of it.

The plural arm interpolating nothing is not evidence either way here, `Margin
labels` holding no `{}`.

## What cli documents

`?cli::pluralization` presents `{?}` only over word forms — `file{?s}`,
`director{?y/ies}`, `{?is/are}` — and `?cli::pluralize` adds nothing about the
content of an alternative. So the behaviour above is established by measurement
and not by a documented promise, which is the reason to record it here rather
than to cite a page.

Two consequences follow for a reader deciding how far to trust it. It is not a
contract cli has made, so it can move without cli considering it a breaking
change; and the failure mode if it does move is loud in the other direction —
a template whose alternatives suddenly interpolated would evaluate `{lab}` in
the raising frame, which is exactly what the arms of this refusal do not want
in the plural case. Nothing in marginplyr writes an alternative containing
braces, so nothing here would break; what would change is that a shorter
spelling became available.

## What this decided

ADR 0023's third amendment, in the same commit. The branch on a count stays,
because it chooses a subject rather than inflecting a noun, and every noun that
does inflect still goes through `{?}` — `column{?s}` behind `cli::qty()` in
each of the refusal's four arms.
