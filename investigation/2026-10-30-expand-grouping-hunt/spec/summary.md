# Grouping specification closure and capture evidence

Investigated: 2026-10-30
Baseline: `28c449aa03fd5289916f0673e41c848cc126999e` (`main`)

This track established no new actionable Grouping specification defect. It
read the public constructor/inspection contracts, ADR 0008's evaluation
amendments, ADR 0026's ambiguity rule, and the October 29 expansion, follow-up,
deep, and systematic investigations. In particular, the qualified namespace
selection-retention and typed-classification repairs for #1038/#1039 were
present in the inspected baseline.

Run from the repository root:

```sh
Rscript investigation/2026-10-30-expand-grouping-hunt/spec/closure-selection-matrix.R
Rscript investigation/2026-10-30-expand-grouping-hunt/spec/deep-capture-retention.R
```

The closure matrix compared direct typed `tidyselect::eval_select()` against
`.by` and all five constructors through list-format inspection, for a local
frame and an immutable-root dtplyr input. Fifteen selector bodies crossed four
closure shapes (direct, default formal, nested helper, and dots forwarding)
and zero, one, or two pairs of parentheses around the helper head: 180
expressions and 2,160 consumer comparisons. The bodies included names-only
metadata reads, typed metadata reads, lexical values, local assignment,
string assembly, duplicate selections, numeric indices, empty selections,
missing columns, and renaming selections.

All 2,160 comparisons matched: 1,824 successful plan answers, 144 matching
External-condition classes, 48 required empty-dimension refusals, and 144
required renaming refusals. The first oracle searched for an incorrect
renaming message fragment and falsely reported those 144 expected refusals as
candidates. It was corrected to the actual public diagnostic, `Can't rename`,
and the complete matrix was rerun. The final script omitted an unused
internal-classifier diagnostic; its verdict used public API comparisons.

The focused capture set combined active namespace-operator bindings with
nested injected quosures and parentheses through depths 0, 1, 4, 16, and 64.
Both namespace operators, five parent constructors, local/dtplyr inputs, and
inspection/expansion consumers made 200 comparisons. Every result matched its
literal-column control. Every caller function ran once and every active
namespace operator was read once. Outer quosure environments deliberately
carried refusing namespace operators, so accidental loss of the inner captured
environment would have failed the probe. Adjacent logs retained the completed
outputs.

Small exploratory probes also checked external-vector dots promises,
parenthesized/computed helper heads, and special lexical names against direct
tidyselect. They established no additional candidate and were not added to the
matrix counts. Hand-forged non-quosure specification fields were not treated as
a new validation promise: earlier investigations and ADR 0015 had already
distinguished that unsupported construction.

These measurements did not enumerate arbitrary caller code or custom classes,
unbounded specification trees or capture depth, changing dependency versions,
or value-dependent derived dtplyr metadata. Prior investigations were context,
not new execution evidence. The measured zero differences established only the
listed fixtures and contracts.
