---
status: accepted
---

# Check generated documentation before omitting heavy checks

For [#1292](https://github.com/sayuks/marginplyr/issues/1292), add unconditional
roxygen regeneration and committed-output consistency to both Review-ready
scopes, making six mandatory checks. Permit source scope for existing-file
explanatory text and formatting, including prose-only roxygen edits, only with
parsed-expression equality and an AI assessment of generated output and
source-text consumers. Other package effects or uncertainty require full.

This amends [ADR 0039](0039-run-review-ready-from-the-target-worktree.md):
manual generation can be forgotten, while file location and parsed expressions
alone cannot establish whether changed text affects verification. The measured
cost of [parsing](../../investigation/review-ready-parse-comparison-2026-11-05.md)
and [generation](../../investigation/review-ready-roxygen-generation-2026-11-05.md)
supports always checking generation consistency and requiring mechanical
expression evidence, rather than repeatedly running the complete suite for
qualifying explanatory edits or relying on AI judgment alone.

The [specification](../specs/review-ready-equivalent-source-edits.md) fixes the
evidence and comparison boundary. The runner and
[local checks](../agents/local-checks.md) adopt the six-check baseline and
evidence-backed scope exception together under #1292. Adoption does not
establish a package-check result for any later SHA.
