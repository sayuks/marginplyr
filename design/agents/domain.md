# Domain Docs

How the engineering skills should consume this repo's domain documentation when exploring the codebase.

## Before exploring, read these

- **`CONTEXT.md`** at the repo root.
- **`design/adr/`** — read ADRs that touch the area about to be changed.

If either location does not exist, proceed silently. Do not suggest creating domain documentation upfront. The `/domain-modeling` skill, normally reached through `/grill-with-docs` or `/improve-codebase-architecture`, creates or updates it when terminology or decisions are actually resolved.

## File structure

This is a single-context repository:

```text
/
├── CONTEXT.md
├── design/
│   └── adr/
│       ├── 0001-keep-margin-operations-verb-neutral.md
│       └── ...
├── R/
└── tests/
```

## Use the glossary's vocabulary

When output names a domain concept—in an issue title, specification, design proposal, hypothesis, or test name—use the term defined in `CONTEXT.md`.

Do not drift to synonyms that the glossary explicitly avoids.

If a needed concept is absent from the glossary, reconsider whether the term belongs to the project. If it represents a real domain gap, note it for `/domain-modeling`.

## Flag ADR conflicts

If proposed work contradicts an existing ADR, surface the conflict explicitly instead of silently overriding the decision.

For example:

> Contradicts ADR-0007 — but worth reopening because…
