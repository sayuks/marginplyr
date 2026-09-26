# Respect SQL identifier equivalence when validating outputs and allocating internal names

Investigated: 2026-09-26
Status at preparation: proposed; publication awaited breakdown approval.

## What to build

**Priority:** P1

Prevent silent value replacement and incorrect aggregation caused by ASCII case-insensitive identifiers on the verified SQLite and DuckDB paths.

Adopt one explicit name-equivalence rule at these backend boundaries, with two outcomes:

1. Public columns that must coexist but have equivalent SQL identifiers are refused with a Package condition naming both original spellings and advising a rename.
2. A private alias that would collide with a caller's name is allocated differently. Valid input must keep its correct groups and values; it is not refused for a collision the package would introduce.

For public names, SQLite input g = a and v = 1 summarized as G under rollup(g) returns Grand total G = NA instead of 1. Other configurations substitute grouping strings or the value of a different summary. For private names, an unrelated input column named ..MARGINPLYR_KEY_1 with value WRONG causes generated ..marginplyr_key_1 to read that column: separate a/b groups collapse into one WRONG group with count 2. DuckDB's portable repeated-set path reproduces that corruption too.

Public-name refusal is the specification decision of this ticket, not a claim that an existing ADR already explicitly requires case-folded refusal. Record that public contract in the appropriate reference and ADR. Do not generalize SQLite/DuckDB behavior to every backend without evidence.

Evidence: [Investigation](https://github.com/sayuks/marginplyr/blob/81ca34a72e060ea7d5619e15849c52321d664319/investigation/bug-hunt-2026-09-26-edge-combinations.md), finding 5; probes `case` and `internal_case` in [executable probes](https://github.com/sayuks/marginplyr/blob/81ca34a72e060ea7d5619e15849c52321d664319/investigation/bug-hunt-2026-09-26-edge-combinations.R). ADR 0015 permits actionable Package conditions; ADRs 0020 and 0031 govern query and SQLite public-column boundaries.

## Acceptance criteria

- [ ] Public equivalent-name conflicts are refused during construction before any source-row read, with marginplyr_error, both original spellings, and an actionable rename remedy. No corrupt successful result is returned.
- [ ] Cover fixed keys, variable dimensions, ordinary outputs, statically named across outputs, share outputs, the requested identifier, and carried expansion columns when those names must coexist.
- [ ] Do not reject an unused source payload merely because an output reuses an equivalent name; ordinary summary replacement semantics still apply.
- [ ] An uppercase internal-prefix input such as ..MARGINPLYR_KEY_1 preserves a/b as separate groups with counts 1 and 1. Every generated SQL alias uses a collision check appropriate to its backend.
- [ ] Exercise SQLite and both native and portable DuckDB paths, including repeated sets with an identifier, one-set plans, text/missing labels, and all Margin-order choices.
- [ ] Use the verified ASCII identifier rule. Distinct non-ASCII names such as Ä and ä remain distinct; ordinary case-sensitive local names such as g and G remain supported.
- [ ] Inspect direct results and supported materializations for exact public names and intended values; preserve SQLite declared types and public-only schemas.
- [ ] Keep caller names intact in diagnostics and final results, preserve query laziness, and document the chosen SQL public-name refusal with regression coverage.
- [ ] Name validation does not add evaluations of ordinary .cols, .names, .fns, or .unpack expressions. Static names and runtime checks must preserve the existing ordinary-evaluation contract.

## Blocked by

None (can start immediately). Public validation and private alias allocation belong together because they must agree on the same backend identifier rule.
