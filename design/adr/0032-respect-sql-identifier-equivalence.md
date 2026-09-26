---
status: accepted
---

# Respect SQL identifier equivalence at Margin boundaries

SQLite and DuckDB resolve identifiers that differ only in ASCII letter case as
the same name. A Margin result can therefore silently replace a value or group
by an input column instead of a package-created key if it treats those names
as distinct. Issue #697 establishes this rule for the verified live backends.

## Decision

When public columns must coexist in a SQLite or DuckDB Margin result, reject
two names with the same ASCII case-folded identifier during lazy construction.
The Package condition names both original spellings and advises renaming one.
The check includes grouping columns, requested identifiers, ordinary and share
outputs, and carried expansion columns. An unused source payload is excluded:
summaries may replace it as they ordinarily do.

Allocate every private SQL alias against the same equivalence rule and the
caller names available at that point. This keeps valid input accepted when a
package-created alias would otherwise shadow it. Preserve original public
spellings in diagnostics and results. No source-row read or extra evaluation
of ordinary summary or selection expressions is needed to compare names.

The rule folds ASCII `A` through `Z` only. Distinct non-ASCII names remain
distinct. Local and other unverified backends retain their existing name
semantics. ADR 0020 governs the construction-time query boundary; ADR 0031
governs SQLite's public-only typed result boundary.
