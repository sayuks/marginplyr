# Expansion and Grouping specification exhaustive boundary hunt

Investigated: 2026-11-06
Baseline: `952a0d9b18fd3ff4631f153432f6024ad62fcac2` (freshly fetched `main`)
Scope: investigation evidence and implementation tickets; no fixes

A dedicated worktree was created from main. Four tracks investigated local
expansion, Grouping specification capture/selection and family semantics,
Arrow/dtplyr lazy execution, and live SQLite/DuckDB composition. The November
3 [composition hunt](expand-grouping-greedy-hunt-2026-11-03.md), November 4
[boundary evidence](2026-11-04-expand-grouping/README.md) and
[invariants evidence](2026-11-04-expand-grouping-invariants/README.md), and
November 5 [greedy](2026-11-05-expand-grouping-greedy/README.md) and
[adversarial](2026-11-05-expand-grouping-adversarial/README.md) investigations
supplied prior coverage and exclusions. Their old measurements were not
counted as new executions. Source and domain-contract readings supplied the
contracts; ordinary backend operations and independent finite oracles supplied
the comparisons.

## Findings and ticket boundaries

| Finding | Disposition | Independently verifiable behavior |
| --- | --- | --- |
| L1 | New [#1364](https://github.com/sayuks/marginplyr/issues/1364), P2 `bug`, `ready-for-agent` | Accept representable typed-missing local list-array dimensions. |
| D1 | New [#1366](https://github.com/sayuks/marginplyr/issues/1366), P2 `bug`, `ready-for-agent` | Acquire faithful metadata for deterministic empty dtplyr projections. |
| G1 | Existing parallel [#1363](https://github.com/sayuks/marginplyr/issues/1363), P2 `bug`, `ready-for-agent` | Resolve stringr pattern consumption with caller vector-proxy dispatch against typed Grouping metadata. |

`to-tickets` produced two new independent implementation slices, with no
blockers or prerequisite refactoring ticket. The user delegated granularity
and publication, so the skill's breakdown quiz was omitted. G1 was independently
found here, but a publication refresh found the matching concurrent issue.
Its full body and comments were read; this hunt reused it and supplied
additional evidence rather than creating a duplicate. No existing issue was
closed or had its specification changed. Repairs were left to those tickets.

### L1: valid NULL-cell array missing rows were refused

A one-row tibble held a one-cell list matrix and an integer payload. A
row-sized `vec_init()` value contained a NULL cell. Ordinary mutation and
`union_all()` preserved both row copies, exact array shape and payload values.
Expansion with a NULL Margin label instead raised the Package condition that
the typed-missing label could not be represented. Included-only and payload
controls passed. The array component validator used scalar `is.na()` for a
NULL list cell, which was false even though the row prototype was missing.

The [local evidence](2026-11-06-expand-grouping-hunt/local/README.md)
recorded 216 false refusals over empty/one/three-row sources, matrix/rank-three
shapes, both typed-missing label spellings, all Margin orders, and direct,
packed and nested-packed dimensions. Zero-width, fixed-key and payload
controls passed. A separate rank-three witness with different values and an
actual NULL source cell confirmed the boundary. Shared validation also refused
all four Margin verbs; that observation did not establish an independent
complete summary/nesting oracle. Closed #1069/#1079 repaired classed array
recursion and auxiliary proxy labels, leaving this different list-cell
missingness case uncovered.

### D1: a literal empty projection was treated as unknown metadata

An immutable dtplyr root followed by an empty selection had an unambiguous
empty schema, represented by literal `0L` in the backend projection. Expansion
and typed inspection refused metadata acquisition. Explicit collection then
expansion, a direct empty root, and equivalent projections after ordering or
caching succeeded. The expected identifier was an empty integer vector; the
columnless data.table's existing zero-row boundary was retained.

The [lazy evidence](2026-11-06-expand-grouping-hunt/lazy/README.md)
recorded 90 false refusals and 90 accepted equivalent representations, with
180 additional direct-empty-root/name-only-inspection controls. No upstream
source predicate or empty-selection predicate ran, and caller data stayed
unchanged. An absent-name selector over a different two-column source supplied
an independent witness. ADR 0020/#625 required faithful metadata or safe
refusal for unknown/value-dependent operations; this fixture had a known
schema and required no source-row evaluation. #184's columnless-row limitation
and #1359's caller bracket-method boundary were separate.

### G1: pattern proxy methods saw integer name placeholders

A standard stringr pattern with an ordinary `vec_proxy.*` S3 method worked
under direct typed tidyselect and the identity-wrapped Grouping selection.
Unwrapped selection ran the method against integer name placeholders, causing
a false caller diagnostic or a natural recycling error. The typed path returned
valid unchanged pattern storage and size. Closed #1344/#1352 had repaired
length dispatch in pattern consumption; vector-proxy consumption remained
outside that classifier check.

The [specification evidence](2026-11-06-expand-grouping-hunt/spec/README.md)
recorded 360 false refusals, 360 typed-wrapper successes, six direct typed
references and three ordinary method-free derived-input controls. It crossed
fixed/regex/collation patterns, exact/inherited methods, local/dtplyr roots,
five consumers and six selection positions. An independent inherited-method
regex expansion also reproduced. Parallel #1363's separate 72-case matrix was
not added to this hunt's measurements or treated as another unique defect.

## Additional measured coverage and exclusions

| Probe | Executed observations | Result and limits |
| --- | ---: | --- |
| Local list-array matrix | 1,944 | 216 L1 failures, 756 exact matches; 972 ordinary list-of-array construction exclusions. |
| Additional local temporal/geography/expression families | 2,430 | 1,983 exact matches, 168 documented refusals, 135 fixture exclusions and 144 ordinary-control failures; no remaining package-only difference. |
| Grouping selector/family oracle | 1,614 | 1,576 exact ordered-family/set-ID matches and 38 required empty rollup/cube refusals; 31 other selectors refused by direct tidyselect were excluded. |
| Arrow/dtplyr row-copy oracle | 720 | All values/types/schema/multiplicities matched independently enumerated sets; row alignment did not independently prove requested Margin order. |
| Live SQL composition oracle | 2,016 | 1,008 SQLite and 924 DuckDB matches; 84 DuckDB bare-query CTE errors also occurred in ordinary dbplyr controls. Public schema, multiplicities and visible physical order keys were compared. |

The [SQL record](2026-11-06-expand-grouping-hunt/sql/README.md) retained the
CTE exclusions and nonfinite-value controls. Track records described corrected
oracle mistakes and final complete reruns; initial partial runs were not added
to the counts. Local ordinary failures included unsupported s2 ordering and zoo
combination with overlapping indices. No arbitrary attribute restoration,
warning-identity claim, unbounded Grouping-family proof or general performance
claim followed from these finite comparisons.

## Main freshness, duplicate searches and replay

A second main fetch before ticket publication still named the baseline SHA.
All three [independent witnesses](2026-11-06-expand-grouping-hunt/independent/README.md)
were rerun in fresh R processes against a separate `git archive` export of
that SHA. Their successful controls passed and each desired-contract assertion
intentionally exited one. These were unfixed-defect confirmations, not failing
package regression tests added by this PR.

The track notes retained all-state issue and relevant PR search terms and prior
report dispositions. The coordinator refreshed exact `"list array"`,
`"empty projection"`, and `"vec_proxy" "pattern"` queries for both issues
and PRs before publication. Initial exact queries returned no matches; a later
recent-issue/pattern refresh found #1363. Its full body and empty comments
matched G1. Full #1069, #625, #1344 and #1359 discussions supplied the other
boundary comparisons. New tickets carried the baseline, environment,
expected/observed behavior, controls, acceptance criteria and no blockers.

Reproductions, CSVs and terminal logs were retained in the
[evidence bundle](2026-11-06-expand-grouping-hunt). Its independent scripts
could be run from an archived package root by absolute path with `--assert`;
the other scripts documented their working-directory/output requirements.
Captured logs had trailing whitespace and final blank lines removed only.
Every owned execution was collected through terminal completion. No full suite,
package build or rendering was used as a substitute for focused feedback.

The environment was arm64 macOS, R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
vctrs 0.7.3, rlang 1.3.0, tidyselect 1.2.1, stringr 1.6.0,
dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5 and
RSQLite 3.53.3. Hosted sources, live PostgreSQL/SQL Server, other platforms and
dependency versions, arbitrary caller classes and unlimited plans remained
outside the measured scope.

Only dated evidence was added under the existing investigation exclusion.
Package code/tests, examples, vignettes, installed files, generated package
documentation, dependency metadata, build rules and verification machinery
were unchanged. No package behavior or source-consumer change followed from
these additions. Those effects justified `source` Review-ready scope; its
six-check result, pinned submitted SHA, main-compatibility assessment and
Standards/Spec review record belonged on the PR. Package build/check, complete
suite and snapshots were omitted. Research introduced no user-facing behavior
change requiring a NEWS entry. CI was not monitored.
