# Grouping specification lookup and early validation

Investigated: 2026-11-04
Repository snapshot: `ca50a281e0707897ff5768b4c20f533cf7194c4c`

The Grouping specification lane compared public inspection and summaries with
ordinary tidyselect, explicit selection controls and the Grouping plan
contracts. The November 2 and November 3 summary/Grouping/share hunts, the
November 4 summary/Grouping/share and nesting hunts, and the November 3
expansion hunt identified recently tested boundaries. Their runs were not
included in the counts here.

R 4.6.1 ran on macOS arm64 with rlang 1.3.0, tidyselect 1.2.1, dplyr 1.2.1,
dtplyr 1.3.3, DuckDB 1.5.5, dbplyr 2.6.0, DBI 1.3.0 and data.table 1.18.6.1.
The scripts loaded the examined source with `pkgload::load_all()`. A private
in-memory DuckDB connection was disconnected after the probes.

## G1. Legal dot-shaped columns were read as dots outside a dots frame

An input had character column `..01` containing p/p and column `a` containing
x/y. Inspection of `grouping_set((..01))` failed with the rlang condition
`'...' used in an incorrect context`. Ordinary tidyselect selected `..01`.
Fixed-key inspection with `.by = (..01)`, an explicit `all_of("..01")`
selection, and a selection nested under `c(..01)` also selected it. A wrapper
with a genuine first dots argument holding `grouping_set(a)` correctly chose
`a`, even though the input also had `..01`.

The documented constructor selection grammar and redundant-parentheses rule
applied to the first call. ADR 0008 and ADR 0026 supplied the distinction
between a captured selection and an available nested-specification reading.
The failure came from `nested_arg_quosure()` asking rlang to capture each
positive numbered-dot spelling without first establishing that its environment
could provide dots. A replay from an archive of
`f5a8d7d1b18c981f044102fd56d92606679c7c52`, the parent of the change
`7cdff9bff25be1b6a8054dc9a8c0fa0efe829483`, returned the expected included
column for the parenthesized call. A literal unparenthesized reference
without an injection can already fail in rlang capture, before the package
preflight. That case was outside this finding.

The matrix produced 360 package refusals across six legal spellings, all five
constructors, one or three parenthesis pairs or an injected bare symbol,
data.frame and tibble, and inspection and summary. Each had an explicit
`all_of()` control with the expected family or summary count. Twelve
parenthesized wrappers with actual first or second dots, all with a
same-spelled input column, selected the forwarded specification.

All-state issue and PR searches for `"grouping" "numbered dots"`,
`"grouping" "..01"`, `"grouping" "incorrect context"` and
`"grouping" "noncanonical"` found closed #1214 and merged #1223. Those
established the required behavior with supplied dots. #1211 concerned
summary-expression forwarding. #971, #968 and #1044 concerned dot-shaped
output names and SQL/dtplyr representation. The searched results contained
no report for a valid selected column outside a dots frame.

## G2. A typed sibling hid already established duplicate Grouping sets

With integer `a` and character `b`, `grouping_sets(a, a, where(predicate))`
already contained two copies of the set with `a` under the default duplicate
policy. The typed predicate selected `b` and did not affect that prefix.
The names-only control `grouping_sets(a, a, b)` reported positions 1 and 2
without invoking DBI. On a live DuckDB table, the typed version invoked the
traced DBI method once and ran the predicate for both columns before reporting
the same positions. On an immutable derived dtplyr step with
`mutate(b = toupper(b))`, it instead reported unsafe metadata and asked the
caller to collect. Explicit collection followed by local inspection confirmed
the same duplicate. The recorded SQL evidence was the metadata invocation,
with no source-row scan asserted.

ADR 0005 required locally decidable errors before backend metadata work.
The mixed leading-selection pass stopped when it encountered the typed
predicate. It retained leading dimensions for the fixed-key overlap check
and did not retain the completed grouping-set occurrences for duplicate
validation. The canonical compiler therefore checked duplicates only after
typed metadata acquisition. The named-only control isolated metadata
precedence from duplicate detection itself. The case did not establish
precedence when an unresolved selector came before the repeated sets, or when
a later typed selection contributed further duplicate positions.

Four forms of a union prefix, including nested and nonadjacent occurrences,
each went through inspection and summary. All eight DuckDB mixed calls made
one metadata invocation each; all eight derived dtplyr mixed calls exposed
the metadata refusal. Eight eager mixed calls produced the expected duplicate
condition. All 24 names-only controls returned the duplicate condition
without invoking DBI or the predicate. The 32 eager or DuckDB mixed controls
with `"drop"` or `"keep"` proceeded and evaluated the typed predicate.

All-state issues and PRs were searched for `"grouping" "duplicate" "metadata"`,
`"Duplicate grouping sets" "typed"`, `"duplicates" "preflight"` and
`"duplicate" "leading" "Grouping"`. Closed #1090 and #1139 addressed
leading invalid selections and established fixed-key overlaps. Closed #1184
and #1210 addressed selection warnings promoted to errors. No matching
duplicate-set report was found in the searched results.

## Replay

Run `grouping-dots.R`, `grouping-duplicate-prefix.R` and `grouping-matrix.R`
with Rscript from the examined source root. Each minimal script's observation
mode and the matrix exited zero. Each minimal script's `--assert` mode
required the desired behavior and exited one on the pinned main snapshot.
The archived comparison ran `grouping-dots.R --before-fix` and
exited zero. These finite probes covered the named fixtures and dependency
versions, without a general guarantee about arbitrary selectors, hosted
backends, dependency versions or operating systems.
