# Bug hunt for Margin summaries, Grouping specifications, and contextual shares

Investigated: 2026-11-05
Repository snapshot: `f158df41168bbf0b64bce01518fcff1bd88cb11f`

The investigation started from fetched main in
`/private/tmp/marginplyr-summary-grouping-share-20261105`, on branch
`investigation/summary-grouping-share-20261105`. The
[November 4 contract hunt](margin-contracts-bug-hunt-2026-11-04.md),
[its summary and share follow-up](summary-spec-total-share-followup-2026-11-04.md)
and the [previous summary hunt](summary-grouping-share-bughunt-2026-11-04.md)
supplied reproductions and repaired controls. The four lanes examined
ordinary Margin evaluation, Grouping preparation and algebra, contextual
share validation, and live SQL results. A second fetch before Issue
publication still resolved main to the tested snapshot.

## Findings and tracker disposition

| Area | Public effect on the tested snapshot | Issue |
| --- | --- | --- |
| Margin summary | A first named across rejected a populated numbered dot as a nonexistent column. | [#1310](https://github.com/sayuks/marginplyr/issues/1310) |
| Margin summary | Caller namespace operators intercepted generated calls and changed successful summary values. | [#1311](https://github.com/sayuks/marginplyr/issues/1311) |
| Margin summary | An Assigned summary name that disappears on frame expansion falsely conflicted with a public key or identifier. | [#1312](https://github.com/sayuks/marginplyr/issues/1312) |
| Grouping specification | Name-only NULL and lexical selections forced typed metadata, blocking derived dtplyr input or sending a DuckDB probe before a missing-column diagnostic. | [#1313](https://github.com/sayuks/marginplyr/issues/1313) |
| Grouping specification | A cube nested in a union and product exhausted a bounded heap even though the enclosing product retained one set. | Existing [#1306](https://github.com/sayuks/marginplyr/issues/1306) |
| Grouping specification | A genuine qualified parenthesis changed how a caller's selection was interpreted, returning an empty set or falsely reporting duplicate sets. | [#1315](https://github.com/sayuks/marginplyr/issues/1315) |
| Parent and Total shares | Literal enclosing-frame depths bypassed dependency validation and allowed summaries to read a different value. | [#1316](https://github.com/sayuks/marginplyr/issues/1316) |
| Parent and Total shares | Locally stored closures escaped dependency validation when their enclosing bindings changed or the closures removed a binding when called. | [#1317](https://github.com/sayuks/marginplyr/issues/1317) |
| Parent and Total shares | Literal field access to the active data mask bypassed both dependency refusals. | [#1318](https://github.com/sayuks/marginplyr/issues/1318) |
| Parent and Total shares | Literal or injected NULL bypassed the contextual across unpacking rule. | [#1319](https://github.com/sayuks/marginplyr/issues/1319) |

The nine new tickets each covered an independently verifiable behavior with
no blockers and received `bug` and `ready-for-agent`. Ticket boundaries
followed the observable contract and separating controls. The user delegated
granularity and publication. The post-publication duplicate comparison
found #1306's exact cube witness, so the new
[#1314](https://github.com/sayuks/marginplyr/issues/1314) was closed as a
duplicate. #1304 concerned caller override of the namespace operator inside
a helper argument. The #1315 witness used the genuine namespace operator
and an ordinary lexical binding inside the wrapper. Supplemental GitHub
searches hit rate limits. Available all-state searches, recent listings,
related report bodies, and the post-publication listing supplied the
duplicate evidence documented in the lane notes.

The [summary lane](2026-11-05-summary-grouping-share/summary/findings.md)
records the first-summary controls, the distinction between generated and
authored namespace lookup, and public versus temporary name collisions.
The [Grouping lane](2026-11-05-summary-grouping-share/grouping/findings.md)
records proxy types, DuckDB metadata queries, equivalent wrapper shapes,
and the bounded-heap experiment. The
[share lane](2026-11-05-summary-grouping-share/share/findings.md) records
both forbidden dependency directions and legal reads from independent
bindings. Those records name the contracts, prior repairs and search terms.

## Additional bounded observations

| Probe | Completed comparison |
| --- | --- |
| [SQL identifiers, keys, and ratios](2026-11-05-summary-grouping-share/sql/identifiers-and-shares.R) | 72 direct collections each on SQLite, DuckDB and PostgreSQL matched eager Margin values. Six identifier layouts covered spaces, an embedded backtick, Boolean words and pronoun names. Cases crossed duplicate policies, typed missing and string labels, and three sort requests. Ordered results also matched in row order. |
| [PostgreSQL ICU collation](2026-11-05-summary-grouping-share/sql/postgres-collation.R) | All 12 collections under a nondeterministic case-insensitive collation matched ratios recomputed from the displayed denominator rows. Fixed keys and detail keys included missing values. |
| [Volatile SQL sources](2026-11-05-summary-grouping-share/share/share-volatile-sql.R) | All 24 SQLite and DuckDB collections matched ratios recomputed from the visible source and denominator rows across rollups and Grouping-set unions. |
| [Contextual dependency matrix](2026-11-05-summary-grouping-share/share/share-dependencies.R) | 216 comparisons over both dependency directions, both helpers and three eager classes gave 120 missing refusals, 60 correct refusals and 36 valid independent reads. |
| [Contextual unpacking matrix](2026-11-05-summary-grouping-share/share/share-unpack-null.R) | 56 comparisons gave 16 missing refusals, 24 correct refusals and 16 valid calls. |

The SQL order comparison normalized numeric storage and unordered row
permutations. The collation comparison checked ratio and row count and
normalized case for matching keys. The probes used finite fixtures with
the versions below and did not cover hosted backends or concurrent
execution paths.

## Replay and verification boundary

The scripts in [the evidence directory](2026-11-05-summary-grouping-share)
load the investigated source root through pkgload. Run from an archive of
the tested SHA, passing the absolute script path to Rscript. From the
checkout holding this note, for example:

```sh
evidence="$PWD/investigation/2026-11-05-summary-grouping-share"
source_dir=$(mktemp -d)
git archive f158df41168bbf0b64bce01518fcff1bd88cb11f | tar -xf - -C "$source_dir"
cd "$source_dir"
Rscript "$evidence/summary/first-numbered-selector.R"
```

Observation mode asserts the dated failures and their controls.
Contract modes intentionally return nonzero at the documented failure. Run
`nested-cube.R` with `R_MAX_VSIZE=96M`. Run the dependency script with
`frames --assert`, `closures --assert` and `mask --assert` to isolate
its three findings. The PostgreSQL replays used a disposable PostgreSQL
17.11 instance on a private Unix socket with port 55689, supplied through
`MP_HUNT_SOCKET`. The server was stopped after the comparison.

The environment was R 4.6.1 on aarch64 macOS, pkgload 1.5.3, dplyr 1.2.1,
dbplyr 2.6.0, tidyselect 1.2.1, rlang 1.3.0, vctrs 0.7.3, tibble 3.3.1,
dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1, DBI 1.3.0, RSQLite 3.53.3,
DuckDB 1.5.5 and RPostgres 1.4.10.

The submission added dated prose and standalone reproductions excluded from
the source package. The diff changed no package-check inputs, which justified
Review-ready `source`. The checks for the submitted commit appear on the
pull request.
