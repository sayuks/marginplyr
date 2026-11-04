# Bug hunt for Margin summary, Grouping, and share contracts

Investigated: 2026-11-04
Repository snapshot: `7a05a1e42d53af62c057a30ad1892ffb35d4eef8`

The investigation used a separate worktree from fetched main. The summary,
Grouping, and share lanes each constructed public-call reproductions with
one-variable controls. A fourth lane exercised live SQL ordering, typed
values, and denominator evaluation. The
[earlier November 4 hunt](summary-grouping-share-bughunt-2026-11-04.md),
[its follow-up](summary-spec-total-share-followup-2026-11-04.md), and the
[November 3 hunt](summary-grouping-share-bughunt-2026-11-03.md) supplied
comparison cases. The tested snapshot included the subsequent tidyselect,
dependency, and DuckDB INTERVAL repairs.

The request delegated ticket granularity and publication. The seven tickets
each covered a separately verifiable public failure with no blockers. They
received the `bug` and `ready-for-agent` labels.

| Ticket | Public effect |
| --- | --- |
| [#1263](https://github.com/sayuks/marginplyr/issues/1263) | A caller's `quote` function changed selected columns, summary values, and output names |
| [#1264](https://github.com/sayuks/marginplyr/issues/1264) | A DuckDB denominator comparison refused valid wide numeric values |
| [#1265](https://github.com/sayuks/marginplyr/issues/1265) | Lexical helper arguments forced typed Grouping metadata for name-only selections |
| [#1266](https://github.com/sayuks/marginplyr/issues/1266) | A guaranteed product duplicate was delayed or hidden by typed metadata |
| [#1267](https://github.com/sayuks/marginplyr/issues/1267) | A caller's `function` binding replaced the nested data-mask wrapper's result |
| [#1268](https://github.com/sayuks/marginplyr/issues/1268) | Literal dispatch to the active tidyselect proxy bypassed the share-source dependency guard |
| [#1269](https://github.com/sayuks/marginplyr/issues/1269) | Inherited removal inside an implicit `local()` bypassed both share dependency guards |

## Margin summary evaluation

### Generated selector quoting

A caller with columns x = 1:3 and y = 10:12 bound a function named `quote`
that always returned the symbol y. Ordinary dplyr gave 6 for the sum of
`pick(x)` and made no call to that binding. The Margin verb gave 33.
An unnamed `across(x, sum)` returned a column named y with 33 instead of
a column named x with 6. Removing the caller binding restored agreement.
A named pick used as a share source likewise changed the values used by
Total shares.

The rewrite in `local_summary_selection_expr()` injected a bare call to
`quote`. Caller code had supplied the selector and its environment without
writing that call. The matrix compared four helpers, two qualification forms,
two positions, three grains and three eager input classes. All 144 shadowed
Margin cases diverged from ordinary dplyr. All unshadowed Margin cases
matched. ADR 0019 supplied the lookup contract. #1212, #1238 and #1176 had
covered other generated calls in dtplyr. All-state issue and PR searches
paired `quote` with `pick`, `across` and `binding`.

[Minimal replay](2026-11-04-margin-contracts/summary-selector-quote.R) and
[selector matrix](2026-11-04-margin-contracts/summary-selector-quote-matrix.R).

### Generated function construction in a nested mask

With x = 1:3, an ordinary summary evaluated a nested `mutate()` that copied
x into z and summed z. A caller binding named `function` supplied a
replacement closure. Ordinary dplyr still returned 6. The Margin call
returned 99 from the replacement. The generated wrapper in
`rewrite_summary_selections()` performed function construction using that
binding. The minimized ordinary call did not invoke it. The unshadowed
Margin control returned 6.

The matrix covered 360 combinations of nested verbs, eager inputs, summary
grains and argument forms. It found 324 shadowed differences and 36 matching
`rowwise()` controls. All 360 unshadowed cases matched. This was a separate
rewrite site from selector quoting. #1237 established that the nested verb's
input stays in the caller's scope for assignment and early return. The
all-state issue and PR search paired `function`, `binding`, `nested` and
`summary`.

[Minimal replay](2026-11-04-margin-contracts/summary-nested-function.R) and
[nested-mask matrix](2026-11-04-margin-contracts/summary-nested-function-matrix.R).

## Grouping specification preparation

### Lexical values available without column types

An immutable dtplyr step derived an `added` column using a string expression.
With `cols` bound to a character vector containing a and added,
`all_of(cols)` in a Grouping constructor selected both names, as did
ordinary tidyselect on a names-only proxy. `all_of(.env$cols)` instead
reported the unsafe typed-metadata condition. Fixed-key selection, literal
bracket lookup, and other ordinary helper arguments reproduced the distinction.
A genuinely typed selector still required metadata. A binding with an active
callback was unforced in the name-only controls.

The loop found 12 incorrect dtplyr refusals and four dtplyr cases in which
the refusal hid a missing-column diagnostic. The corresponding four
missing-column selections on DuckDB each entered DBI's query method once
before reporting the diagnostic. The ordinary lexical binding controls
required no metadata request. ADR 0020 supplied the name-only boundary.
The earlier #1255 and PR #1260 covered stringr modifiers, including lexical
lookups inside the modifier. #975 covered a typed data-pronoun read. Issue
and PR searches included `".env" Grouping`, `dtplyr "name-only"`, and
`"env pronoun" "Grouping"`.

### Equal product occurrences before a typed factor

The prefix of `grouping_spec(grouping_sets(a, a), where(predicate))`
contained two equal occurrences. The right factor contributes one grouping
set, including when the selection is empty. Both prefixes therefore union
with the same membership and give duplicate positions 1 and 2. DuckDB
entered DBI's query method and ran the predicate before reporting the
duplicate. An unsafe derived dtplyr step hid the duplicate behind the
metadata refusal. A literal right factor and a repaired leading union
reported the duplicate before metadata or predicate evaluation.

The matrix crossed three product forms with inspection and summary
construction. It observed six DuckDB query method entries, 12 predicate
calls and six hidden dtplyr duplicates. Eight literal or union controls
and three valid, keep, or drop controls behaved as expected. ADR 0005
placed locally established errors ahead of metadata. #1228, #1246 and
PR #1250 covered leading completed union occurrences. #1245 covered
compact duplicate-drop expansion. The all-state issue and PR search
included `duplicate "Cartesian product" metadata`.

[Grouping replay and controls](2026-11-04-margin-contracts/grouping-selection-product.R).
The DBI trace counted entry into the query method. It did not measure rows
scanned by the server.

## Contextual shares

### Wide DuckDB denominators

A DuckDB rollup summarized the maximum of values one and the largest
`DECIMAL(38,0)`. The ordinary summary collected. Either share caused a
conversion error for the denominator to `DECIMAL(38,1)`. The largest
`HUGEINT` and `UHUGEINT` showed the same behavior. An isolated comparison
to integer zero returned false. The comparison to decimal zero reproduced
the error. This distinguished the denominator zero check from the arithmetic
used to bind the source's type.

The loop covered native and portable rollups and both share helpers. The
three wide types failed in all 12 cases. Twelve `DECIMAL(37,0)`,
`DECIMAL(38,1)`, and double controls matched the expected double ratios.
Six Grand-total-only controls gave one. ADRs 0010 and 0017 owned eligibility
and ratio behavior. #429 and #444 covered the source arithmetic guard.
#1229 and PR #1261 covered INTERVAL rejection. All-state searches included
`DECIMAL OR BIGNUM`, `"denominator" "scale"` and `"DECIMAL(38,1)"`.

[DuckDB numeric replay](2026-11-04-margin-contracts/wide-duckdb-denominator.R).

### Reading an alias through the selection proxy

A rollup first summarized x = 1/3 into a column named prior. A later source
selected between x and z = 7/9 based on the first prior value in the active
tidyselect proxy. Direct `tidyselect::peek_data()` and a named callback
both triggered the earlier-alias refusal. Literal dispatch with
`base::do.call()` reached the same proxy but returned source values 7/9/4
and ratios 1.75/2.25/1. Explicit inline forwarding and other literal dispatch
forms reproduced the bypass. Reading an independent input from the proxy
remained valid.

There were 60 missing refusals and 24 required direct or named-callback
refusals across five dispatch variants, two controls, three eager input
classes, two share helpers and two ordinary selection helpers. The source
restriction applied to actual earlier-alias reads under ADRs 0010 and 0017.
#1254 preserved that restriction while allowing an alias to be excluded.
All-state issue and PR searches paired `peek_data` with `do.call` and
`alias`.

[Selection proxy replay](2026-11-04-margin-contracts/share-selector-proxy-dispatch.R).

### Inherited removal in the implicit local environment

An inline function bound a formal prior to one, called base `local()`
with an inherited removal of prior, then summed prior. After a share named
prior, this returned the input column's 100/300/400 beside shares
0.25/0.75/1. A share source using the same form read an earlier ordinary
alias and also escaped refusal. Moving the removal to a direct nested
function or using an explicit enclosing evaluation environment restored
the required refusals. Removing an inner shadow alone preserved the valid
read of the formal.

The loop established 24 missing refusals, 24 required refusals, and 12
successful local-only controls. It crossed the two dependency directions,
both share helpers and three eager inputs. #1230 covered visible enclosing
targets and literal inheritance. The implicit `local()` environment was
the distinguishing boundary. All-state searches used
`"local" "inherits" "share"` and `"inherits" "local()" in:title,body`.

[Scope replay](2026-11-04-margin-contracts/share-local-inherited-removal.R).

## Additional controls and boundaries

| Probe | Bounded observation |
| --- | --- |
| [Grouping widths](2026-11-04-margin-contracts/grouping-width-controls.R) | 16 inspection and summary pairs covered widths 1, 30, 31 and 32 on eager tibbles, dtplyr, SQLite and DuckDB. Expected bits and occurrence IDs matched. At 32 dimensions, inspection retained bits and the numeric summary identifier was refused. |
| [SQLite ordering and share oracle](2026-11-04-margin-contracts/sqlite-order-oracle.R) | 144 retrievals matched separate per-set SQL aggregates and fixed-partition ratios under BINARY, NOCASE and RTRIM. The comparisons crossed ordinary and Boolean-word names, leading and repeated Grand total sets, typed missing and string labels, and direct and materialized retrieval. All 96 ordered retrievals also matched the structural order oracle. |
| [PostgreSQL volatile sources](2026-11-04-margin-contracts/postgres-displayed-denominators.R) | Both shares agreed with the displayed per-occurrence sequence values in six native and portable retrievals. Each result used exactly three sequence values. Construction left the sequence uncalled. |
| SQLite BLOB keys | 24 direct/materialized pairs agreed on column values and order after aligning the outer data-frame class. Both set orders, three sort options, two label forms and share presence were varied. |

The summary lane also ran 204 eager expression comparisons, 285 ordinary
summary configurations across eager, dtplyr, Arrow, DuckDB and SQLite
inputs, and 48 lazy callback comparisons. The eager comparisons with a
successful side agreed on values and warning counts. Paired errors were
outside that comparison. Arrow's absorbing first/last/nth calls,
ordinary dtplyr's scalar key lookup and SQL's columnless projection
limitations explained the observed backend differences. The
[caller-scope controls](2026-11-04-margin-contracts/summary-scope-controls.R)
also confirmed the behavior fixed by #1237 and explicit caller-authored
lookup under both summary verbs.

An exploratory DuckDB aggregate produced a NaN share when the displayed
numerator was NaN. ADR 0010 excludes database-native NaN and infinity from
the portable ratio contract. A callable Grouping specification produced by
an unrecognized nested factory followed the valid-predicate route already
recorded by ADR 0008 and PR #314. Neither was a new ticket. The matrices
covered the named fixtures and versions. Hosted backends, other dependency
versions, unbounded plans and arbitrary caller code were outside their scope.

## Replay environment

The scripts in [the evidence directory](2026-11-04-margin-contracts) run
from the investigated source root with Rscript. For the PostgreSQL replay,
provide a disposable server on a Unix socket using
`MARGIN_CONTRACTS_PG_SOCKET` and port 55673. PostgreSQL 17.11 ran only on
that socket and was stopped after the replay.

The environment was R 4.6.1 on aarch64 macOS, pkgload 1.5.3, dplyr 1.2.1,
dbplyr 2.6.0, tidyselect 1.2.1, rlang 1.3.0, vctrs 0.7.3, tibble 3.3.1,
dtplyr 1.3.3, data.table 1.18.6.1, RSQLite 3.53.3, DuckDB 1.5.5,
RPostgres 1.4.10, Arrow 25.0.1, and DBI 1.3.0. The report modes reproduce and check the
observations. The contract assertions are expected to fail at this SHA.

In a separate archive of the investigated SHA, eleven report invocations
exited zero and the seven desired-contract invocations exited one after
their controls. The [replay results](2026-11-04-margin-contracts/replay-results.csv)
record both exit statuses. The PostgreSQL replay ran separately against
the same package source.

The change adds investigation evidence excluded from the source package.
Package runtime, tests, dependencies, examples, vignettes and generation
inputs were untouched. This justified Review-ready `source`. The
verification record belongs to the submitted commit and appears on the pull
request.
