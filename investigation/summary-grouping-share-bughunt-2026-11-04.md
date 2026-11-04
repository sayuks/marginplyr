# Bug hunt: summaries, Grouping specifications, and Total shares

Investigated: 2026-11-04
Repository snapshot: `78ebbc36200bdc5cef70838dd4e772c5c4a1401c`

The investigation started in a new worktree created from main after fetching
origin. The worktree was `/private/tmp/marginplyr-summary-grouping-share-20261104`,
on branch `investigate/summary-grouping-share-2026-11-04`. A fresh fetch before
ticket publication still resolved main to the same snapshot. Four investigators
divided ordinary summary capture/evaluation, Grouping family/selection,
contextual-share dependency/ratio evaluation, and live SQL composition.

The November 2 and 3
[summary investigation](summary-grouping-share-bughunt-2026-11-03.md), its
[predecessor](summary-grouping-share-bughunt-2026-11-02.md), and the earlier
notes cited by those investigations guided the remaining boundaries. The
recent expansion and nesting specification lanes supplied additional controls.
Merged repairs were compared with the tested source and successful controls;
their historical failures were not counted as new defects. No earlier note was
amended or superseded.

The user requested investigation and ticket publication without implementation.
Only dated research prose, standalone R harnesses and captured evidence were
added. Package implementation, package regression tests, dependencies,
generation sources, generated documentation and build exclusions were not
changed. The user explicitly delegated ticket granularity and asked not to be
consulted, so the `to-tickets` interactive breakdown checkpoint was omitted.
Three independently verifiable P2 tickets were published with bug and ready-for-agent labels, each with no blockers.

| Finding | Ticket | Established effect |
| --- | --- | --- |
| F1 | [#1211](https://github.com/sayuks/marginplyr/issues/1211) | R-supported numeric dots spellings lost capture in selectors and changed forwarded function-factory evaluation |
| F2 | [#1212](https://github.com/sayuks/marginplyr/issues/1212) | Caller functions replaced package-generated dtplyr ratio helpers and silently changed shares |
| F3 | [#1213](https://github.com/sayuks/marginplyr/issues/1213) | Forced formal defaults removed bindings without updating later contextual-share dependency analysis |

## Confirmed findings

### F1: noncanonical numbered dots crossed two summary boundaries

A data frame with x = 1:3 was sufficient. A dots function selecting its first
argument through pick using `..01` returned six under ordinary dplyr. The
Margin call reported a nonexistent column named `..01`; changing only the
reference to canonical `..1` returned six. The same boundary affected across,
if_any and if_all, bare or qualified, on three eager input classes and three
summary grains. All 432 additional-spelling cases failed only in the Margin
call, while all 144 canonical controls agreed.

A forwarded function factory demonstrated a wrong successful result at the
same incomplete syntax predicate. With dynamic unpacking, ordinary dplyr
called the factory twice and returned five/four, while the Margin call called
it once and returned four/four. Canonical dots preserved the ordinary result;
disabling unpacking legitimately used one call in both. The 72-case factory
matrix established 36 value/count differences and 36 agreements. This was one
forwarding ticket across two consumers, rather than one ticket per helper.

The [summary lane](2026-11-04-summary-grouping-share-bug-hunt/summary/findings.md)
held both minimal reproductions, contracts, causal alternatives, matrices and
the comparison with canonical-forwarding repairs #1138, #1170 and #1182.

### F2: dtplyr ratio expressions called functions in the root environment

An immutable dtplyr input held groups a/b and values one/three. The returned
query initially displayed summaries one/three/four and Total shares
one-quarter/three-quarters/one. Adding only a root-environment is.na function
that answered true for every element, after query construction, changed the
shares to missing/missing/one. Input and no-share controls stayed correct;
removing the binding restored the ratios.

Package-generated calls to is.na, as.double, multiplication, division,
equality and logical-or all reached same-named caller functions. The 336-call
matrix established 96 wrong-ratio cases, 96 callback errors and 96 unwanted
callback-invocation cases, with 48 repaired-membership controls. Both share
helpers, early/late bindings, fixed partitions and direct/across sources were
covered. The previous #1203 repair had explicitly scoped itself to membership;
these remaining ratio expressions formed an independent follow-up.

The [share lane](2026-11-04-summary-grouping-share-bug-hunt/share/findings.md)
held the silent wrong-result reproduction, generated-call observations,
ordinary/source controls and duplicate comparison.

### F3: a forced formal default changed lookup after its scope was analyzed

The input held x values one/three and a prior payload of 100/300. An ordinary
summary following a contextual share invoked an inline function whose default
for formal prior removed that binding and returned one. Its body forced prior
and read prior again. The operation succeeded with displayed shares
one-quarter/three-quarters/one but derived values 100/300/400 from the input.
Moving only the removal into the body produced the documented Package refusal.

In the reverse direction, a requested share source read a preceding ordinary
alias after the same default removed its formal binding, bypassing the
self-contained-source refusal. Both helpers reproduced both missing refusals.
The 192-case matrix established missing refusals across four removal forms,
while body-removal, retained-binding, ordinary and unchanged-source controls
passed. This was a separate scope-analysis ticket from F2's runtime helper
ownership, despite both touching the share module.

The [share lane](2026-11-04-summary-grouping-share-bug-hunt/share/findings.md)
held the default/body syntax pair, expected refusal classes, conservative static
analysis contract, prior-report comparisons and matrix accounting.

## Bounded negative observations

These counts described separate finite loops, not disjoint API inputs or an
exhaustiveness guarantee. Minimized examples overlapped their larger matrices.

| Loop | Completed scope | Observation |
| --- | --- | --- |
| Grouping family algebra | 591 bounded generated trees × three duplicate policies | 1,589 exact family/order/bits/identifier matches and 184 required duplicate refusals |
| Grouping summary consumption | 75 generated trees and every retained occurrence | Counts and sums matched independent ordinary-dplyr summaries |
| Grouping dots selection | 1,008 comparisons over three inputs, eight dots shapes and six positions | 372 name matches, 612 matching External-condition classes, 24 required empty-specification refusals; all force counts matched |
| Live SQL share windows/composition | 320 SQLite/DuckDB calls over finite/zero/missing/empty data, fixed keys, aggregation policies, sort requests and five execution forms | Both shares matched visible denominator rows |
| Live SQL collation | 16 SQLite/DuckDB calls with NOCASE fixed/variable keys and missing keys | Normalized local key, summary and share oracles agreed |

The [Grouping lane](2026-11-04-summary-grouping-share-bug-hunt/grouping/findings.md)
held independent algebra/selection oracle construction and the explicit
nine-tree size exclusion. The Grouping dots controls did not share F1's
ordinary-summary defect. The
[SQL lane](2026-11-04-summary-grouping-share-bug-hunt/backend/findings.md)
held live composition and collation evidence and the exclusion of derived empty
SQLite type expectations outside ADR 0031's direct-result guarantee. Those SQL
loops checked values and public composition, not an independent Margin-order
oracle. Earlier harness-only setup errors were distinguished from package
findings in the lane records.

The evidence covered the retained fixtures, syntax forms, input classes and
installed versions. It established no guarantee for arbitrary caller code,
unbounded plans, all extension classes or collations, hosted services,
concurrency, interruption, other dependency versions or operating systems.
PostgreSQL's previous alias investigation was read but not rerun. No full
package suite was used as a bug-hunting loop, and no repair was attempted.

## Reproduction and publication

The evidence directory was
[`2026-11-04-summary-grouping-share-bug-hunt`](2026-11-04-summary-grouping-share-bug-hunt).
Each lane named its commands, tested SHA, controls, relevant limitations and
open/closed issue/PR searches. The coordinator additionally searched all-state
issues and PRs for noncanonical and formal default; noncanonical searches
returned no reports, while the formal-default results included earlier
argument-matching/scope reports rather than the forced-removal reproduction.
The final per-finding duplicate comparisons were in the lane records.

The coordinator independently ran all four minimized scripts in fresh R
processes, sequentially. Each report mode asserted its controls and the observed
defect and exited zero. Each `--assert` mode required the desired contract and
exited one. The coordinator replay completed through terminal handle `77208`,
exit zero, with all eight expected statuses retained in
[the replay results](2026-11-04-summary-grouping-share-bug-hunt/coordinator/replay-results.csv).
These were deliberately red research signals, not installed regression tests
or evidence that the defects were repaired.

`environment.txt` recorded R 4.6.1 on aarch64 macOS, dplyr 1.2.1, dbplyr 2.6.0,
dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5, RSQLite 3.53.3
and RPostgres 1.4.10. Text artifacts had trailing whitespace and extra final blank lines removed; the artifact manifest recorded the retained file hashes.
Every research execution and its owned work reached terminal completion before
Review-ready submission. No disposable database server remained.

The selected publication scope was `source`. The standalone research files
were excluded from the source package. Their effects changed no package-check
input;
package build/check, the complete suite and snapshots could therefore be
omitted. The exact committed SHA, five baseline outcomes, retained execution
handle, terminal result, fetched-main compatibility assessment and two-axis
review record were to be published on the pull request. CI monitoring was
outside the requested scope.
