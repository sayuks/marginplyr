# Nesting and Grouping specification registry-boundary bug hunt

Investigated: 2026-11-07
Tested main: `e1dabaabb392a3791177e5c48e983f780c7c93ec`

The investigation started in a new worktree from freshly fetched main, on
branch `investigation/nest-spec-bughunt-20261107`. Three parallel lanes examined
local nesting, Grouping specification capture/algebra and lazy-backend
preparation. The coordinator independently replayed the confirmed failures in
a separate Git archive of that main revision. No package implementation,
permanent package test, dependency or maintained contract was changed.

Recent November 6 nesting reports in retained worktrees and the merged
[expansion](expand-grouping-bughunt-2026-11-06.md),
[deep expansion](expand-grouping-deep-bughunt-2026-11-06.md) and
[summary/Grouping](summary-grouping-share-exhaustive-bughunt-2026-11-06.md)
investigations supplied prior controls and known boundaries. Their historical
case counts were not counted as new executions. The search was deliberately
broad within a finite corpus; it did not establish exhaustive correctness for
arbitrary R programs or backend extensions.

## Findings and ticket boundaries

| Finding | Observed effect | Implementation ticket |
| --- | --- | --- |
| Registered root bracket dispatch | Typed inspection lost a character dimension or fixed key; both Nest verbs combined two source partitions into one keyless cell. | [#1418](https://github.com/sayuks/marginplyr/issues/1418), P2 |
| Registered dtplyr converter dispatch | An unverified metadata converter changed zero-row column types despite correct native collection; the same three consumers selected the wrong grain on direct and derived inputs. | [#1419](https://github.com/sayuks/marginplyr/issues/1419), P2 |

The user delegated ticket granularity and publication without confirmation.
The `to-tickets` workflow therefore produced two independently verifiable
repair slices, each labelled `bug` and `ready-for-agent`, with no blockers.
The converter failure reproduced with ordinary bracket registration and on
derived graphs already protected by bracket admission. Repairing either
boundary was not a prerequisite for the other. No prefactoring prerequisite
was established, and this evidence-only PR did not close either ticket.

Both findings used ordinary immutable data.table roots, with head registration
unchanged. Replacements delegated to ordinary behavior and changed the
character key only in zero-row results. Native collection retained the
original values; name-only inspection and restored-registration controls
retained the intended plan. The minimal typed-inspection callbacks saw only
zero-row results. Those witnesses established incorrect metadata and
partitioning, not populated-row exposure or source mutation.

ADR 0020's dtplyr metadata amendment required faithful metadata or safe Package
refusal. The [backend lane](2026-11-07-nest-spec-bughunt/lazy/README.md) records
the distinct dispatch paths, minimized reproductions, hypotheses, controls,
case classifications and unknown-registration probes. Closed #1359 / PR #1371
protected derived bracket dispatch; closed #1397 / PR #1409 protected registered
head dispatch. Neither repair covered both demonstrated operations.

## Finite comparisons and excluded candidates

| Lane | Completed comparison | Observation |
| --- | --- | --- |
| [Local nesting](2026-11-07-nest-spec-bughunt/local/README.md) | 1,536 source-membership/projection calls, 272 name/empty-shape calls and 80 caller-binding calls | 1,800 semantic matches; 88 documented or independently paired native refusals; no new defect. |
| [Grouping specification](2026-11-07-nest-spec-bughunt/spec/README.md) | Three properties over the same 1,500 seeded generated specifications, plus 160 namespace controls | All 4,500 algebra/order/complete-plan comparisons and all 160 controls matched the corrected oracle. |
| Registered bracket boundary | 60 graph/consumer/selection/registration cases | 15 wrong-grain outcomes, 27 correct outcomes, 12 metadata-safety refusals and six structural grouped-input refusals. |
| Registered converter boundary | 48 graph/consumer/selection/registration cases | 24 wrong-grain outcomes and 24 ordinary-registration controls. |
| Ordinary dtplyr graphs | 1,242 comparisons across 23 graph shapes, three source sizes, three predicates, two selector positions and three consumers | 1,134 matches and 108 safe metadata refusals; no additional mismatch. |
| Other lazy boundaries | Six registry-frontier cases, four source-sharing variants and 96 SQL/Arrow calls | Unknown active/lazy registrations ran captured source-reading callbacks in typed inspection; ordinary source-sharing and remote admission/read controls passed. |

The local oracle used explicit grouping families and original-source equality,
rather than package plan expansion. It checked row multiplicity, distinct
source groups and exact cell projections across sixteen scalar/structured
types, both Nest verbs and keep choices, empty inputs and all sort requests.
Sort variants checked membership preservation, not every physical Margin-order
term. The Grouping oracle independently modeled set union, hierarchical
prefixes, subsets and Cartesian products at depth up to three. Its complete
inspection comparison retained included/omitted dimensions, fixed keys, named
Grouping bits, Grouping identifiers and first-occurrence order.

Harness mistakes were corrected before the retained final runs: local payload
projection and empty-vector equality, a reserved-name fixture prepared through
tibble subsetting, and a Grouping oracle that treated a multi-column selector
as a composite dimension. Their preliminary mismatches were not package
findings. Lane notes identify those corrections and the final commands.

Private-qualified specification-valued parentheses were investigated and
reproduced on pre-#1396 main as well. The complete #1288 triage discussion had
excluded that new recognized spelling. The broad #1396 acceptance wording did
not establish a new implementation regression; the lane recorded this known
recognition boundary without opening another ticket. The classed stored-FALSE
flag admission refusal was already recorded by #1360, and no new admission
contract was inferred. Upstream reserved-name refusals were paired with native
tidyr/dplyr behavior instead of being reported as package defects.

## Independent replay, freshness and publication

The coordinator changed the witness names to `region` and `units`, used
different values, and replayed both dispatch operations against an independent
archive. Each operation had six public consumer/selection-position failures.
Observation modes asserted the dated wrong grain and exited zero; contract
modes demanded correct grain or safe refusal and exited one. Native,
name-only, restored-registration and source-immutability controls passed.
The [replay table](2026-11-07-nest-spec-bughunt/coordinator/replay-results.csv)
records expected/observed exits and script digest. These intentional failures
were research evidence, not failing tests added to the package suite.

Replay from a source archive of the tested SHA, passing source root, script
directory and output directory:

```sh
Rscript investigation/2026-11-07-nest-spec-bughunt/coordinator/replay.R \
  /path/to/tested-source \
  investigation/2026-11-07-nest-spec-bughunt/coordinator \
  /path/to/replay-output
```

Each child was sequential and set `TESTTHAT_PARALLEL=false`. All owned
executions were collected before final verification. The first converter
replay attempt had not loaded dtplyr before asking for its registered method;
it failed during harness setup and established no package outcome. The
retained replay loaded the namespace before lookup.

Before Issue publication, main was fetched again and still named the tested
SHA. The all-state CLI exports contained 711 Issues and 699 PRs before the two
new tickets. Targeted live searches and complete relevant body/comment reads
are recorded in the
[duplicate-search record](2026-11-07-nest-spec-bughunt/coordinator/duplicate-search.md).
Both created Issues were fetched and their titles, bodies and labels checked.
Tracker exports remained scratch data rather than committed GitHub copies.
Captured text logs had trailing horizontal whitespace removed for Git hygiene;
diagnostic wording, values and exit outcomes were retained.

The environment was arm64 macOS with R 4.6.1, dplyr 1.2.1, tidyr 1.3.2,
dtplyr 1.3.3, data.table 1.18.6.1, rlang 1.3.0, tidyselect 1.2.1 and vctrs 0.7.3.
Lane logs record additional package versions. Other dependency versions,
operating systems, arbitrary classes, hosted services, interruptions and
unbounded plan/resource behavior were not established by these comparisons.

## Submission scope

The cumulative diff added dated research prose, standalone reproductions and
captured text/CSV evidence. Investigation content had no package-check,
installed-file, documentation-generation or verification-tool consumer; the
unchanged source-package exclusion omitted it. Package code/tests, examples,
vignettes, dependencies, generated documentation and build rules were unchanged.
Those content and consumer effects justified Review-ready `source` scope:
package build/check, complete suite and snapshots omitted. The exact submitted
SHA, current-main assessment, mandatory checks and formal review record belong
on the PR. No NEWS entry was needed for this research-only change. CI monitoring
was outside the requested scope.
