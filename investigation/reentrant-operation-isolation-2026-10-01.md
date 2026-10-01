# Reentrant Margin operation state isolation

Investigated: 2026-10-01
Revised: 2026-10-01 — typed-predicate and summary-selection reentry coverage (#778)
Scope: investigation evidence only; no product fix or adopted specification
Target: `e0cac536b68ada12563e427223d7e3efffbc7ce5`

## Findings and delegated decisions

The investigation established one existing-contract violation: supported
reentry could mix independent operations' SQL audit rows and could append a
missing-SQL `result` for a local result. The computed A/B values, grouping
correspondence, input preservation, ordinary aggregation context, share
calculations, diagnostic causes and recovery checks did not establish another
marginplyr defect within the executed scope.

A separate specification question remained: “last call” did not explicitly
settle start order versus active-call restoration versus completion order when
A began, B ran from A's supported user code, and A resumed. This question was
not used to turn every surprising owner into a bug.

| Outcome | Ticket and delegated judgment |
|---|---|
| Confirmed existing-contract violation | [#778](https://github.com/sayuks/marginplyr/issues/778): high priority; one fix ticket grouped the shared audit-environment symptoms |
| Specification decision required | [#777](https://github.com/sayuks/marginplyr/issues/777): medium priority; decide the owner at each reentrant observation point before fixing complete ownership expectations |
| Dependency | #778 was natively blocked by #777 for its complete owner-specific regression oracles; the invariant violation was established independently of that decision |

The `to-tickets` skill was read and used. The user had delegated splitting,
priority, dependencies, acceptance criteria and posting without individual
confirmations. Those decisions were recommendations made under that delegation;
the user did not individually approve either ticket or adopt a new guarantee.
The decision ticket recommended active-call restoration (A before B, B inside
B, A on return to A and after its completion/failure) for evaluation, without
requiring a stack, all-argument forcing or a reentry ban.

All open issues and relevant closed issues/merged PRs were checked before
posting. The `.data` ordering issue [#455](https://github.com/sayuks/marginplyr/issues/455)
and audit warning-state issue [#655](https://github.com/sayuks/marginplyr/issues/655)
explicitly excluded non-`.data` reentry. Their fixes, including
[PR #660](https://github.com/sayuks/marginplyr/pull/660), were in the target.
[PR #776](https://github.com/sayuks/marginplyr/pull/776) fixed the preceding
transaction investigation. No matching open ticket or unmerged fix was found.
No old issue, product code or public contract was amended by this campaign.

## Target, isolation and environment

The starting checkout was clean on `main` at the target SHA. A Git archive of
that SHA was installed into a disposable library outside the repository.
Experiments used fresh `Rscript --vanilla` processes, `R_LIBS_USER` pointing
only to that library, and `R_LIBS_SITE=NULL`. The runtime library search was
that copied library followed by R's base/recommended library. Neither
`pkgload` nor `testthat` was loaded in experiments. No target namespace was
patched, traced or rebound.

The first temporary root was
`/private/tmp/marginplyr-reentrant-2026-10-01-zq2t8sxx`; the final extraction
and reproduction root was `/private/tmp/marginplyr-reentrant-final-co5nwhjo`.
All scripts, databases, CSV/RDS/log evidence, manifests and archives stayed
outside the repository. SQLite and DuckDB used disposable in-memory databases;
DuckDB used `shared_home = FALSE`. Inputs were synthetic. Existing tracked
files and normal dependency-library files were hashed at the start and checked
for preservation. Input references were compared with serialized independent
copies, or with an independently copied source tibble after retrieval.

| Component | Observed version |
|---|---|
| R | 4.6.1 (2026-06-24), macOS arm64 |
| dplyr / dbplyr | 1.2.1 / 2.6.0 |
| rlang / tidyselect | 1.3.0 / 1.2.1 |
| DBI | 1.3.0 |
| RSQLite / SQLite engine | 3.53.3 / 3.53.3 |
| duckdb / DuckDB engine | 1.5.5 / v1.5.5 |
| dtplyr / data.table | 1.3.3 / 1.18.6.1 |
| Arrow | 25.0.1 |
| lintr / pkgload / jarl | 3.4.0 / 1.5.3 / 0.6.0 |

The copied dependency closure also contained cli 3.6.6, generics 0.1.4,
glue 1.8.1, lifecycle 1.0.5, magrittr 2.0.5, pillar 1.11.1, R6 2.6.1,
tibble 3.3.1, vctrs 0.7.3, utf8 1.2.6, pkgconfig 2.0.3, withr 3.0.3,
blob 1.3.0, purrr 1.2.2, tidyr 1.3.2, stringr 1.6.0, cpp11 0.5.5,
stringi 1.8.9, bit64 4.8.6, memoise 2.0.1, bit 4.6.0, cachem 1.1.0,
fastmap 1.2.0 and assertthat 0.2.1. The setup code regenerates a package,
source-library path and version manifest. Matching these versions is necessary
for an exact environment replay; using later dependencies is a new follow-up.

## Contract, implementation and existing assertions read

The applicable root `AGENTS.md`, `CLAUDE.md` and its `@AGENTS.md` closure,
`CONTEXT.md`, `investigation/README.md`, agent local-check/tracker/triage/review
instructions, lint configuration and lint CI were read. Source references in
this note refer to the target SHA, rather than treating dated evidence as a
newer authority than the implementation or ADR.

The main source readings were `R/sent-queries.R` (`reset_sent_queries()`,
`remember_sent_query_backend()`, `record_sent_query()`, `last_sent_queries()`),
`R/grouping-plan.R` (`prepare_grouping_plan()` and discarded-pass verbosity),
`R/margin-operation.R` (operation preparation/finalization),
`R/grouping-adapter-union.R`, `R/inspect-grouping.R`, `R/conditions.R`
(branch buffers, repeated-condition identity and replay), `R/share.R`
(source assertions and dialect cache), and the public verbs/spec compilation.

Relevant ADRs were 0005/0007/0008 (validation, captured expressions and nested
specs), 0010/0017/0019 (contextual shares and static helper spelling),
0013/0015/0016 (inspection, conditions and result attributes),
0020/0021/0022/0027 (lazy reads, repeated diagnostics, caller context and SQL
audit), 0029 (mutable dtplyr refusal) and 0035 (available handler boundaries).
In particular, ADR 0027's `.data` amendment was not generalized to all other
arguments. Later collection/materialization was not treated as an audit ledger.

Actual assertions were read in `test-sent-queries.R` and its process fixture:
serial replacement, piped outer ownership, `.data` failure retention,
entry-time audit options, result SQL equality and the structural
`force(.data)`-then-reset gate. Grouping/interface/plan/inspection tests
established that a top-level factory may return a spec, while an unrecognized
function call nested in a constructor is not the same grammar. They also
asserted single evaluation of recognized nested specs and separate compilation
passes for selections. Summary/share tests covered captured/injected quosure
environments, ordinary summary evaluation, unpack/across evaluation counts,
static contextual helper recognition and rejected direct group-context
helpers. Execution/competing-condition/warning-handler-boundary assertions
covered wrapping, retained causes, warning aggregation/replay, warn=2,
external handler errors and nonlocal transfers. Their guarantees were not
inferred from search hits alone.

Historical evidence read included `session-state-and-last-call-accessors.md`,
`exception-safety-recovery-2026-09-30.md`, the downstream external-package
integration evidence, condition-handler/replay/competing-condition notes,
`grouping-plan-identity-boundaries-2026-10-01.md` and
`cold-start-transaction-safety-2026-10-01.md`. Those campaigns established
serial/history, consumer environment, recovery and transaction facts. The new
difference was an overlapping lifetime: A had begun recording/preparation,
user R code entered B, and A resumed. Historical notes did not override
updated ADR 0035's external-handler boundary.

## Reentry sites and state ownership

All public verbs taking `.grouping` forced `.data` before starting their
record. Per-call operation/plan objects, captured quosures, selection snapshots,
share tokens and branch-condition buffers were local objects. The SQL audit
record/flags and SQL dialect verdict cache were package environments shared
by the process. User options and dplyr's evaluation/warning machinery were
process state; the investigation observed their permitted context rather than
requiring contextual helpers in arbitrary locations.

| Adopted site | Public paths executed | Timing and live state | Ownership expectation and evidence difference |
|---|---|---|---|
| Top `.grouping` factory | summarize, summarise, expand, nest, nest_by, inspect where the backend supported the verb | A had reset its record; `eval_tidy()` ran before A classified its backend | A's expression environment/arguments remained live; single-call audit was required, exact last owner remained #777; serial/piped tests did not cover this overlap |
| `.by` and Grouping `all_of()` selectors | summary on local, SQLite, DuckDB, dtplyr, Arrow | Backend flag and name snapshot existed; Grouping selection could run in discarded and canonical passes, with a typed proxy between them | Selection belonged to A; the name-only selector ran twice in these fixtures, matching its control; this was not nested-spec “once” |
| Recognized nested constructor, active or delayed spec binding | summary across those backends | Preflight evaluated a supported constructor/name in its captured environment | Spec value belonged to A and was evaluated once in the fixture; it was not an unrecognized `grouping_sets(factory(...))` grammar |
| Ordinary scalar summary | local summary/alias; dtplyr construction and retrieval separately | Local A's data mask/branch buffer/share preparation were live; immutable dtplyr deferred the function to collection | Vector and allowed ordinary dplyr context had to return to A; dtplyr retrieval was post-return execution rather than construction reentry |
| `across()` function | local and dtplyr summary | Same, with local `cur_column()` observed only in `.fns` context | Column/vector/n() observations before and after B agreed; contextual share helpers stayed statically recognized syntax |
| Scalar `.margin_label`, `.key`, `.format` producers | summary across backends; local/dtplyr nests; inspection | Public validation/format selection evaluated these promises after A reset; exact later preparation differed by verb | Correct scalar was returned; no all-argument eager-force requirement was assumed |
| Warning handler, one entry only | local summary with independent SQL B | A replayed its completed branch warning buffer; result recording followed on resume | R handler return/muffle/warn=2 behavior matched controls; exact audit owner remained #777 |

Sequential A then B, B in A's `.data` before A reset, and immutable-dtplyr
summary during later collection were executed as distinct boundaries. A's
`.data` producer executing SQL B left A's local inspection record empty, as
ADR 0027 required. An accessor-time option change did not change a previously
recorded call's audit flag. dtplyr's summary/across callback count was zero at
construction and six after retrieval; its B record at retrieval was not
classified as a construction audit defect.

## Fixtures, independent oracles and calibration

A used `g = a,a,b,b`, `h = x,y,x,x`, `v = 1,3,10,20`, a distinct output
`outer_total`, a three-set rollup and `outer_set`. With margins sorted last,
its hand-calculated totals were `1,3,4,30,30,34`, and set IDs were
`1,1,2,1,2,3`. B used `k = u,v,w`, `v = 101,203,307`, `inner_total`, a
two-set rollup, a different label and first sorting; its totals were
`611,101,203,307` and IDs `2,1,1,1`. C used values 7 and 11, total 18,
and its own grouping/output. An exchanged input/plan could not accidentally
produce the same answer.

Expansion had twelve rows, four per set, total replicated input value 102.
Nests had six rows with payload sizes `1,1,2,2,2,4`; inspection had included
sets `(g,h)`, `(g)`, empty and grouping IDs `0,1,3`. Every reentrant result
was compared exactly with the same supported workflow whose callback omitted
B. Each inner result was also checked separately, including B built from A's
current vector as `x + 100`. Local share tests used independent lexical
offsets 0/100 and identical source name `total` with different values; A's
parent fractions were `1/4,3/4,4/34,1,30/34,1` and its total fractions were
A totals divided by 34. B had different totals and fractions. Grouped data
frames and direct data.table input were included with independent serialized
input snapshots. Ordinary R sum/arithmetic supplied the numeric oracles.

The first local and SQLite pilots confirmed callback order, correct normal
seeds and non-reentrant controls before expansion. Simple local event buffers
captured audit/options/context before B, after B, after A construction,
after explicit retrieval and after C. The observers themselves called no
additional Margin operation. Explicit B/A collection and input-verification
reads were caller-owned checks; SQL render/audit was not used as a complete
DBI ledger. Entire SQL rows were matched against independent source/output
controls; a substring naming B alone was never the audit-mixing oracle.

Synthetic wrong answers were passed to the same comparisons: foreign totals,
row omission/duplication, numeric type changes, mixed audit rows and a wrapper
with its inner cause deleted. All were rejected. Whole-record equality
against either A or B deliberately did not choose an owner. Its `FALSE`
result was an investigation signal, subsequently classified from the rows and
contract, rather than automatically counted as a separate bug.

The development harness had errors that were corrected and excluded from
product findings: passing a scalar quosure as a literal formal instead of an
injected expression; a conditional inside a SQL summary that contained an
untranslatable R callback even in its unused arm; using local n()/column
context inside a deferred dtplyr function; assuming Arrow accepted a
`.data[[...]]` aggregate spelling when the ordinary control refused it; and
observer variable/formatting mistakes while replacing superassignment with
an explicit environment. Corrected final blocks were executed and linted
again. Ordinary dtplyr controls reproduced its absent n() context; Arrow
used the supported bare-column aggregate. Arrow printed sandbox-denied CPU
cache `sysctlbyname` notices while its assertions and processes succeeded.
These notices were retained as environment output, not a Margin finding.

## Executed results and classifications

The final generated construction/value campaign comprised 109 defined cases,
all with successful value/input/context/C assertions. Forty-one whole-record
comparisons matched neither normal A nor normal B. They were overlapping
symptoms, not forty-one discoveries. The diagnostic campaign comprised 46
cases, including factory failures through expansion, both nest verbs and
inspection, and ordinary-summary failures through the summarise alias.
Six targeted auxiliary cases and five minimal/share cases completed. Counts
recorded the executed set; neither a count nor a discovery quota stopped work.

| Observation | Evidence and classification |
|---|---|
| SQL A → SQL B in top factory | SQLite left B result followed by A result; DuckDB left B proxy/result followed by A proxy/result. Both complete result rows matched their separate controls. Confirmed #778 regardless of which single call was intended to own the record |
| Local A summary/across → SQL B | Correct A/B values; final B result plus local A `result = NA`. Local A had no SQL result to render. Confirmed #778 |
| Local A summary → SQL inspection B | SQLite inspection's control had no result row; A still appended a lone `result = NA`. Confirmed #778, not a genuine translation refusal |
| DuckDB selector → DuckDB inspection | B proxy plus A result remained; A's earlier proxy was overwritten. The final record could belong to neither call. Confirmed same #778 mechanism; any complete A policy must also address its lost proxy |
| SQL A selectors/spec binding → SQL B | Backend flag was already live; B reset it and A resumed using B state. Normal values held; mixing reproduced in both native and portable paths. Same root cause |
| Local A factory → SQL B with B-only record, or SQL A → local B with empty record | Owner-specific interpretation depended on #777. Classified as specification-required observations, not independently confirmed missing-record bugs |
| Inner temporarily unaudited, then user restored the option | Accessor could refuse after audited A resumed because B's flag remained. Start-time option association/owner decision was retained in #777; restoring the global option was verified and not treated as retroactive auditing |
| Inner success; caught external/package failure; uncaught failure; A failure after B success | Correct continuation or error outcome, retained external leaf class/token/parent chain, options/input preservation and normal C recovery. No separate contract violation in the checked scope |
| Warnings/messages and handler return | A summary ran three times; inner B summary ran four times per entry, twelve raw B warnings/messages in the nested-warning fixture. One outer warning report was expected from branch aggregation, not one callback evaluation. B-only warnings retained identifiable B text. Messages were A=3/B=12. Muffle under warn=0/1/2 and handler return including warn=2 failure matched controls |
| Handler raises its own error or escapes nonlocally | An independent successful B completed first; retained handler error or escape value matched ordinary dplyr and non-reentrant Margin controls. Classified within ADR 0035/R handler boundaries |
| Immutable dtplyr callback during collect | Values and vector checks passed; B ran only after A returned. Expected post-construction behavior, not a demand to append every later execution to A's audit |
| Shares/quosures/grouped/data.table input | Separate values/environments, recognized share requests and allowed local contexts survived reentry. DuckDB B performed the cold dialect question/control and A reused the session verdict; A/B shares matched hand arithmetic and normal controls. SQLite numeric sources used its supported explicit `.check_share_source = FALSE` assertion. No separate finding |

The top-level error call naming A was not itself evidence of lost B identity.
External B leaf conditions remained identical in the parent chain, including
the custom class, token and own parent. Package errors were checked for their
stable package class in the chain, rather than assuming every wrapper must
have B's top-level call. C was a silent, independent public operation after
both success and error paths.

Warning counts distinguished raw signals, dplyr aggregation and Margin branch
replay. The combined A/B warning fixture's visible report named its first
A warning and the remaining warning count, as ordinary aggregation does;
that presentation alone was not classified as lost B causes. The B-only
fixture separately verified identifiable B warning delivery. Handler reentry
was guarded by a local once flag and introduced no recursive warning loop.

## Limits and termination

The adopted site/state-owner table was revisited after the controls, expansion
and minimum examples. Value/plan/quosure/share and diagnostic-buffer hypotheses
had passed the stated observations; the audit hypothesis had independent
minimum controls and a source-level shared-state explanation across both SQL
construction mechanisms. Owner-dependent missing records/configuration were
left as a decision, not silently resolved. No further concrete high-priority
hypothesis in the adopted one-level supported workflows remained unexecuted.
This was the reason for concluding the campaign, not its case count.

Parallel workers, deeper recursion, arbitrary callback support, upstream
backend context expansion, product fixes and audit/error redesign were outside
the campaign. PostgreSQL/other remote services, Windows/Linux, dependency-floor
versions, every possible summary/type/grouping combination and physical DBI
send tracing were not executed. SQLite and DuckDB gave distinct metadata/native
versus portable construction; immutable dtplyr and Arrow gave distinct deferred
and metadata evaluation. No new transaction-specific or dialect-probe R
callback hypothesis justified rerunning the preceding full transaction matrix.
A SQL-translated arbitrary R summary callback was not claimed as supported.

No remaining adopted case was blocked by resource exhaustion. Additional
backend-specific or deeper-state hypotheses should be resumed against a fresh
fixed code/dependency snapshot, added to this same campaign note as a dated
follow-up, and rerun through these controls before classification. A clean
owner-specific oracle after #777 is a fix acceptance activity, not an
unperformed prerequisite to establishing #778.

## Revisions (2026-10-01, #778)

The campaign's final site-table review identified a concrete additional
hypothesis: type-dependent `where()` predicates read A's canonical typed
snapshot, whereas the earlier `all_of()` selections could run in a discarded
name-only pass. The `.by` predicate similarly resolves after acquiring that
snapshot. This extends the earlier site's coverage and termination statement;
it does not change the confirmed shared-audit mechanism or adopt an owner.
The same campaign and this one note were continued after PR creation.

Additional assertions read in `test-grouping-backends.R` counted one typed
proxy for dtplyr/DuckDB predicate selections and one Arrow schema acquisition
for a `.by` predicate. `prepare_grouping_plan()` handed that per-call snapshot
to both the unresolved fixed-key selection and canonical Grouping compilation.
Those counts remained product constraints, separate from investigation runs.

| Added site | Valid controls and actual timing | Result and ownership classification |
|---|---|---|
| Grouping `where(group_predicate)` | local, DuckDB, immutable dtplyr, Arrow; four column predicates during construction, after A's typed snapshot existed | Each returned `is.character()` for A's typed column after B, matching the normal selection control; correct A/B values and unchanged inputs; the summary record matched neither A nor B, another #778 symptom |
| `.by = where(fixed_predicate)` | Same four backends; four typed column predicates during construction | A's fixture used integer fixed `f = 1L` and double `v`, so only f was fixed; correct A/B values/IDs/inputs; same #778 audit interference |
| DuckDB inspection with Grouping predicate | Correct list inspection control; four predicates during typed compilation | Values matched; final record belonged to SQL B alone after A's earlier proxy was reset. This remained an owner-specific #777 observation, not a new independently confirmed omission bug |
| Ordinary summary `across()` `.cols` and `.names` producers | local, SQLite, DuckDB, dtplyr, Arrow; normal controls had three local evaluations (one per grouping-set branch) and one construction-time evaluation on each lazy backend | Returned A's column/name with unchanged A/B values, types, IDs and inputs; same #778 record mixing or fake local result |
| `.cols`/`.names` with contextual total-share planning | local and DuckDB; normal and reentrant controls evaluated the producer once, matching cached planning rather than the ordinary local three-branch route | A's fraction was its own total divided by 34; B had different grouping/outputs; source-name/selection cache and share requests remained A's; same audit defect |
| SQLite ordinary predicate controls | Both Grouping and `.by` were refused before the supplied predicate was evaluated; callback count zero and stable `marginplyr_error` diagnostic | Expected backend boundary: types were not reported without a query the caller had not requested. No B reentry occurred, and no extra implicit type-reading query was required |

The follow-up also read `R/summary-selections.R`'s per-call name-rule state,
selection rewriting and template evaluation. The existing share assertion
"Parent planning evaluates across arguments once" measured ordinary and share
column/name producers separately. Ordinary selection/name producers with
one-level B reentry were therefore tested independently of aggregation-body
callbacks, then with a contextual-share request holding that planning state.
Ten caught/propagated/outer-failure cases at these two additional sites retained
the required cause classes and external leaf identities and recovered through C.

Eight valid typed-selector summary cases, one inspection case, ten ordinary
summary-selection/name cases and four share-planning cases were added. The
final extracted campaign had 132 construction/value cases, 56 diagnostic
cases and eleven auxiliary/minimal cases, all completing their assertions.
Sixty-three whole-record signals in the 132 cases overlapped the same audit
mechanism; none was a discovery quota. A share-planning control could include
its cold dialect probe while a subsequent call used the legitimate session
cache: full-record inequality alone was not a defect. Independently attributable
B/A result rows or a fake local result established the classification.

The ordinary SQLite predicate refusal and a reserved `sqlite_` fixture
table-name mistake were classified as the backend boundary and fixture setup
respectively, then excluded from positive reentry cases. The final driver
omitted unsupported SQLite predicate cases while the auxiliary control checked
both refusals and zero predicate evaluations. Package-error reasons were compared with a separate ordinary B refusal,
in addition to class checks. Value/context observations were
also serialized before B to prevent a shared reference from changing both
sides of the comparison. The inner local `across()` also used grouping key
`v` and measure `w`, with different names and values from A: its permitted
`cur_column()` was asserted to be `w`, and local A resumed with `v` and its
own `n()`/vector. Normal and derived-input controls passed; immutable dtplyr
still deferred A's function until collection. An attempted literal-name
assertion for B's `cur_group()` failed in the non-reentrant Margin control
because its branch representation contained private keys; that observer
assumption was removed, rather than classified as a product defect. Existing
before/after group-context comparisons retained independent snapshots.

The complete updated code blocks below were extracted again. Construction
cases and diagnostics were replayed in fresh processes; the changed auxiliary
block was executed again; unchanged minimal sources retained their exact
successful execution identities. Both linters, applicable repository hygiene
checks, code-byte identity and one-file diff verification were refreshed.
The expanded site/state-owner map had no further concrete unexecuted priority
hypothesis within the adopted one-level workflows. No new ticket or broader
remedy was warranted by these additional observations.

## Verification and repository scope

The final note's executable R blocks were extracted outside the repository,
compared byte-for-byte with the executed code, and run with their required
setup against the unmodified archived target. Package-aware
`lintr::lint_package()` after `pkgload::load_all()` and `jarl check .` passed;
all extracted R blocks passed standalone lintr and jarl with the repository's
DPLYR/expect_not selections. Context-budget and maintained-document-reference
verifiers and `git diff --check` passed. Preservation checks found no change
to any starting tracked file or snapshotted normal dependency file.

This was a Repository-only investigation note under
`design/agents/local-checks.md`, excluded from package generation. The full
review-ready coverage/source-tarball gate was not applicable and was not run;
its absent result was not reported as a pass. No code review was run, as
recorded in the investigation PR. The committed and PR change comprised only
this Markdown file. No automatic issue-closing keyword was used.

## Reproduction after loss of the temporary directory

The following blocks are the complete final executable sources. Save each
block under its heading's basename outside the repository, or use the extractor
below. They create only temporary evidence. Start from the target archive;
do not load the working tree into the experiment. The setup copies the
installed dependency closure and installs that archive without writing the
normal library. It requires provisioned dependencies at the versions above;
it does not install or update them in the normal environment.

The extractor below is a shell setup command. Run it from the repository root;
its destination is a new temporary directory, and it extracts only the named
source blocks. `run.py` and the other drivers enumerate hypotheses rather than
imposing a numerical investigation limit. Exit zero from a known-bug minimal
example means the documented wrong audit was reproduced, not that it was fixed.

```bash
task_root=$(mktemp -d /private/tmp/marginplyr-reentrant-replay-XXXXXX)
mkdir "$task_root/source"
git archive e0cac536b68ada12563e427223d7e3efffbc7ce5 |
  tar -x -C "$task_root/source"
python3 - investigation/reentrant-operation-isolation-2026-10-01.md "$task_root" <<'PYEXTRACT'
import re, sys
from pathlib import Path
note, root = Path(sys.argv[1]), Path(sys.argv[2])
pattern = r"^### ([\w.-]+\.(?:R|py))\n\n```(?:r|python)\n(.*?)^```"
for name, source in re.findall(pattern, note.read_text(), re.M | re.S):
    (root / name).write_text(source)
PYEXTRACT
Rscript --vanilla "$task_root/setup.R" "$task_root"
python3 "$task_root/run.py"
python3 "$task_root/run-diagnostics.py"
python3 "$task_root/run-extra.py"
```

For a minimum example without the full expansion, use the isolated environment
and `Rscript --vanilla minimal.R <temporary-root> factory`, `summary`, or
`inspection`. The factory example proves two independent whole SQL rows; the
summary and inspection examples prove a local fake result. `shares_sqlite`
and `shares_duckdb` exercise contextual-share state at that same factory site.

### setup.R

```r
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(args[[1L]], mustWork = TRUE)
library <- file.path(root, "library")
dir.create(library, showWarnings = FALSE)
dir.create(file.path(root, "results"), showWarnings = FALSE)
requested <- c("dplyr", "dbplyr", "rlang", "tidyselect", "RSQLite", "duckdb",
               "dtplyr", "data.table", "arrow")
installed <- utils::installed.packages()
dependencies <- tools::package_dependencies(
  requested, db = installed, which = c("Depends", "Imports", "LinkingTo"),
  recursive = TRUE
)
packages <- unique(c(requested, unlist(dependencies, use.names = FALSE)))
packages <- packages[!packages %in% c("R", rownames(installed)[
  installed[, "Priority"] %in% c("base", "recommended")
])]
rows <- lapply(packages, function(package) {
  path <- find.package(package)
  destination <- file.path(library, package)
  if (!dir.exists(destination)) {
    stopifnot(file.copy(path, library, recursive = TRUE))
  }
  data.frame(package = package, path = path,
             version = as.character(utils::packageVersion(package)))
})
utils::write.table(do.call(rbind, rows),
                   file.path(root, "dependencies.tsv"),
                   sep = "\t", row.names = FALSE, quote = FALSE)
status <- system2(
  file.path(R.home("bin"), "R"),
  c("CMD", "INSTALL", paste0("--library=", shQuote(library)),
    shQuote(file.path(root, "source"))),
  stdout = file.path(root, "install.log"),
  stderr = file.path(root, "install.log")
)
stopifnot(status == 0L)
cat("Setup PASS:", root, "\n")
```

### minimal.R

```r
args <- commandArgs(trailingOnly = TRUE)
root <- args[[1L]]
mode <- args[[2L]]
.data <- rlang::.data
stopifnot(startsWith(find.package("marginplyr"), file.path(root, "library")))
options(marginplyr.audit_sql = TRUE)
con <- if (mode == "shares_duckdb") {
  DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
} else {
  DBI::dbConnect(RSQLite::SQLite(), ":memory:")
}
a <- tibble::tibble(g = c("a", "b"), v = c(1, 3))
b <- dplyr::copy_to(con, tibble::tibble(k = "u", v = 101), "B_source")
inner <- function() {
  marginplyr::summarize_with_margins(
    b, B_total = sum(.data[["v"]], na.rm = TRUE),
    .grouping = marginplyr::rollup("k")
  )
}
if (startsWith(mode, "shares_")) {
  a <- dplyr::copy_to(con, a, "A_source")
  b <- dplyr::copy_to(
    con, tibble::tibble(k = c("u", "v", "w"), v = c(101, 203, 307)),
    "B_shares"
  )
  state <- new.env(parent = emptyenv())
  factory <- function() {
    state$inner <- marginplyr::summarize_with_margins(
      b, B_total = sum(.data[["v"]], na.rm = TRUE),
      B_fraction = marginplyr::share_of_total(B_total),
      .grouping = marginplyr::rollup("k"), .sort = "last",
      .check_share_source = mode != "shares_sqlite"
    )
    state$inner_record <- marginplyr::last_sent_queries()
    marginplyr::rollup("g")
  }
  actual <- marginplyr::summarize_with_margins(
    a, A_total = sum(.data[["v"]], na.rm = TRUE),
    A_fraction = marginplyr::share_of_parent(A_total),
    .grouping = factory(), .sort = "last",
    .check_share_source = mode != "shares_sqlite"
  )
  record <- marginplyr::last_sent_queries()
  actual_value <- dplyr::collect(actual)
  inner_value <- dplyr::collect(state$inner)
  stopifnot(identical(as.numeric(actual_value$A_total), c(1, 3, 4)),
            isTRUE(all.equal(actual_value$A_fraction, c(0.25, 0.75, 1))),
            identical(as.numeric(inner_value$B_total), c(101, 203, 307, 611)),
            isTRUE(all.equal(inner_value$B_fraction,
                             c(101, 203, 307, 611) / 611)),
            sum(record$purpose == "result") == 2L)
  control <- marginplyr::summarize_with_margins(
    a, A_total = sum(.data[["v"]], na.rm = TRUE),
    A_fraction = marginplyr::share_of_parent(A_total),
    .grouping = marginplyr::rollup("g"), .sort = "last",
    .check_share_source = mode != "shares_sqlite"
  )
  stopifnot(identical(dplyr::collect(control), actual_value))
} else if (mode == "factory") {
  a <- dplyr::copy_to(con, a, "A_source")
  outer <- function(spec) {
    marginplyr::summarize_with_margins(
      a, A_total = sum(.data[["v"]], na.rm = TRUE), .grouping = spec
    )
  }
  control <- outer(marginplyr::rollup("g"))
  record_a <- marginplyr::last_sent_queries()
  baseline_b <- inner()
  record_b <- marginplyr::last_sent_queries()
  factory <- function() {
    inner()
    marginplyr::rollup("g")
  }
  actual <- outer(factory())
  record <- marginplyr::last_sent_queries()
  stopifnot(identical(dplyr::collect(actual), dplyr::collect(control)),
            identical(record, dplyr::bind_rows(record_b, record_a)),
            sum(record$purpose == "result") == 2L,
            identical(dplyr::collect(baseline_b)$B_total, c(101, 101)))
} else if (mode == "summary") {
  reenter <- new.env(parent = emptyenv())
  reenter$enabled <- FALSE
  aggregate <- function(x) {
    if (reenter$enabled) inner()
    sum(x)
  }
  outer <- function() {
    marginplyr::summarize_with_margins(
      a, A_total = aggregate(.data[["v"]]),
      .grouping = marginplyr::rollup("g")
    )
  }
  control <- outer()
  stopifnot(nrow(marginplyr::last_sent_queries()) == 0L)
  inner()
  record_b <- marginplyr::last_sent_queries()
  reenter$enabled <- TRUE
  actual <- outer()
  record <- marginplyr::last_sent_queries()
  stopifnot(identical(actual, control),
            identical(record[1L, ], record_b),
            nrow(record) == 2L, record$purpose[[2L]] == "result",
            is.na(record$sql[[2L]]))
} else if (mode == "inspection") {
  baseline_b <- marginplyr::inspect_grouping(b)
  stopifnot(nrow(marginplyr::last_sent_queries()) == 0L)
  reenter <- new.env(parent = emptyenv())
  reenter$enabled <- FALSE
  aggregate <- function(x) {
    if (reenter$enabled) marginplyr::inspect_grouping(b)
    sum(x)
  }
  outer <- function() {
    marginplyr::summarize_with_margins(
      a, A_total = aggregate(.data[["v"]]),
      .grouping = marginplyr::rollup("g")
    )
  }
  control <- outer()
  reenter$enabled <- TRUE
  actual <- outer()
  record <- marginplyr::last_sent_queries()
  stopifnot(identical(actual, control), nrow(record) == 1L,
            record$purpose == "result", is.na(record$sql))
}
print(record)
DBI::dbDisconnect(con)
cat("minimal", mode, "PASS\n")
```

### campaign.R

```r
state <- new.env(parent = emptyenv())
args <- commandArgs(trailingOnly = TRUE)
root <- args[[1L]]
outer_kind <- args[[2L]]
inner_kind <- args[[3L]]
site <- args[[4L]]
verb <- args[[5L]]
action <- args[[6L]]
label <- paste(args[-1L], collapse = "-")
.data <- rlang::.data
stopifnot(startsWith(find.package("marginplyr"), file.path(root, "library")))
stopifnot(!any(c("pkgload", "testthat") %in% loadedNamespaces()))
options(marginplyr.audit_sql = TRUE)
outer_data <- tibble::tibble(f = "p", g = c("a", "a", "b", "b"),
                             h = c("x", "y", "x", "x"),
                             v = c(1, 3, 10, 20))
inner_data <- tibble::tibble(k = c("u", "v", "w"), v = c(101, 203, 307))
if (site %in% c("predicate", "by_predicate")) outer_data$f <- 1L
original_a <- unserialize(serialize(outer_data, NULL))
original_b <- unserialize(serialize(inner_data, NULL))
state$connections <- list()
make_input <- function(data, kind, name) {
  if (kind == "local") return(data)
  if (kind == "dtplyr") return(dtplyr::lazy_dt(data, immutable = TRUE))
  if (kind == "arrow") return(arrow::arrow_table(data))
  con <- if (kind == "sqlite") {
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  } else {
    DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  }
  state$connections[[length(state$connections) + 1L]] <- con
  dplyr::copy_to(con, data, name = name, temporary = TRUE)
}
read_value <- function(x) {
  if (is.data.frame(x)) x else dplyr::collect(x)
}
snapshot <- function() {
  tryCatch(list(record = marginplyr::last_sent_queries()),
           error = function(cnd) list(error = cnd))
}
option_state <- function() {
  options()[intersect(c("marginplyr.audit_sql", "warn",
                        "lifecycle_verbosity", "rlib_warning_verbosity"),
                      names(options()))]
}
chain <- function(cnd) {
  if (!inherits(cnd, "condition")) return(list())
  c(list(cnd), chain(cnd$parent))
}
a <- make_input(outer_data, outer_kind, "outer_source")
b <- make_input(inner_data, inner_kind, "inner_source")
state$log <- list()
state$inner_values <- list()
state$inner_records <- list()
state$contexts <- list()
state$callback_calls <- 0L
state$reentry_enabled <- FALSE
record_event <- function(event, context = NULL) {
  entry <- list(event = event, record = snapshot(),
                options = option_state(), context = context)
  state$log[[length(state$log) + 1L]] <- entry
}
run_inner <- function(data = b) {
  if (action == "inspection") {
    marginplyr::inspect_grouping(data, .grouping = marginplyr::rollup("k"),
                                 .format = "list")
  } else if (site == "across" && inner_kind == "local") {
    local_data <- dplyr::rename(data,
                                v = tidyselect::all_of("k"),
                                w = tidyselect::all_of("v"))
    original <- unserialize(serialize(local_data, NULL))
    result <- marginplyr::summarize_with_margins(
      local_data, dplyr::across("w", inner_column_summary,
                                .names = "inner_total"),
      .grouping = marginplyr::rollup("v"), .id = "inner_set",
      .margin_label = "B_Total", .sort = "first"
    )
    stopifnot(identical(local_data, original))
    result
  } else {
    marginplyr::summarize_with_margins(
      data, inner_total = sum(.data[["v"]], na.rm = TRUE),
      .grouping = marginplyr::rollup("k"), .id = "inner_set",
      .margin_label = "B_Total", .sort = "first"
    )
  }
}
inner_column_summary <- function(x) {
  stopifnot(dplyr::cur_column() == "w", dplyr::n() == length(x))
  sum(x, na.rm = TRUE)
}
old_b <- if (action == "audit_off") options(marginplyr.audit_sql = FALSE)
base_b <- run_inner()
record_b <- snapshot()
if (action == "audit_off") options(old_b)
value_b <- read_value(base_b)
if (action != "inspection") {
  stopifnot(identical(as.numeric(value_b$inner_total), c(611, 101, 203, 307)),
            identical(as.integer(value_b$inner_set), c(2L, 1L, 1L, 1L)))
} else {
  stopifnot(identical(value_b$included, list("k", character())),
            identical(value_b$grouping_id, c(0L, 1L)))
}
enter_inner <- function(x = NULL) {
  if (!state$reentry_enabled) return(invisible(NULL))
  record_event("before B")
  old <- if (action == "audit_off") options(marginplyr.audit_sql = FALSE)
  on.exit(if (action == "audit_off") options(old), add = TRUE)
  data <- if (action == "derived") {
    stopifnot(!is.null(x))
    tibble::tibble(k = rep("derived", length(x)), v = x + 100)
  } else {
    b
  }
  out <- run_inner(data)
  current <- snapshot()
  state$inner_records[[length(state$inner_records) + 1L]] <- current
  record_event("after B")
  actual <- read_value(out)
  if (action == "derived") {
    stopifnot(identical(as.numeric(actual$inner_total),
                        rep(sum(x + 100), 2L)))
  } else {
    stopifnot(identical(actual, value_b))
  }
  state$inner_values[[length(state$inner_values) + 1L]] <- actual
  invisible(NULL)
}
trigger <- function() {
  state$callback_calls <- state$callback_calls + 1L
  record_event("callback")
  enter_inner()
  invisible(NULL)
}
factory <- function() {
  trigger()
  marginplyr::rollup("g", "h")
}
select_h <- function() {
  trigger()
  "h"
}
select_f <- function() {
  trigger()
  "f"
}
group_predicate <- function(x) {
  trigger()
  is.character(x)
}
fixed_predicate <- function(x) {
  trigger()
  is.integer(x)
}
summary_columns <- function() {
  trigger()
  "v"
}
summary_names <- function() {
  trigger()
  "outer_total"
}
label_value <- function() {
  trigger()
  "A_Total"
}
key_value <- function() {
  trigger()
  "payload"
}
format_value <- function() {
  trigger()
  "list"
}
# Recognized constructor syntax evaluates the ordinary function binding.
rollup <- function(...) {
  trigger()
  marginplyr::rollup(...)
}
context_n <- function(x) {
  if (outer_kind == "local") dplyr::n() else length(x)
}
context_column <- function() {
  if (outer_kind == "local") dplyr::cur_column() else "v"
}
summary <- function(x) {
  before <- list(x = x, n = context_n(x))
  before <- unserialize(serialize(before, NULL))
  state$callback_calls <- state$callback_calls + 1L
  record_event("callback", before)
  enter_inner(x)
  after <- list(x = x, n = context_n(x))
  stopifnot(identical(before, after), before$n == length(x))
  state$contexts[[length(state$contexts) + 1L]] <- list(
    before = before, after = after
  )
  sum(x)
}
across_summary <- function(x) {
  before <- list(x = x, n = context_n(x),
                 column = context_column())
  before <- unserialize(serialize(before, NULL))
  state$callback_calls <- state$callback_calls + 1L
  record_event("callback", before)
  enter_inner(x)
  after <- list(x = x, n = context_n(x),
                column = context_column())
  stopifnot(identical(before, after), before$n == length(x))
  state$contexts[[length(state$contexts) + 1L]] <- list(
    before = before, after = after
  )
  sum(x)
}
run_outer <- function() {
  bound_spec <- NULL
  spec <- switch(site,
    nested_constructor = rlang::quo(
      marginplyr::grouping_sets(rollup("g", "h"))
    ),
    selector = rlang::quo(
      marginplyr::rollup("g", tidyselect::all_of(select_h()))
    ),
    factory = rlang::quo(factory()),
    predicate = rlang::quo(
      marginplyr::rollup(tidyselect::where(group_predicate))
    ),
    active = rlang::quo(marginplyr::grouping_sets(bound_spec)),
    delayed = rlang::quo(marginplyr::grouping_sets(bound_spec)),
    rlang::quo(marginplyr::rollup("g", "h"))
  )
  if (site %in% c("active", "delayed")) {
    env <- rlang::env(rlang::current_env())
    if (site == "active") {
      makeActiveBinding("bound_spec", factory, env)
    } else {
      delayedAssign("bound_spec", factory(), assign.env = env,
                    eval.env = environment())
    }
    spec <- rlang::quo_set_env(spec, env)
  }
  by <- if (site == "by") {
    rlang::quo(tidyselect::all_of(select_f()))
  } else if (site == "by_predicate") {
    rlang::quo(tidyselect::where(fixed_predicate))
  } else {
    rlang::quo(NULL)
  }
  call_args <- list(.data = a, .grouping = spec, .by = by)
  if (verb == "inspect_grouping") {
    call_args$.format <- if (site == "format") {
      rlang::expr(format_value())
    } else {
      "list"
    }
  } else {
    call_args$.id <- "outer_set"
    call_args$.sort <- "last"
    call_args$.margin_label <- if (site == "label") {
      rlang::expr(label_value())
    } else {
      "A_Total"
    }
    if (action == "portable") call_args$.duplicates <- "keep"
    if (verb %in% c("nest_with_margins", "nest_by_with_margins")) {
      call_args$.key <- if (site == "key") {
        rlang::expr(key_value())
      } else {
        "payload"
      }
    }
    if (verb %in% c("summarize_with_margins", "summarise_with_margins")) {
      dot <- switch(site,
        summary = rlang::quo(summary(.data[["v"]])),
        across = rlang::quo(dplyr::across("v", across_summary)),
        summary_cols = rlang::quo(dplyr::across(
          tidyselect::all_of(summary_columns()),
          ~ sum(.x, na.rm = TRUE), .names = "outer_total"
        )),
        summary_names = rlang::quo(dplyr::across(
          "v", ~ sum(.x, na.rm = TRUE), .names = summary_names()
        )),
        rlang::quo(sum(.data[["v"]], na.rm = TRUE))
      )
      if (outer_kind == "arrow" &&
            !site %in% c("summary_cols", "summary_names")) {
        dot <- rlang::new_quosure(rlang::expr(
          sum(!!rlang::sym("v"), na.rm = TRUE)
        ), env = environment())
      }
      if (site %in% c("across", "summary_cols", "summary_names")) {
        call_args <- c(call_args, list(dot))
      } else {
        call_args$outer_total <- dot
      }
      if (action == "shared_plan") {
        call_args$outer_fraction <- rlang::quo(
          marginplyr::share_of_total(!!rlang::sym("outer_total"))
        )
      }
    }
  }
  rlang::inject(getExportedValue("marginplyr", verb)(!!!call_args))
}
base_a <- run_outer()
record_a <- snapshot()
value_a <- read_value(base_a)
control_calls <- state$callback_calls
if (verb %in% c("summarize_with_margins", "summarise_with_margins")) {
  column <- if (site == "across") "v" else "outer_total"
  stopifnot(identical(as.numeric(value_a[[column]]), c(1, 3, 4, 30, 30, 34)),
            identical(as.integer(value_a$outer_set), c(1L, 1L, 2L, 1L, 2L, 3L)))
  if (action == "shared_plan") {
    stopifnot(isTRUE(all.equal(
      value_a$outer_fraction, value_a$outer_total / 34
    )))
    if (outer_kind == "local") stopifnot(control_calls == 1L)
  }
} else if (verb == "expand_with_margins") {
  stopifnot(nrow(value_a) == 12L,
            all(table(value_a$outer_set) == 4L), sum(value_a$v) == 102)
} else if (verb == "inspect_grouping") {
  stopifnot(identical(value_a$included,
                      list(c("g", "h"), "g", character())),
            identical(value_a$grouping_id, c(0L, 1L, 3L)))
} else {
  stopifnot(nrow(value_a) == 6L,
            identical(vapply(value_a$payload, nrow, integer(1)),
                      c(1L, 1L, 2L, 2L, 2L, 4L)))
}
state$log <- list()
state$contexts <- list()
state$callback_calls <- 0L
state$reentry_enabled <- TRUE
record_event("before A")
actual <- run_outer()
record_event("after construction")
construction_calls <- state$callback_calls
actual_value <- read_value(actual)
record_event("after retrieval")
stopifnot(identical(actual_value, value_a),
          state$callback_calls == control_calls,
          state$callback_calls > 0L)
if (site %in% c("summary", "across") && outer_kind == "dtplyr") {
  stopifnot(construction_calls == 0L)
}
final_record <- snapshot()
valid_owner <- identical(final_record, record_a) ||
  identical(final_record, record_b)
record_event("before C")
third <- marginplyr::summarize_with_margins(
  tibble::tibble(c_key = c("c", "c"), c_value = c(7, 11)),
  c_total = sum(.data[["c_value"]]), .grouping = marginplyr::rollup("c_key")
)
stopifnot(identical(third$c_total, c(18, 18)), nrow(snapshot()$record) == 0L)
stopifnot(identical(read_value(a), original_a),
          identical(read_value(b), original_b),
          isTRUE(getOption("marginplyr.audit_sql")))
record_event("after C")
if (outer_kind == "duckdb" && verb %in%
      c("summarize_with_margins", "summarise_with_margins")) {
  sql <- as.character(dbplyr::sql_render(actual))
  stopifnot(grepl(if (action == "portable") "UNION ALL" else "GROUPING SETS",
                  sql, fixed = TRUE))
}
# Synthetic wrong answers calibrate the value, audit and cause comparators.
bad <- value_a[-1L, ]
stopifnot(!identical(bad, value_a))
if (!is.null(record_a$record) && !is.null(record_b$record) &&
      nrow(record_a$record) && nrow(record_b$record)) {
  mixed <- list(record = dplyr::bind_rows(record_a$record, record_b$record))
  stopifnot(!identical(mixed, record_a), !identical(mixed, record_b))
}
cause <- errorCondition("B cause", class = "inner_cause", token = "B-token")
wrapped <- errorCondition("A wrapper", parent = cause)
stopifnot(any(vapply(chain(wrapped), inherits, logical(1), "inner_cause")),
          !any(vapply(chain(errorCondition("A wrapper")), inherits,
                      logical(1), "inner_cause")))
saveRDS(list(label = label, events = state$log, valid_owner = valid_owner,
             record_a = record_a, record_b = record_b,
             final_record = final_record,
             value_a = value_a, value_b = value_b,
             inner_values = state$inner_values,
             inner_records = state$inner_records, contexts = state$contexts,
             callback_calls = state$callback_calls,
             construction_calls = construction_calls,
             session = utils::sessionInfo()),
        file.path(root, "results", paste0(label, ".rds")))
cat(label, "VALUE/INPUT/CONTEXT/C PASS; callbacks", state$callback_calls,
    "construction", construction_calls, "single-call audit", valid_owner, "\n")
for (con in state$connections) DBI::dbDisconnect(con)
```

### diagnostics.R

```r
state <- new.env(parent = emptyenv())
args <- commandArgs(trailingOnly = TRUE)
root <- args[[1L]]
site <- args[[2L]]
action <- args[[3L]]
kind <- args[[4L]]
warn_level <- as.integer(args[[5L]])
verb <- if (length(args) >= 6L) args[[6L]] else "summarize_with_margins"
label <- paste("diagnostic", site, action, kind, warn_level, sep = "-")
if (length(args) >= 6L) label <- paste(label, verb, sep = "-")
.data <- rlang::.data
stopifnot(startsWith(find.package("marginplyr"), file.path(root, "library")))
options(marginplyr.audit_sql = TRUE, warn = warn_level)
a <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 3, 10))
b <- tibble::tibble(k = c("u", "v", "w"), v = c(101, 203, 307))
original_a <- unserialize(serialize(a, NULL))
original_b <- unserialize(serialize(b, NULL))
con <- NULL
if (kind == "sql") {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  b <- dplyr::copy_to(con, b, name = "inner_diagnostic")
}
chain <- function(cnd) {
  if (!inherits(cnd, "condition")) return(list())
  c(list(cnd), chain(cnd$parent))
}
audit <- function() {
  tryCatch(marginplyr::last_sent_queries(), error = identity)
}
reference_b <- if (kind == "package") {
  tryCatch(marginplyr::inspect_grouping(b, .duplicates = "invalid"),
           error = identity)
} else {
  NULL
}
assert_package_reason <- function(cnd) {
  stopifnot(any(vapply(chain(cnd), function(cause) {
    inherits(cause, "marginplyr_error") &&
      identical(conditionMessage(cause), conditionMessage(reference_b))
  }, logical(1))))
}
leaf <- errorCondition("B cause", class = "inner_cause", token = "B-token",
                       parent = simpleError("B parent"))
outer_leaf <- errorCondition(
  "A cause", class = "outer_cause", token = "A-token"
)
leaf_bytes <- serialize(leaf, NULL)
state$events <- character()
state$observed <- list()
state$caught <- list()
state$records <- list()
state$reenter <- FALSE
state$handler_entered <- FALSE
emit <- function(event) state$events <- c(state$events, event)
inner_fun <- function(x) {
  emit("B summary")
  if (action %in% c("caught", "propagate") && kind == "external") stop(leaf)
  if (action == "messages") message("B-message")
  if (action %in% c("warnings", "inner_warnings")) {
    warning("B-warning", call. = FALSE)
  }
  sum(x)
}
run_inner <- function() {
  emit("B enter")
  on.exit(emit("B exit"), add = TRUE)
  if (kind == "package" && action %in% c("caught", "propagate")) {
    return(marginplyr::inspect_grouping(b, .duplicates = "invalid"))
  }
  expression <- if (kind == "sql") {
    rlang::quo(sum(.data[["v"]], na.rm = TRUE))
  } else {
    rlang::quo(inner_fun(.data[["v"]]))
  }
  result <- rlang::inject(marginplyr::summarize_with_margins(
    b, inner_total = !!expression,
    .grouping = marginplyr::rollup("k"), .sort = "last"
  ))
  current <- audit()
  state$records[[length(state$records) + 1L]] <- current
  if (!is.data.frame(result)) result <- dplyr::collect(result)
  stopifnot(identical(as.numeric(result$inner_total), c(101, 203, 307, 611)))
  result
}
maybe_inner <- function() {
  if (!state$reenter) return(invisible(NULL))
  emit("A before B")
  if (action == "caught") {
    tryCatch(run_inner(), error = function(cnd) {
      state$caught[[length(state$caught) + 1L]] <- cnd
      emit("B caught")
      invisible(NULL)
    })
  } else {
    run_inner()
  }
  emit("A after B")
  if (action == "outer_failure") stop(outer_leaf)
  invisible(NULL)
}
factory <- function() {
  emit("factory")
  maybe_inner()
  marginplyr::rollup("g")
}
selector <- function() {
  emit("selector")
  maybe_inner()
  "g"
}
output_name <- function() {
  emit("summary name")
  maybe_inner()
  "outer_total"
}
output_columns <- function() {
  emit("summary selection")
  maybe_inner()
  "v"
}
summary <- function(x) {
  before <- unserialize(serialize(list(x = x, n = dplyr::n()), NULL))
  emit("A summary")
  if (action == "messages") message("A-message")
  if (action %in% c("warnings", "handler", "handler_return")) {
    warning("A-warning", call. = FALSE)
  }
  if (!action %in% c("handler", "handler_return", "messages")) maybe_inner()
  if (action == "messages" && state$reenter) maybe_inner()
  stopifnot(identical(before, list(x = x, n = dplyr::n())))
  sum(x)
}
run_outer <- function() {
  specification <- rlang::quo(if (site == "factory") {
    factory()
  } else if (site == "selector") {
    marginplyr::rollup(tidyselect::all_of(selector()))
  } else {
    marginplyr::rollup("g")
  })
  parameters <- list(.data = a, .grouping = specification)
  if (verb == "inspect_grouping") {
    parameters$.format <- "list"
  } else {
    parameters$.sort <- "last"
  }
  if (verb %in% c("summarize_with_margins", "summarise_with_margins")) {
    if (site == "summary_names") {
      parameters <- c(parameters, list(rlang::quo(dplyr::across(
        "v", sum, .names = output_name()
      ))))
    } else if (site == "summary_cols") {
      parameters <- c(parameters, list(rlang::quo(dplyr::across(
        tidyselect::all_of(output_columns()), sum, .names = "outer_total"
      ))))
    } else {
      parameters$outer_total <- rlang::quo(if (site == "summary") {
        summary(.data[["v"]])
      } else {
        sum(.data[["v"]])
      })
    }
  }
  rlang::inject(getExportedValue("marginplyr", verb)(!!!parameters))
}
handler <- function(cnd) {
  state$observed[[length(state$observed) + 1L]] <- cnd
  emit("warning observed")
  if (state$reenter && action %in% c("handler", "handler_return") &&
        !state$handler_entered) {
    state$handler_entered <- TRUE
    maybe_inner()
  }
  if (action != "handler_return") invokeRestart("muffleWarning")
}
execute <- function() {
  tryCatch(withCallingHandlers(run_outer(), warning = handler,
                               message = function(cnd) {
                                 emit(paste("message observed",
                                            conditionMessage(cnd)))
                                 invokeRestart("muffleMessage")
                               }), error = identity)
}
control <- execute()
if (inherits(control, "error")) {
  stopifnot(action == "handler_return", warn_level == 2L,
            grepl("converted from warning", conditionMessage(control),
                  fixed = TRUE))
} else {
  if (verb %in% c("summarize_with_margins", "summarise_with_margins")) {
    stopifnot(identical(control$outer_total, c(4, 10, 14)))
  } else if (verb == "expand_with_margins") {
    stopifnot(nrow(control) == 6L, sum(control$v) == 28)
  } else if (verb == "inspect_grouping") {
    stopifnot(identical(control$included, list("g", character())))
  } else {
    stopifnot(nrow(control) == 3L)
  }
}
control_warnings <- state$observed
control_events <- state$events
state$events <- character()
state$observed <- list()
state$reenter <- TRUE
before_options <- options()[c("warn", "marginplyr.audit_sql")]
actual <- execute()
after_a <- audit()
if (action %in% c("propagate", "outer_failure")) {
  stopifnot(inherits(actual, "error"))
  required <- if (action == "outer_failure") {
    "outer_cause"
  } else {
    if (kind == "package") "marginplyr_error" else "inner_cause"
  }
  stopifnot(any(vapply(chain(actual), inherits, logical(1), required)))
  if (kind == "package") assert_package_reason(actual)
  if (kind == "external" && action == "propagate") {
    causes <- Filter(function(cnd) inherits(cnd, "inner_cause"), chain(actual))
    stopifnot(length(causes) == 1L, identical(causes[[1L]], leaf))
  }
} else if (action == "handler_return" && warn_level == 2L) {
  stopifnot(identical(class(actual), class(control)),
            identical(conditionMessage(actual), conditionMessage(control)))
} else {
  stopifnot(identical(actual, control))
}
if (action == "caught") {
  stopifnot(length(state$caught) > 0L)
  for (cnd in state$caught) {
    required <- if (kind == "package") "marginplyr_error" else "inner_cause"
    stopifnot(any(vapply(chain(cnd), inherits, logical(1), required)))
    if (kind == "package") assert_package_reason(cnd)
  }
}
if (action %in% c("handler", "handler_return")) {
  stopifnot(state$handler_entered, sum(state$events == "B enter") == 1L,
            length(state$observed) == length(control_warnings),
            identical(vapply(state$observed, conditionMessage, character(1)),
                      vapply(control_warnings, conditionMessage, character(1))))
}
if (action %in% c("warnings", "inner_warnings")) {
  stopifnot(sum(state$events == "B summary") == 12L,
            sum(state$events == "A summary") == 3L)
}
if (action == "inner_warnings") {
  stopifnot(length(state$observed) == 1L,
            grepl("B-warning", conditionMessage(state$observed[[1L]]),
                  fixed = TRUE))
}
if (action == "messages") {
  stopifnot(sum(state$events == "A summary") == 3L,
            sum(state$events == "B summary") == 12L,
            sum(state$events == "message observed A-message\n") == 3L,
            sum(state$events == "message observed B-message\n") == 12L)
}
stopifnot(identical(serialize(leaf, NULL), leaf_bytes),
          identical(a, original_a),
          identical(if (is.data.frame(b)) b else dplyr::collect(b), original_b),
          identical(options()[c("warn", "marginplyr.audit_sql")],
                    before_options))
state$reenter <- FALSE
# A subsequent silent public operation must work even after warn=2 failure.
third <- marginplyr::summarize_with_margins(
  a, total = sum(.data[["v"]]), .grouping = marginplyr::rollup("g")
)
stopifnot(identical(third$total, c(4, 10, 14)), nrow(audit()) == 0L)
# A comparator that only examined the outer call would miss a deleted cause.
stopifnot(any(vapply(chain(errorCondition("wrapper", parent = leaf)),
                     inherits, logical(1), "inner_cause")),
          !any(vapply(chain(errorCondition("wrapper")),
                      inherits, logical(1), "inner_cause")))
saveRDS(list(label = label, actual = actual, caught = state$caught,
             reference_b = reference_b,
             events = state$events, control_events = control_events,
             observed = state$observed, control_warnings = control_warnings,
             after_a = after_a, inner_records = state$records,
             session = utils::sessionInfo()),
        file.path(root, "results", paste0(label, ".rds")))
cat(label, "VALUE/CAUSE/OPTIONS/INPUT/C PASS; warnings",
    length(state$observed), "\n")
if (!is.null(con)) DBI::dbDisconnect(con)
```

### extra.R

```r
state <- new.env(parent = emptyenv())
args <- commandArgs(trailingOnly = TRUE)
root <- args[[1L]]
mode <- args[[2L]]
.data <- rlang::.data
stopifnot(startsWith(find.package("marginplyr"), file.path(root, "library")))
options(marginplyr.audit_sql = TRUE)
state$events <- character()
if (mode %in% c("shares", "data_table", "grouped")) {
  a <- tibble::tibble(f = "p", g = c("a", "a", "b", "b"),
                      h = c("x", "y", "x", "x"), v = c(1, 3, 10, 20))
  if (mode == "data_table") a <- data.table::as.data.table(a)
  if (mode == "grouped") a <- dplyr::group_by(a, .data[["f"]])
  before <- serialize(a, NULL)
  b <- tibble::tibble(k = c("u", "v", "w"), v = c(101, 203, 307))
  state$reenter <- FALSE
  state$contexts <- list()
  inner <- function() {
    offset <- 100
    expression <- rlang::quo(sum(.data[["v"]]) + offset)
    result <- rlang::inject(marginplyr::summarize_with_margins(
      b, total = !!expression, fraction = marginplyr::share_of_total(total),
      .grouping = marginplyr::rollup("k")
    ))
    stopifnot(identical(result$total, c(201, 303, 407, 711)),
              identical(result$fraction, c(201, 303, 407, 711) / 711))
  }
  aggregate_a <- function(x) {
    context_state <- unserialize(serialize(
      list(n = dplyr::n(), group = dplyr::cur_group(), x = x), NULL
    ))
    if (state$reenter) {
      state$events <- c(state$events, "A before B")
      inner()
      state$events <- c(state$events, "A after B")
    }
    stopifnot(identical(context_state,
                        list(n = dplyr::n(), group = dplyr::cur_group(),
                             x = x)))
    state$contexts[[length(state$contexts) + 1L]] <- context_state
    sum(x)
  }
  outer <- function() {
    offset <- 0
    expression <- rlang::quo(aggregate_a(.data[["v"]]) + offset)
    rlang::inject(marginplyr::summarize_with_margins(
      a, total = !!expression,
      parent_fraction = marginplyr::share_of_parent(total),
      total_fraction = marginplyr::share_of_total(total),
      .grouping = marginplyr::rollup("g", "h"), .id = "A_set", .sort = "last"
    ))
  }
  inner()
  control <- outer()
  state$reenter <- TRUE
  actual <- outer()
  stopifnot(identical(actual, control), identical(serialize(a, NULL), before),
            identical(actual$total, c(1, 3, 4, 30, 30, 34)),
            isTRUE(all.equal(actual$parent_fraction,
                             c(1 / 4, 3 / 4, 4 / 34, 1, 30 / 34, 1))),
            identical(actual$total_fraction, actual$total / 34))
  # Exact comparisons must reject swaps, omissions, duplicates and type changes.
  wrong <- actual
  wrong$total <- rep(711, nrow(wrong))
  stopifnot(!identical(wrong, control), !identical(control[-1L, ], control),
            !identical(control[c(seq_len(nrow(control)), 1L), ], control))
  wrong <- actual
  wrong$total <- as.integer(wrong$total)
  stopifnot(!identical(wrong, control))
  control_audit <- marginplyr::last_sent_queries()
  stopifnot(nrow(control_audit) == 0L)
  saveRDS(list(events = state$events, contexts = state$contexts,
               actual = actual),
          file.path(root, "results", paste0("extra-", mode, ".rds")))
} else if (mode == "handler_transfer") {
  a <- tibble::tibble(g = c("a", "b"), v = c(1, 3))
  b <- tibble::tibble(k = "u", v = 101)
  leaf <- errorCondition(
    "handler failure", class = "handler_cause", token = "H"
  )
  make_warning <- function(x) {
    warning("A-warning", call. = FALSE)
    sum(x)
  }
  run <- function(engine, transfer, enabled) {
    tryCatch(withCallingHandlers({
      if (engine == "margin") {
        marginplyr::summarize_with_margins(
          a, total = make_warning(.data[["v"]]),
          .grouping = marginplyr::rollup("g")
        )
      } else {
        dplyr::summarize(dplyr::group_by(a, .data[["g"]]),
                         total = make_warning(.data[["v"]]))
      }
    }, warning = function(cnd) {
      if (enabled) {
        inner <- marginplyr::summarize_with_margins(
          b, total = sum(.data[["v"]]), .grouping = marginplyr::rollup("k")
        )
        stopifnot(identical(inner$total, c(101, 101)))
        state$events <- c(state$events, "B completed")
      }
      if (transfer == "error") stop(leaf)
      rlang::return_from(run, "escaped")
    }), error = identity)
  }
  for (transfer in c("error", "return")) {
    reference <- run("dplyr", transfer, FALSE)
    control <- run("margin", transfer, FALSE)
    actual <- run("margin", transfer, TRUE)
    stopifnot(identical(reference, control), identical(actual, control))
  }
  saveRDS(list(events = state$events, actual = actual),
          file.path(root, "results", "extra-handler_transfer.rds"))
} else if (mode == "boundary") {
  a <- tibble::tibble(g = c("a", "b"), v = c(1, 3))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  b <- dplyr::copy_to(con, tibble::tibble(k = "u", v = 101), "B_source")
  inner <- function() {
    state$events <- c(state$events, "B")
    marginplyr::summarize_with_margins(
      b, B_total = sum(.data[["v"]], na.rm = TRUE),
      .grouping = marginplyr::rollup("k")
    )
  }
  inner()
  sequential <- marginplyr::inspect_grouping(a)
  stopifnot(nrow(marginplyr::last_sent_queries()) == 0L)
  outer_data <- function() {
    inner()
    a
  }
  actual <- marginplyr::inspect_grouping(outer_data())
  stopifnot(identical(actual, sequential),
            nrow(marginplyr::last_sent_queries()) == 0L)
  # Accessor-time options cannot change an already recorded call's audit flag.
  inner()
  expected <- marginplyr::last_sent_queries()
  options(marginplyr.audit_sql = FALSE)
  stopifnot(identical(marginplyr::last_sent_queries(), expected))
  options(marginplyr.audit_sql = TRUE)
  third <- marginplyr::inspect_grouping(a)
  stopifnot(identical(third, sequential))
  DBI::dbDisconnect(con)
  saveRDS(list(events = state$events, actual = actual),
          file.path(root, "results", "extra-boundary.rds"))
}
if (mode == "backend_controls") {
  input <- tibble::tibble(g = c("a", "b"), v = c(1, 3))
  lazy <- dtplyr::lazy_dt(input, immutable = TRUE)
  bad <- function(x) sum(x) + dplyr::n()
  ordinary <- tryCatch(dplyr::collect(dplyr::summarize(
    lazy, total = bad(.data[["v"]]), .by = "g"
  )), error = identity)
  margin <- tryCatch(dplyr::collect(marginplyr::summarize_with_margins(
    lazy, total = bad(.data[["v"]]), .grouping = marginplyr::rollup("g")
  )), error = identity)
  stopifnot(inherits(ordinary, "error"), inherits(margin, "error"),
            grepl("Must only be used", conditionMessage(ordinary),
                  fixed = TRUE),
            grepl("Must only be used", conditionMessage(margin), fixed = TRUE))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  sql_input <- dplyr::copy_to(con, input, "predicate_source")
  counter <- new.env(parent = emptyenv())
  counter$calls <- 0L
  predicate <- function(x) {
    counter$calls <- counter$calls + 1L
    is.character(x)
  }
  for (selection in c("grouping", "by")) {
    refused <- tryCatch({
      if (selection == "grouping") {
        marginplyr::summarize_with_margins(
          sql_input, total = sum(.data[["v"]], na.rm = TRUE),
          .grouping = marginplyr::rollup(tidyselect::where(predicate))
        )
      } else {
        marginplyr::summarize_with_margins(
          sql_input, total = sum(.data[["v"]], na.rm = TRUE),
          .by = tidyselect::where(predicate)
        )
      }
    }, error = identity)
    stopifnot(inherits(refused, "marginplyr_error"), counter$calls == 0L,
              grepl("doesn't report column types", conditionMessage(refused),
                    fixed = TRUE))
  }
  DBI::dbDisconnect(con)
  table <- arrow::arrow_table(input)
  supported <- marginplyr::summarize_with_margins(
    table, total = sum(!!rlang::sym("v"), na.rm = TRUE),
    .grouping = marginplyr::rollup("g"), .sort = "last"
  )
  stopifnot(identical(as.numeric(dplyr::collect(supported)$total), c(1, 3, 4)))
  saveRDS(list(ordinary = ordinary, margin = margin),
          file.path(root, "results", "extra-backend_controls.rds"))
}
cat("extra", mode, "PASS\n")
```

### run.py

```python
import subprocess, pathlib, os, csv
root=pathlib.Path(__file__).parent
env=dict(os.environ,R_LIBS_USER=str(root/'library'),R_LIBS_SITE='NULL')
cases=[]
verbs=['summarize_with_margins','summarise_with_margins','expand_with_margins','nest_with_margins','nest_by_with_margins','inspect_grouping']
for outer in ['local','sqlite','duckdb','dtplyr','arrow']:
 for verb in verbs:
  if outer in ['sqlite','duckdb','arrow'] and verb.startswith('nest'):continue
  for inner in ['local','sqlite']:
   cases.append((outer,inner,'factory',verb,'normal'))
for outer in ['local','sqlite','duckdb','dtplyr','arrow']:
 for site in ['selector','by','predicate','by_predicate','summary_cols','summary_names','nested_constructor','active','delayed','label']:
  if outer == 'sqlite' and site in ['predicate','by_predicate']:continue
  cases.append((outer,'sqlite',site,'summarize_with_margins','normal'))
for outer in ['local','dtplyr']:
 for site in ['summary','across']:
  for inner in ['local','sqlite']:
   cases.append((outer,inner,site,'summarize_with_margins','normal'))
  cases.append((outer,'local',site,'summarize_with_margins','derived'))
 for verb in ['nest_with_margins','nest_by_with_margins']:
  cases.append((outer,'sqlite','key',verb,'normal'))
for outer in ['local','sqlite','duckdb','dtplyr','arrow']:
 cases.append((outer,'sqlite','format','inspect_grouping','normal'))
for outer,inner,site in [('sqlite','sqlite','factory'),('sqlite','local','selector'),('local','sqlite','summary'),('duckdb','duckdb','selector')]:
 for action in ['inspection','audit_off']:
  cases.append((outer,inner,site,'summarize_with_margins',action))
for site in ['factory','selector']:
 cases.append(('duckdb','duckdb',site,'summarize_with_margins','portable'))
cases.append(('duckdb','sqlite','predicate','inspect_grouping','normal'))
for outer in ['local','duckdb']:
 for site in ['summary_cols','summary_names']:
  cases.append((outer,'sqlite',site,'summarize_with_margins','shared_plan'))
rows=[]
for case in cases:
 label='-'.join(case)
 p=subprocess.run(['Rscript','--vanilla',str(root/'campaign.R'),str(root),*case],env=env,capture_output=True,text=True)
 (root/'results'/(label+'.log')).write_text(p.stdout+p.stderr)
 rows.append([*case,p.returncode,p.stdout.strip(),p.stderr.strip()])
 print(label,p.returncode,p.stdout.strip()[-140:],p.stderr.strip()[-200:] if p.returncode else '',flush=True)
with (root/'cases.csv').open('w') as f:
 w=csv.writer(f);w.writerow(['outer','inner','site','verb','action','exit','stdout','stderr']);w.writerows(rows)

if any(row[5] != 0 for row in rows):
 raise SystemExit("A construction/value case failed; inspect cases.csv")
```

### run-diagnostics.py

```python
import subprocess, os, csv
from pathlib import Path
root=Path(__file__).parent
env=dict(os.environ,R_LIBS_USER=str(root/"library"),R_LIBS_SITE="NULL")
cases=[]
for site in ("factory","selector","summary"):
    for kind in ("external","package"):
        for action in ("caught","propagate"):
            cases.append((site,action,kind,"1"))
    cases.append((site,"outer_failure","sql","1"))
for action in ("warnings","inner_warnings","messages","handler","handler_return"):
    for warn in ("0","1","2"):
        cases.append(("summary",action,"sql" if action.startswith("handler") else "external",warn))
for verb in ("expand_with_margins", "nest_with_margins", "nest_by_with_margins", "inspect_grouping"):
    for action,kind in (("caught","external"),("propagate","external"),("outer_failure","sql")):
        cases.append(("factory",action,kind,"1",verb))
for action in ("caught","propagate"):
    for kind in ("external","package"):
        cases.append(("summary",action,kind,"1","summarise_with_margins"))
for site in ("summary_cols", "summary_names"):
    for action in ("caught", "propagate"):
        for kind in ("external", "package"):
            cases.append((site,action,kind,"1"))
    cases.append((site,"outer_failure","sql","1"))
rows=[]
for case in cases:
    label="diagnostic-"+"-".join(case)
    result=subprocess.run(["Rscript","--vanilla",str(root/"diagnostics.R"),str(root),*case],env=env,text=True,capture_output=True)
    (root/"results"/(label+".log")).write_text(result.stdout+result.stderr)
    rows.append((label,result.returncode,result.stdout.strip(),result.stderr.strip()))
    print(label,result.returncode,(result.stdout+result.stderr)[-500:],flush=True)
with (root/"diagnostic-cases.csv").open("w") as f:
    writer=csv.writer(f); writer.writerow(["case","exit","stdout","stderr"]); writer.writerows(rows)

if any(row[1] != 0 for row in rows):
    raise SystemExit("A diagnostic case failed; inspect diagnostic-cases.csv")
```

### run-extra.py

```python
import subprocess, os, csv
from pathlib import Path
root = Path(__file__).parent
env = dict(os.environ, R_LIBS_USER=str(root / "library"), R_LIBS_SITE="NULL")
rows = []
for script, modes in [("extra", ["shares", "data_table", "grouped", "handler_transfer", "boundary", "backend_controls"]),
                      ("minimal", ["factory", "summary", "inspection", "shares_sqlite", "shares_duckdb"])]:
    for mode in modes:
        result = subprocess.run(["Rscript", "--vanilla", str(root / (script + ".R")), str(root), mode],
                                env=env, text=True, capture_output=True)
        (root / "results" / (script + "-" + mode + ".log")).write_text(result.stdout + result.stderr)
        rows.append((script, mode, result.returncode))
        print(script, mode, result.returncode, flush=True)
with (root / "extra-cases.csv").open("w") as file:
    writer = csv.writer(file)
    writer.writerow(["script", "mode", "exit"])
    writer.writerows(rows)
if any(row[2] != 0 for row in rows):
    raise SystemExit("An auxiliary/minimal case failed; inspect its log")
```
