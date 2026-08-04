# Review disposition ledger

Every observation from a recorded review of the 0.1.0 stabilization work is
listed here as **fixed** or **rejected**, with a test name, command, or
reproduction result behind it. This is what makes "review complete" mean
something other than "the reviewer stopped talking": a suggestion that was not
implemented has to say why, in a form the next reader can check.

A recorded review is one whose findings were written down in the issue tracker
— an issue comment, a pull-request body, or a commit message that names what
review found. Findings raised and fixed inside one commit before it was pushed
are not separately listed; they left no trace to reconcile and the commit is
the record.

Rejecting an observation is a normal outcome. A review suggestion is a
hypothesis about the code, and several of the ones below were disproved by
coverage that already existed. Defensible design is not rewritten to make a
review comment go away.

Adding an entry: give the observation in one sentence, name its axis, state
the disposition, and point at something executable. "Looks fine" is not
evidence; a test title, a `git` command, or a reproduced result is.

---

## 1. The pre-release review (produced #23)

A two-axis review plus a Docs & Tests audit of the 0.1.0 tree at
`1c782eb` produced the 47 user stories of #23. Each cluster of observations
became a blocker-aware ticket, and the ticket's merged work is its disposition.
All are **fixed**; each entry names coverage that executes the contract through
the public interface, or the command that reproduces the result.

**Displayed Margin labels corrupt local cardinality matching for
non-character dimensions** — #27. `test-parent-share.R` "default Margin labels
preserve typed grouping identity" and "Parent identity separates missing keys
from displayed margins".

**Parent shares must hold for every documented dimension storage type** —
#27. `test-parent-share.R` "direct Parent shares use the immediate rollup
parent" and "three-dimension Parent shares advance one rollup level".

**Genuine missing grouping values must stay distinct from omitted
dimensions** — #27. `test-margin-id.R` ".id distinguishes source missing
values from margins".

**Adversarial column names can collide with implementation temporaries** —
#27. `test-parent-share-backends.R` "lazy Parent-share staging avoids
adversarial user-name collisions".

**Duplicate occurrences must be retained while parent lookup skips them** —
#27. `test-parent-share-backends.R` "lazy Parent shares skip duplicate
grouping-set occurrences".

**Fixed `.by` partitions must not share denominators, including on empty
input** — #27. `test-parent-share-backends.R` "lazy Parent shares preserve
empty-input root and partition behavior".

**Ineligible source types and non-scalar sources must raise a targeted
error** — #24, #27. `test-parent-share.R` "Parent-share sources are numeric
scalar summaries", "semantic and nonnumeric classes are not Parent-share
sources", "cardinality errors identify the affected Parent-share request".

**dtplyr silently coerces an ineligible source or emits Cartesian rows** —
#28. `test-parent-share-backends.R` "dtplyr validates Parent-share source
types during collection", "dtplyr rejects every ineligible Parent-share source
type", "dtplyr rejects non-scalar Parent-share sources on collection", "dtplyr
integer and double Parent shares match local results".

**Arrow cannot preserve the condition contract through its execution
bridge** — #29. `test-parent-share-backends.R` "Arrow rejects Parent shares
before constructing a query", "Arrow ordinary Margin summaries remain lazy and
available", "Arrow Parent-share planning errors precede backend rejection";
the reasoning is recorded in ADR 0010's amendment.

**General dbplyr must stay lazy with no validation-only probe** — #30.
`test-parent-share-backends.R` "general dbplyr leaves incompatible summary
types to execution" and "general dbplyr reports static Parent-share errors
without probing".

**`.grouping` is evaluated through two paths** — #25. `test-parent-share.R`
"Parent planning evaluates the grouping expression once", "Parent planning
preserves every public-call environment", "Parent planning evaluates across
arguments once".

**`across()` diagnostics conflate duplicate, unknown, and predicate cases** —
#25. `test-parent-share.R` "Parent-share across classifies source-name
failures", "Parent-share across rejects non-name contracts", "Parent-share
across does not infer bare predicate symbols", "caller function symbols are
not inferred to be predicates".

**Package errors are inferred from third-party message text** — #24, #31,
#32. `test-parent-share.R` "Parent-share execution preserves user-expression
conditions".

**Package-created errors need a stable base class, external ones their
provenance** — #31, #32, #33. `test-grouping-interface.R` "the documented
`marginplyr_error` handler catches conditions" and "Grouping tidyselect
conditions retain their provenance", plus the "use the package condition seam"
tests in six files.

**Local cardinality validation rescans the input once per Grouping set** —
#27. Validation is wrapped into the ordinary summary by
`wrap_parent_sources()`, so no separate pass exists to scale;
`dev/benchmark-parent-share-local.R` and
`investigation/parent-share-local-benchmark.md` carry the measurement.

**Suggested packages are used without complete guards; a dependency-only
check fails** — #34. `_R_CHECK_DEPENDS_ONLY_=true R CMD check` on the built
tarball went from 3 ERRORs to status OK, and `release-matrix.yaml`'s
`depends-only` job runs it on every push.

**Unused Suggests inflate installation cost** — #34. `data.table`, `ggplot2`,
`purrr`, `sessioninfo`, and `stringr` removed after a repository-wide usage
audit; nothing promoted to Imports.

**Supported-backend claims are not backed by live execution** — #35, #38.
`test-parent-share-backends.R` "RSQLite executes portable Parent shares end to
end" and "DuckDB Parent shares agree across native, portable, and local
paths", executed by the four `backend` jobs.

**Reference documentation duplicates contracts that can drift** — #36. All
three tests in `test-documentation.R`.

**Citation, NEWS, Quarto requirement, and CRAN comments disagree with the
release** — #37. `inst/CITATION`, `NEWS.md`, `DESCRIPTION`'s
`SystemRequirements`, and `cran-comments.md` as of 9874313.

**A green fully provisioned check is not a release gate on its own** — #38.
`.github/workflows/release-matrix.yaml` checks one built tarball through a
`depends-only` job, five `tarball` jobs, and four `backend` jobs.

**Architecture, ADRs, and the disposition record lag the implementation** —
#39. `design/architecture.md`, ADR 0014, the ADR 0001, 0009, and 0010
amendments, and this file.

The same review's remaining requirement — a fresh two-axis review with no
unresolved findings — is #40 and is deliberately not answered here. This
ledger records dispositions; it does not certify the next review.

## 2. Disproved claims from the pre-release review

These were reported as gaps and are **rejected**: the coverage already existed.
#23 puts them out of scope explicitly so they cannot return as busywork, and
#35 re-verified each one rather than taking the earlier judgement on trust.

**Contextual Grouping helpers have no outside-context tests** (Docs & Tests).
**Rejected — disproved.** `test-grouping-interface.R` "grouping helpers
validate their context and columns" calls `grouping_bit()` and `grouping_id()`
at top level and asserts both the message and the `marginplyr_error` class;
`test-parent-share.R` "share_of_parent reports its required context" does the
same for the third helper and additionally asserts that `conditionCall()` names
`share_of_parent`. All three contextual helpers are covered.

**Zero-row results have no integer `.id` test** (Docs & Tests).
**Rejected — disproved.** `test-margin-id.R` "zero-row results retain an
integer .id column" runs all four Margin verbs over a zero-row input and
asserts `nrow() == 0L` and `identical(result$set, integer())` for each, so the
column is proved present *and* integer-typed. "empty ungrouped nest_by retains
its sole occurrence identifier" covers the `nest_by` one-row empty case.

**Factor returns are defective** (Spec).
**Rejected — no observable gap.** No factor-return defect reproduces at the
public interface. `test-margin-label.R` "factor NA levels and missing values
follow the eight-case contract" walks the whole ADR 0012 table; "NA factor
levels stay structural when collision checks are disabled" covers the
`.check_margin_label = FALSE` clause; "dtplyr applies mixed named labels
lazily and restores factors" and "DuckDB uses typed missing for a missing
factor Margin label" cover factor returns from lazy backends. Returned columns
are factors with the documented levels on every backend tested.

**A custom printer must be removed** (Standards).
**Rejected.** The package's one custom printer is
`print.margin_grouping_spec`, which prints a Grouping specification — an input
value the caller constructed — and nothing else carries one. The related
question about *results* was decided in the opposite direction and separately:
ADR 0013 requires `inspect_grouping()` to return a plain tibble with no custom
class or printer, and `test-inspect-grouping.R` covers it.

## 3. `/code-review` of the #32 condition migration (9644e0b)

Recorded on #39 and #33.

**Summary tidyselect conditions no longer carry a marginplyr prefix** (Spec).
**Rejected.** #32 removed a `tryCatch` that re-signalled tidyselect failures
with a marginplyr message. The criterion the reviewer applied governs
package-created errors; a propagated tidyselect failure is an External
condition, which #23 user story 27 and ADR 0015 both require to keep its
original class. *Evidence:* `test-summarize-operation.R` "summary tidyselect
conditions retain their provenance" and `test-grouping-interface.R` "Grouping
tidyselect conditions retain their provenance" assert
`expect_identical(class(error), class(baseline))`.

**`abort_dbplyr_representation()` should have stayed an internal assertion**
(Spec). **Fixed in #33.** Initially rejected on the grounds that the message
was written for a user — which describes message style, not a rule. ADR 0015
supplied the rule: a Package condition is raised exactly when the caller can
avoid it by rewriting the call, and no rewrite of a
`summarize_with_margins()` call avoids a change in dbplyr's internal
`lazy_query` structure. The guard reverted to bare `stop()`, and the
`expect_s3_class(error, "marginplyr_error")` assertion #32 had added was
removed; the message assertion stays. *Evidence:*
`abort_dbplyr_representation()` in `R/grouping-adapter-native.R` is a bare
`stop()` carrying a comment that names ADR 0015;
`test-grouping-backends.R` "native SQL reports incompatible dbplyr query
representations".

The second half of the same observation — that the guard carries no public
Margin call — stands as **rejected, inherent**. `dbplyr::sql_build()` runs at
print or collect time, after the Margin verb has returned and its call frame is
gone. It is also moot: a bare `stop()` makes no public-class promise.

**Snapshot files gained a trailing blank line** (both axes).
**Rejected, no action.** This is the current testthat's canonical snapshot
format, not an edit. *Reproduction:* `git checkout tests/testthat/_snaps/`
followed by `testthat::test_local(filter = "parent-share")` reproduces the
identical trailing newline, so any subsequent commit that runs the suite
carries it instead.

**`abort_marginplyr()` class arguments are inconsistent across migration
batches** (Standards). **Fixed in #33.** 45 sites passed
`class = "simpleError"` and 34 omitted it, tracking which ticket migrated the
site rather than any semantic difference. ADR 0015 settled it: `simpleError`
was a transitional shim from f7ac9e7, the resulting object claimed to be both
`simpleError` and `rlang_error`, and 0.1.0 has no released handlers to keep
compatible. *Evidence:* `grep -rn simpleError R/` returns nothing;
`test-grouping-interface.R:940` asserts `expect_false(inherits(caught,
"simpleError"))`.

**The `abort_marginplyr()` / `stop()` boundary is documented nowhere**
(both axes). **Fixed in #33.** Both review axes flagged independently that no
ADR, comment, or contributor doc recorded the rule separating the migrated
sites from the four surviving bare `stop()` calls. *Evidence:* ADR 0015 states
the avoidability predicate; `R/conditions.R` carries it at the only
constructor; `?marginplyr` documents the `marginplyr_error` contract for
users; `design/architecture.md` has a Package conditions section.

**`match_margin_choice()` duplicates each option vocabulary** (Standards).
**Fixed in #33.** `match.arg(x)` derived choices from the formal default;
the explicit-choices helper wrote each vocabulary twice, and the two copies
could drift with no test failing. *Evidence:*
`margin_duplicates_choices` and `margin_label_position_choices` in
`R/margin-operation.R` are the single vocabularies, and
`test-grouping-interface.R` "documented option formals match the shared choice
vocabularies" holds every public formal to the constant it mirrors.

**ADR 0009's documentation-placement statement conflicts with #36**
(Docs & Tests). **Fixed in #39.** #36 centralized the Grouping-identity
comparison table in the `grouping_bit()` reference, which contradicts ADR
0009's statement that the article holds the table and each reference keeps a
compact copy. *Evidence:* the amendment section in ADR 0009;
`test-documentation.R` "the Grouping-identity comparison has exactly one
canonical home".

## 4. Reviews during the documentation and release tickets

**A broad mid-test `skip_if_not_installed()` silently swallowed coverage for
the backends listed after it** (Docs & Tests, found during #34).
**Fixed in #34.** The skip masked one ungated SQLite render that a
dependency-only check would otherwise have caught, so the test file looked
green while proving less than it claimed. *Evidence:* the guards were narrowed
to the branch that needs them; running the suite against a library with only
RSQLite removed produced 0 failures and explicit skips, and the ungated render
became visible.

**`attachment::att_amend_desc()` cannot express conditionally used Suggests**
(Standards, raised during #34, tracked as #44). **Fixed in #44.** The tool
statically scans `R/` and promotes anything it finds to Imports — including
`arrow::schema()` sitting behind a backend-kind guard in
`R/backend-metadata.R` — which would violate #23's "no optional package is
promoted to Imports solely to make checks pass". *Evidence:* no
`pkg_ignore`/`extra.suggests` setting fixes it, because the false positive is
inherent to static scanning; `dev/config_attachment.yaml` was deleted and
`AGENTS.md`'s "Dependency metadata" section documents the manual audit that
replaces it, naming `_R_CHECK_DEPENDS_ONLY_` as the authority on optionality.
Nothing outside this ledger names the deleted config, and `DESCRIPTION` was
unchanged by its removal.

**`_R_CHECK_DEPENDS_ONLY_` described as already wired into CI** (Spec, PR #47).
**Fixed before merge.** The new `AGENTS.md` section claimed a CI gate that #38
had not yet built. *Evidence:* the merged text named it as not yet wired and
pointed at #38; it now describes `release-matrix.yaml`'s `depends-only` job,
which exists.

**A two-line comment duplicated at three sites explained the linter rather
than the code** (both axes, PR #49). **Fixed before merge.** Extracting
`dtplyr_lambda_pronoun()` put the reason on one definition and let each call
site say what the pronoun is. *Evidence:* `R/parent-share.R:909`; three
`# nolint: object_usage_linter` comments deleted; `R CMD check` reports no
`.x` NOTE in either check mode.

**`AGENTS.md` describes a `.lintr` file this repo does not have** (Standards,
PR #49 review). **Fixed in #48** (commit f98abdb), spun out rather than
widening that PR.

**Site-verification markers pinned phrases the rewritten prose no longer
renders** (Docs & Tests, PR #51). **Fixed before merge.**
`.github/scripts/verify-site.R` still checked for the removed Database-backends
table and for an empty-input table that had moved to `share_of_parent()`.
*Evidence:* commit 8670ad4, verified against a full local
`altdoc::render_docs()` build and then running the script against it, matching
what the altdoc CI job does.

**`grouping_bit()` claimed its return type "follows the backend"** (Spec, found
during #36). **Fixed.** Native `GROUPING()` is reached only when the plan runs
as native `GROUPING SETS`, so PostgreSQL falls back to the portable literal
under `.duplicates = "keep"`. *Evidence:* `man/grouping_bit.Rd`;
`test-grouping-backends.R` "PostgreSQL duplicate keep falls back
conservatively".

**`share_of_parent()`'s backend table conflated always-local checks with
type and cardinality checks** (Spec, found during #36). **Fixed.** The
reference now separates the syntax, source, dependency, and naming rules
validated locally on every backend from the type and cardinality rules only
some backends can prove — the distinction ADR 0010 already made. *Evidence:*
`man/share_of_parent.Rd`; ADR 0010's amendment section.

**`MARGINPLYR_REQUIRED_SUGGESTS` proves installation, not execution**
(Spec, PR #53). **Fixed before merge.** A contract test that was renamed,
deleted, or skipped for an unrelated reason still left a `backend` job green.
*Evidence:* each `backend` matrix entry names the tests it proves, and
`.github/scripts/verify-backend.R` fails the job unless every one ran and
passed. It immediately caught a real gap — `expect_snapshot()` skips under
CRAN semantics, so "Arrow rejects Parent shares before constructing a query"
had never executed inside `R CMD check`. The `backend` jobs now set `NOT_CRAN`;
the CRAN-emulating jobs deliberately do not.

**`R-CMD-check.yaml` asked for snapshot artifacts it can never produce**
(Docs & Tests, PR #53). **Fixed.** `upload-snapshots: true` implied coverage
that matrix does not have. *Evidence:* commit cb450a7 removes the request and
records where snapshots actually run.

Five further findings from the same review, all **fixed before merge** in
commit 7c5e38f: remote CRAN incoming checks turned off, because their NOTEs
depend on the network and the clock and each would have been annotated as
unexpected; the "checking CRAN incoming feasibility" header no longer treated
as an understood NOTE, which would have classified every future incoming
finding as understood; `<test>.Rout.fail` read alongside `<test>.Rout`, so a
failing suite is not reported as a suite that never ran; both verifier steps
run on failure, where their diagnosis is worth most; and `ci-helpers.R`
extracted for the summary writing and check-directory handling all three
scripts had grown a copy of.

**`AGENTS.md`'s release-matrix claim was false once the workflow existed**
(Standards, PR #53). **Fixed in the same PR.** *Evidence:* the "Release
matrix" section describes the merged workflow and names the three edit sites
an added backend needs.

## 5. Lint-environment findings (#41, #45)

**149 of 161 `object_usage_linter` suppressions existed only because the
linter ran without the package loaded** (Standards). **Fixed in #41.**
*Reproduction:* `lintr::lint_package()` reported 158 `object_usage_linter`
lints; `pkgload::load_all(".")` first reduced that to 12. Installing the
package was not sufficient — it must be loaded. *Evidence:* `AGENTS.md`
records the requirement, `lint.yaml` loads before linting, and the
suppressions are gone.

**A `# nolint` on a roxygen line leaks into user-facing `.Rd` help text**
(Docs & Tests). **Fixed in #41.** *Evidence:* the surviving roxygen `# nolint`
comments are markdown table rows only, where roxygen discards whatever follows
the row's final `|`; `document.yaml` fails CI on any leak into `man/`.

**Three surviving suppressions are genuine NSE artifacts** (Standards).
**Rejected for `grouping_sets_sql` and `new_levels`; amended and fixed for
`.x`.** The first two are values read only from a glue string or an NSE
pronoun that `codetools` cannot follow, which is the correct use of an inline
suppression. The `.x` disposition was **amended by #45**: that expression also
produced an `R CMD check` NOTE, which a `# nolint` cannot suppress, so the
suppression never settled the finding — and `rlang::sym(".x")` returns an
`identical()` symbol while showing static analysis only a string, making it a
spelling choice rather than an irreducible artifact. *Evidence:*
`codetools::checkUsage()` silent on all three rewritten functions;
`R CMD check` `Status: OK` from the tarball in both normal and
`_R_CHECK_DEPENDS_ONLY_=true` modes.

## 6. CRAN observations

**Dependency-only check fails in examples, tests, and vignettes.**
**Fixed in #34.** *Command:* `_R_CHECK_DEPENDS_ONLY_=true R CMD check` on the
built tarball, 3 ERRORs → status OK. Re-verified against a library with only
RSQLite removed. Now a gate rather than a manual step.

**`no visible binding for global variable '.x'` NOTE in every check mode.**
**Fixed in #45.** *Command:* `R CMD check` from the tarball reports
`checking R code for possible problems ... OK` in both modes.

**New-submission NOTE.** **Accepted and recorded.** It is expected for a first
submission and is the only NOTE the fully provisioned run reports.
*Evidence:* `cran-comments.md`, which records both gates with their actual
results — `_R_CHECK_DEPENDS_ONLY_=true` status OK, 1083 pass / 68 skip, and
fully provisioned `--as-cran` 1 NOTE, 1487 pass / 3 skip.

**Snapshot skips inside `R CMD check`.** **Accepted and explained.** testthat
skips `expect_snapshot()` under CRAN semantics; the three skips in the
provisioned run are exactly those. They are not lost coverage, because the
`backend` jobs run them with `NOT_CRAN` set. *Evidence:* `cran-comments.md`
names the skips; `release-matrix.yaml` explains the split.

**Quarto vignette engine behavior without the Quarto binary.**
**Accepted and documented.** The engine writes a placeholder rather than
failing, so a check run without Quarto has no vignette coverage.
*Evidence:* verified empirically by rebuilding the vignettes with Quarto off
the `PATH`; recorded in `cran-comments.md` so a reviewer is not misled by a
pass.

## 7. Review of this ticket's own changes (#39)

The two-axis review of the reconciliation commit found several places where
the new prose misdescribed the code — an entry-point list that omitted
`wrap_parent_sources()`, an adapter table that credited the lazy non-SQL
adapter with a join the local adapter takes too, a test-seam list missing
`test-documentation.R`, and two recorded observations with no ledger row. All
were fixed before the commit was pushed, so they are the commit rather than
entries here. One finding was rejected, and it is the kind most likely to be
raised again.

**The lazy non-SQL Parent-share adapter is a Middle Man** (Standards).
`execute_non_sql_parent_shares()` is a strict subset of
`execute_local_parent_shares()`: both call `apply_joined_parent_shares()` with
`sql_join = FALSE`, and local merely runs `check_local_parent_share_types()`
first. Two of the three adapters therefore have near-identical bodies, which
the reviewer read as undercutting ADR 0014's own rejection of one adapter per
backend kind.

**Rejected.** The three-adapter split is a decision of the parent spec, not an
artifact: #23 states "There are three genuine Parent-share adapters: local,
general dbplyr, and lazy non-SQL", and pre-empts collapsing them on size —
"New pass-through helpers are not introduced merely to reduce line count" and
"File count and line count are not design goals". Merging is also not
behaviour-preserving in either direction: folding the lazy non-SQL kinds into
the local adapter would run `check_local_parent_share_types()` against a
result dtplyr has not materialized, and folding them into the dbplyr adapter
would build a `sql_on` condition with no connection to build it from. The
emptiness of the third adapter is the contract — a lazy non-SQL backend joins
as local data does but cannot have its sources type-checked here, because its
validation already happened inside the ordinary summary.

*Evidence:* `R/parent-share.R` `parent_share_adapter()` and the three adapter
bodies; ADR 0014's Decision section states the same reasoning as a decision
rather than a defence, so a future reader meets it before the code. The
contracts that would break under either merge are
`test-parent-share-backends.R` "dtplyr validates Parent-share source types
during collection" and "dtplyr integer and double Parent shares match local
results".

---

## 8. The final CRAN-readiness review (#40)

A fresh two-axis review of `1c782eb...HEAD` plus a separate Docs & Tests audit,
run against the tree at `bd0b3b3`. The local gates are clean and are not
findings; they are the baseline the findings below sit against.

*Commands, all run on R 4.6.1, aarch64-apple-darwin23, Quarto 1.9.38, against
one `R CMD build` tarball:* `pkgload::load_all(".")` then
`lintr::lint_package()` → 0 lints. `roxygen2::roxygenise()` → no change to
`man/` or `NAMESPACE` (`git status --porcelain` empty). `R CMD check --as-cran`
→ **Status: OK**, 1506 pass / 3 skip. `_R_CHECK_DEPENDS_ONLY_=true R CMD check
--as-cran` on the same tarball → **Status: OK**, 1102 pass / 68 skip, with
arrow, duckdb, dtplyr, and RSQLite all withheld.
`spelling::spell_check_package()` → no errors.

*CI evidence:* release-matrix run
[30889837726](https://github.com/sayuks/marginplyr/actions/runs/30889837726) on
`bd0b3b3`, all 11 jobs green. The three tarball jobs #40 names — ubuntu
`release`, `devel`, `oldrel-1` — each report `Status: OK`, so zero ERRORs and
zero WARNINGs, as do macOS and Windows at release and the `depends-only` job.
The four `backend` jobs each report one NOTE, dispositioned below, and each
`verify-backend.R` step passed, so all twelve named contracts ran.

### Findings

**Every ubuntu release-matrix job restores a fully provisioned package library
from a shared cache, so no job in the workflow runs with the optional backends
actually withheld** (Docs & Tests). **Fixed — #64, PR #68.**
`setup-r-dependencies@v2` sets `restore-keys` to the prefix
`<os>-<R version>-<arch>-<cache-version>-`, and `R-CMD-check.yaml` and all four
`release-matrix.yaml` jobs shared `cache-version: 3`, so the fully provisioned
library that `R-CMD-check.yaml` saves was restored into jobs that ask for hard
dependencies only. *Evidence for the finding:* in run 30889837726, job `Tarball
ubuntu-latest (release)` logs `Cache hit for restore-key: ...-3-bb148ad6...`
after missing its own primary key `2f5979dd...`; the `Session info` step of
every ubuntu job lists `arrow 25.0.0`, `dtplyr 1.3.3`, `duckdb 1.5.5`, and
`RSQLite` among 122–124 packages in `/home/runner/work/_temp/Library`; and
`Live RSQLite` newly installs only `rcmdcheck`, `RSQLite`, `sessioninfo`, and
`testthat` on top of what the cache supplied. Three documented claims were
therefore false of what ran: `release-matrix.yaml`'s "Optional backends are
deliberately absent from this matrix", its "Every other optional backend is
absent on purpose: proving one backend in isolation also proves it does not
depend on the others", and the R-devel rationale in both that file and
`R-CMD-check.yaml` that arrow and duckdb "are simply not part of this job". No
contract was unproven — `verify-backend.R` passed in all four jobs — but
backend independence was, and the #57 R-devel separation was not what those
runs demonstrated. `depends-only` was unaffected, because
`_R_CHECK_DEPENDS_ONLY_=true` restricts the library at check time and
`verify-depends-only.R` confirmed all four backends skipped.

The fix gives each dependency request its own `cache-version` — `full-1` for
the provisioned jobs, `hard-1` for `depends-only` and `tarball`,
`backend-<name>-1` for each of the four `backend` jobs, which had shared one
prefix with each other as well as with the provisioned matrix. Disabling the
cache on the isolation-critical jobs was the alternative and was not taken:
separate prefixes cost nothing and `cache: false` would spend every run
reinstalling the hard dependency closure. A cache key is not self-checking, so
the disposition is not the key change but the gate beside it:
`.github/scripts/verify-library-isolation.R` reads the job's own
`MARGINPLYR_REQUIRED_SUGGESTS` declaration and fails before the check when a
requested backend is absent or an unrequested one is on `.libPaths()`.
`check-tarball.R` sources it rather than the workflow calling it as a step,
which is what makes the criterion's "cannot regress silently" hold: a step can
be deleted, and deleting it would restore exactly this finding, whereas every
job that checks the tarball necessarily makes the assertion. That covers
`depends-only`, all five `tarball` jobs, and all four `backend` jobs — the ten
that claim a backend is absent — and it also refuses a `required` package that
`optional_backends()` does not track, so adding a fifth backend without
registering it fails rather than going unchecked. The two isolation claims and
both copies of the R-devel rationale are rewritten to what the jobs install.

*Evidence for the fix:* release-matrix run
[30907216394](https://github.com/sayuks/marginplyr/actions/runs/30907216394) on
`0a6363e`, all 11 jobs green. `Tarball ubuntu-latest (release)` now logs `Cache
not found for input keys: ...-hard-1-2f5979dd..., ...-hard-1-` — the same
primary key as before, with a restore-key prefix that no longer reaches the
provisioned library — and its isolation report lists all four backends absent,
as do `depends-only` and the macOS and Windows tarball jobs. Each `backend` job
reports its own backend and the other three absent: `Live Arrow` arrow 25.0.0,
`Live DuckDB` duckdb 1.5.5, `Live dtplyr` dtplyr 1.3.3, `Live RSQLite` RSQLite
3.53.3, each at `/home/runner/work/_temp/Library`. All twelve named contracts
still ran, and `depends-only` reports 1107 passed with 66 skipped.
`Tarball ubuntu-latest (devel)` is the case the rewritten R-devel rationale
turns on, and this is the first run where it was actually withheld from the
provisioned cache: it missed both keys, built the whole set from source under
R 4.7.0, and reported `Status: OK` in 10m12s against a `timeout-minutes: 60` —
against the over-55-minute arrow build that `R-CMD-check.yaml` avoids. The
gate's
states — nothing requested and nothing installed, a requested backend absent,
an unrequested one present, the mixed case, and an untracked `required`
package — were exercised locally against a controlled `R_LIBS_USER`, as was the
ordering that stops the job before `check-tarball.R` looks for a tarball.

**`cran-comments.md` reports test counts the current tree does not produce**
(Docs & Tests). **Not fixed — blocking, #65.** The file
records "1487 tests pass" for the provisioned run and "1083 tests pass" for the
dependency-only run; the tarball built from `bd0b3b3` produces 1506 and 1102.
The counts were accurate when #37 wrote them and drifted as #55, #56, and #57
added tests. *Evidence:* the two `R CMD check` commands above. The skip
accounting in the same file still holds: the dependency-only run skips 64 tests
for a missing optional package plus 2 for no supported lazy backend and 2 for
CRAN snapshot semantics.

**`investigation/r-devel-binary-compatibility.md` states live configuration in
the present tense the notes' own rule forbids** (Standards). **Not fixed —
split to a new ticket.** `investigation/README.md` lines 41–42: "Do not write
bare `now`, `currently`, or `today`." The note written by `0a2842c` does so at
lines 110, 128, 237, 252, and 253 — including the heading "The failure mode is
a clean error today". This is the failure #61 already corrected once in a
neighbouring note. *Evidence:* `grep -nE '\b(currently|today)\b'
investigation/r-devel-binary-compatibility.md`.

**Two new `# nolint` comments carry no expression-specific reason**
(Standards). **Not fixed — #66.** `AGENTS.md`: "every one in
R code sits next to a comment stating the expression-specific reason."
`tests/testthat/test-nest-operation.R:154` and `:158` suppress
`line_length_linter` with nothing beside them, and neither needs the
suppression — the `key_missing` case at `:161` already wraps the same
`quote()` shape across lines. *Evidence:* the two lines; the wrapped sibling
seven lines below is the available fix.

**A new test name uses a term the glossary bans** (Standards). **Not fixed —
split to a new ticket.** `CONTEXT.md:11` lists `_Avoid_: Grouping expression`
under Grouping specification. `tests/testthat/test-parent-share.R:1389` is
`test_that("Parent planning evaluates the grouping expression once", ...)`, and
§1 of this file quotes that name. Renaming the test also breaks nothing in
`release-matrix.yaml`, which does not name it. *Evidence:* `grep -rn "grouping
expression" R tests vignettes design CONTEXT.md` — the two hits in
`vignettes/database_backends.qmd` predate `1c782eb` and are outside this diff.

**`apply_joined_parent_shares()` discovers local state with `exists()`**
(Spec). **Not fixed — #66.** #23's Implementation Decisions:
"Fragile local-state patterns are removed: initialize optional locals
explicitly." `R/parent-share.R:1682` and `:1687` test
`exists("right_join_names", inherits = FALSE)` and
`exists("join_key_names", inherits = FALSE)` to learn whether the
`length(child_ids) > 0L` branch ran. Behaviour is correct — the fallback is
`character()` — and the guard is already half redundant, since line 1643
assigns `right_join_names <- character()` explicitly in the other branch.
*Evidence:* the two `exists()` calls; initialising both above the branch is the
fix, and `test-parent-share-backends.R` "lazy Parent-share staging avoids
adversarial user-name collisions" covers the cleanup path either way.

**The eligible-type diagnostic exists twice, and one copy drops its structured
fields** (Spec). **Not fixed — #66.**
`check_parent_scalar()` (`R/parent-share.R:1020`, message at `:1039`) attaches
`parent_output`, `source_summary`, and `call` to the "requires source summary
... to be a plain
integer or double scalar" error. `check_local_parent_share_types()`
(`R/parent-share.R:1820`, called from `:1522`) rebuilds the same message with a
bare `abort_marginplyr()`. The class contract of US26 still holds — both
inherit `marginplyr_error` — but the handler-visible fields and the call
vanish on the local path. *Evidence:* the two message bodies are
character-identical apart from the operands; `test-parent-share.R`
"Parent-share sources are numeric" asserts the message, not the fields, which
is why the drift survived.

**No test asserts structurally that per-Grouping-set input rescans are gone**
(Spec). **Not fixed — #66.** #23's Testing Decisions:
"Assert the removal of Grouping-set-proportional full input scans
structurally; do not put wall-clock thresholds in package tests." The
benchmark half exists (`dev/benchmark-parent-share-local.R` and
`investigation/parent-share-local-benchmark.md`), but nothing under
`tests/testthat/` fails if a future change reintroduces a pass per Grouping
set. US42 is currently guarded only by a developer script that CI does not
run. *Evidence:* `grep -rn "rescan\|single pass"
tests/testthat/` returns nothing.

**One `skip_if_backend_absent()` call reverses the argument order a CI gate
depends on** (Standards). **Not fixed — #66.**
`optional_backends()` in `.github/scripts/ci-helpers.R`, which #64 moved there
from `verify-depends-only.R`, excludes DBI
because `skip_if_backend_absent("duckdb", "DBI")` "skips on
the first missing package, so a `{DBI} is not installed` line never appears".
`tests/testthat/test-margin-label.R:295` calls it as `("DBI", "duckdb")`. The
gate still passes, because `test-margin-id.R` and `test-expand-operation.R`
call it duckdb-first and supply the required `{duckdb} is not installed`
line — so the convention the script documents is unenforced, not broken.
*Evidence:* the dependency-only run above skips 14 tests for `{duckdb}`.

### Rejected

**The `retail_sales` class change, ADR 0016, and the R-devel/P3M work are
scope creep beyond #23** (Spec). **Rejected.** Each arrived as its own
triaged, approved, and closed ticket raised after the #24–#40 decomposition:
#55 and #56 for the data frame class and ADR 0016, #57 and #61 for the R-devel
CI contract, #60 for the investigation-note policy. #23's Further Notes
anticipate this: "The implementation should be split into blocker-aware
tracer-bullet tickets before coding." A finding that work was done is not a
finding that it was unapproved. *Evidence:* `gh issue view 55 56 57 60 61`,
all closed with merged PRs #58, #59, #62, and #63.

**Tests call `tibble::tibble()` without a Suggests guard** (Spec).
**Rejected.** #23 asks that "Suggested packages are guarded at every
executable use", and `tests/testthat/test-inspect-grouping.R:16`,
`test-grouping-interface.R:923`, and `test-grouping-backends.R:603` call
`tibble::tibble()` bare. tibble is not optional at those call sites: dplyr, an
Import, declares `Imports: tibble (>= 3.2.0)`, so tibble is inside the hard
dependency closure `_R_CHECK_DEPENDS_ONLY_` builds. *Evidence:* the
dependency-only check above is `Status: OK` with those three tests executing,
and `Rscript -e 'packageDescription("dplyr")$Imports'` names tibble. The
DESCRIPTION entry is worth revisiting only if dplyr ever drops it, which is the
manual audit's job under `AGENTS.md`, not a guard's.

### NOTEs

**New submission.** **Accepted.** Expected for a first release and the only
NOTE a provisioned `--as-cran` run reports once CRAN's incoming checks are
reachable. Neither local run above reproduces it, because both set
`_R_CHECK_CRAN_INCOMING_REMOTE_=false`, and both report `checking CRAN incoming
feasibility ... OK`. *Evidence:* `check-tarball.R`'s `understood_notes` records
it; `cran-comments.md` states it.

**`checking top-level files ... NOTE Files 'README.md' or 'NEWS.md' cannot be
checked without 'pandoc' being installed`, in the four `backend` jobs.**
**Accepted.** Those jobs deliberately omit `setup-pandoc`, because their
toolchain is scoped to what a backend contract needs and they run with
`--ignore-vignettes`. It is a property of the runner, not the tarball: the same
tarball checked with pandoc present reports `checking top-level files ... OK`
in every `tarball` job and in both local runs. *Evidence:* run 30889837726,
`Status: 1 NOTE` in each backend job against `Status: OK` in each tarball job.

**Days since last update.** **Accepted, not reached.** Recorded in
`check-tarball.R` for a resubmission during a review cycle; no run has emitted
it.

---

## Status

Every observation from every recorded review is dispositioned above. The
release gate #23 sets is not met by this file alone: it also requires a fresh
two-axis review and Docs & Tests audit with no unresolved findings, which is
#40. New findings from that review are dispositioned here as they arrive.

**The gate is not met as of #40's review.** Local checks, the release matrix,
and the two-axis review of package behaviour are clean, and no finding above
changes a result marginplyr returns. What is not clean is the evidence layer
the gate is written in terms of. Two of its three tickets are still open:
`cran-comments.md` states counts the tree no longer produces (#65), and the
seven smaller standards and spec findings are #66. The third, #64, is fixed —
the release matrix now withholds the optional backends it documents
withholding, and a gate fails the workflow if it stops doing so. #40 stays open
until #65 and #66 close, and no submission is made before then.
