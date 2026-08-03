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

## Status

Every observation from every recorded review is dispositioned above. The
release gate #23 sets is not met by this file alone: it also requires a fresh
two-axis review and Docs & Tests audit with no unresolved findings, which is
#40. New findings from that review are dispositioned here as they arrive.
