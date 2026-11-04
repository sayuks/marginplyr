# Always-loaded instruction ownership audit (#1202)

Investigated: 2026-11-04

## Evidence boundary

The starting revision was `82d0f300711997a16a5fd575a3ba43fe47074751`,
fetched from main before creating the dedicated worktree. The audit read
`CLAUDE.md`, its complete `AGENTS.md` reference, `CONTEXT.md`, and the
ownership contracts at the destinations below before editing them. It covered
the four candidate sections named by #1202, and checked the other sections for
interacting ownership and retrieval paths. It did not re-run the earlier
removals.

Prior decisions read in full were issues [#284], [#293], [#294], [#295],
PR [#329] and its review record, and PR [#327] and its review record.
In particular:

- #284 established a measured closure ratchet, rather than a target size.
- #293 left the one-backend policy in `AGENTS.md`; #294 left guard routing and
  its assertions there because `inst/suggests/guard.R` explicitly assigned
  that authority.
- #295 and #329 retained the manual dependency audit because `DESCRIPTION`
  could carry neither the rule nor a pointer to it. The review restored a
  removed Imports rule: a scanner's observation was not sufficient evidence
  for promoting a dependency.
- #329's site-render commands stayed because the verifier did not produce its
  own input and the CI installation sequence did not state the local one.

The audit treated these as constraints, rather than using their earlier byte
reductions as permission for another move.

[#284]: https://github.com/sayuks/marginplyr/issues/284
[#293]: https://github.com/sayuks/marginplyr/issues/293
[#294]: https://github.com/sayuks/marginplyr/issues/294
[#295]: https://github.com/sayuks/marginplyr/issues/295
[#327]: https://github.com/sayuks/marginplyr/pull/327
[#329]: https://github.com/sayuks/marginplyr/pull/329

## Paragraph accounting

The identifiers in the first column name the starting text, not line numbers.
Mixed paragraphs are split by meaning so that moving an explanation cannot
silently move its rule. “Keep” means no edit to that passage.

| Candidate | Classification and disposition | Owner and evidence |
|---|---|---|
| Linting: “Before pushing” | Rule required before publication; keep. | `design/agents/local-checks.md` required both linters in both scopes. |
| Linting: namespace resolution and false undefined globals | Duplicated argument; delete this copy and add a retrieval trigger. | `.github/workflows/lint.yaml`, comment beside *Lint*, already explained resolution and prescribed `pkgload::load_all()` before lintr. |
| Linting: installing was insufficient | Unique reference; move to the same *Lint* comment. | The workflow prescribed loading but did not explicitly distinguish installation. The trigger was “Before running or diagnosing lint”. The required load order stayed in the workflow and Local checks. |
| Linting: `.lintr`, expression-specific `# nolint`, environment repair, named linter and adjacent reason | Rules needed before choosing configuration or suppression; keep. | No subject owner already held all these obligations. `design/agents/local-checks.md` owned invocation, not suppression policy. |
| Linting: roxygen leakage, table-row exception, reference links and generation check | Rule with the reference needed to apply its exception; keep. | The workflow did not hold the exception or link examples. Disclosure would need a new owner or split a rule from the exception an R-code edit could need immediately. No such move was justified. |
| Documentation: generated files, regeneration commands, commit outputs and CI comparison | Rules needed before choosing an edit target; keep. | The workflow implemented the comparison, but agents needed to know to edit generation sources before touching `man/` or `README.md`. |
| Documentation: README reach and excluded source | Duplicated argument; delete this copy. | `.github/workflows/document.yaml`, comment beside *Regenerate README.md*, already stated GitHub/site reach and why tarball checking could not detect the stale source/output pair. |
| Documentation: `build_readme()` installation order | Unique reference; move beside *Regenerate README.md*. | The workflow owned generation order but lacked this fact. The AGENTS trigger required reading it before rendering `README.qmd`. The citation in `tools/cran-release.md`, *Create the release candidate*, was redirected to the new owner. |
| Documentation: stable Quarto, bundled Pandoc, logged versions, stale output versus writer drift, regeneration remedy | Duplicated argument and procedure; delete this copy. | `.github/workflows/document.yaml` already held renderer setup, *Record README renderer versions*, and *Check README.md is up to date*, including the remedy in its diagnostic. The new trigger also covered diagnosing generated-documentation diffs. |
| Dependency metadata: hand audit, advisory-only `attachment`, conditional Arrow use, scanner false results and investigation citation | Must remain; keep. | #295/#329 placed the decision here. The release runbook's *Dependency-metadata advisory audit* was release-specific and could not replace the general rule for an ordinary dependency edit. |
| Dependency metadata: direct and bare-name grep, fields and timing of audit | Rule needed before a dependency edit; keep. | `DESCRIPTION` was DCF and supplied no pointer. No other general edit owner held the manual audit. |
| Dependency metadata: depends-only authority and release-matrix gate | Must remain; keep. | This distinguished the question a grep could not answer; the workflow ran after editing and was not a replacement for that distinction. |
| Dependency metadata: transitive Suggest, imported closure, direct declaration and `DBI = FALSE` | Rule with conditional evidence; keep. | The helper's `optional_suggest_spec()` comments explained the DBI case; this paragraph supplied the general declaration rule and lookup command. #329 deliberately retained its connection here. |
| Dependency metadata: unused Suggest and `Config/Needs/website` | Must remain; keep. | The altdoc workflow owned the constraint argument and re-check, already reached by the existing pointer. The surviving paragraph explained the other limit of package checking; no further duplicate removal was established. |
| Code comments: two kinds and header contract | Rules needed before writing comments in `R/`; keep. | Internal functions had no roxygen contract elsewhere. Moving this behind a lint-only trigger would miss code editing. |
| Code comments: inline invariant, order, backend behavior and suppression reasons | Rules needed before writing comments in `R/`; keep. | No other owner held this division; enforcement documentation did not own authoring policy. |
| Code comments: citation versus local consequence, ADR reasoning and non-coordinate targets | Rules needed across maintained documents; keep. | These applied beyond R-code edits. `design/architecture.md`, *Repository placement* and *Answering a review*, explicitly referred back here rather than owning them. |
| Code comments: present tense and exclusion of review history | Rule needed before editing comments; keep. | The commit and tests were the specified owners of history and coverage claims, not a new comment-policy file. |
| Code comments: claim length, delete false claims, preserve required contract/reason | Rules needed before editing or answering review; keep. | *Answering a review* referred back to this policy, so moving it there would reverse ownership. |
| Code comments: no scan and hand audit | Deliberate enforcement decision; keep explicitly. | AGENTS retained “These rules are audited by hand”; the changed pointer did not authorize a gate or transfer the authoring rules. |
| Code comments: semantic scan limits, blank-line workaround, namespace absence, `srcref`, installed-source absence and forbidden skips | Unique reference; move to *Structural gates* in `design/architecture.md`. | That section already owned what namespace gates could observe and why source-dependent skips were invalid, but lacked the comment-specific limits. The trigger was “Before changing comment enforcement”. |

The remaining sections were left in place: Local checks and Always-loaded
context routed every completion and context edit; Installation instructions
governed claims in ordinary documentation; Rejected calls governed executable
examples; Site verification retained the local installation/render sequence;
Optional-dependency guards and Release matrix retained their declared rules;
Verifier invocation and Queries against a lazy input retained their structural
and public-contract obligations; Agent skills supplied the entry pointers.
Their references already disclosed the subject-specific mechanisms. No new
ownership evidence justified reopening #293/#294 or those sections.

No rule changed owners. Workflow commands, package behavior, manual-audit
obligations and prose policy were unchanged. The only newly disclosed passage
in architecture described enforcement limits, beside the existing mechanism;
the two workflow additions supplied facts their adjacent steps needed. No
separate maintained document was introduced.

## Representative entry paths

These were reading exercises before the relevant action, not claims that a
package test could enforce prose meaning:

| Task presented to the entry | Path exercised | Rules reached before action |
|---|---|---|
| Edit an internal R function and its header | `CLAUDE.md` → `AGENTS.md`, *Code comments* and *Linting*; before lint, follow the new *Lint* pointer | Header/inline distinction, local citation consequence, suppression reason, both linters, and package loading before lintr. An enforcement change additionally reached *Structural gates* and its comment-specific limit. |
| Edit roxygen and README generation sources | `CLAUDE.md` → `AGENTS.md`, *Linting* and *Documentation* → document workflow setup, *Regenerate documentation*, *Regenerate README.md* and comparison steps | Edit sources, roxygen suppression exception, regenerate/commit outputs, working-tree installation, stable Quarto with bundled Pandoc, logged versions and drift remedy. |
| Add an optional package or move a dependency field | `CLAUDE.md` → `AGENTS.md`, *Dependency metadata*, *Optional-dependency guards* and *Release matrix* → guard header, optional-Suggest table and altdoc field comment as applicable | Manual semantic audit before editing, advisory-only scanner, conditional use, direct declaration despite transitive availability, version-aware guards and both registration sites. The entry did not require knowing to open a release runbook. |

The read exercises found the moved text at the named step or heading. The
release runbook's changed citation reached the README installation comment.
Each trigger named an action before which its reference was needed; none used
an `@` include, so the reference did not join the always-loaded closure.

## Measurement and verification

At the starting revision, `Rscript .github/scripts/verify-context-budget.R`
reported `CLAUDE.md` 11 bytes plus `AGENTS.md` 21,240 bytes, total 21,251,
within the existing 22,005-byte baseline. After this audit's changes the files
measured 11 plus 19,972 bytes, total 19,983: a reduction of 1,268 bytes.
The baseline was lowered to 19,983 in the same commit. The added pointers were
paid for by those reductions, without adding any behavioral restriction.

The closure traversal, missing-reference checks, mechanism assertions and
document-reference verifier were retained. Change-specific verification used
their existing command interfaces, plus checks that workflow executable YAML
was unchanged:

- The budget verifier passed at 19,983 of 19,983 bytes. In a disposable copy,
  one extra byte failed with `over by 1`, a dangling `@` reference failed with
  `which no file answers`, and an entry with no `@` failed the reference-reader
  mechanism assertion. Each rejected
  copy exited with status 1; the unchanged copy exited with status 0.
- The document-reference verifier resolved all 314 paths across 428 maintained
  documents, retaining its seven justified exemptions.
- R's YAML parser produced identical executable structures before and after
  both workflow comment edits. The changed R budget script parsed, and
  `git diff --check` passed.

Package build/check, complete suite and snapshots were omitted
under `source`: the changed workflow lines were comments; the budget change
only tightened a repository-hygiene bound; no package or dispatch input changed.
The pinned Review-ready outcome belonged in the PR verification record rather
than being inferred from this note's measurements.
