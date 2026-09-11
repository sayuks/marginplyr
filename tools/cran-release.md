# CRAN release playbook

This is the durable release authority for marginplyr. A local Codex agent
normally operates the commands, interprets their evidence, and updates one
English GitHub issue per release. A human can follow the same procedure without
Codex. The process prepares one candidate commit and one retained source
tarball, proves them, leaves CRAN submission to the maintainer, and follows the
published release for 72 hours.

The release is not resumable state hidden in a script. The repository, the
external preflight bundle, and the release issue are the records. After an
interruption, read them, verify the candidate identity again, and continue at
the first unchecked stage whose evidence still belongs to that candidate.

## Work and approval at a glance

| Kind | Meaning | Examples |
|---|---|---|
| Automatic | A command computes or checks deterministic local state. It may run without approval when read-only. | helper preview, local preflight, publication lookup |
| Agent-operated | Codex runs a documented command, reads its result, and summarizes it. A human can run the same command instead. | CI inspection, semantic review, R-hub inspection |
| Approval-gated | Codex stops immediately before changing tracked or external state and states the exact target and action. | tracked edits, commit/push, workflow dispatch, issue update, win-builder send, tag/Release, PR |
| Human-only | Codex may prepare evidence but cannot perform the action. | CRAN upload, confirmation/rejection/follow-up email |

The helper previews tracked edits by default. `--apply` is an explicit local
mechanism, not approval: an agent must still obtain human approval immediately
before using it. The same applies to direct `git`, `gh`, R-hub, and web actions.
There is no global `--yes` setting or approval database.

## Roles and durable artifacts

- [`tools/cran-release.R`](cran-release.R) makes only deterministic preparation
  and post-publication edits and performs a read-only CRAN publication lookup.
  It is stateless and requires a clean worktree.
- [`tools/cran-preflight.R`](cran-preflight.R) remains the directly invoked,
  non-mutating candidate gate. It neither calls the release helper nor performs
  an external release action.
- One release issue is the current checklist and evidence ledger. It records
  durable summaries and URLs, never credentials, authentication material, raw
  local logs, or an unexpired private win-builder result URL.
- The repository-external preflight bundle holds the canonical candidate
  tarball, raw logs, and machine-readable evidence. Keep it until monitoring is
  complete and the GitHub Release assets have been verified.

Every approval request must name the operation, exact branch/SHA/file or remote
target, and expected effect. Approval for one checkpoint does not authorize a
later one.

## Initial release and update release

Both use the same stages below. `Config/marginplyr/cran-status` selects the
genuinely different work:

| Concern | Initial release (`unpublished`) | Update (`published`) |
|---|---|---|
| Preparation version | Already at the first release version | Advance the `.9000` development version to the chosen release version |
| CRAN comments | Explain `New submission` | Give the update rationale and all current NOTE dispositions |
| External evidence | No existing CRAN results or reverse dependencies | Inspect current CRAN flavors; run reverse-dependency checks when applicable and record downstream communication |
| Publication-only edit | Change status to `published`; add CRAN install text and badge | Preserve the already-published status and CRAN text |
| Follow-up version | `<release>.9000` | Corresponding `<release>.9000` development version |

The publication-only initial edits never run merely because CRAN accepted an
upload. They run only after the public CRAN source index contains the expected
version.

## Helper interface

All mutating operations preview first:

```sh
Rscript tools/cran-release.R prepare --version 0.1.0
Rscript tools/cran-release.R prepare --version 0.1.0 --apply

Rscript tools/cran-release.R verify-publication --version 0.1.0

Rscript tools/cran-release.R post-release --version 0.1.0
Rscript tools/cran-release.R post-release --version 0.1.0 --apply
```

`prepare` changes only deterministic version and NEWS headings. The maintainer
still writes release notes, metadata, and `cran-comments.md`. `post-release`
first verifies CRAN publication, then conditionally adds the initial-only CRAN
README text, sets the development version, adds its NEWS heading, and regenerates
`README.md` with the repository-pinned Pandoc 3.10.1. It installs the edited tree
into a disposable library for rendering, so the README cannot load an older
installed marginplyr.

If a helper refuses dirty or unexpected state, inspect `git status --short` and
the named file. Do not bypass the refusal. If `--apply` is interrupted or a
generation command fails, inspect `git diff`; either fix the named prerequisite
and rerun after committing the intended partial edit, or restore only the
helper-owned files from the disposable release branch and retry. Never reset an
unrelated working tree.

## Complete sequence

### 1. Inspect prerequisites and state

Purpose: establish whether this is an initial or update release and whether the
local and hosted tools can produce valid evidence.

Prerequisites: repository root, clean default branch, `gh` authenticated,
current release/R-patched R, Quarto, Pandoc 3.10.1, TeX, all `Suggests`, and
`Config/Needs/preflight` installed. Show the branch and remote update to be
integrated and obtain approval before synchronizing with `git pull --ff-only`.

Agent-operated, read-only commands:

```sh
git status --short
git branch --show-current
sed -n '1,80p' DESCRIPTION
sed -n '1,80p' NEWS.md
sed -n '1,160p' cran-comments.md
gh auth status
gh run list --limit 20
Rscript .github/scripts/verify-context-budget.R
```

For an update, also open the package's current CRAN check page and inspect every
flavor before choosing the version. Determine whether reverse dependencies now
exist and record the required check/communication plan.

Success evidence: clean status, current default branch, valid publication state,
version and NEWS that agree, authenticated GitHub access, and a written list of
missing prerequisites or none. Block on unexplained dirty state, an invalid
publication field, an unresolved current CRAN ERROR/WARNING/NOTE, or a missing
required tool. Recovery: install/fix prerequisites or move work into a clean
disposable worktree; rerun the inspection. A human fallback is to run and read
the same commands and CRAN page.

### 2. Create the release checklist issue

Purpose: create the one-run checklist and evidence ledger. Posit's
`create-release-checklist` and `usethis::use_release_issue()` are checklist
inputs, not workflow engines or runtime dependencies.

Approval-gated external action: after showing the proposed English title and
body, create it with:

```sh
release_issue_body=$(mktemp)
# Copy the ledger template below into "$release_issue_body" and adapt it.
gh issue create \
  --title "Release marginplyr 0.1.0 to CRAN" \
  --body-file "$release_issue_body"
```

Use the template under [Release issue ledger](#release-issue-ledger). Success
evidence is the issue URL and an unchecked “Next human action” naming stage 3.
Block if an open issue already owns the same version, the version is unsettled,
or approval is absent. Recovery: edit the existing issue after approval rather
than creating a duplicate. Human fallback: create the issue in GitHub's UI with
the same title and template.

For every later ledger change, first read the current body, edit a temporary
copy, show the exact proposed diff, and obtain approval before the external
update:

```sh
release_issue_body=$(mktemp)
gh issue view <issue-number> --json body --jq .body > "$release_issue_body"
# Edit the temporary copy, then show its diff against the fetched body.
gh issue edit <issue-number> --body-file "$release_issue_body"
```

The human fallback is the issue's Edit action in GitHub's UI after reviewing
the same proposed checkbox/evidence change.

### 3. Prepare and merge the release PR

Purpose: make version, NEWS, `cran-comments.md`, metadata, and generated files
reviewable without disturbing the maintainer's normal checkout.

Prerequisites: stages 1–2 complete and a chosen version. Create a disposable
branch/worktree after approval:

```sh
git fetch origin
git worktree add ../marginplyr-release-0.1.0 -b release/0.1.0 origin/main
cd ../marginplyr-release-0.1.0
Rscript tools/cran-release.R prepare --version 0.1.0
```

Show the preview. Obtain approval for the exact tracked edits, then run:

```sh
Rscript tools/cran-release.R prepare --version 0.1.0 --apply
```

#### Dependency-metadata advisory audit

Before making a tracked dependency-metadata edit, run
`attachment::att_amend_desc()` only on a disposable copy of the exact clean
preparation worktree. It is an advisory scan, not a metadata editor: never run
it in the release worktree and never copy its mutated `DESCRIPTION` back. The
preparation commit SHA is the clean worktree's `HEAD`; record it before making
the copy. A later change that can affect dependency usage requires a new audit
from its own clean preparation commit.

The copy and its evidence stay outside the repository and release issue. The
issue ledger records their durable location and summary rather than raw local
logs. A release agent records the preparation commit SHA, `attachment` version,
the exact invocation, console output (including its exit status), and resulting
DESCRIPTION diff. For example:

```sh
preparation_sha=$(git rev-parse HEAD)
git status --short
test -z "$(git status --short)"
audit_root=$(mktemp -d)
git clone --no-hardlinks . "$audit_root"
(
  cd "$audit_root" || exit 1
  test "$(git rev-parse HEAD)" = "$preparation_sha"
  Rscript -e 'packageVersion("attachment"); attachment::att_amend_desc(document = FALSE, must.exist = FALSE, check_if_suggests_is_installed = FALSE, use.config = FALSE)' \
    > attachment-console.txt 2>&1
  printf 'attachment exit status: %s\n' "$?" >> attachment-console.txt
  git diff -- DESCRIPTION > attachment-DESCRIPTION.diff
)
```

Partial scanner output or evaluation errors are not a clean result.
Preserve them in the console output, label the scan `partial/error` in the
evidence, and do not read an empty or partial diff as evidence that a dependency
is unused. Block if the dependency-audit evidence is incomplete, including a
missing preparation identity, tool version, invocation, console output, or
`DESCRIPTION` diff.

The release agent then semantically reviews every proposed addition, removal,
and field move. Inspect direct and bare-name usage across package code, tests,
examples, vignettes, installed files, optional-dependency guards,
`VignetteBuilder`, and repository-only tooling fields such as
`Config/Needs/website`. For each proposal, record one disposition — `remove`,
`keep`, `reclassify`, or `scanner false result` — with repository evidence and
an explanation of whether the dependency is genuinely optional. Dispositions
must be accepted before making any tracked dependency-metadata edit; a proposed
change without acceptance remains unchanged.

Write/review NEWS, `cran-comments.md`, DESCRIPTION metadata, examples, and any
other necessary release content. Regenerate after source edits, using the
repository authorities:

```sh
Rscript -e 'roxygen2::roxygenise()'
R CMD INSTALL .
Rscript -e 'rmarkdown::render("README.Rmd", quiet = TRUE)'
git diff --check
git diff
```

Confirm `quarto pandoc --version` reports 3.10.1 before the README command. The
working tree is installed first because its chunks load marginplyr, as
`AGENTS.md` documents. Obtain separate approvals before commit, push, and PR
creation:

```sh
git add <reviewed-paths>
git commit -m "Prepare marginplyr 0.1.0 for CRAN (#507)"
git push -u origin release/0.1.0
gh pr create --fill
```

Success evidence: reviewed PR, generated files equal their sources, required PR
checks green, and merge commit on the default branch. Block on unresolved
metadata/content, generated diffs, failing checks, or missing approval. Recovery:
amend the branch and rerun affected generation/checks; if abandoned, remove only
this disposable worktree/branch. Human fallback: make the same edits and PR by
hand, using helper preview as a checklist.

### 4. Freeze the candidate SHA

Purpose: bind every later artifact and result to one merged commit.

After the preparation PR is merged, show the remote/default-branch target and
obtain approval before the switch/pull. Then run:

```sh
git switch main
git pull --ff-only
candidate_sha=$(git rev-parse HEAD)
git status --short
printf '%s\n' "$candidate_sha"
```

This is read-only after the pull. Success evidence is a 40-character SHA on the
clean default branch, recorded in the release issue after approval. Block if the
preparation PR is not merged or status is dirty. Recovery: synchronize or use a
clean worktree at the merge commit, then recompute. Human fallback is the same
commands or GitHub's merged-commit page.

Any later tracked change to metadata, documentation, examples, dependencies or
guards, executable source, or `cran-comments.md` creates a new candidate and
invalidates all applicable evidence below.

### 5. Run the exact-tarball local preflight

Purpose: produce and check the canonical source tarball without changing the
candidate worktree.

Choose a durable absolute directory outside the repository:

```sh
Rscript tools/cran-preflight.R \
  --output /absolute/external/path/marginplyr-0.1.0-preflight
```

This automatic local gate is specified in
[`tools/cran-preflight.md`](cran-preflight.md). Success evidence: exit 0, the
candidate SHA, retained tarball path, SHA-256 manifest, passing/allowed result
summary, and identical before/after worktree records. Record only the SHA,
digest, and durable summary in the issue after approval; keep raw evidence
external. Block on every nonzero exit. Recovery follows the preflight summary;
fix candidate defects through a new PR/SHA, or repair tooling/infrastructure and
rerun into a new external directory. Human fallback is the same command.

### 6. Require CI for the candidate

Purpose: prove the pushed candidate in each existing CI owner; hosted archives
are bound to the commit but are not assumed byte-identical to the retained
tarball.

Inspect without changing remote state:

```sh
gh run list --commit "$candidate_sha" \
  --json databaseId,name,event,status,conclusion,headSha,url
```

Require routine R CMD check, documentation, lint (including both release
contract verifiers), site, coverage, and the release matrix's release/devel/
oldrel, depends-only, suite-coverage, library-isolation, and live backend jobs.
If no strict manual release-matrix result exists for this SHA, obtain approval
before dispatch:

```sh
gh workflow run release-matrix.yaml --ref main
gh run list --workflow release-matrix.yaml --limit 5
gh run watch <run-id> --exit-status
```

Verify `headSha` with `gh run view <run-id> --json headSha,conclusion,url`.
Success evidence is every required green URL at the candidate SHA. Block on a
failure, cancellation, SHA mismatch, or missing job. Recovery: rerun a transient
infrastructure failure after approval and record both outcomes; a package change
requires a new PR and restarts at stage 4. Human fallback: dispatch and inspect
the Actions UI.

### 7. Perform the semantic CRAN review

Purpose: review requirements deterministic checks cannot decide. This is a
non-mutating agent review; every finding needs a human disposition.

#### Dependency-audit candidate confirmation

First reopen the Stage 3 dependency-audit evidence and compare every recorded
dependency disposition with the exact candidate. Confirm that every accepted
`remove`, `keep`, `reclassify`, or `scanner false result` is reflected in the
candidate `DESCRIPTION`, or explicitly remains unchanged where no tracked edit
was accepted. Record candidate-file evidence for each row. Block if the
dependency-audit evidence or a Stage 7 disposition is missing; no dependency
metadata may pass this stage by an unrecorded scanner result.

Use the commit-pinned Posit `cran-extrachecks` rubric at
[`b58a92e`](https://github.com/posit-dev/skills/blob/b58a92e7c479b7795f4f003490b046c01e345fce/r-lib/cran-extrachecks/SKILL.md)
as a checklist. CRAN policy and marginplyr repository rules take precedence.
Record that rubric SHA and the candidate SHA. Review:

- DESCRIPTION Title/Description, software/package naming, URLs, acronyms,
  `Authors@R`, copyright holder role, license, and bundled license;
- meaningful runnable exported examples and every `\dontrun{}`/`\donttest{}`;
- references for implemented methods/algorithms and citation form;
- user/global state, filesystem, external-resource, and parallelism risks;
- `cran-comments.md`, optional-backend explanations, and installation text;
- for updates, update rationale, current CRAN results, reverse-dependency
  evidence, and downstream communication.

Each finding cites a candidate file/location and an authority, or says
“heuristic.” Before updating the issue, show the proposed finding table and get
approval. Success evidence is a disposition of `fix`, `accept`, or `not
applicable` for every item. Concrete policy conflicts, missing required content,
and reproducible defects block. Heuristics remain advisory but cannot be left
undispositioned. Recovery: fix through a PR/new candidate or record the approved
reason for acceptance. Human fallback: open the pinned rubric and perform the
same file-by-file review.

Repeat this stage only when a later change touches DESCRIPTION, NAMESPACE, `R/`,
manual sources, examples, vignettes, README, NEWS, licensing, dependencies or
guards, or `cran-comments.md`.

### 8. Dispatch one targeted R-hub v2 check

Purpose: cover one current R-devel Linux environment not duplicated by routine
CI. Prerequisites: stages 5–7 green/dispositioned, pushed candidate SHA, existing
R-hub workflow, authenticated GitHub access, and `rhub::rhub_doctor()` passing.

Resolve by capability each release rather than assuming a stale alias:

```r
platforms <- rhub::rhub_platforms()
platforms[, c("name", "type", "os_type", "os_name", "r_version")]
```

Choose one container whose OS is non-Ubuntu Linux and whose R is R-devel; at the
time this manual was written, `atlas` satisfied that rule. If none exists,
choose the current Ubuntu-clang R-devel container. Do not add no-Suggests,
sanitizer, LTO, valgrind, `rchk`, or all-platform checks without a concrete new
risk. Show the resolved platform and branch, obtain approval, then run:

```r
rhub::rhub_check(platforms = "<resolved-name>", branch = "main")
```

Inspect the resulting Actions run and verify its `headSha` equals the candidate.
Success evidence: SHA, resolved image/platform, date, green result URL. Block on
SHA mismatch, unavailable current target, or check failure. Recovery: choose the
documented fallback or rerun transient infrastructure after approval; a package
fix restarts at stage 4. Human fallback: run the same R calls interactively and
inspect Actions.

### 9. Send the retained tarball to win-builder R-devel

Purpose: test the exact candidate archive on CRAN-like Windows-devel
infrastructure. Prerequisites: stages 5–8 complete and the external bundle still
intact.

Verify identity first:

```sh
shasum -a 256 /absolute/path/marginplyr_0.1.0.tar.gz
cat /absolute/path/SHA256SUMS
```

Show the exact path and matching digest and obtain approval before the external
send. A human or approved agent browser then opens
`https://win-builder.r-project.org/upload.aspx`, chooses R-devel, and selects
that exact retained tarball. Add release/oldrelease only for a concrete
Windows-specific risk or missing CI coverage.

Success evidence: matching digest, environment/date, and a passing emailed
result. Archive the result into the external evidence bundle before it expires;
record a durable summary in the issue, not the unexpired private URL. Block on a
digest mismatch, unexpected archive, send failure, or check finding. Recovery:
never rebuild silently—locate the retained archive or restart at stage 5; retry
transient service failure only after approval. Human fallback is the same web
form and email handling.

### 10. Present the final readiness decision

Purpose: make the last pre-submission decision explicit. Codex or the maintainer
summarizes, without mutating state:

- candidate SHA and clean status;
- retained tarball name/path and SHA-256;
- required CI and strict matrix URLs with matching SHA;
- semantic findings and every disposition;
- R-hub platform/SHA/result and win-builder environment/result;
- `cran-comments.md` status and initial/update-specific evidence;
- remaining human-only upload and email steps.

The result is exactly `READY` or `NOT READY`, with blockers listed. Update the
release issue only after approval. Success evidence is `READY` with no open
blocker and “Next human action: upload the retained tarball to CRAN.” Recovery:
return to the stage named by each blocker. Human fallback: assemble the same
ledger entries manually.

### 11. Human CRAN upload and email handling

Purpose: submit the proven bytes. This stage is human-only. The maintainer opens
`https://cran.r-project.org/submit.html`, selects the retained tarball whose
digest appears in stage 10, submits it, and handles confirmation, rejection, or
follow-up email.

Record submission time and a non-secret summary in the issue after approval.
Do not paste authentication, private email headers, or private links. An
acceptance email is evidence of acceptance, not publication. Block on any CRAN
question or requested change until the maintainer decides the response.

If rejected or changes are requested, keep the issue open. A textual answer that
does not change the candidate may retain evidence after human review; any
tracked change gets a PR/new candidate and restarts at stage 4. If submission is
withdrawn, record why and the next action. Human fallback is intrinsic: no agent
performs this stage.

### 12. Verify public CRAN publication

Purpose: prove the expected version appears in authoritative CRAN source package
data before publication-only edits.

Automatic, read-only command:

```sh
Rscript tools/cran-release.R verify-publication --version 0.1.0
```

Success evidence is the exact requested version. A missing package, different
version, malformed index, or unavailable CRAN endpoint blocks. Recovery: wait
and retry the same read-only command; if CRAN rejected/archived it, return to
stage 11. Acceptance email alone never changes the publication field. Human
fallback: check `https://cran.r-project.org/package=marginplyr` and the source
package index, confirming the exact version.

### 13. Tag and publish the GitHub Release

Purpose: bind a public tag and release assets to the frozen candidate. Prepare
release notes from the matching NEWS section and a two-space-separated digest
manifest for the retained tarball:

```sh
release_manifest=$(mktemp)
release_notes=$(mktemp)
shasum -a 256 /absolute/path/marginplyr_0.1.0.tar.gz > "$release_manifest"
# Copy the reviewed NEWS-derived notes into "$release_notes".
```

Show tag name, candidate SHA, notes, tarball path, and manifest. Obtain approval
before creating/pushing the tag, then verify it:

```sh
git tag -a v0.1.0 "$candidate_sha" -m "marginplyr 0.1.0"
git push origin v0.1.0
git rev-list -n 1 v0.1.0
```

Obtain a separate approval before publication:

```sh
gh release create v0.1.0 \
  /absolute/path/marginplyr_0.1.0.tar.gz \
  "$release_manifest" \
  --verify-tag \
  --title "marginplyr 0.1.0" \
  --notes-file "$release_notes"
```

Success evidence: tag resolves to the candidate SHA; Release is published;
explicit tarball and manifest are attached in addition to GitHub-generated
source archives; manifest digest equals the retained candidate. Block on any
SHA/digest mismatch or unreviewed notes. Recovery: do not move a public tag
silently; stop for human disposition. A draft Release can be corrected before
publication. Human fallback: use GitHub's Releases UI with the same tag target,
notes, and two assets.

### 14. Prepare the one post-publication PR

Purpose: atomically publish the initial CRAN state/documentation when needed,
advance to development, add the development NEWS heading, and regenerate files.

Show the branch/worktree target and obtain approval, then create it from current
main and preview:

```sh
git fetch origin
git worktree add ../marginplyr-post-release-0.1.0 \
  -b post-release/0.1.0 origin/main
cd ../marginplyr-post-release-0.1.0
Rscript tools/cran-release.R post-release --version 0.1.0
```

The preview repeats publication verification without editing. Show the diff and
obtain approval before:

```sh
Rscript tools/cran-release.R post-release --version 0.1.0 --apply
git diff --check
git diff
```

For the initial release, verify that status is `published`, the CRAN badge/link
and install call appear once, version is `0.1.0.9000`, and NEWS begins with that
heading. For an update, status and CRAN documentation must remain unchanged
while version/NEWS advance. Run documentation and relevant tests, then obtain
separate approvals for commit/push and PR creation. Put all of these changes in
one PR:

```sh
Rscript .github/scripts/verify-context-budget.R
Rscript .github/scripts/verify-doc-references.R
Rscript .github/scripts/verify-cran-release.R
jarl check .
Rscript -e 'pkgload::load_all(".", quiet = TRUE); lintr::lint_package()'
Rscript -e 'pkgload::load_all(".", quiet = TRUE); testthat::test_dir("tests/testthat")'
git diff --check

git add DESCRIPTION NEWS.md README.Rmd README.md
git commit -m "Start marginplyr 0.1.0.9000"
git push -u origin post-release/0.1.0
gh pr create --fill
```

Success evidence: green reviewable PR with no duplicated heading, badge, or
installation text. Block on publication mismatch, dirty/unexpected state,
generation failure, or a split/partial change. Recovery: inspect helper output;
fix the disposable branch, or discard only that branch and recreate it. Human
fallback: follow this stage by hand, render with Pandoc 3.10.1, and create one
PR.

### 15. Monitor site deployment and CRAN flavors for 72 hours

Purpose: detect publication regressions before closing the ledger. After the
post-publication PR merges, inspect the site deployment and all CRAN check
flavors periodically for up to 72 hours. Agent-operated read-only examples:

```sh
gh run list --workflow altdoc.yaml --limit 10
gh run view <run-id> --json headSha,conclusion,url
```

Also inspect `https://cran.r-project.org/web/checks/check_results_marginplyr.html`.
Delayed macOS/Windows binaries alone are not a release failure. A new ERROR,
WARNING, or NOTE requires a human disposition before closure. Record observation
times and durable URLs/summaries after approval; never copy transient private
data.

Success evidence: deployed site, no undispositioned CRAN finding, and the full
72-hour window complete. Block while the window is open or any finding lacks a
disposition. Recovery: investigate; fixes use the normal development/release
process rather than rewriting this release. Human fallback: inspect Actions,
the public site, and CRAN's check page on the same schedule. Close the release
issue only when every checkbox is complete. Show the final ledger and obtain
approval before the external close:

```sh
gh issue close <issue-number> \
  --comment "Release monitoring completed; all findings are dispositioned."
```

The human fallback is GitHub's Close issue action with the same final comment.

## Release issue ledger

Adapt this body for the version; keep it readable rather than pasting raw logs:

```markdown
## Next human action

- [ ] Review and approve release preparation edits.

## Candidate

- [ ] Preparation PR merged: <url>
- [ ] Candidate SHA: `<40-char-sha>`
- [ ] Clean candidate worktree confirmed
- [ ] Submission kind: initial / update
- [ ] `cran-comments.md` reviewed: <summary>

## Dependency-audit evidence

- [ ] Preparation commit SHA: `<40-char-sha>`
- [ ] `attachment` version and invocation: <version/command>
- [ ] Console output and DESCRIPTION diff: <external durable location; complete or partial/error>
- [ ] Every proposed change has a semantic disposition and maintainer acceptance: <table/link>

## Exact local artifact

- [ ] Preflight exit 0
- [ ] Retained tarball: `marginplyr_<version>.tar.gz`
- [ ] SHA-256: `<digest>`
- [ ] Durable preflight summary: <location or approved summary>

## Candidate-SHA evidence

- [ ] Routine R CMD check: <url>
- [ ] Documentation: <url>
- [ ] Lint and release contracts: <url>
- [ ] Site and coverage: <urls>
- [ ] Strict release matrix and all optional-dependency jobs: <url>

## Semantic and remote review

- [ ] Semantic rubric commit and review: <sha/url>
- [ ] Every finding has a human disposition: <table/link>
- [ ] Every dependency-audit disposition is confirmed against the exact candidate or explicitly unchanged: <table/link>
- [ ] R-hub platform, candidate SHA, date, result: <durable url>
- [ ] win-builder environment, date, digest, archived result summary

## Human submission and publication

- [ ] Final decision is READY
- [ ] Human uploaded the retained tarball
- [ ] Human handled confirmation/rejection/follow-up email
- [ ] CRAN source index publishes exactly `<version>`

## Public release and follow-up

- [ ] Tag targets candidate SHA
- [ ] GitHub Release notes published
- [ ] Retained tarball and matching SHA-256 manifest attached
- [ ] One post-publication PR merged: <url>
- [ ] Site deployment healthy
- [ ] CRAN flavors monitored for 72 hours
- [ ] Every new ERROR/WARNING/NOTE has a human disposition
```

For an update, add current CRAN result URLs, reverse-dependency evidence (or a
reason it is not applicable), downstream communication, and update rationale.
For an initial release, add the `New submission` explanation and confirmation
that the unpublished installation policy held until stage 12.

## Conversational operation

Examples for local Codex or Codex Remote Control:

```text
Prepare the next CRAN release.
Show the current release status and remaining human actions.
Here is the win-builder result URL: <url>.
I submitted the candidate to CRAN; verify publication and continue.
Prepare the GitHub Release and development-version follow-up.
```

Codex responds by reading this playbook, the release issue, repository state,
and retained evidence; it does not infer completion from a prior conversation.
For a private win-builder URL, it archives the result externally and proposes
only a durable non-secret summary for the issue.

## Interrupted, rejected, and restarted releases

After interruption, run stages 1 and 4 read-only checks, inspect the issue's
next action, and verify every claimed result still names the candidate SHA and
retained tarball digest. Continue from the first unchecked valid stage. Never
infer state from a half-run helper or from conversation history.

After CRAN rejection, record the non-secret reason and maintainer decision. A
candidate change goes through a new preparation PR, SHA, preflight bundle, CI,
semantic review where affected, remote checks, and readiness decision. A retry
with unchanged bytes may retain evidence only after the maintainer confirms the
rejection did not invalidate it. Rapid resubmission notes are explained in
`cran-comments.md`; they are never automatically allowlisted.

If publication or a remote service is temporarily unavailable, keep the issue
open and retry the read-only check. Do not interpret absence, timeout, delayed
binaries, or an acceptance email as successful publication.
