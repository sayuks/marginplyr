# Code review: `mattpocock-skills:code-review`

Reviews here run through that skill. It reviews the diff since a fixed point on two axes — Standards and Spec — as parallel sub-agents. This file maps what it expects onto what this repository has.

What may be a finding, and how one is answered, is not here. `design/architecture.md`'s *Answering a review* governs that, for this reviewer and for any other.

That section binds a reviewer only if the reviewer is handed it. This skill composes its own sub-agent prompts (`SKILL.md`, step 4), so pass what a finding about prose may be alongside the standards sources, in the same call.

`.claude/hooks/code-review-contract.sh` puts this file in front of the session that invokes the skill, so editing it changes what every later review is handed.

## What is reviewed, and where the record goes

The subject is a diff, so the record goes where the diff is: the pull request. An issue gets a pointer and not a copy — the issue is where the ticket is argued, and a second copy of the record is one more thing to keep in step.

So the branch is pushed and the pull request is open *before* the record is written. A record names the snapshot it read, and a SHA on an unpushed branch resolves for nobody: #280's review was posted to the issue while its branch was still local, so every SHA in it cited a commit no reader could open, and the record had to be moved (PR #286). For the same reason, rebase before the review and not after — a rebase rewrites the SHAs a published record already named.

The fixed point is the merge-base with `main`, which is what the three-dot form `git diff <base>...HEAD` computes. Reviewing against a `main` the branch has fallen behind reports that gap as though it were the branch's.

What the record carries is small, because most of a review is recorded by what it produced. A finding answered in code is recorded by the code and by the test that fails without it; one answered in prose is recorded by the diff that changed the prose. Restating either is a second copy nothing compares against the first, which is what *Code comments* in `AGENTS.md` refuses for a comment, on an argument that does not stop at comments.

So the record says what the round raised and what became of each finding, and stops. What a disposition needs beyond that is decided by its kind, and every kind already has a home:

| the disposition | what records it |
|---|---|
| fixed in code | the commit that names the finding, and the test that fails without the fix |
| fixed in prose | the diff that changed the prose |
| rejected, where a test, a comment, or a runnable command already answers it | that test, comment, or command |
| an alternative weighed and rejected | *Considered options* in the ADR holding the decision it would have changed |
| a premise measured false | a dated note under `investigation/` |
| a site deliberately left alone | a comment at the site, or the ADR that leaves it |

The table is what the second column of a ledger's entries turned out to be. `design/review-dispositions.md` recorded review dispositions for a second time until #288 retired it: of its thirteen rejections, nine pointed at a test, a command, an issue, or a comment rather than holding anything, and of the rest, one moved to the site it was about and one was about a helper that no longer exists. Nothing there routes anywhere the table does not, which is why the file has no successor and this section is not one.

## What the skill looks for, and what is here

| The skill looks for | Here |
|---------------------|------|
| `docs/agents/issue-tracker.md` | `design/agents/issue-tracker.md`. `docs/` is altdoc's generated site — gitignored, no tracked file — so do not run `/setup-matt-pocock-skills`: it would write into what the next site build deletes. |
| `CODING_STANDARDS.md` or `CONTRIBUTING.md` | `AGENTS.md`. Pass the sections the diff reaches, not the file. |
| a spec found by searching | Pass the issue number or a path as an argument. |
| the Fowler smell baseline | Written for OO. This package is functional R with S3, so several of those smells have no site here. They arrive as judgement calls and are dispositioned as any finding is. |
| standards that tooling already enforces | The `verify-*.R` scripts, `lint.yaml`, and `document.yaml`. `AGENTS.md` names what each fails on. |

The diff also carries generated files — `man/`, `NAMESPACE`, and `README.md` — which `document.yaml` regenerates and checks against their sources. They are outside the review.

## Running it a second time

A second run reviews the diff since the same fixed point, so it reads the answers the first run produced. Verify a finding against the evidence its answer landed with instead, and run the skill again only over executable behaviour the first run did not see.
