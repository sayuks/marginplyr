# Code review: `mattpocock-skills:code-review`

Reviews here run through that skill. It reviews the diff since a fixed point on two axes — Standards and Spec — as parallel sub-agents. This file maps what it expects onto what this repository has.

What may be a finding, and how one is answered, is not here. `design/architecture.md`'s *Answering a review* governs that, for this reviewer and for any other.

That section binds a reviewer only if the reviewer is handed it. This skill composes its own sub-agent prompts (`SKILL.md`, step 4), so pass what a finding about prose may be alongside the standards sources, in the same call.

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
