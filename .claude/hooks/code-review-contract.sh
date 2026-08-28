#!/usr/bin/env bash
# Hands `design/agents/code-review.md` to the session that is about to run a
# review, as a `PreToolUse` hook on the `Skill` tool.
#
# `AGENTS.md` points at that file, and a pointer is read only if the reader goes
# looking. What it holds is not advice: the skill composes its own sub-agent
# prompts, so a scope the invocation does not carry reaches neither axis; a
# record written before the branch is pushed names SHAs nobody can open (#286);
# and a fixed point that is not the merge-base reports `main`'s gap as the
# branch's. Each of those is spent before anything fails, so there is nothing to
# gate afterwards. This is what makes the file arrive rather than be looked up.
#
# It injects the file rather than a copy of what the file says, so there is
# nothing here to drift from it. Editing the contract is editing that file.
#
# Silence is the only failure mode: outside a repository, on any other skill, or
# with the file absent, it exits 0 and adds nothing.

set -u

input=$(cat)

root=$(git rev-parse --show-toplevel 2>/dev/null) || exit 0
contract="$root/design/agents/code-review.md"
[ -f "$contract" ] || exit 0

# Matches the plugin skill and the built-in one alike: what the contract governs
# is a review of this repository, not one reviewer's implementation of it.
printf '%s' "$input" |
  jq -e '(.tool_input.skill // "") | test("code-review")' >/dev/null 2>&1 || exit 0

jq -n --rawfile contract "$contract" '{
  hookSpecificOutput: {
    hookEventName: "PreToolUse",
    additionalContext: (
      "This repository adapts that skill. `design/agents/code-review.md`, in full:\n\n"
      + $contract
    )
  }
}'
