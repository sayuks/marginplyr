# Bug hunt

A hunt looks for defects nobody has reported, at one pinned commit, and ends
with each finding filed and decided. One session coordinates it. The
coordinator plans, replays and decides; it does not search for candidates.

The package, its tests and generated files stay unchanged throughout. A repair
is a later task under [Implementing a spec](implement-spec.md).

## Steps

### 1. Pin and build

Fetch `origin/main` and pin its SHA. Build that commit outside the checkout
into a private library under the session's scratch directory, so every
observation comes from the installed build. Measure and write down the R and
dependency versions, and which of [the optional engines](#engines-and-sandbox)
answer.

Done when `library(marginplyr, lib.loc = <private library>)` loads the build
of the pinned SHA and the versions and engine list are written.

### 2. Sweep before searching

A family with a mechanical oracle is swept, not searched. Run each sweep at the
pinned commit, on every engine version available:

| Family | Sweep | What a miss is |
| --- | --- | --- |
| Names DuckDB binds with no call head | `tests/testthat/test-duckdb-implicit-reach.R`, on DuckDB 2.x and on 1.5 | A statement that reaches a caller macro the preflight admits |
| dplyr verbs that own a data mask | `tests/testthat/test-nested-data-masks.R` | An export dplyr documents as masking that the registry lacks |
| Reads of an earlier share by a name not written | `Rscript tools/hunt/share-reader-sweep.R . <out>` | A call whose value follows a same-named input column |

Add a sweep when a family yields three findings in one hunt and an oracle
exists: a list copied from a dependency's vocabulary is the usual sign. A
family with a sweep gets no finder lane and no second round.

Done when every miss of every sweep is a candidate file or is matched to an
open issue.

### 3. Plan the lanes

Read the *Not covered* and *Covered without a finding* sections of the three
latest hunt notes under `investigation/`, and `tools/hunt/decided-shapes.md`.
Give each lane a starting point none of those notes used, and state for each
the oracle its expectations come from. Count the things a lane brief names
(handlers, call sites, exports) with a script before writing the count down.

Size the hunt by [the budget](#budget), and split it so that each phase fits
one usage window: finders, then verification, then a second round.

Done when each lane brief names its starting point, its oracle and the notes it
must not repeat, and the planned agent count is written.

### 4. Find

Write the finder brief from `tools/hunt/brief-finder.md` and one lane brief
each, then run the finders as one workflow from
`tools/hunt/finders.workflow.js`. Convert the journal into one file per
candidate:

```sh
Rscript tools/hunt/journal-results.R <scratch> <journal.jsonl>
```

Done when every finder has returned or is recorded as not run, and each
candidate has a file and a script.

### 5. Replay, then look up

Replay each candidate's script yourself in a fresh process. A candidate that
does not reproduce is dropped here and recorded as such; it gets no refuter.

For each one that reproduces, attach what the tracker and the notes already
hold:

```sh
Rscript tools/hunt/known-issues.R <candidate.md>
```

Group candidates that share a cause site.

Done when every candidate is dropped, or carries its replay output and its
lookup, and belongs to one cause group.

### 6. Refute

One refuter per cause group, in a fresh context, on a model other than the
finder's, from `tools/hunt/brief-refuter.md` and
`tools/hunt/verify.workflow.js`. The refuter argues that the candidate is not a
defect and judges the lookup's hits.

A refuter that fails is retried once on the same model. A second failure is
left failed: read why before choosing another model, and when the cause is a
usage limit, resume the same run on the same model once the limit clears.

Done when every cause group has a verdict from the model assigned to it.

### 7. Decide and file

For each surviving claim, replay it once more, read the cause code, and where
the decision rests on a remedy, replay that remedy in memory. Then decide:

- a defect becomes `bug` with `ready-for-agent`;
- a question of specification is decided here, with the decision and what it
  costs written in the body, and becomes `ready-for-agent`, `documentation`
  with `ready-for-agent`, or `wontfix` closed as not planned;
- a third finding with one cause is not a third issue. Decide the rule that
  ends the family, in the ADR that owns it, and file that.

Write a small spec per finding and let the script assemble the body from the
candidate and the refutation:

```sh
Rscript tools/hunt/issue-body.R <scratch> <spec.md>
```

[Issue tracker](issue-tracker.md) owns the commands. File each issue as soon as
it is decided.

Done when every candidate is one of: filed, folded into a named issue, or
rejected with its reason, in the ledger.

### 8. Second round

Search again only next to confirmed findings, with at most four finders, and
only for families that have no sweep. Steps 5 to 7 apply unchanged.

### 9. Record

Write the dated note and its script directory under `investigation/`, following
`investigation/README.md`; every script exits 0 from the repository root.
Append the hunt's decisions to `tools/hunt/decided-shapes.md`. Commit, run
Review-ready with `--scope source`, push, and open the pull request under
[PR preparation](pr.md).

Report a verdict table (confirmed, decided here, rejected) with issue numbers,
the agents run against the plan, the lanes not run, and each decision that
traded one behaviour for another.

Done when the pull request is open and the report names every candidate.

## Budget

Plan `lanes + cause groups` agents; a second round adds its own. The 2026-11-27
hunt ran 73 against a plan of 50: 12 finders, 42 refuters, 11 duplicate
lookups and 8 refuters run again after a model's usage limit sent them to
another model. Three changes since then remove the last two terms and shrink
the second:

- the lookup is a script (step 5);
- a failed refuter is not moved to another model unread (step 6);
- 19 of the 35 issues filed came from the two families step 2 now sweeps, 12
  of them from one dependency walk.

Give Fable the coordinator's seat and the refutation of a claim at P2 or
above that another model found. Give contract-reading lanes to Opus and lanes
that walk a vocabulary against an oracle to Sonnet. Effort stays `high` or
above.

Keep the coordinator's context for decisions: read a refutation's verdict,
strongest argument and surviving claim, and open the rest when a decision
needs it.

## Engines and sandbox

- PostgreSQL does not start inside the sandbox. The user starts a temporary
  cluster and gives the socket; an agent that uses it creates one schema named
  `hunt_<lane>_<suffix>`, keeps every object there, and drops it.
- An older DuckDB and the DuckDB json extension may be in the package's R cache
  directory, `tools::R_user_dir("marginplyr", "cache")`: look under
  `duckdb-2-compatibility/` and `duckdb-json/` before writing that an engine
  was not run. Nothing is downloaded or installed during a hunt.
- `gh issue view <n> --comments` prints nothing inside the sandbox and exits 0.
  Read an issue with `gh issue view <n> --json title,body,labels,comments`.
- `gh` fails outside a git checkout; run it from the worktree root.
- A workflow agent cannot write a report file. Its structured return is its
  report; it can write scripts.
- A long workflow result is cut in the task's output file. The whole of it is
  in the run's `journal.jsonl`.
- A workflow stopped by a usage limit resumes with
  `Workflow({scriptPath, resumeFromRunId})`; finished agents replay from the
  journal.
- Send GitHub a file from the session's scratch directory, never from
  `$TMPDIR`, which sessions share.

## Why the workflow scripts are JavaScript

The Workflow tool runs JavaScript and nothing else, so
`tools/hunt/*.workflow.js` are the one place under `tools/` that R cannot
serve. Everything a workflow does not have to run is R.
