# Investigation notes

A note in this directory records what was established on one date. It is not a
description of how things currently are, and it is not maintained afterwards.
When a later investigation overturns part of a note, the original findings stay
in place and readable as history; the note gains a dated revisions section
above them rather than being rewritten.

Everything here follows that one rule. There is no separate lifecycle for a
literature search, an audit of `R/`, a benchmark, or an implementation plan,
even though they age for different reasons — a reading of P3M's documentation
or R's source ages when the outside world moves, `backend-api-audit.md` ages
every time `R/` changes, and `parent-share-local-benchmark.md` never ages at
all, because a measurement taken on a date stays a measurement taken on that
date. Giving those separate treatment is what produced four different header
conventions before this file existed.

## Header

A note opens with its title and a keyed block:

```text
# <title>

Investigated: <YYYY-MM-DD>
```

`Investigated:` is the date the work was done, which is not necessarily the
date the file was committed. A revised note carries one further line per
revision; see [Supersession](#supersession).

Extra fields are allowed where they state something true —
`backend-api-improvement-plan.md` carries `Status: implemented`, `Completed:`,
and `Primary input:`. Those belong to that note and do not become a convention.
Only `Investigated:` is required of every note, and `Revised:` of every note
that has been superseded in part or in whole.

## Tense

A note is read as of its `Investigated:` date, but the date on line 3 does not
survive contact with prose that claims otherwise. Do not write bare `now`,
`currently`, or `today`. State a live configuration either in the past tense
with its date — "as of 2026-07-18 the R-devel job installed only hard
dependencies" — or by pointing at the file that is authoritative for it.

This is not a style preference. `github-actions-modernization.md` carried a
correct `Research date: 2026-07-18` and still misled a reader for two weeks,
because its "the R-devel job **now** installs only hard dependencies" outranked
its own header in every reading (#61).

## Authority

A note and a workflow comment can carry the same argument without either being
redundant, because they are authoritative about different things.

- **The note is authoritative for the evidence**: which primary sources were
  read, what they said, what was measured, and what was searched for and not
  found. `r-devel-binary-compatibility.md` establishing that R publishes no
  binary-compatibility guarantee anywhere — the absence being the finding —
  exists nowhere else, and is why the workflow comment that depends on it can
  be trusted.
- **The artifact is authoritative for the decision and for current state**: a
  workflow comment, an ADR under `design/adr/`, `AGENTS.md`, or the code
  itself. `R-CMD-check.yaml`'s R-devel comment is what the job does and why;
  no note outranks it on that question.

When the two genuinely contradict each other:

- about **this repository**, the artifact wins and the note is stale by
  definition — a note never describes the present;
- about **the outside world**, the newest note wins, because notes are the only
  place primary sources are read, and the artifact needs updating.

So when `R_INTERNALS_UUID` moves, the copy that gets edited is the workflow
comment in `R-CMD-check.yaml`, because that is a statement about what the job
now relies on. The note also gains a revisions entry, because the value moving
is itself a new finding about R.

## Pointers

Pointers run one way. An artifact cites a note by repo-relative path, as
`R-CMD-check.yaml` and `AGENTS.md` already do. A note does
not list the artifacts that cite it: that list would have to be edited every
time a new citation appeared, and a stale consumer list is worse than none
because it is trusted.

Discovery is a grep instead. **Before amending or superseding a note, run**

```sh
grep -rn "investigation/<note>.md" .
```

**and update every citing artifact in the same commit.** Amending a note
without touching what it justifies is the same failure as #61, run backwards.

## Supersession

There is one mechanism, used whether a single paragraph or the entire note is
overturned.

1. Add one line to the header block:

   ```text
   Revised: <YYYY-MM-DD> — investigation/<successor>.md
   ```

   one per revision, in date order after `Investigated:`.

2. Add a section headed `## Revisions (<YYYY-MM-DD>)`, placed after the
   findings it amends, stating what changed and why. Name the note or artifact
   that established the correction.

3. Leave the original findings in place, unedited. The point of the note is
   what was believed on its date; deleting the superseded text destroys the
   only record of that.

A note superseded in full is not a second mechanism. It is a revisions entry
whose first sentence says so — "This note is superseded in full by
`investigation/<successor>.md`" — followed by what it got wrong.

## Graduation

A note is evidence, not a decision. The first time an artifact cites a note as
a *reason* — a workflow depends on its finding, an ADR rests on it, a CI job
exists because of it — the durable part moves to `AGENTS.md` or to an ADR in
that same commit, and the note keeps the evidence and the primary sources.

This is what #57 did with the R-devel CI contract: the rule went to `AGENTS.md`
and to `R-CMD-check.yaml`'s comment, while `p3m-binary-actions.md` and
`r-devel-binary-compatibility.md` kept the P3M readings and the `Defn.h`
argument that justify it. A decision that lives only in `investigation/` has
not been made yet.

## Notes are not deleted

A superseded note is still an accurate record of what was established on its
date, which is the only thing it ever claimed to be. Notes accumulate; they are
not pruned, and a note being wholly wrong is a reason to add a revisions entry
saying so, not to remove the file.
