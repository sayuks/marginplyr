# Refuse a summary a backend would absorb

A Margin verb refuses a summary expression an Absorbing backend would evaluate
by reading the caller's input into R, and refuses it before any row is read.
The refusal is a Package condition naming the argument as the caller spelled it
and the two rewrites that get the summary computed: collect the input first, and
select the columns the summary needs before collecting.

CONTEXT.md defines *Absorbing backend* and its opposite. Only Arrow absorbs
today, and only some Arrow inputs do: an Arrow table, an Arrow record batch, and
a query over either. An Arrow dataset and a query over one refuse on their own,
and that refusal is Arrow's answer and propagates as an External condition
unchanged.

## Why a caller is asked rather than served

Absorbing and refusing reach the same place — the input is materialized and the
summary is computed in R — and they differ only in who asks for it. That is the
whole of the decision, and ADR 0020's title is the answer to it.

Two facts decide which side of that line this falls on, and neither is about the
cost of a query:

**A backend absorbs the whole input, not the part the summary uses.** Arrow
collects the input as the verb received it, including every column the
expression does not mention. A caller told to collect can `select()` first, and
for a wide table that is the difference between materializing two columns and
two hundred. Nothing about absorbing can be narrowed by the caller, because the
caller is never consulted.

**The alternative is not cheaper, only quieter.** A caller who collects pays one
read, which is exactly what a correctly bounded absorption would cost. Refusing
therefore takes nothing away except the silence.

## What this does not rest on

The obvious argument — that absorbing sends an unrequested query to an external
system — is not available here, and recording that is the point of this section.
An input that absorbs is, by class, already in this process's memory; an Arrow
dataset is the shape that may sit in object storage, and Arrow refuses there
without being asked to. So the fallback fires exactly where no external system
is involved, and ADR 0020's cost reasoning reaches none of it. ADR 0020's own
statement that this distinction cannot be computed is corrected in that ADR,
because it is what its predicate's exactness was argued from.

What survives is the other half of ADR 0020: a lazy input yields a lazy result,
and when the caller's data is read is the caller's to decide.

## Why the property is established and not looked up

`grouping_backend()` returns one kind for every Arrow input, and absorbing splits
that kind in two. So there is no entry in `backend_capabilities()` this could be,
and adding one would mean a capability table that is wrong for half the inputs it
covers. The property is read from the backend's own behaviour instead, at the
moment it would matter.

Nor can it be read from the caller's expression, which is what rules out ADR
0019's route of deciding by spelling. `sum(v[v > 1])` is refused with `sum` at
its head and `sum` translatable; a user-written function whose body composes
translatable operations is itself translatable, so neither the head nor the
author of a call answers the question. Only the backend can, and only by being
asked to build the query.

## Considered options

**Let the backend absorb.** Rejected on the two facts above. It restores parity
with a plain `dplyr::summarize()` on the same object, which is the strongest
thing that can be said for it, and it is real: the same call on the same input
answers where a Margin verb refuses. What it cannot restore is the caller's
choice about what to read, since absorbing takes every column and asks nobody.

**Collect once at admission and continue as a local operation.** Rejected, and
recorded because it is the version of absorbing that is not wasteful — one read
rather than one per grouping set. It fails on the same point, one step later: it
still reads every column, and it still does not ask. It also turns
`.check_margin_label`'s default from `FALSE` to `TRUE` behind the caller's back,
that default being read from the input's class.

**Record the limitation and wait for the backend.** Rejected. The condition a
caller receives today is neither the backend's answer nor an actionable defect,
and the read it was thought to prevent has already happened when it is raised.

**Refuse by rebuilding the input as a dataset.** Rejected as the mechanism, not
as the disposition: wrapping an Arrow table as a dataset makes Arrow refuse in
its own words, which is exact and free, but it cannot be done for a query over a
table without reading Arrow's internal structure, and getting that wrong turns a
dataset's own refusal into marginplyr's.

**Refuse after a zero-row probe.** Rejected for the same reason in a different
place. Taking zero rows of a dataset yields a table, so a probe would have to tell
absorbing and refusing inputs apart before probing, which is the introspection
above. It also adds a query where the chosen mechanism adds none.

## How the refusal is raised

Two mechanisms, chosen so that they fail in opposite directions.

The **handler** reads the warning an Absorbing backend marks the absorption
with, which is raised before the read. Arrow's carries no class, no cause, and no
call, so its text is the only thing to recognise it by. That is undocumented
behaviour of another package, and it is gated rather than trusted: a test asserts
that Arrow still absorbs the expressions the refusal is asserted over and still
marks them with that text, so a re-wording fails there instead of switching the
refusal off silently.

The **guard** reads the branch result's class — a local data frame from an input
that was not one — which cannot stop matching, but only answers after the branch
has run. It bounds a missed absorption at one branch rather than one per grouping
set, and it raises the same refusal rather than an internal invariant: a caller
who reaches it can act on it, and the action is the one the refusal already
names. ADR 0015's third category would be right about the defect and wrong about
the caller, who would be handed a bug report in place of a remedy they have.

## Documentation consequences

The pages that claim Arrow summaries are supported and lazy say what is refused
and why, and say that which expressions are affected is decided by the backend's
version. They do not enumerate them. A list would be a claim about one version of
Arrow, and `DESCRIPTION` admits a range; the same reasoning keeps every other
gate in `AGENTS.md` derived rather than listed.

`?marginplyr` is unchanged. Nothing a caller catches is new: an absorbed summary
raises the promised `marginplyr_error`, and an Arrow dataset's refusal keeps
propagating as any backend's does.

## Test strategy

The regression is in two parts, because a single one would fail without saying
why. The first asserts that the backend still absorbs the expressions the second
is written over; the second asserts the refusal. A release that translates one of
them fails the first, which names the drift, rather than the second, which would
read as marginplyr having stopped refusing.

The expressions are chosen for how long they will keep being absorbed rather than
for how the defect was found: a group collapsed to one string, and a subset inside
an aggregate. `first()` and `last()` are absorbed too and deliberately unused,
being the likeliest of that set to gain a kernel.

Neither part may skip. `verify-backend.R` fails a job for a skip naming no
withheld backend, so a test that stepped aside when the boundary moved would fail
the job it was meant to keep green.

`test-query-policy.R` carries the maintainer's half. Its other readings walk `R/`
and so cannot see a read another package performs on marginplyr's behalf; this one
counts the backend's own reads while a verb runs, over a summary the backend
evaluates, an expansion that carries no caller expression, and the refusing path.

## Related decisions

ADR 0020 is the rule this serves and the one this corrects; its Arrow sentence is
amended there. ADR 0015 is the boundary the guard is placed against. ADR 0019 is
the route ruled out above. ADR 0021 governs what a condition raised once per
grouping set reports, and is untouched: the refusal stops at the first branch, so
there is nothing repeated to report. ADR 0022 and ADR 0024 are what the refusal
spells its subject by, and ADR 0023 is the idiom it is authored in.
