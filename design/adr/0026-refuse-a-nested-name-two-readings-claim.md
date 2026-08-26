# Refuse a nested name two readings claim

A Nested specification position decides what an argument means by how it is
written. One argument defeats that rule: a bare name that is a column of the
input *and* is bound in the caller's environment to a Grouping specification of
a kind the position admits. Both readings are available, the spelling settles
neither, and the position resolved it by asking the data — the one thing the
rule says does not decide.

That name is refused. The refusal is a Package condition naming both readings
and a spelling that settles each:

```r
grouping_sets(all_of("s"))   # the column, whatever is bound
grouping_sets(!!s)           # the specification, whatever columns exist
```

Neither spelling is new. Both worked before this decision and are what the
refusal points at.

## Why a caller is asked rather than served

The reason this is worth a refusal rather than a documented precedence is that
the wrong plan is **silent and well-formed**. `grouping_sets(s)` over an input
with a column `s` produced a plan with the right shape, the right column type,
and no missing values, so nothing downstream reported anything — and every
value derived from the plan went with it: the grouping sets, `.id`,
`grouping_bit()`, `grouping_id()`, and the parent a Parent share divides by
(#255).

Choosing either precedence keeps that property and only moves which reading is
silently wrong. Preferring the binding makes the documented sentence true as
written, and silently changes what an existing call returns for a caller who
has a column and an unrelated binding of that name in scope. Preferring the
column is what the position does today. Refusing is the only disposition that
removes the silence in both directions, and it removes nothing else: a caller
who meant either reading has a spelling for it, and had one before.

It is also what this position already does with the other argument it cannot
read. #190 could have been answered by evaluating every nested argument and
accepting whatever turned out to be a specification; the position refused
instead and named the spelling that works. ADR 0015's criterion for a Package
condition — an error the caller can avoid by rewriting the call within the
documented public interface — is satisfied twice over here, because both
rewrites already existed.

## What makes the second reading available

There are two readings only where the specification reading is *available*, and
availability is decided by the **kind** of the bound specification — never by
the input, which is the rule this decision exists to keep:

- `grouping_sets()` and `grouping_spec()` admit every kind, so any colliding
  name bound to a specification is refused there.
- `rollup()` and `cube()` admit a `grouping_set()` composite dimension, so a
  colliding name bound to anything else keeps the column reading it has today.
- `grouping_set()` admits no kind at all, so a colliding name there is always
  the column, and the binding is never read.

That is the ticket's second acceptance criterion read precisely: the behaviour
is the same in every position *that takes a nested specification*, and where the
position takes none there is nothing to be ambiguous about.

Which kinds a position admits is derived by asking that position's own
`validate_nested()` rule from the kind registry ADR 0008 centralized, so a sixth
kind is admitted or refused here by the rule that decides it everywhere else and
there is no second list of what nests inside what. The rule answers about an
instance, so it is asked about a stand-in of each kind rather than about the
caller's value, and the stand-in carries one argument: `rollup()` and `cube()`
read a composite's arity as well as its kind, so a stand-in carrying none
answers "nothing is admitted" for both and turns the refusal off in two of the
five positions.

**Availability, not disagreement.** Whether the two readings would produce
different grouping sets cannot be known without resolving the specification
against this input, and deciding by the input is the defect. A name whose two
readings happen to agree is therefore refused with the rest.

**The kind, and nothing further.** A specification of an admitted kind that is
invalid on its own terms — one with no arguments, one holding a family its own
constructor forbids, a composite that is empty once its selection has run —
makes the name ambiguous just the same, because what is wrong with it is a
property of what the caller wrote. What the advice promises is the reading, not
that the reading succeeds: `!!s` gives a plan where the specification is valid
and the diagnostic about it where it is not, which is exactly what the silent
column selection withheld. Drawing the line further in would put arity and
validity on the same footing as the input, and only one of those is the
caller's spelling — a composite that empties only after its selection runs
cannot be judged at all without resolving that selection against the data,
which is deciding by the input.

## Where the decision is made

In the structural preflight, on the branch where the spelling gate answered
"selection", and not in the gate itself. The preflight runs once for a whole
operation and is handed to the compilation passes, where the gate runs again on
each. Deciding in the gate would read a colliding binding once per pass instead
of once in all — three times where the plan is settled by names alone, since
such a plan is compiled against the names first so that a plan error need not
wait for a backend read, and twice where it is not — and make that many of
whatever forcing it does visible to the caller.

The derivation is not recursive. Asking the preflight about the bound
specification would let an ambiguity inside it swallow the refusal outside it —
a binding that raises is a binding that is not a specification, so #255 would be
reachable again one level further in — as well as costing time exponential in
depth and answering a cyclic binding by the parity of its depth.

The answer per parent kind is kept for the session, because asking costs a
raised Package condition for every kind the parent refuses — four of five under
`rollup()` and `cube()`, all five under `grouping_set()` — with a cli expansion
and a backtrace apiece, for conditions no caller sees. The key is the parent's
kind, which rests on no rule reading anything of a parent but its kind: of the
three rules the five kinds share, two ignore the parent entirely and the third
reads its type to name it in a message. A rule reading more of a parent than
its kind would need that key widened.

The refusal fires before `grouping_selection_proxy()`, so no backend metadata is
acquired first (ADR 0005), and a lazy input is not read (ADR 0020).

## Considered options

**Honour the rule.** Prefer the bound specification over a same-named column,
making `CONTEXT.md`'s sentence true as written. Rejected: it keeps the silence
and swaps which reading is silently wrong. A caller who meant the column, with
an unrelated specification bound to that name, would get the specification's
plan with no condition raised — the same defect from the other side, and
arriving in working code that has no reason to expect it.

**Record the exception.** State the column-wins precedence in `CONTEXT.md` and
on the reference page and pin it with a test. Rejected: it is the cheapest
disposition and the only one that changes no behaviour, and what it writes down
is that this position decides by the data. Every other rule about the position
is a rule about the spelling, and a caller reading only the sentence still gets
the plan they did not write.

**Decide in the spelling gate.** Rejected above on the evaluation count, which
is not a performance argument: the count is what a caller can observe when the
binding is a wrapper's own lazy argument.

**Refuse by comparing the two plans and accepting where they agree.** Rejected:
resolving the specification against this input is what deciding by the input
means, and a name would then be refused or accepted according to the columns the
data happens to have — which is the defect, arriving through the refusal that
was meant to remove it.

## What this changes, and the ADR 0008 amendment

ADR 0008's compatibility constraints hold this position's evaluation behaviour,
and two of its bullets are amended here.

**"quosure environments or the number and timing of evaluations without a
separately accepted decision."** This is that decision, for one argument.
Deciding whether the two readings differ is a property of the bound value, so
the binding has to be read, and there is no cheaper question: which kind a name
is bound to cannot be known without reading it. Measured, a colliding name in a
position that admits any kind goes from 0 reads to 1, once for each argument it
is written as, and whether or not its reading then changes — deciding whether it
changes is what the read is for. No argument outside a collision is read more
often than before, and a `grouping_set()` position reads nothing. Quosure
environments are unchanged.

Timing moves with the count. Where the refusal fires, the arguments written
after it are not read at all.

**"error condition classes, complete messages, public call contexts, or
detection order."** This bullet carries no "separately accepted decision"
clause, and this decision moves every item in it, so it is amended whole rather
than in part. Detection order moves by position and by depth:
`grouping_sets(s, rollup())` now reports the ambiguity where it reported the
grammar error, while `grouping_sets(rollup(), s)` still reports the grammar
error. What is displaced is every diagnostic reachable past the refusal and not
one of them — a missing column, a duplicate grouping set, a `.by` overlap, each
nesting-grammar rejection, and #190's own refusal among them. Where the
displaced rejection was an External condition, a caller who was receiving
tidyselect's class and tidyselect's blamed call now receives a
`marginplyr_error` blamed on the Margin verb.

Everything else in that list is untouched: exported functions and their
arguments, specification classes, grouping-set membership and order, duplicate
handling, Grouping identifiers, backend behaviour, metadata acquisition counts,
laziness, and the Grouping plan representation.

**The cost accepted.** What the read costs a caller is the forcing itself and
not only the count, and it is not confined to the bindings that turn out to be
specifications: the read is what establishes which those are, so a wrapper's own
lazy argument is forced here whenever its name collides, whatever it holds. That
is for a call whose answer may not depend on it, so a warning or a message the
argument raises reaches that wrapper's caller, and R's own `restarting
interrupted promise evaluation` does where such a binding raises and the name is
written more than once. This decision accepts that rather than hiding it, since
the alternative to reading the binding is deciding by the input, which is the
defect.

## Documentation consequences

`CONTEXT.md`'s *Nested specification position* states the refusal and the
narrowing by kind, and two of its sentences stop being true as written: that the
position never evaluates an argument to find out, and that a specification
written inside a selection keeps the selection's own report. The second was
never true where a column shares the name — `grouping_sets(c(s, grade))` selects
the column and raises nothing — and the same sentence stood in `?grouping_set`,
`design/architecture.md`, and two code comments.

`?grouping_set` carries the same rule in a caller's terms, with both spellings,
and the `.grouping` parameter documentation points at it. Inside a selection the
column is the only admissible reading, and that is now said where it is claimed
rather than left to the collision-free case.

## Test strategy

The behavioural half runs through the compiler and one Margin verb, per the
seam ADR 0013 established: the refusal in every position, derived from the kind
registry rather than listed, asserting the complete message; the narrowing,
where an inadmissible binding still selects the column; and the readings that do
not change — a bound name with no colliding column, a column with nothing bound,
a constructor call, a caller's own function, a specification inside a selection,
and top-level `.grouping`, which has no column-selection reading at all and so
cannot be ambiguous.

Two contracts need more than the public seam gives, and in different ways.

The **admitted set per parent kind** is asserted below it, against the
derivation directly, because an empty answer reads as a position that admits
nothing rather than as a derivation that stopped working, and those two are the
same silence. The `rollup()` and `cube()` entries are what pin the stand-in's
arity.

The **number of reads** is asserted through the seam like everything else; what
sits below it is the observation. An active binding reports every read of a
name where a promise reports only the first. The counter is asserted to work
before any zero is concluded from it, for the reason ADR 0025's absorption gate
asserts its own mechanism: a count of zero from a mechanism that stopped
counting reads exactly like a read that did not happen.

Both printed spellings are executed and asserted to produce the reading they
name, and they are read back out of the diagnostic that printed them, over a
syntactic name and two non-syntactic ones. A name holding a backtick is what
makes the quoting load-bearing: `rlang::expr_deparse()` writes `` `a`b` `` for
it, which does not parse, so the quoted form comes from `encodeString()`
instead.

No test here requires a member of `optional_backends()`.

## Related decisions

ADR 0008 owns the kind registry this derives availability from, and is amended
above. ADR 0019 registers the constructor spellings the gate reads, and is
untouched: what this adds is read after the gate has answered, and only for a
name the gate declined. ADR 0015 is the boundary the refusal is placed against,
and ADR 0023 and ADR 0024 are the idiom and the spelling rule it is authored in
— the caller's own name, quoted as R would parse it. ADR 0005 and
ADR 0020 order the refusal before any metadata read and any read of a lazy
input. ADR 0013 is the seam the behavioural half is asserted through.
