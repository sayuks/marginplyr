# Margin Grouping

marginplyr describes SQL-style multi-grain grouping in terms that apply
consistently to local and remote data.

## Language

**Grouping specification**:
A user-declared description of the grouping sets, rollups, cubes, or products
to be applied.
_Avoid_: Grouping expression

**Grouping specification kind**:
The semantic form of one node in a Grouping specification: grouping set,
grouping-set union, rollup, cube, or Cartesian product.
_Avoid_: Grouping specification type

**Nested specification position**:
An argument of a Grouping specification constructor, where a nested Grouping
specification and a column selection are both allowed. Which one is meant is
decided by how the argument is written: a call to a constructor, or a name
bound to a specification, is a nested specification, and every other argument
is a column selection. Redundant parentheses are transparent to that reading,
around the name or around the whole call, because `(` is the identity
function. A spelling the position does not recognize is never
evaluated to find out, because a selection such as `starts_with("re")` has no
meaning outside a selection context. An argument the caller left empty gets
neither reading, and is refused naming the constructor and the position; a
trailing comma is not one, because R captures no argument for it. That refusal
reaches the position and not what a selection written there contains: an empty
argument inside such a selection leaves the position filled, and is read as
tidyselect reads one. Both
readings claim one argument at once: a bare name that is a column of the
input and is also bound, in the caller's environment, to a specification of a
kind the position admits — which kinds those are varies by constructor. How
that name is written settles neither reading, so it is refused rather than
resolved. ADR 0026 decides that, and holds the admitted kinds and what the
read costs a caller.
_Avoid_: Nested grouping, nested slot

**Grouping plan**:
The backend-independent grouping semantics obtained by fully expanding a
grouping specification.
_Avoid_: Execution plan, backend plan

**Grouping set identifier**:
The one-based position of a grouping-set occurrence in a Grouping plan. It
distinguishes repeated identical sets and changes when the plan order changes.
_Avoid_: Grouping identifier, grouping mask

**Grouping identifier**:
A bit mask encoding which chosen grouping dimensions are absent from a
grouping set. Identical absence patterns have the same identifier regardless
of their position in a Grouping plan.
_Avoid_: Grouping set identifier, set number, grouping mask

**Grouping bit**:
A zero-or-one flag indicating whether one chosen grouping dimension is absent
from a grouping set.
_Avoid_: Missing-value flag, grouping set identifier

**Margin operation**:
One request to summarize, expand, or nest data across the grouping sets in a
grouping plan, from preparation through finalization.
_Avoid_: Grouping plan, operation context

**Margin label**:
A display value inserted into a grouping dimension that is absent from a
grouping set. For factor dimensions, it is a synthetic level placed last by
default and may be placed first explicitly. `NA_character_` and an absent
label represented by `NULL` use a typed missing value instead of a synthetic
factor level.
_Avoid_: Total value, missing-value replacement

**Margin label collision**:
A grouping dimension holding, as one of its own values, the non-missing Margin
label chosen for it — so that a margin row and a row of the source data cannot
be told apart by that column. A collision is *declared* when the colliding
value is a factor level, which the column's type records, and *observed* when
it appears only among the column's values. A typed-missing Margin label is not
a collision: it displays as missing wherever the column already holds missing
values, and a Grouping bit or Grouping identifier is what tells the two apart.
_Avoid_: Duplicate label, ambiguous margin

**Margin order**:
The row order a Margin operation produces when it is asked to order its
result. Within each fixed key, every grouping dimension contributes its
Grouping bit before its own value, so a margin row sits with the rows it
summarizes rather than wherever its Margin label falls among the values. Every
column in the key carries a missingness term, fixed keys included, so missing
values come last wherever they appear. It is a property of the
result a Margin verb returns, not of any table derived from it — except a
materialization of that same result, which carries it — and it is
distinct from Grouping-plan order, which numbers grouping-set occurrences.
_Avoid_: Report order, result order, row order, sort order

**Grand total set**:
The grouping set in which every variable grouping dimension is omitted. A
Grouping plan contains at most one, except when duplicate occurrences are
retained; every occurrence of it holds the same values, so which one a
calculation uses as its denominator is not specified. With fixed `.by`
columns it holds one row for each fixed partition.
_Avoid_: Root set, root row, total row, margin row

**Parent share**:
For a row in a rollup result, the ratio of one named scalar summary value to
the corresponding value in the immediately less detailed grouping set. A row
of the Grand total set has a parent share of one. A missing numerator, zero
denominator, or missing denominator produces a missing double value. The
source is a previously defined numeric scalar summary and the result is
always a double; finite ratios are not clamped. Parent shares are defined for
a rollup, including composite dimensions, and are calculated independently
within each fixed `.by` group. Duplicate occurrences remain in the result but
are skipped while finding the next strictly less detailed parent set.
_Avoid_: Subtotal share, percent of grand total

**Total share**:
For a row in a multi-grain result, the ratio of one named scalar summary
value to the corresponding value in the Grand total set. A row of the Grand
total set has a total share of one. A missing numerator, zero denominator, or
missing denominator produces a missing double value. The source is a
previously defined numeric scalar summary and the result is always a double;
finite ratios are not clamped. Total shares are defined for any Grouping plan
that contains a Grand total set, and are calculated independently within each
fixed `.by` group, so a fixed key never contributes to another partition's
denominator.
_Avoid_: Percent of total, root share, grand total percentage

**Converting dialect**:
A SQL dialect that answers an aggregate over a value of the wrong type by
converting it to a number rather than raising. Its opposite is a *refusing*
dialect, which rejects the same aggregate. Which of the two a dialect is
decides whether the eligible-type rule for a share source can be left to the
database: a refusing dialect applies it and reports an ineligible source in
its own diagnostic, while a converting dialect applies nothing and returns a
number whatever the source held, so a share over one is refused unless the
caller establishes the source themselves. Which of the two a dialect is, is a
property of the dialect and not of one connection, so it is established once and
reused for every later connection carrying that dialect. A dialect that could
not be asked is neither, and is refused as a converting one is — but that is a
fact about one attempt rather than about the dialect, so nothing is recorded and
the next share request asks again.
_Avoid_: Coercing backend, lenient database, permissive SQL

**Absorbing backend**:
A backend that answers a summary expression its own engine cannot evaluate by
reading the caller's input into R and evaluating it there, rather than
refusing it. Its opposite is a *refusing* backend, which reports the
expression as unsupported and leaves the input unread. Which of the two a
backend is, is a property of the input's class and not of the expression: an
Arrow table, an Arrow record batch, and a query over either absorb, while an
Arrow dataset and a query over one refuse, and every SQL backend refuses at
the caller's own execution. It is therefore finer-grained than a backend kind,
one kind holding both, which is why it is established from the input rather
than looked up. Which expressions it applies to cannot be read from the
call — a composition of translatable operations is translatable however it is
spelled and whoever wrote the function around it — and the boundary moves with
the backend's own version, so it is described rather than enumerated. A Margin
verb refuses on the caller's behalf wherever a backend would absorb, because
absorbing reads every column of the input while a caller who is told can read
fewer.
_Avoid_: Pulling backend, fallback backend, collecting backend

**Contextual helper**:
A spelling whose meaning inside a Margin summary arises only through static
rewriting, and which is therefore recognized by spelling and never resolved
from the calling environment. It is recognized when the name matches and the
namespace is absent or the owning package; any other qualifier is an ordinary
call. Redundant parentheses are transparent to that reading, around the name
or around the whole call, because `(` is the identity function; a head that
must be evaluated to know what it calls is not recognized at all. A caller
binding of the same name never changes what a Margin verb does with it, and
the rewritten call names the owning package so that what executes is what was
analyzed. An argument one reads as a bare name is resolved the same way —
against the Grouping plan, or among the preceding summaries — so a name
forwarded by injection is read for the name it carries and the environment
carried with it is not consulted, there being no lookup for one to answer.
Every other name in a summary expression follows ordinary lexical and data-mask
lookup, including `dplyr::n()`. Grouping
specification constructors are not Contextual helpers: their spelling decides
only whether a nested argument is evaluated, and the caller's own function
runs when it is. A diagnostic refusing one tells the caller their spelling is
*reserved*; that is this same fact addressed to someone who has not read this
glossary, and not a second term.
_Avoid_: Contextual function, masked helper, reserved argument

**Option argument**:
An argument whose value is one of a fixed set of strings, spelled in full. An
abbreviation is not shorthand for the value it begins, and a `NULL` is not
shorthand for the default; both are refused. Its default is asked for by
leaving the argument out, or by passing the vocabulary as the signature spells
it — which is what leaving it out already does, and what lets a caller's own
wrapper repeat the signature and hand the argument on. No other vector of more
than one value is accepted, a reordering included. Several arguments do give a
`NULL` a
meaning, each stating for itself what it is, and those name a value, a column,
or a plan — such a name has a natural absent case for a `NULL` to mean.
An Option argument has none, because it already has a default that does
something, so a `NULL` reaching one is reported rather than resolved.
_Avoid_: Choice argument, enum argument, flag argument, mode argument

**Package condition**:
An error marginplyr itself raises, inheriting the `marginplyr_error` base
class. It reports something the caller can avoid by rewriting the call within
the documented public interface. It is always an error: marginplyr states what
it will not do by refusing, and raises no warning of its own. A subject it
names that the caller supplied — a column, a value, an argument — is spelled as
the caller spelled it, up to the two whitespace characters `?marginplyr` names,
so that what a reader searches for is what they wrote. The wording around that
subject is marginplyr's own and carries no promise.
_Avoid_: Package error, internal error, user-facing error

**External condition**:
A condition raised by a user summary expression, tidyselect, dplyr, or a
backend *as its answer to the question put to it*, and propagated with its own
class, its own diagnostic, and its own cause intact. Warnings and errors are
alike External conditions. What a Margin verb may adjust is the Condition
context; the condition itself is the caller's to receive unchanged.

An answer is what the qualification is for. A backend that reports an
expression as unsupported has answered; a backend that violates one of its own
invariants while working out what to answer has not, and the condition that
escapes is neither its answer nor a defect the caller can act on. Passing one
on unchanged satisfies every word of the first paragraph and still tells the
caller nothing they can use, which is how such a condition reads as
contract-abiding while being the shape ADR 0015 removes from marginplyr's own
code.
_Avoid_: Third-party error, foreign error

**Condition context**:
The lines an External condition carries naming where it arose — the argument
it is attributed to, the grouping values in force when it did, and the call
blamed for it. It is distinct from the condition's class and diagnostic. A
Margin verb owes the caller a context written in the names and the expressions
the caller can act on, so a grouping value is reported under the column the
caller named, an argument is quoted as the caller spelled it, and the blamed
call is the Margin verb the caller wrote, rather than the columns, the
expressions, and the calls marginplyr allocated to compute the grouping sets. A
context it cannot restate in those terms it leaves as it found it, because a
context that misdirects is still worth more than none.
_Avoid_: Error context, provenance, backtrace

**Repeated condition**:
One External condition raised once per grouping set, because a Margin
operation evaluates the caller's summary expression once per grouping set
although the caller wrote it once. Two occurrences are repetitions of one
condition when they agree on identity — the class, the diagnostic, and the
argument they are attributed to as the caller wrote it — and are distinct
conditions otherwise. Which grouping set produced an occurrence is never part of
that identity, since it is the part that necessarily differs, and neither is a
rewrite that differs between grouping sets because the occurrences sit in
different ones. A Margin verb reports a Repeated
condition once, and says how many further grouping sets raised it. It answers
only for the occurrences raised while it runs: a lazy input evaluates the
caller's expression when the caller later collects the result, and that is the
collecting call's to report.
_Avoid_: Duplicate warning, redundant condition, repeated diagnostic
