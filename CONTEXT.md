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
is a column selection. The position never evaluates an argument to find out,
because a selection such as `starts_with("re")` has no meaning outside a
selection context. A specification a caller's own function returns is
therefore refused there, in marginplyr's own words and with the binding that
works, while the same call is accepted as `.grouping` itself. The position
answers for its own argument and not for a part of one: a specification
written inside a selection is the wrong kind of object where it sits, and
keeps the selection's own report.
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
A grouping dimension holding, as one of its own values, the Margin label
chosen for it — so that a margin row and a row of the source data cannot be
told apart by that column. A collision is *declared* when the colliding value
is a factor level, which the column's type records, and *observed* when it
appears only among the column's values.
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
analyzed. Every other name in a summary expression follows
ordinary lexical and data-mask lookup, including `dplyr::n()`. Grouping
specification constructors are not Contextual helpers: their spelling decides
only whether a nested argument is evaluated, and the caller's own function
runs when it is. A diagnostic refusing one tells the caller their spelling is
*reserved*; that is this same fact addressed to someone who has not read this
glossary, and not a second term.
_Avoid_: Contextual function, masked helper, reserved argument

**Package condition**:
An error marginplyr itself raises, inheriting the `marginplyr_error` base
class. It reports something the caller can avoid by rewriting the call within
the documented public interface.
_Avoid_: Package error, internal error, user-facing error

**External condition**:
An error raised by a user summary expression, tidyselect, dplyr, or a backend
and propagated with its original class and provenance intact.
_Avoid_: Third-party error, foreign error
