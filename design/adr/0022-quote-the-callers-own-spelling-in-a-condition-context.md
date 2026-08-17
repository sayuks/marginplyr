# Quote the caller's own spelling in a Condition context

A Margin verb's Condition context quotes the expression the caller wrote rather
than marginplyr's rewrite of it, whenever it can identify which of the caller's
summary arguments dplyr attributed the condition to. Where it cannot, the
quotation is left as it was found, which is what ADR 0021 already requires of
every part of a context a verb cannot restate in the caller's terms. This
completes the third part of #141's context leak, which ADR 0021 deferred.

The rewriting is not one rewrite. Four layers separate what dplyr quotes from
what the caller typed: `resolve_summary_selections()` turns a selection into
`dplyr::all_of()` over resolved source names, `across()`'s `.names` is evaluated
and its arguments normalised for dtplyr, `wrap_share_sources()` wraps the
expressions a share reads, and `rewrite_grouping_dots()` replaces a Grouping
helper with the branch's own constant. The contract is therefore written over
expressions rather than over any one of those layers, and one mechanism covers
all four.

## The identification is a test against marginplyr's own rendering

ADR 0021 rejected this fix as "deparsing the caller's dots and text-matching
them against dplyr's own deparse of the rewritten ones". That is the wrong end
of the comparison, and rejecting it was right. What the fix actually compares is
dplyr's quotation against *marginplyr's own rendering of the expression it
handed dplyr*: each branch dot is labelled the way dplyr labels it, the labels
are compared for equality with the backticked span dplyr quoted, and a span that
equals exactly one branch dot's label is replaced by the label of the dot the
caller wrote. Both sides of that equality are expressions marginplyr generated,
so nothing is parsed out of dplyr's formatting except the span itself, and a
span that matches nothing changes nothing.

Where several branch dots share a label, the substitution runs only if the
callers' own labels agree as well. A label is `name = expr` for a named
argument, so the collision needs two unnamed arguments rendering alike; there
the replacement is unique whichever one dplyr meant, and where it is not the
quotation stays as dplyr wrote it.

An error needs no span at all. Its `$message` is the argument bullet and
nothing else — measured, one named character vector whose name is `i` — so the
bullet is rebuilt rather than edited. A warning's is one bullet inside the
rendered text dplyr flattened before signalling. Only the span inside the
backticks is replaced: the sentence around it is dplyr's, and editing a
sentence is what ADR 0021 refused for the blamed call, because a wording change
would leave it silently naming the wrong thing rather than falling back.

The bullet is read as the line it was *written* as rather than as the lines it
was rendered onto, through the same `message_line_runs()` the deduplication key
uses, and for the same reason. cli wraps a bullet it cannot fit, so a span read
off the line a bullet opens is a prefix of the label at any narrow width: the
Grouping-helper collapse below held at 80 columns and not at 40, which is the
console width deciding how many conditions a caller receives. A run that is
restated is emitted as the one line it was written as, since what replaces it
is a line of another length and cli is no longer there to wrap it; every other
run is given back exactly as it arrived. A wrap cli had to make inside a token
rather than at a space does not rejoin to a label, and is left alone by the
same rule as any other span that matches nothing.

## The restoration runs before the deduplication key is computed

This is not only a matter of spelling. `total = sum(as.numeric(grade)) +
grouping_bit(region)` under `rollup(region)` renders `+ 0L` in one branch and
`+ 1L` in the other, so `branch_warning_identity()` sees two identities and the
caller receives two reports of a warning they provoked with one expression.
Restoring the spelling first collapses them, for the same reason ADR 0021 gives
for excluding the grouping values from an identity: what necessarily differs
between branches is marginplyr's, not the caller's. CONTEXT.md's *Repeated
condition* entry therefore reads "the argument they are attributed to" as the
argument the caller wrote.

## Scope, and why it is the same scope as ADR 0021 for a different reason

The scope is `summarize_margin_union()`, as ADR 0021's is, and the reason that
ADR gives does not carry: the selection rewrite is not the union adapter's.
`summary_all_of_expr()` runs inside `plan_summary_expressions()`, before either
adapter, so the native grouping-sets adapter hands dplyr the same rewritten
expressions.

What confines this instead is which backends hold `native_grouping_sets`:
duckdb and postgres, both lazy. A branch `summarize()` there builds a query
without evaluating the caller's expression, so there is no condition raised
while the verb runs and nothing to restate — the same reason ADR 0021 gives for
covering eager inputs only. The exception is an error raised while dbplyr
translates the rewritten expression, which does arrive while the verb runs and
is left quoting the rewrite.

## dplyr's rendering is reproduced only as far as it is observable

dplyr labels an argument with `error_label_named()`, which is
`paste0(name, " = ", expr_as_label(expr))` for a named argument, and
`expr_as_label()` calls `rlang::as_label()` with rlang's infix labelling
suppressed through an undocumented option. marginplyr labels with plain
`rlang::as_label()` and the same `name = ` convention, so the two disagree
exactly where dplyr abbreviates a long infix expression — `total = +...` where
`as_label()` answers `sum(as.numeric(grade)) + ...` — and there the span matches
nothing and the quotation stands.

That costs nothing a caller can see. A label dplyr truncated renders the same
whichever expression it came from, so substituting the caller's own would print
the same `+...`; and because the truncation removes what the branches differ in,
the deduplication key already agrees across branches without any restoration.
Reproducing the option would buy an unobservable substitution in exchange for
depending on an internal name in two packages.

## Considered Options

**Carry the caller's label as an attribute on each quosure.** Rejected on how
it fails. Four rewriters build fresh quosures, so each has to copy the
attribute, and one that forgets restores nothing while reading as protection;
whether an attribute survives `!!!` and dplyr's own `enquos()` is undocumented
besides. The labels are carried as a parallel vector instead, remapped where
`plan_share_expressions()` already remaps the cardinality positions, and a
length that stops agreeing with the dots is an invariant that stops the
operation (ADR 0015).

**Align the caller's dots with the rewritten ones by position.** Rejected on
measurement: share planning drops a dot to `NULL` and expands a placeholder
into one dot per output before flattening, so position is not preserved.

**Reproduce `expr_as_label()` faithfully, undocumented option included.**
Rejected above: what it covers is unobservable.

**Extend the restatement to the native grouping-sets adapter.** Rejected as
covering nothing today, since both backends holding the capability are lazy.
It becomes worth revisiting if an eager backend ever gains
`native_grouping_sets`.

**Leave it and document it.** ADR 0021's position, and correct while the fix
looked like the fragile parse that ADR describes. It is not that parse.

## Consequences

A caller reading a restated context sees the expression they wrote, so the
context no longer names `dplyr::all_of()` over resolved column names, a share
wrapper, or a branch-local Grouping-helper constant.

A warning that differs between branches only in a Grouping helper's constant is
now one report saying how many further grouping sets raised it, where it was
one report per branch. That collapse holds at any console width, which is what
reading the bullet as a written line rather than a rendered one buys; a test
asserts it across the same widths as the one ADR 0021 added.

A restated bullet is one line where dplyr may have wrapped it over several. The
alternative was re-wrapping text cli had already laid out, which would rewrite
lines the caller's own diagnostic wrote.

`dplyr::last_dplyr_warnings()` keeps the rewritten expressions, for the reason
ADR 0021 gives for the key names: that store is dplyr's and is written before
marginplyr sees anything.

A warning still names `dplyr::summarize()` as the call it arose in. Nothing here
changes that asymmetry, which ADR 0021 records.

Tests assert on rendered messages rather than on structure, as ADR 0021's do,
and two of them assert the degradations rather than the restorations: a long
infix expression exercises the non-matching span through a real call, and a
synthetic condition standing in for a dplyr wording change asserts that a span
matching nothing is left alone. A restoration that quietly stopped happening
would otherwise read exactly like a package whose contexts were all still
faithful.
