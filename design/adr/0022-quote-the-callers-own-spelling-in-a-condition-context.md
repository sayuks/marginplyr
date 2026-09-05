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
expressions a share reads, and `rewrite_grouping_dots()` replaces
`grouping_bit()` and `grouping_id()` with the branch's own constant. The contract is therefore written over
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
was rendered onto, through the same written-line reading the deduplication key
uses -- one shared helper, so the two readings cannot drift. cli wraps a bullet
it cannot fit, so a span read off the line a bullet opens is a prefix of the
label at any narrow width: the constant-rewrite collapse below held at 80
columns and not at 40, which is the console width deciding how many conditions
a caller receives. A run that is restated is emitted as the one line it was
written as, since what replaces it is a line of another length and cli is no
longer there to wrap it; every other run is given back exactly as it arrived.
A wrap cli had to make inside a token rather than at a space does not rejoin
to a label, and is left alone by the same rule as any other span that matches
nothing.

Which part of a message may be restated at all is bounded positionally, as
every reading of dplyr's format here is. A warning's rendered text carries the
caller's own diagnostic after its `Caused by` line, and a diagnostic can spell
anything -- including dplyr's bullet over a label a branch really handed dplyr
-- so only the runs before that line are dplyr's to restate, and a message
carrying no such line is left whole. An error needs no bound: its `$message`
is dplyr's bullet alone, and the caller's diagnostic is `$parent`, which is
never touched. Rewriting a caller's own text would be replacing an External
condition's diagnostic, which ADR 0015 rules out.

*Left whole* covers one aggregation as well as every non-aggregation, and is
stated as the bound rather than as a test for dplyr's involvement for that
reason: a caller whose own diagnostic renders empty is aggregated into
`There were 2 warnings in ...` with an argument bullet and no `Caused by`
line at all. Such a message is not restated, which is this decision's ordinary
degradation, and ADR 0021's identity does not collapse it either -- measured
on `main`, that plan reports twice, so nothing here made it worse.

A message in which nothing is restated is returned as the object that arrived,
byte for byte, rather than rebuilt from its lines -- rebuilding dropped a
trailing newline, and the degradation the constraint on #199 asks for is the
absence of any edit, not an edit that happens to read the same.

A restated line is rendered plain, and that is the contract rather than a
defect. The reading this shares with ADR 0021's identity undoes cli's rendering
before matching anything, so the text a restatement puts back carries neither
the styling nor the hyperlinks the line arrived with. The alternative was to
match against the reading and splice the replacement into the raw line, and it
was rejected: locating the span there rests on dplyr and cli not styling the
backticks around it, an undocumented property of exactly the kind that produced
#217 in the first place. Nothing is lost that a restated line was keeping
anyway -- such a line already collapses to the one line it was written as,
because what replaces it is a length cli is no longer there to wrap -- and
every line the restatement does not touch reaches the caller as it arrived, so
a caller in a colour session still receives a coloured warning.

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
`expr_as_label()` has two branches of its own: `rlang::as_label()` with
rlang's infix labelling suppressed through an undocumented option, and a data
pronoun — `.data$x`, `.data[["x"]]` — deparsed instead of labelled, where
`as_label()` answers `x`. marginplyr labels with plain `rlang::as_label()` and
the same `name = ` convention, so the two disagree in those two places, and in
both the span matches nothing and the quotation stands.

That costs nothing a caller can see. A label dplyr truncated renders the same
whichever expression it came from, so substituting the caller's own would print
the same `+...`; because the truncation removes what the branches differ in,
the deduplication key already agrees across branches without any restoration;
and an argument written as a pronoun is one the caller and the branch spell
alike, so the map drops it as unchanged and there is nothing to restore.
Reproducing the internals would buy an unobservable substitution in exchange
for depending on an internal name in two packages.

Neither divergence can quote the *wrong* expression, which is the failure
#199 rules out, and the reason is that they are divergences in dplyr's
direction only: `as_label()` emits neither `+...` nor `.data$x`, so dplyr's
label of one argument can never equal marginplyr's label of another.

## Considered Options

**Carry the caller's label as an attribute on each quosure.** Rejected on how
it fails. Four rewriters build fresh quosures, so each has to copy the
attribute, and one that forgets restores nothing while reading as protection;
whether an attribute survives `!!!` and dplyr's own `enquos()` is undocumented
besides. The labels travel beside the dots in one value instead, constructed
at the single point both halves are final — after every rewrite, remapped
where `plan_share_expressions()` already remaps the cardinality positions — so
a pair whose lengths disagree cannot be built, and one that stops agreeing
later is an invariant that stops the operation (ADR 0015).

**Align the caller's dots with the rewritten ones by position.** Rejected on
measurement: share planning drops a dot to `NULL` and expands a placeholder
into one dot per output before flattening, so position is not preserved.

**Reproduce `expr_as_label()` faithfully, undocumented option included.**
Rejected above: what it covers is unobservable. That reaches a condition label
and nothing else, which the amendment for #439 records.

**Extend the restatement to the native grouping-sets adapter.** Rejected as
covering nothing today, since both backends holding the capability are lazy.
It becomes worth revisiting if an eager backend ever gains
`native_grouping_sets`.

**Leave it and document it.** ADR 0021's position, and correct while the fix
looked like the fragile parse that ADR describes. It is not that parse.

## Consequences

A caller reading a restated context sees the expression they wrote, so the
context no longer names `dplyr::all_of()` over resolved column names, a share
wrapper, or `grouping_bit()`'s branch-local constant.

A warning that differs between branches only in `grouping_bit()`'s branch
constant is now one report saying how many further grouping sets raised it,
where it was one report per branch. That collapse holds at any console width,
which is what reading the bullet as a written line rather than a rendered one
buys; a test asserts it across the same widths as the one ADR 0021 added.

Width is not the only thing a rendered message varies with, and the other one
is inherited rather than introduced. In a session with colour, cli styles the
markers, so every pattern read here and in ADR 0021's identity misses and
both features no-op: measured on `main`, the `cube(region, grade)`
reproduction reports four times with `cli.num_colors` above one and once
without. This decision rests on the same reading and therefore inherits the
same bound; stripping the styling before either reading is what would lift it,
for both, and belongs with ADR 0021's identity rather than here. Filed as
#217.

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

## Amendment: the native adapter's translation error is restated too

*Considered Options* rejected extending the restatement to the native
grouping-sets adapter "as covering nothing today, since both backends holding
the capability are lazy". *Scope* names the exception in the same document — an
error raised while dbplyr translates the rewritten expression does arrive while
the verb runs — so the two disagreed about whether anything was there. The
exception is what is there, and it was never measured. #410 measured it:
`investigation/what-truncates-an-argument-label-on-the-native-adapter.md`.

The scope therefore includes that error. A caller whose summary expression
dbplyr cannot translate will be quoted the expression they wrote, on duckdb and
postgres as on a local input, and the sentence above confining the scope to
`summarize_margin_union()` no longer holds. #411 implements what follows.

What is extended is the restatement and not `with_branch_conditions()`. The
native adapter summarizes the caller's expressions and repeats nothing, so the
deduplication and the grouping-value restatement have nothing to act on; the one
sentence of the old reasoning that survives is that one. The error is caught,
`restate_condition_arguments()` applied with the map `branch_argument_map()`
builds from the rewritten dots and the caller's labels, and the condition
re-raised.

It is caught around `native_summary_output_names()` and not the grouped
`summarize()`. The adapter hands dplyr those expressions twice — once ungrouped
to learn the output names, once grouped to build the query — and the ungrouped
call runs first, so it is the one dbplyr translates and the one that raises.
Which of the two dbplyr reaches first is not a contract, and nothing here rests
on it being permanent: the tests assert the restored spelling through the verb,
so a dbplyr that moved the translation to the grouped call fails them rather
than dropping the restatement silently. #432 searched seventeen expression shapes
for one the grouped call refuses and the ungrouped call accepts, and found
none: `investigation/which-native-summarize-raises-a-translation-error.md`.
The only other thing that moves in #411 is forcing that call out of the check's
lazy argument, which keeps the check's own Package conditions outside the
catch.

Errors only. A branch `summarize()` on a lazy backend still builds a query
without evaluating the caller's expression, so everything the original reasoning
covers is still covered by it; a translation *warning* was searched for and not
produced, and writing the mechanism against an unmeasured shape is what the rest
of this decision declines to do.

`restate_argument_bullet()`'s pattern requires a trailing period, and dbplyr's
bullet does not carry one, so the mechanism reads the native bullet as matching
nothing and passes it through even where the map's key already equals the span.
The period becomes optional **and is put back as it was found**: rebuilding with
an unconditional period would add one to dbplyr's sentence, and the sentence
around the span is dplyr's.

One argument above stops carrying, and this is what replaces it. *dplyr's
rendering is reproduced only as far as it is observable* rests the #199
constraint on `as_label()` emitting neither `+...` nor `.data$x`, so that dplyr's
label of one argument can never equal marginplyr's label of another. On the
native adapter marginplyr's own label **is** `+...`, and the same measurement
makes that section's "substituting the caller's own would print the same `+...`"
false here. What holds in its place is the rule *The identification is a test
against marginplyr's own rendering* already states: a label two dots share is
restored only where the callers' own labels agree as well, and left as dplyr
wrote it where they do not. The constraint is met by that degradation rather
than by the asymmetry.

Tests assert on rendered messages through `dbplyr::simulate_postgres()`, which
needs no optional backend, so the native path is asserted wherever the suite runs
rather than only where a database is installed. Both directions are asserted, as
above: the restoration, and the shared label that leaves dplyr's quotation alone.

`R/summarize_with_margins.R` carries this ADR's rejected reasoning beside its
citation, as a comment saying no condition is raised while the verb runs. It was
false on the day it was written, and it is the second copy ADR 0023 names: the
citation stays correct across this amendment and the re-derivation did not. It
goes with #411.

## Amendment: the blamed call moves with the argument

#411 restated the native adapter's translation error's argument and left its
`call` as dbplyr set it, so the error blamed
`dplyr::summarize(dplyr::ungroup(.data), !!!dots)` — a call spelled with names
the caller never wrote. #432 is the ticket, and the argument for the change is
its.

The two are one context and not two, which is what this amendment records: a
Condition context is the argument, the grouping values, and the blamed call
together, and CONTEXT.md's entry owes all three in the caller's terms. Nothing
above ever decided that the native adapter keeps dbplyr's call — the section
this amends reaches only the argument, because restating it was the whole of
what #411 asked. So this extends the same amendment rather than reversing
anything in it: the adapter takes the Margin verb from the executor and assigns
it after `restate_condition_arguments()`, which returns the condition untouched
when the map is empty and is therefore not where the second half can live.

`with_branch_conditions()` is still not the vehicle, for the reason above: the
adapter issues one `summarize()` and repeats nothing, so the deduplication and
the grouping-value restatement have nothing to act on. What the two paths now
share is the field assignment alone, which is why it is written at each of them
rather than extracted.

The grouping values are the third part, and they need nothing here. The native
adapter groups by `pick(all_of(group_vars))` rather than by internal key
columns, so dplyr already reports them under the names the caller wrote.

## Amendment: an Assigned summary name reproduces the option

*dplyr's rendering is reproduced only as far as it is observable* rests on a
label dplyr truncated rendering the same whichever expression it came from, and
*Considered Options* rejects reproducing `expr_as_label()` on that ground. The
subject of both is a **condition label**, and there the argument holds
unchanged. It does not reach an Assigned summary name, which #430 introduced
after this decision was taken: that name is a column of the result, so
`... + 5` where dplyr writes `+...` is a column the caller subscripts by, and
`?summarize_with_margins` fixes dplyr's naming as the contract for `...`. #439
is the ticket.

`name_rewritten_summary_dots()` therefore labels through `dplyr_auto_name()`,
which is `rlang::as_label()` under the option dplyr sets. Reproducing the option
is the whole of it. `expr_as_label()`'s other branch deparses a bare data
pronoun instead of labelling it, and no expression reaching that call site is
one: a summary spelled `.data$x` is rewritten by nothing, so it is never
assigned a name, and a pronoun inside a larger expression is not the expression
the labeller reads.

Nothing else moves. `summary_argument_labels()` goes on labelling with plain
`rlang::as_label()`, for the reason the amended section gives, so the two
callers now spell one expression two ways where it abbreviates — which is what
the sections above already describe as the label matching nothing and the
quotation standing.

What the option's disappearance would cost is a silent return to the old
spelling, and the test is written against that rather than against a string:
it computes both the assigned name and `dplyr::summarize()`'s own name for the
same expression, and asserts separately that the expression is still one the
two labellers disagree about. An rlang that dropped the option fails the first;
one that stopped abbreviating at that width fails the second.
