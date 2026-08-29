# Resolve Contextual helper names by static spelling

A Margin verb reads some of the names in a caller's expression before anything
runs, and rewrites what it finds. Which names those are, and whether a caller
who binds one of them in their own environment changes what the verb does, was
never written down. Issue #172 recorded the question as a spec conflict: the
package behaved three different ways across the families of names it reads, and
two of those disagreed with `dplyr::summarise()` in opposite directions.

This decision states the rule, records the mechanism behind the disagreement,
and fixes the one family whose execution did not match the analysis already
running against it.

Evidence: `investigation/contextual-helper-name-resolution.md` for what the
package and dplyr were measured to do, and
`investigation/contextual-helper-execution-mechanism.md` for which of dplyr's
two expansion paths each spelling takes, which is what the fix below rests on.

## Decision

### The rule

> A spelling whose meaning inside a Margin summary arises **only through static
> rewriting** is a **Contextual helper**: it is recognized by spelling and is
> never resolved from the calling environment. Every other name follows
> ordinary lexical and data-mask lookup.

It is stated as a criterion rather than as a list of families so that a name
added later inherits it without a second rule to keep in step. `CONTEXT.md`
carries the term.

The criterion is what a name *is*, not what marginplyr happens to look at. A
spelling marginplyr merely reads is not covered by it: `tibble()`,
`data.frame()`, and `tibble::data_frame()` are examined by
`known_summary_output_names()` so that a data-frame-valued summary's output
names are known before the query is built, but they are ordinary functions that
really run, and a caller who binds `tibble` gets their own function. Nothing
about their meaning arises from rewriting, so they are not Contextual helpers
and this decision does not reserve them.

### Namespace forms

A spelling is recognized when the name matches **and** the namespace is either
absent or the owning package. Any other qualifier passes through to ordinary
evaluation, where R answers it. `marginplyr::grouping_id()` and
`dplyr::cur_group_id()` are recognized; `stats::grouping_id()` and
`stats::pick()` are not, and fail with R's own "not an exported object" error.

`where()` has two owners, because tidyselect exports it and dplyr re-exports
it, so both qualifiers are accepted. That is a property of the name and is
recorded with the name.

### What the criterion decides

| spelling | resolution |
| --- | --- |
| `grouping_id`, `grouping_bit` | Contextual helper |
| `share_of_parent`, `share_of_total` | Contextual helper |
| `cur_group`, `cur_group_id`, `cur_group_rows`, `cur_data`, `cur_data_all` | Contextual helper — refused |
| `across`, `if_any`, `if_all`, `pick` | Contextual helper |
| `where` | Contextual helper |
| `grouping_set`, `grouping_sets`, `rollup`, `cube`, `grouping_spec` | ordinary lookup; the spelling gates *evaluation only*, in a nested specification position |
| `n`, and every other name | ordinary lookup |

`n()` is the case that shows the criterion is doing work rather than describing
a habit. dplyr's `n()` has real semantics that marginplyr does not rewrite, so
a caller who binds `n` gets their own function, exactly as under
`dplyr::summarise()`.

The constructor row is the one entry that is not a Contextual helper and is
still read statically. Its five spellings are the `constructor` fields of
`grouping_kind_rules()` and are written out here only for the reader; the
registry derives them, so a sixth kind adds a sixth spelling without this row
being edited — and `product` is the kind key of `grouping_spec()` rather than a
spelling of its own. A nested argument of a Grouping specification is
ambiguous between a tidyselect selection and a nested specification, so
`grouping_arg_spec()` evaluates a nested call only when its head names a known
constructor; evaluating every nested call would run `starts_with("re")` outside
a selection context. The gate therefore decides *whether the argument is
evaluated at all*, and the caller's own function runs when it is. That is
ordinary lookup with a static admission test in front of it, not a Contextual
helper.

### The recognized spellings live in one registry

`static_spelling_rules()` is the single table naming every recognized
spelling, its owning namespaces, and the family that decides what recognition
does. Every site that reads one of these spellings derives from it, and their
namespace test is implemented once inside it rather than repeated at each site.
Before this decision every reader carried a spelling and a test of its own, and
one of them — `where`, in `contains_selection_predicate()` — had no namespace
test at all;
`investigation/contextual-helper-execution-mechanism.md` tabulates them.

The language-capture primitives are read statically too and stay out of the
registry, for the reason `design/architecture.md` records: their `base`
namespace test answers to the calling environment, since a capture is refused
where the head names a binding the analysis can see, and every family here
refuses to read the calling environment at all. Putting them under one reader
would put two rules under it.

The table records per family whether its spellings are Contextual helpers, and
`contextual_helper_families()` is that subset. The distinction is load-bearing
rather than descriptive: what a Contextual helper promises is that a caller
binding cannot win, and asserting that of a constructor would assert the
opposite of the entry above. Three families are not Contextual helpers: the
Grouping specification constructors, and the data-frame constructors
`tibble()`, `tibble::data_frame()`, and `data.frame()`, whose spellings are
read only to predict a data-frame-valued summary's output names and which run
whatever the caller has bound.

Every entry in the table is a function, so asking about one family builds that
family alone. That is not an optimization: the share family derives from
`share_kind_rules()`, so an eagerly built table would make a
`grouping_helper_name()` lookup evaluate the contextual-share module for a fact
that is not about shares — the reach `design/architecture.md` separates on
#179's authority, arriving one module further out. `test-contextual-helpers.R`
asserts the laziness rather than trusting it.

The table does not restate what another table already owns. The share family is
derived from `share_kind_rules()` and the constructor family from
`grouping_kind_rules()`, both of which existed and were already the
authoritative description of their helpers; a third helper added to either
appears in this registry without being written down again. ADR 0008 made the
grouping kind rules authoritative for the specification grammar, and this
decision extends that registry's reach to name recognition rather than starting
a competing list.

### Refusing a shadowed `cur_group_id()` is deliberate

dplyr resolves `cur_group_id()` by ordinary lookup, so refusing one that a
caller has shadowed is stricter than dplyr. It stays refused. Honouring the
shadow would mean resolving a call head against the calling environment, which
is the reflective lookup #130 deliberately declined to perform, and which the
rule above forbids by construction.

What changes is the diagnostic, which said only that the verb "does not
support" the helper and left a caller who had bound the name themselves with no
way to tell why their own function did not run. It now says that the spelling is
reserved and is not resolved from the calling environment.

The message keeps `does not support` as its opening phrase. Six assertions
match that phrase by regular expression rather than by condition class, and the
phrase is still true; nothing is gained by making them all say something else.

### Analysis and execution must agree

The criterion has a consequence that is stronger than a naming rule: if a
spelling is recognized statically, the code that runs has to be the code the
analysis read. Otherwise a rule is checked against an expression that does not
execute, which is what #172 found.

**`across()`, `if_any()`, and `if_all()` did not satisfy this.**
`find_summary_context_helpers()` and `rewrite_summary_selections()` recognized
the spelling and checked the grouping-column exclusion rule against it, and a
caller binding of the same name then ran instead:

```r
across <- function(...) "CALLER"
summarize_with_margins(d, k = across(units, sum), .grouping = rollup(region))
#>   region      k
#> 1      E CALLER
```

**The mechanism is in dplyr, and it is not head qualification.**
`dplyr:::summarise_cols()` processes each dot as `expand_pick()` and then
`expand_across()`, and the two have different reach:

- `expand_pick_call()` matches `pick` syntactically and descends through every
  argument of the expression, so a `pick()` anywhere in a dot is expanded
  before evaluation and no binding can capture it. It stops at `~` and
  `function`, which is the one position where plain dplyr does let a caller's
  `pick` run.
- `expand_across()` returns the quosure untouched unless the dot's **top-level**
  expression is a call to `across` **and** the dot is unnamed. A named dot
  (`k = across(...)`), a nested `across()` (`ncol(across(...))`), and every
  `if_any()`/`if_all()` in a summary are left to be evaluated in the data mask,
  where the head resolves by ordinary lexical lookup from the quosure
  environment.

Every summary marginplyr stages is a named dot, so it always took the second
path. The asymmetry with `pick()` was never about the call head — both rewrites
end in `rebuild_static_call()`, which preserves it — but about which of dplyr's
two paths the dot reached. Measured against dplyr 1.2.1 and recorded in
`investigation/contextual-helper-execution-mechanism.md`.

**The fix is to qualify the recognized head.** A recognized `across`, `if_any`,
`if_all`, or `pick` is rebuilt as `dplyr::across` and so on, so the call that
executes names the function the analysis read. Qualification is chosen because
it is the smallest change that closes every position at once: it settles the
runtime lookup in the data mask, and it does not disturb dplyr's own static
expansion, which accepts an absent or `dplyr` qualifier alike. dbplyr's
`partial_eval()` matches these four names without examining the qualifier, so
lazy backends are unaffected by the change.

Marginplyr's own walk descends into a `function` or `~` written as an
*argument*, where dplyr's expansion does not, so qualification also makes those
positions agree — a place where marginplyr previously analysed a helper that
plain dplyr would have let a binding capture.

It does not descend into a function literal used as a *call head*:
`(function() cur_group_id())()` has no readable name, so the walk reads it as a
call to nothing and never visits the body. That blind spot is uniform — the
analysis and the execution agree, because neither sees it — so it is not the
disagreement this decision fixes, and it is not introduced by it. It is the
same conservative reading of a computed head that "Boundary for callable
identity" above records, and it belongs to #178's question rather than this
one.

The marginplyr-owned helpers need nothing here. `grouping_id()`,
`grouping_bit()`, `share_of_parent()`, and `share_of_total()` are error stubs
whose bodies raise; inside a Margin verb they are rewritten away and the
function object is never called, so there is no execution for an analysis to
disagree with.

### Boundary for callable identity

Identity here is **syntactic**, never environmental. `(grouping_id)(region)` is
the same spelling through transparent parentheses, and treating it as such is a
change to what counts as the same written call. `get("grouping_id")(region)`
and a caller binding both require reading the environment, and stay outside the
rule under the conservative #130 policy.

#178 asks for the parenthesized form to be recognized. This decision fixes its
reading in advance so that it is a change to spelling normalization and not a
first step toward environmental resolution. Parenthesis transparency is not
implemented here; the amendment at the end of this file is where it is.

### Recorded behaviour changes

Three, all of them a name resolving differently. `DESCRIPTION`'s
`Config/marginplyr/cran-status` reads `unpublished`, so no released version
carried any of the old behaviour.

**A shadowed `across`, `if_any`, or `if_all` now runs dplyr's helper.** This is
the change the decision is for, and the one the acceptance criteria name.

**A shadowed `pick` now runs dplyr's `pick()` in a `~` lambda or a `function`
body.** dplyr's `expand_pick_call()` returns early on both, so a caller's
binding used to run there while marginplyr's own walk — which descends into
both — had already analysed the call. It is the same disagreement as the first
change, in the one position where plain dplyr has it too, and qualification
closes it in the same motion.

**A `where()` qualified with a package that does not own it is no longer read
as a selection predicate.** `contains_selection_predicate()` matched the name
with no namespace test at all, so `mypkg::where(...)` inside a contextual
share's `across()` tripped a refusal written for tidyselect's. Bringing it
under the namespace rule is what makes the rule uniform; the cost is that such
a call now reaches ordinary evaluation, which is what the rule says it should
have done all along.

## Test strategy

The regression tests derive their cases from `static_spelling_rules()` rather
than enumerating spellings, so a spelling added to the registry is covered
without editing the tests. Each registered spelling is exercised unqualified,
namespace-qualified, under a foreign qualifier, and against every other
family; each Contextual helper is additionally exercised with a caller binding
of the same name in scope, on a local input and on a lazy one.

A behaviour case needs a call to write, which no rule can derive, so the probe
that supplies one per helper is asserted to cover exactly the registered
Contextual helpers. That assertion is what keeps the derivation honest: a
spelling registered with no probe fails there rather than being silently
uncovered, which is the failure a hand-maintained list produces.

An enumeration of demonstrated spellings was rejected for the reason
`verify-site.R` derives its page list: a list that is maintained by hand
records what someone remembered to add, and the failure it has to catch is a
spelling nobody remembered.

## Considered options

**Resolving a Contextual helper from the calling environment, matching dplyr's
treatment of `cur_group_id()`.** Rejected. It is the reflective lookup #130
declined, it cannot be done at all for the four marginplyr helpers, whose
function objects only raise, and it would make the analysis a Margin verb runs
before execution depend on bindings that analysis cannot see.

**Leaving `across()` as it was and removing the static rules checked against
it.** Rejected: the grouping-column exclusion rule is the rule
`summarize_with_margins()` exists to extend across grouping sets, and it has to
be checked before a query is built (ADR 0005).

**Rewriting the head to a call to the function object rather than to a
qualified name.** Rejected: a call whose head is a function value is not
readable by dbplyr's `partial_eval()` or by dplyr's own expansion, both of
which match a name, so it would trade a caller-binding problem for a backend
problem.

**Refusing a summary that shadows a Contextual helper.** Rejected: the shadow is
often incidental — a caller with their own `pick` used elsewhere in a script —
and refusing the call teaches nothing the Contextual helper rule does not
already say.

**Registering the data-frame constructors (`tibble()`, `data.frame()`)
alongside the helpers**, since they too are read statically. Rejected above:
they really run, so reserving them would change behaviour rather than describe
it.

## Related decisions

- ADR 0007 fixes where a caller's expressions are captured. This decision
  covers how names inside those captured expressions are resolved.
- ADR 0008 makes the grouping kind rules authoritative for the specification
  grammar; the constructor spellings this decision reads are derived from that
  registry.
- ADR 0005 requires locally decidable errors before a backend read, which is
  why the recognized spellings are read statically at all.
- ADR 0015 separates Package conditions from internal invariants; the refusal
  diagnostic reworded here is a Package condition.
- `design/architecture.md` gains the registry as a module and records why it
  builds one family at a time.
- #190 is split out of #172 and is not decided here. Nested specification
  positions refuse a genuine `margin_grouping_spec` returned by a caller's own
  function, with a diagnostic saying the object is the wrong kind for a
  position where it is exactly the right kind. It sits one layer below this
  rule — the gate above decides whether the argument is evaluated, and #190 is
  about what happens to the value once it is.
- #178 is the parenthesized spelling, bounded by "Boundary for callable
  identity" above and implemented in the amendment below.

## Amendment: redundant parentheses are read through, in one place

"Boundary for callable identity" above fixed the reading and left the
transparency unimplemented. #178 implements it. Nothing the decision states
changes: identity is still syntactic, a caller binding still cannot win, and a
head that must be evaluated to know what it calls is still unresolved.

**Both positions a caller can write a pair in.** `(grouping_id)(region)` is the
pair around the head, which the decision above names. `(grouping_id(region))`
is the pair around the whole call, which it does not, and the two are one
question: `(` is the identity function, so all three writings of that call
evaluate identically and R's own parser records the difference in the tree
rather than in what runs. Both are read through, however many pairs deep.

**One place, not one place per family.** The reading lives in the shared
readers of `R/utils.R` — the name, the namespace, the head, and the arguments —
so every analysis that reads an expression inherits it: the registry's four
recognition sites, the constructor gate, the data-frame output-name
prediction, the share dependency walk, and the two rewrites. That is what the
ticket asks for over a fix at the sites that had been demonstrated, and it is
the same argument the registry itself rests on. Name and operands are read
through the same unwrapping, so no node is named as its content and
subscripted as its wrapper — the split the quosure reading in that file already
warns about, reached by a different route.

`static_callee_name()` is brought under it rather than left beside it. That
reader carried a pair-of-parentheses rule of its own from #130, written with a
different arity test, so a `(` call built by hand with two arguments was a
droppable pair to one reader and a call to `(` to every other. One rule now
answers both.

**Where the head unwrapping stops.** Only down to a name. `("sum")(1)` is not a
call to `sum` — R raises "attempt to apply non-function" for it, while a bare
`"sum"(1)` is parsed as the symbol — and `(function(x) x)(1)` names nothing
either way. Unwrapping to either would make this package recognize, and
rewrite, a call R refuses or cannot name.

**The one place the two rules pull apart.** A capture is refused where the head
names a binding the analysis can see, which is the one static reading here that
answers to the calling environment. A parenthesized head is evaluated as a
*value*, so R's function lookup never runs and any binding wins outright:
`(quote)(share)` therefore claims no capture at all and the walk reports the
read it would otherwise hide, while `(pick)(units)` is dplyr's `pick()`
whatever the caller has bound. Reading the same pair of parentheses as a
spelling in one place and as an unresolvable head in the other is the
asymmetry the registry's exclusion of the capture primitives already records.

**Every writing this makes recognized was previously an error**, so no result
changes into a different result: a helper stub reporting it can only be used
inside the verb, `object 'pick' not found` from the data mask, a tidyselect
refusal of a specification object, or a share refused for not being the
complete right-hand side. `DESCRIPTION`'s `Config/marginplyr/cran-status` reads
`unpublished`.

Two consequences reach expressions this does *not* make recognized, and both
are recorded rather than avoided.

*A caller's redundant parentheses do not survive into the rewritten
expression.* The rewrites rebuild every call they descend into, and they
rebuild it from the node the readers unwrapped, so a summary written
`sum((a + b))` is staged as `sum(a + b)` and a diagnostic dplyr raises about
that argument echoes it without the pair. What is evaluated is unchanged —
the pair is redundant in the tree, not only on the page — and this is the
normalization the decision above already performs far more visibly, since
`across(v, mean)` reaches the same diagnostics as
`dplyr::across(dplyr::all_of("v"), mean)`. Preserving the pair would mean
rebuilding a node in a spelling the analysis had just been written not to
distinguish.

*A name written as a parenthesized literal is now resolved rather than
over-reported.* `get(("share"))` reads the share it names; before, the string
was unreadable through the pair, and the share dependency walk answered an
unresolvable lookup by reporting a read of every alias in scope. Both refuse a
summary that reads an earlier share, so the change is which summaries are
refused *besides* that one, in the direction of the read the caller actually
wrote.

**Test strategy.** The writings are derived, as the spellings are. Each
registered spelling is exercised bare and under every owner, and each of those
with a pair around the head, two pairs around the head, and a pair around the
whole call — locally and on a lazy input, with and without a caller binding of
the same name. Two pairs are what says the reading is not one pair deep. The
foreign-qualifier writings are asserted to *differ*, which is what stops the
agreement being two calls that both fail the same way, and the unresolvable
heads are asserted unrecognized so that the boundary is a case rather than a
sentence.

## Amendment: an injected name is read for the name it carries

The decision above fixes how a Contextual helper's *spelling* is resolved and
says nothing about the arguments it reads. #169 is that question, asked of the
four helpers marginplyr owns, each of which takes a bare name —
`grouping_bit()` and `grouping_id()` a grouping column, `share_of_parent()` and
`share_of_total()` a preceding ordinary summary. All four asked whether the
argument was a symbol — the grouping helpers through `is_name_part()`, which
#181 had already given them, and the share helpers through
`rlang::is_symbol()` — and a quosure carrying a bare symbol is not one, so each
refused the standard tidy-eval wrapper:

```r
level_by <- function(data, col) {
  q <- rlang::enquo(col)
  summarize_with_margins(
    data,
    level = grouping_id(!!q),
    .grouping = rollup(region)
  )
}
level_by(d, region)
#> Error: `grouping_id()` only accepts bare grouping columns.
```

The caller did write a bare grouping column, one call further out. That is the
property #163 and #165 were each filed over: a diagnostic describing the
caller's input incorrectly. There was a workaround — `!!rlang::ensym(col)`
injects a symbol and has always worked — which is why the severity was low, and
nothing in the message said so.

**The rule.** An argument read as a bare name is read through an injected
quosure, however many deep, and only what it carries decides the answer. A
quosure carrying a bare name *is* one. A quosure carrying anything else —
`.data$region`, a string, a call, the empty argument — is refused exactly where
that expression is refused when it is written out, and by the same diagnostic.

It is one rule for all four helpers, and it mirrors the un-injected spelling
rather than adding a second grammar for the injected one. Accepting a string
under injection was the alternative, and was rejected for that reason: it would
make `share_of_total("t")` a refusal and `share_of_total(!!rlang::quo("t"))` an
acceptance, which is a difference no caller can be told the reason for.

**The environment is discarded, and that follows from the decision above rather
than sitting beside it.** A Contextual helper's argument is resolved against
the Grouping plan, or among the preceding summaries, *by spelling* — never
looked up in an environment — so there is no lookup for a quosure's environment
to answer. Consulting it is not declined here; it is not possible, and the
statement is what stops that reading like an omission.

That is the opposite of the answer #165 reached one layer out, where a walk
keeps a quosure's environment because a selection inside it really is evaluated
there, and the two are one rule seen from two sides: **the environment is
honoured wherever evaluation happens, and these arguments are never
evaluated.** A caller who injects a quosure built where `region` is bound to
something else gets the column `region`, exactly as writing `grouping_id(region)`
does. The Grouping specification constructors are the case that shows the rule
is about the criterion and not about ownership: they are marginplyr's too, and
they are not Contextual helpers, so their nested arguments *are* evaluated and
the caller's environment decides.

**The diagnostic changes for every form that stays refused.** The headline is
unchanged, because it is still the right thing to say about what the helper
accepts, and a clause is added naming the injection and what it carries:

```
`grouping_id()` only accepts bare grouping columns. The injected quosure
carries `.data$region`, which is not a bare name.
```

What the clause is for is not correcting the headline, which is accurate here —
what it refuses really is not a bare name, the case where it is one having been
accepted above. It is that the expression the headline is about is not on the
page the caller is reading: the source says `grouping_id(!!q)`, and nothing in
`only accepts bare grouping columns` says what `q` turned out to hold. The
written spellings need no clause because there the refused expression is
written out. The reader who *did* write a bare name is answered by the rule
rather than by any message, since their call now works.

The clause is absent from a refusal that has nothing to do with an injection,
which is what stops it describing a mistake in the other direction. It writes
the carried expression with `deparse1()` rather than with `rlang::as_label()`,
the house spelling elsewhere: `as_label()` reads `.data$region` as `region`, so
a message saying the part is not a bare name would quote it as one.

**Where the reading lives.** `unwrap_injected_quosure()`,
`unwrap_injected_args()`, `injected_quosure_clause()`, and `call_part_label()`
sit in `R/utils.R` beside `is_name_part()` and `static_call_args()`, which is
where the shared readers of a call's parts already are, so the four helpers ask
one question rather than four. The unwrapping is a recursion and not a `while`
loop, because the loop form assigns the carried expression to a name and a
quosure carrying the empty argument then raises base R's untyped
`missingArgError` — the #168 hazard `static_call_args()` already documents,
reached by a different route.

`unwrap_injected_args()` is a named reader over a whole argument list rather
than an `lapply()` spelled out at each call site, and that is what keeps the
same hazard's gate derived: the scans in `test-utils.R` recognize a list of call
parts by the reader that produced it, so adding a reader to that list is how
this package says a list of parts came from somewhere new. Why a mapping cannot
be recognized in a reader's place is recorded beside that scan.

`injected_quosure_clause()` reads the arguments as written rather than what the
unwrapping carried out of them, because whether a part arrived injected is
precisely the fact the unwrapping discards and precisely the fact the clause
reports.

**The clause is composed by the message, not by the argument.** A caller can
make two mistakes at once, and which diagnostic wins is each helper's own
decision rather than the clause's. The share helpers' headline covers the arity
and the name-ness together — "exactly one bare name" — so a two-argument call
carrying an injected non-name takes the clause. `grouping_bit()` counts columns
in a message of its own, reached before the non-column one deliberately (#181),
because a caller who passed two of anything needs the count rather than a
remark about one of them; that message does not compose the clause. What #169
requires to be one answer across the four helpers is *which injected forms are
accepted*, and that is one answer.

**Recorded behaviour change.** Three, each a refusal becoming an acceptance or
a better-worded refusal; no result changes into a different result.
`DESCRIPTION`'s `Config/marginplyr/cran-status` reads `unpublished`, so no
released version carried the old behaviour.

Two are the rule and the diagnostic above: an injected quosure carrying a bare
name is now accepted where all four helpers refused it, and one carrying
anything else is refused with a clause naming the injection.

The third is not about injection, and it is recorded here because it is a
change to an un-injected spelling, which #169 fenced off as out of scope.
Asking the name question once meant asking it with `is_name_part()` in the
share helpers too, where it had been `rlang::is_symbol()`. That is not
behaviour-neutral: the empty argument is a symbol whose name is `""`, and
`share_of_total(x = )` is *one* argument and empty, so it was admitted and
refused one layer on as

```
Total share `k` refers to unknown preceding ordinary summary ``.
```

which names a summary nobody wrote — #181's defect, in the family that ticket
did not reach. It is now the share helpers' own "exactly one bare name"
refusal. Restoring `rlang::is_symbol()` on that branch was the alternative and
was rejected: it would reinstate a diagnostic describing input the caller did
not write, which is the property this whole amendment exists to remove, and it
would leave the four helpers asking two different questions to preserve it.

**Test strategy.** The four helpers are derived from `static_spelling_rules()`
as the registered spellings marginplyr owns *and* declares Contextual, so a
fifth owned Contextual helper fails the coverage assertion rather than
inheriting an answer nobody decided. Both halves of that criterion do work:
none of the dplyr-owned families reads a bare name — a selection, a predicate
function, and nothing at all, across the three — and the constructors are owned
but not Contextual. Each helper's probe is a function from the argument to the whole
call, since the argument is the only thing that varies, and the refused shapes
are derived from the bare name that helper takes rather than written out. The
acceptance case injects a quosure built on the *empty* environment, so it
asserts the environment answer in the same expectation as the acceptance one,
and the refusal case asserts message equality with the written spelling plus
the clause, which is what stops the injected form reporting a different fault.
A caller's two mistakes at once are a case of their own, since which message
wins is a per-helper decision that no equality above would notice changing, and
so is the named empty argument, which is the only writing that reaches the
un-injected change recorded above.

## Amendment: the read-through reaches a reading, not an expression

"Redundant parentheses are read through, in one place" above put the reading in
`R/utils.R`'s shared readers and said that every analysis inherits it, naming
the constructor gate among them. That is true of what those readers answer — a
name, a namespace, a head, a set of arguments — and it is not true of a site
that puts a shape test to the caller's own expression instead of asking one of
them. Such a site inherits nothing, and there is no reading for it to inherit:
`is.symbol()` is R's, and `(s)` is a call to `(`.

Three sites in `R/grouping-plan.R` were of that kind, and #259 brings them
under the rule. `grouping_arg_spec()` recognized a nested constructor call
through a pair of parentheses, because it reads that call's name through
`static_spelling_name()`, while asking `is.symbol()` about a name and
`is.language()` about an injected object — so `(rollup(region))` was the
specification it is and `(s)` and `(!!s)` were column selections.
`check_ambiguous_nested_name()` asked `is_name_part()`, so ADR 0026's refusal
of a name two readings claim was withheld from `(s)`, which selected a
colliding column silently — the reading that ADR exists to remove, reached by a
pair of parentheses. And `resolve_grouping_selection()` compared
`rlang::as_label()` of the caller's expression against the subscript tidyselect
refused; tidyselect descends into a `(` call before refusing anything, so the
two labels could not match and #190's diagnostic was withheld from
`(f(region))`.

**What the amendment above establishes still stands**, and the correction is to
its reach rather than to its argument. Identity is still syntactic, a caller
binding still cannot win, and the reading still lives in one place: these three
sites now *ask* for it, through `nested_arg_expr()`, which is
`unparenthesized_value()` put to a quosure; the gate additionally restarts on
the argument the pair wraps, because what follows its shape test is an
evaluation of that argument. Nothing is re-implemented, which is what "one
place, not one place per family" means for a caller.

**The refusal names the argument the pair wraps, and that is decided here
rather than by ADR 0024.** `resolve_grouping_selection()` writes one label, and
it is both the key compared against tidyselect's subscript and the subject
`abort_nested_grouping_spec()` names, so `grouping_sets((f(region)))` is
refused in the words `grouping_sets(f(region))` is refused in. Splitting the
two would let the refusal name `(f(region))`, which is closer to what ADR 0024
asks of a Package condition's subject. It is not taken, for the reason the
first amendment already accepts of a rewritten expression — "a diagnostic dplyr
raises about that argument echoes it without the pair" — and because a caller
who wrote one spelling and read the other's diagnostic would be reading a
difference this whole amendment exists to say does not exist. What ADR 0024
protects is a spelling a reader searches their source for, and a redundant pair
is one they find their argument inside of; what it was written for is a value
whose bytes cli would otherwise collapse, which no parenthesis rule touches.

**What a read-through argument gains is the reading of the argument it wraps,
evaluation count included.** Measured against the pre-#259 package through
`inspect_grouping()`, with the binding an active binding and the caller's
function counting its own calls: no *bare* argument's count moves, and a
parenthesized name gains what the bare name costs. `grouping_sets((s))` reads
`s` once before and three times after, which is what `grouping_sets(s)` read
before and reads still — whether the binding is a specification the gate then
recognizes or something else it then declines, since the gate has to read one
to tell them apart.

That is a cost, and it is the decision rather than an oversight: a spelling
read as another spelling costs what that spelling costs, and a parenthesis
rule that stopped short of the evaluation would be the two readings this
amendment exists to remove, moved one step down. A pair around a *call* is free
by comparison, the gate having recognized `(f(x))` all along.

#259's sixth acceptance criterion asks for an unchanged count "for every
argument whose reading does not change", and blesses the recognized `(s)` that
gains one. The shape between the two is a parenthesized name bound to
something that is *not* a specification: the plan is what it was, and the gate
now reads the binding to establish that. It is the same read on the same line,
so it is accepted with the criterion's second sentence rather than against its
first.

Pinning each form's number is #260's, so what #259 asserts is the equality of
the two spellings' counts: neither can drift from the other without the
assertion failing, and nothing pins the same number twice.

**Where a pair is not around the argument, nothing changes.** `c((s), region)`
is a selection containing a specification, which tidyselect refuses and reports
for itself; the label the position compares is the whole argument's, which is
not the sub-selection tidyselect refused. That separation is the reason the
comparison exists, and reading through does not touch it.
