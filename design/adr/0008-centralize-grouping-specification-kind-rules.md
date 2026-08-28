# Centralize Grouping specification kind rules

Grouping specification kinds are interpreted during admission, structural
preflight, nesting validation, and expansion. We will make one private
strategy registry in the Grouping plan module authoritative for those
semantics. Each kind rule will identify its public constructor and provide
behavior for empty-argument validation, nested-specification validation, and
expansion. This removes the repeated kind lists and conditions identified in
Issue #14 without turning them into a table of flags and error strings.

## Decision

The registry will be keyed by the existing internal kind values `set`, `sets`,
`rollup`, `cube`, and `product`. Its order will preserve the current public
constructor order. Valid kinds and the constructor names recognized while
examining nested arguments will be derived from this registry rather than
maintained as separate vectors.

Every rule will own:

- the corresponding public constructor name;
- `validate_empty()` behavior;
- `validate_nested()` behavior; and
- `expand()` behavior.

The common traversal will validate the specification class and basic shape,
look up its kind rule, and invoke that behavior without switching on the kind.
It will recursively validate a nested specification before asking the parent
rule to validate the parent-child relationship. Existing error helpers and
kind-specific validators will continue to construct errors; the registry will
not become a catalog of Boolean permissions or message strings.

The rules express the existing grammar:

- `set` accepts column selections only and allows an empty argument list to
  represent the empty grouping set;
- `sets` accepts column selections or any valid nested kind, combines their
  families by union, and rejects an empty argument list;
- `rollup` and `cube` accept column selections or a non-empty `set` as a
  composite dimension, reject every other nested kind, and require at least
  one resolved dimension;
- `product` accepts column selections or any valid nested kind, combines their
  families by Cartesian product, and treats an empty argument list as its
  identity.

Structural preflight and selection-dependent validation remain separate.
Syntactically empty `sets`, `rollup`, and `cube` nodes are rejected before
metadata acquisition. A selection that resolves to no rollup or cube
dimensions, and a composite `set` that resolves to no columns, continue to be
rejected during selection resolution. The distinction preserves error
messages, detection order, and the rule that locally decidable errors precede
typed backend metadata reads.

The registry is a compilation-only detail. It, the source specification, and
its rules will not be stored in the Grouping plan. This decision does not
change the Grouping plan class or fields, make the plan opaque, or change how
executors and backend adapters consume it. A later opaque-plan change may add
a private constructor and accessors without changing this specification
grammar.

## Change targets

A later implementation of this decision should be confined primarily to
`R/grouping-plan.R`: introduce the private registry and rule lookup, route
early and nesting validation through the selected rule, derive nested
constructor recognition from the registry, and route family expansion through
the rule's expansion behavior. Existing expansion algorithms and error
helpers should remain focused functions and may be shared by multiple rules.

`R/grouping-spec.R` remains responsible for public constructors and quosure
capture. `R/margin-operation.R` remains responsible for lifecycle timing,
backend metadata acquisition order, and propagation of the public call.
Grouping executors and backend adapters continue to receive the same expanded
plan and require no rule awareness.

## Compatibility constraints

The implementation must not change:

- exported functions, arguments, syntax, or returned specification classes;
- grouping-set membership, ordering, duplicate handling, or Grouping
  identifiers;
- error condition classes, complete messages, public call contexts, or
  detection order;
- quosure environments or the number and timing of evaluations without a
  separately accepted decision;
- local, dtplyr, Arrow, or database behavior;
- metadata acquisition counts, laziness, SQL semantics, or backend dispatch;
  or
- the Grouping plan representation or its consumers.

## Amendment: one nested argument, whose reading the spelling does not settle

Two of the constraints above are amended by ADR 0026, which refuses a bare name
in a Nested specification position that is both a column of the input and bound
to a specification of a kind the position admits. Everything else in the list
stands, and the registry this ADR centralizes is what that refusal derives
availability from: it asks the parent's own `validate_nested()` rule rather than
carrying a second list of which kinds nest inside which.

**Evaluations.** The constraint holding the number and timing of evaluations
does so "without a separately accepted decision", and ADR 0026 is that
decision, for that one argument. Deciding whether the two readings differ is a
property of the bound value, so the binding is read — once, in the structural
preflight, where it was read not at all. No argument outside such a collision is
read more often than before, quosure environments are unchanged, and a position
admitting no nested kind reads nothing. Timing moves with the count: where the
refusal fires, the arguments written after it are not read.

**Conditions and detection order.** The constraint holding error condition
classes, complete messages, public call contexts, and detection order carries
no such clause, and the refusal moves every item in it, so ADR 0026 amends it
whole rather than naming a part of it. The refusal is reported in place of
whatever the call would have been rejected for further along — a missing
column, a duplicate grouping set, a `.by` overlap, each nesting-grammar
rejection above, and #190's refusal among them — and where that displaced
rejection was an External condition, the caller now receives a
`marginplyr_error` blamed on the Margin verb instead of tidyselect's class and
tidyselect's blamed call.

## Amendment: the condition class of a specification the guard could not read

The constraint holding error condition classes fixed is amended by #262, in one
direction and for one thing: an object carrying `margin_grouping_spec` that
cannot answer for a field the guard reads. `validate_grouping_spec_early()`
read those fields having established only the class, so whatever the object
raised when it was asked came out of that line rather than reaching
`abort_invalid_grouping_spec()` below it. Some such objects cannot be asked at
all — an atomic vector, a closure — and others can be asked and raise, per
field: an object that answers for its kind and raises on its arguments is why
the two fields are read through a catch each. The class such a call raises
therefore moves to `marginplyr_error`, from whatever base R or the object
itself raised — `simpleError` for an atomic vector, `notSubsettableError` for
a closure, and a class of the object's own where the object is what raised.

This is the guard's own answer arriving rather than a new one being chosen, and
that is what separates it from ADR 0026's amendment above, which moves the same
constraint by deciding something. ADR 0015 already assigns a malformed
specification reaching this guard a Package condition, and the refusal that
raises it was already written for an object whose fields do not read as a
specification's; what reading one too early did was keep the object from
getting there. Nothing else in the constraint moves with it: every object that
reached the refusal reaches it with the same message, the same call context,
and at the same point, and no specification that compiled stops compiling —
the reading is `$` itself, so an object is refused for what it says it is and
never for how it is stored. The constraint on the number and timing of
evaluations is not reached, since no field is read any earlier or later for a
well-formed specification.

"No specification that compiled stops compiling" is a claim about what the
object says, and reading through a shared function has a cost of its own that
the amendment below states as a property. It is the same reader, so the same
cost is paid here: a `$` doing something other than answer can be refused where
it compiled. The property is written out once, below.

## Amendment: the printed line for a specification the printer could not read

The constraint holding error condition classes fixed is amended again by #264 —
a third time, after ADR 0026 moved it whole and #262 moved it for the guard —
at the site the amendment above does not reach. That one is scoped to
"a field the guard reads", and `print.margin_grouping_spec()` is not a guard.
It read the kind having established only the class, so an object carrying
`margin_grouping_spec` over something `$` cannot be asked raised from that line
— `simpleError` for an atomic vector, `notSubsettableError` for a closure —
with no refusal below it to reach. What such a call raises therefore moves to
raising nothing at all: the object prints, on the line an object answering no
kind already printed.

The move is over the read and not over the whole line. A field that answers
with a value `cat()` cannot render still raises from that call, having written
the opening of the line first, so this method is not one that never raises;
what it no longer does is raise for an object it could not ask. That remainder
is #268, and it is named here because a decision record saying a class of
errors moved should say which errors did not.

The direction differs from the amendment above and the reason it is admissible
is the same. There, the guard's own refusal was being kept from arriving; here
there is no refusal to arrive, because a print method has none, and a print
method that raises for an object it was asked to print is the defect rather
than the diagnostic. Nothing else in this constraint moves with it: every
object that printed before reaches the same line, with the same wording, at the
same point, for every object whose `$` is an ordinary field read — one that
answers from the object and the same way each time it is asked, returns rather
than signalling, does nothing else while answering, and does not consult the
call stack. That is every object a constructor builds, since one builds a list
with base R's `$`.

The qualification is written as a property rather than as a list of exceptions
because what falls outside it is a class of behaviours and not a fixed number
of them: reading through a shared function rather than in place is visible to
any `$` that does something other than answer. Three are known and are recorded
below, the last two of which are `grouping_spec_kind()`'s rather than this
site's — chosen by #262 for the two guards, and inherited here rather than
decided again. None of the three is a property of a specification.

**Evaluations.** The constraint holding the number and timing of evaluations
does so "without a separately accepted decision", and this is that decision,
for one branch of one line. Where no rule answered the kind, the field was read
twice — once to ask the registry, once to print the field itself — and it is
read once now. The branch a rule answers read once before and reads once now,
so no well-formed specification is affected: no kind a constructor produces
takes the other branch. For an object whose `$` does not answer the same way
twice, the count decides which answer is printed — the answer that chose the
branch, rather than a second one asked for after it — and, where the second
answer would have raised, whether a line is printed at all. For one whose `$`
does something while answering, it decides how many times that is done, whether
or not the line changes.

**The frame the read is made on.** The read moved behind
`grouping_spec_kind()`, which reads inside a `tryCatch()`, so a `$` method that
answers from the call stack rather than from the object answers differently:
`sys.nframe()` and the bindings of `parent.frame()` are not what they were.
The frame is deeper as well as different, which is a fact about the answer such
a method gives and not about what the printer can do: the depth at which a
printed specification exhausts `getOption("expressions")` was measured
unchanged, and the shared reader was measured never to answer `NULL` for a
well-formed specification at any depth below it. This is not a constraint the
list above holds, and it is recorded here because the qualification above
excludes it and would otherwise read as an omission. Preserving it would mean
choosing a call depth as a public contract, which nothing in this package
promises and no specification depends on.

**The handler the read is wrapped in.** `tryCatch(error = )` is an exiting
handler, so a `$` that signals a condition inheriting `error` without stopping
— returning an answer afterwards, as `signalCondition()` allows — is now
unwound where it used to resume, and a calling handler the caller established
for that class no longer runs. This is the same trade `grouping_spec_kind()`
already made at both guards, and it is what catching by class costs anywhere:
a condition is caught for what it says it is. Not catching it is the
alternative that was rejected in #262, since an object that cannot answer for
its kind is the case the reader exists for.

## Amendment: the printed line for a kind that is no name

The amendment above says which errors did not move, and #268 is that remainder:
a field that answers with a value `cat()` cannot render. Deciding it moves two
things, and only one of them is the constraint the amendments above move.

**Condition classes.** The constraint holding error condition classes fixed is
amended a fourth time, in the same direction as the amendment above and at the
same site. A kind `cat()` refuses — a list, a closure, or an environment with
something in it — raised `simpleError` from that call, and now raises nothing.
It raised having already written the opening of the line, so what the reader
was left with was an unterminated line as well.

The same amendment covers the question between those two, which is asked of the
same unvalidated value and can be answered the same way. Deciding whether a
character kind is one name reaches the value's own `is.na()` and `length()`
methods, both of which an object carrying the class can define, and either can
raise instead of answering — before `cat()` is reached, so this half raised no
partial line and was never visible as one. It is caught for the reason the read
is, in a catch of the printer's own rather than the reader's, and that catch is
narrow in what it decides and not in what it swallows, as
`grouping_spec_kind()`'s is: a value no name can be got out of has none,
whatever stopped it. It is `tryCatch(error = )` again, so the trade the
amendment above records under *the handler the read is wrapped in* is made a
second time on this line.

With that, `print.margin_grouping_spec()` completes a line for every object
carrying the class whose fields and methods either answer or raise an error.
Three questions are asked of a value nothing has validated — the field is read,
what the read produced is classified, and the classification is rendered — and
the first two are caught, while the third cannot raise on what the second
answers with: every branch of it answers with a character — the empty string, a
constructor name the registry holds, or the kind itself, which
`is.character()` has answered for.

The qualification is the read's as much as this one's. `tryCatch(error = )`
catches a condition for what it says it is, so a condition raised with `stop()`
whose class does not inherit `error` is caught by neither and reaches the
caller. Widening either to `condition` is the rewrite #268 rejects, since it
would take a warning signalled on the way to answering for a failure to answer,
and a test holds that direction. Under `options(warn = 2)` a warning is an
error and is caught, which is R's semantics for the option rather than
something decided here.

A completed line is also not always one line. A kind that is one name and
contains a newline prints across two, exactly as it did before this change: it
reaches `cat()` through the fallback that names a kind the registry does not
know, since no constructor this registry holds is spelled with one.

The guards read the same field and reach the same methods, and this leaves them
alone, in #280. What a guard does about a raising method is a change to the
condition a call raises rather than to a printed line.

**The printed line.** The claim the amendment above makes — the wording is
unchanged for every object whose `$` is an ordinary field read — is what moves
here, and it moves for exactly the three shapes #264 pinned against it: `1:3`
printed `123`, `c("a", "b")` printed `ab`, and `NA_character_` printed `NA`.
Each prints the empty name now: `is_grouping_kind_name()` is asked before the
registry is — the predicate `find_grouping_kind_rule()` asks, so that what may
be printed and what may be looked up remain one question — and a kind that is
no name answers nothing without reaching the fallback at all.

What those lines were evidence of goes with them. #264 pinned them because this
printed line was where a reader narrowed to a character scalar could be seen —
the shared reader answering with what the field held rather than with a name —
and the line no longer varies with that narrowing. No test distinguishes it
now, since a guard refuses an object no rule answers for and is given no rule
whichever way the reader answers. One behaviour still does, and it is the one
#280 is filed for: a kind whose classification raises reaches a guard as an
error rather than as a value. Until that ticket decides, the reader's contract
is recorded here rather than observed.

The line printed instead is not new: an object answering no kind prints it, an
object whose kind cannot be read prints it per the amendment above, and a kind
of no length printed it before this change — `character()` and an empty
environment among them — `cat()` rendering anything of no length as nothing.
Which of them a given object is goes unreported, for the reason the amendment
above gives.

Rendering the field rather than dropping it is the alternative, and it was
rejected for what it would have to promise. A rendering that always works is
one marginplyr chooses for objects it never builds: `deparse()` and `format()`
are both unbounded in length, and `format()` dispatches, so a `format()` method
of the value's own raises exactly where the rendering is supposed to save the
line. Guarding the `cat()` call is not an alternative at all, since the opening
of the line is written by the time it raises.

Nothing a constructor builds is affected, here as in the amendment above: every
kind one stores is one name, so the branch is not reached. The constraint on
the number and timing of evaluations is not reached either — the kind is read
once, as the amendment above left it, and the predicate reads the value that
read returned rather than the field again.

## Test strategy

Before replacing the branches, add characterization coverage for:

- the five parent kinds against the five nested kinds, plus ordinary column
  selections;
- syntactically empty arguments for every kind;
- dimensions and composite sets that become empty only after selection
  resolution;
- child errors that must precede parent nesting errors;
- exact condition class, complete message, and public call context;
- grouping-set expansion and order for singleton, union, rollup, cube, and
  Cartesian-product behavior; and
- lazy inputs proving that locally detectable errors occur without a typed
  metadata read.

These tests should assert observable compiler and public-verb behavior, not
registry fields or helper layout. The existing local, operation-lifecycle,
backend, laziness, and SQL tests remain the integration gate. SQL assertions
should continue to test semantic query shape rather than byte-for-byte
formatting.

## Considered options

A flat rule table or parent-child permission matrix was rejected because
flags cannot express phase-sensitive empty rules, kind-specific errors, and
child-before-parent detection without recreating a switch in the interpreter.
A message catalog in that table would further separate errors from the
behavior that gives them meaning.

Kind-specific validators without expansion dispatch were rejected because
the authoritative kind list and dispatch would remain duplicated in the
expansion branch.

Kind-specific S3 subclasses were rejected because they would make class,
inheritance, printing, and serialized specification objects observably
different while merely moving the dispatch into `UseMethod()`.

A normalized or validated specification tree was deferred because caching
nested interpretation can change quosure or symbol evaluation counts. It is
not required to centralize the rules and would broaden the compatibility
surface of this change.

A separate rules file was rejected for now because the rules do not form an
independent module; they are the grammar used exclusively by Grouping plan
compilation. Making the Grouping plan opaque at the same time was also
deferred so rule consolidation is not mixed with changes to executor,
adapter, and test access to plan fields.

## Related decisions

The constructor names this registry derives are also the spellings that gate
evaluation of a nested specification argument. ADR 0019 registers every
statically recognized spelling in one place and derives the constructor family
from this table rather than restating it, and records why a constructor is not
a Contextual helper even though its spelling is read before anything runs.

ADR 0026 refuses the one nested argument whose reading a spelling does not
settle, deriving which kinds each position admits from this registry's own
nesting rules, and is what the amendment *one nested argument, whose reading
the spelling does not settle* was written for. There are two amendments above
as of #262, and this names the one it means.
