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
Each prints the empty name now: the value is classified before the registry is
asked — by the same classification `find_grouping_kind_rule()` makes, so that
what may be printed and what may be looked up remain one question — and a kind
that is no name answers nothing without reaching the fallback at all.

What those lines were evidence of goes with them. #264 pinned them because this
printed line was where a reader narrowed to a character scalar could be seen —
the shared reader answering with what the field held rather than with a name —
and the line no longer varies with that narrowing. No test distinguishes it
now, since a guard refuses an object no rule answers for and is given no rule
whichever way the reader answers. One behaviour still does, and it is the one
#280 is filed for: a kind whose classification raises reaches a guard as an
error rather than as a value. The amendment below decides it, and the tests it
names are where the reader's contract is observed.

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

## Amendment: the condition class of a kind the guard could not classify

The amendment above says the guards are left alone and why, and #280 is where
that was reconsidered. The constraint holding error condition classes fixed is
amended a fifth time, at the two guards in `validate_grouping_spec_early()`,
and in the direction #262 already chose for the read there.

A kind that was read still has to be classified, and deciding whether it is one
name asks the value's own `is.na()` and `length()` methods. Either can raise
instead of answering, and what came out of the guard was then whatever the
object raised — `simpleError` for a `stop()` in the method, a class of the
object's own where it aborted with one — in place of the
`Invalid grouping specification.` refusal below it. The class such a call
raises therefore moves to `marginplyr_error`, for the same reason #262 gives:
this is the guard's own answer arriving rather than a new one being chosen,
since ADR 0015 already assigns a malformed specification reaching this guard a
Package condition and the refusal was already written for an object whose
fields do not read as a specification's.

Nothing else in the constraint moves with it. Every object that reached the
refusal reaches it with the same message, the same call context, and at the
same point, and no specification that compiled stops compiling: a kind that is
one name and not the missing one is classified as it was and looked up as it
was.

**Where the classification is made.** The amendment above put a catch on this
question in `print.margin_grouping_spec()`, and said its extent was what only
that site knew. Giving the guards the same answer makes that false, so the
catch moves down into `grouping_kind_name()`, which every reader of an
unvalidated kind now shares as they already share `grouping_spec_kind()`. One
function reads the field, one classifies what the read returned, and neither
site can disagree with the other about what a kind is. The printer keeps no
catch of its own: what it holds after classifying is a name or nothing.

**What the classification answers with.** `grouping_kind_name()` answers with
the name rather than with whether there is one, and the class is stripped from
that answer. The class is what carries the raising methods, and the questions
asked of a kind are not done being asked when it has been classified: `%in%`
reaches `as.character()`, which dispatches too. A caller that classified a kind
and then matched what it classified would raise on the line after the catch,
which is what `check_ambiguous_nested_name()` did. The registry lookup needs no
such protection — `[[` dispatches on the list and not on the index — and is
inside the shared function anyway, so that one place asks a kind anything.

The answer is then classified a second time, and that is not the first
repeated. Catching what a method raises answers only the method that fails to
answer; a method that answers wrongly passes the catch, and `length()`
reporting `1` over two strings is such an answer. The first classification is
put to the value the caller holds, so it is the value's own methods that
answer it; the second is put to the answer, which has no class left to
dispatch on, so it is R's reading of a character vector and cannot be a
method's. What the function returns is therefore one string because nothing it
returns has been taken on a method's word — not because the object was
believed and then trusted. Whether the object *said* it was one string is what
the first classification decides, and it decides it for the printed line, per
the amendment above.

**The nested-name site.** That site is not a guard and its answer is not a
guard's: a kind it cannot classify is not a kind the position admits, so the
column reading stands, which is the answer a binding that raises when it is
read already gets. Three behaviours move there. A kind whose `length()` raises
reached the caller as an untyped error and now decides nothing. A kind whose
`as.character()` raises reached them the same way and is now classified, the
class coming off before `%in%` is reached, so it is refused as ambiguous —
which is what a kind spelling `set` in that position is for, and the raising
method never bore on it. And a kind whose `is.na()` raises was refused as
ambiguous, because that site never asked `is.na()`, and now declines.
Classifying in one place is what removes the last, and an accidental difference
between two sites reading one field is what this ADR centralizes kinds to
avoid.

**Evaluations.** The constraint holding the number and timing of evaluations is
not reached. The field is read once, as the amendments above left it. How often
classifying asks the value's own methods is fixed by no decision, which the
amendment above already records, and it moves in both directions: it falls at
the guards and at the printed line, where the registry lookup's own guard asked
them a second time and is now handed a plain string; and it is unchanged in
number at the nested-name site, which asks `is.na()` where it did not and
reaches `as.character()` where it no longer does.

**The handler the classification is wrapped in.** `tryCatch(error = )` a third
time, so the trade the amendment for a specification the printer could not read
records under *the handler the read is wrapped in* is made once more. An error
is caught and a condition is not, so a method that warns on its way to
answering still answers.

## Amendment: one reading of a recognized nested argument

The constraint holding the number and timing of evaluations does so "without a
separately accepted decision", and this is that decision, for every argument a
Nested specification position recognizes as a specification. It accepts what
*Considered options* below defers — "a normalized or validated specification
tree was deferred because caching nested interpretation can change quosure or
symbol evaluation counts" — in the narrow form that sentence describes, and
changes those counts on purpose. Nothing else about the specification tree is
normalized: the caller's own quosures are what the passes still resolve a
column selection from.

The structural preflight was already the first reader of every nested argument,
and it now records what each one resolved to alongside the specification it
returns. Expansion reads that record instead of asking `grouping_arg_spec()`
again, so the compilation passes read no nested argument at all — the deferred
cache, kept to one call rather than held across calls.

**Evaluations.** Four shapes reach a nested position, and this is where their
counts are written down. Each is a count per call, not per pass.

| The argument | Evaluations | Where |
| --- | --- | --- |
| A nested constructor call, and a bare name bound to a specification | 1 | the preflight |
| A bare name that is a column and is bound to a specification of an admitted kind | 1 | the preflight, per ADR 0026, and then refused |
| A column selection | 1 per compilation pass — 2 where the plan is settled by names alone, 1 otherwise | each pass, against that pass's own proxy |
| A call the spelling declines to evaluate, such as a caller's own function | 1 | the selection resolution that refuses it (#190) |

The first row is what moves, from 3 and 2 to 1 and 1 (#260): a recognized
argument was read by the preflight and then again by each pass, so its count
followed the number of passes, which is decided by whether names alone settle
the plan — a property of the whole specification and not of that argument. It
no longer does. The other three rows are unchanged, and the second and fourth
are stated here because a table of what a nested position costs that named only
some of its arguments would be read as naming all of them.

**Timing.** The first reading of every argument is where it was: the preflight
still reads in argument order, and still stops at the first argument it
refuses. What moves is that the later readings do not happen, so whatever
forcing a recognized argument does is done before typed metadata is acquired
and not again after it.

**What does not move.** Which arguments are recognized, and by what spelling,
are untouched — the gate is the same function, called from one site instead of
five. So are grouping-set membership, ordering, duplicate handling, and
Grouping identifiers; error condition classes, messages, public call contexts,
and detection order; laziness and metadata acquisition counts; and the Grouping
plan and its consumers. A specification that compiled compiles to the same
plan, because a reading taken once and a reading taken three times differ only
where the argument answers differently each time, and an argument that does
that is one whose count this amendment is about.

## Amendment: the condition class of a specification stored as a function

The constraint holding error condition classes fixed is amended a sixth time,
by #265, at the selection resolution and in the direction #262 chose for the
guard: this is a refusal that was already written arriving, rather than a new
one being chosen.

#190 gave a Nested specification position its own refusal for a specification
that reaches it where a column selection is expected, and wrote it from the
condition tidyselect raises about a subscript it will not use. tidyselect
raises no such condition about a function: `is.function()` decides its first
branch, before any class is read, so it calls the value as the predicate form
of a selection. What came out of the position was then whatever the call
produced — `simpleError` naming `X[[i]]` for a signature tidyselect cannot
call, the class the body raised for one it can, and tidyselect's own report
about a predicate's output for one that returns the wrong thing. The class such
a call raises therefore moves to `marginplyr_error`, and the diagnostic to the
refusal #190 already writes for every other way of storing the same object.

**What decides it.** The object is read from the frames tidyselect applied it
from, as the error unwinds. Neither that nor the two readings that decide
whether the position speaks for it — the argument's own expression, and which
of those frames holds the object — is a second reading of the caller's quosure,
which is what the constraint below forbids and what #190's refusal avoids by
reading the condition. The evidence — that the frame holding the value outlives
the call, that no condition carries it, that a selection helper the caller
called holds it without applying it, and that `allow_predicates = FALSE`
answers by removing `where()` rather than by identifying anything — is in
`investigation/reading-a-specification-tidyselect-called.md`.

**Evaluations.** The constraint holding the number and timing of evaluations is
not reached. The table above gives such an argument one evaluation, in the
selection resolution that refuses it, and it has one still: the frames are
read, not the quosure. A binding tidyselect has not forced is skipped for that
reason and not for tidiness.

**What does not move.** The wording of the refusal, and every object that
reached it before, are untouched. So is the rule that the position speaks for
its own argument and not for a part of one: a specification under an operator
tidyselect walks in parts, and one a selection helper the caller called was
handed, each keep tidyselect's report, exactly as a specification refused
inside `c()` does. The distinction is drawn twice here, from the caller's
expression and from which frame holds the object, because a condition about a
predicate names no subscript to compare a label with and because the two
shapes reach it separately.

**What is left.** One shape of the same object is unreached: a function that is
a valid predicate as well as a specification — one argument, a logical scalar
back — is applied without failing, so the position receives a selection instead
of a refusal and no condition exists to read the frames from. Nothing marginplyr
constructs has that shape, since a constructor builds a list. It is recorded
here because an amendment saying a class of errors moved should say which
errors were not there to move.

## Amendment: a kind classified with its class off

The amendments *the printed line for a kind that is no name* and *the condition
class of a kind the guard could not classify* decide what a reader does when
classifying a kind raises. This one decides that classifying a kind does not
raise, which reverses both of them (#289). The constraint holding error
condition classes fixed is amended a seventh time, and in the opposite
direction to the fifth: a guard that raised `marginplyr_error` for a kind
carrying a raising method now compiles it, and the nested-name site raises one
where it raised nothing. Every kind that is no name still gets the same
refusal, with the same message and the same call context, and detection order
does not move.

`grouping_kind_name()` strips the class before it classifies rather than after.
`is.character()` answers for the type, `unclass()` takes the class off, and
`length()` and `is.na()` are then put to a character vector with no class.
Only the class comes off — a name and any other attribute the value carried
survive onto the answer, and `%in%` and `[[`, which are what the callers put it
to, read neither. Neither call of the first pair can be intercepted: both are
primitives that dispatch on neither S3 nor S4, so `setMethod()` refuses a
method for either — "must supply a function skeleton" — and a
`registerS3method()` into `base` is ignored, with `is.character()` answering
`TRUE` for a classed character that has registered one. An S4 object extending
`character` unclasses to a character whose `class()` is `"character"`, so an S4
`length()` or `is.na()` method the object carried is not found on the answer
either.

What that buys is the removal of the mechanism those two put in. The
`tryCatch(error = )` goes, and with it a third making of the trade recorded
above under *the handler the read is wrapped in*: an exiting handler unwinds a
condition that a calling handler of the caller's would otherwise have resumed
from, and a `signalCondition()` inheriting `error` without stopping is caught
rather than resumed. That trade is still made wherever a field is read, and no
longer where a kind is classified. The second classification goes with it,
because the first is already R's own reading of a character vector: a
`length()` answering `1` over two strings is an answer nothing takes now.

**What a raising method decides.** Nothing, at any of the four readers.

- The printed line names the constructor of the kind underneath, where it
  printed the empty name. `structure("set", class = "raises")` prints
  `grouping_set`, the kind being `set` and the class carrying a method that was
  never relevant to it.
- The guards in `validate_grouping_spec_early()` admit it, where they refused
  it. The `Invalid grouping specification.` refusal still answers every kind
  that is no name — two strings, none, or the missing one — and answers it on
  the stripped value, so a `length()` claiming `1` over two strings does not
  reach the decision.
- The nested-name site refuses it as ambiguous, where a raising `is.na()` or
  `length()` left the column reading standing. All three methods that site ever
  reached — those two and the `as.character()` that `%in%` dispatches through —
  are unreachable, so a kind spelling `set` there is refused for spelling `set`,
  which is what the refusal is for.

**Evaluations.** The constraint holding the number and timing of evaluations is
not reached. The field is read once at each reader, as those two amendments
left it. How often classifying asks the value's own methods is fixed by no
decision, and it is now none, at every reader.

## Amendment: a kind read as a name and compared as a value

The amendment above decides what comes off a kind while it is classified, and
leaves everything but the class on. That answer is enough for what read it then
— `%in%`, `[[`, and the `cat()` a printed line ends at — and it is not enough
for `identical()`, which is what a share compares a kind with. The constraint
holding the Grouping plan representation and its consumers fixed is amended by
#317, for the one field a plan records a kind in.

`grouping_kind_name()` answers with a bare name: every attribute comes off, not
only the class. The name a kind is does not depend on what the value carrying it
was labelled with, which is the same argument the class came off under, made
about the rest of the attributes. `unclass()` takes the class and
`attributes<-` takes what is left. The second is a primitive like the first
two, so `setMethod()` refuses a method for it with the same "must supply a
function skeleton" the amendment above records — and by the time it is called
there is no class left for anything to dispatch on in any case, an S4 object
extending `character` having unclassed to a character whose `class()` is
`"character"`.

**What a plan records.** The name, rather than the field as read. Every plan is
compiled from a preflighted specification, so the field is one name by the time
it is recorded and the classification never answers `NULL` there — what it
removes is the attributes a caller's kind carried onto the plan. Putting the
question here rather than at each consumer is what makes a plan's kind one
string by construction: `identical()` on it then reads what the specification
said and nothing about how it was stored, for every consumer this plan reaches
and every one written later.

**What the share readers ask.** `check_parent_grouping_kind()` compares
`plan$kind` directly, because a plan holds a name. `check_parent_grouping_spec()`
runs before a plan exists — it is installed as the specification validator, and
`validate_grouping_spec_early()` has already established there is a kind to read
— so it classifies the field it read and compares that. The two readers agree
because both compare a name, and not because both were written to remember to.

**What moves.** A specification whose kind spells `rollup` under any attribute
compiled as a rollup and was refused by `share_of_parent()` as though it were
not one, which is the defect. It is accepted now, in both shapes: a kind
carrying names, which reached the field before the amendment above, and a kind
carrying a class, which reaches it because that amendment stopped a class on a
kind being a reason to refuse the specification. No refusal changes its
condition class, its message, its call context, or its position in the detection
order, and no specification that was accepted is refused — the change is one
population moving from refused to accepted, at one guard.

**Evaluations.** The constraint holding the number and timing of evaluations is
not reached. Each site reads its field once, as the amendments above left it,
and classifying asks the value's own methods nothing.

**What does not move.** Nothing a constructor builds: `rollup()` stores the
string `"rollup"`, so stripping is the identity for every kind marginplyr
writes, and grouping-set membership, ordering, duplicate handling, and Grouping
identifiers are untouched for every specification. The other consumers of a
classified kind read no attribute either: `%in%` compares values, `[[` indexes
a list by one, and the printed line reaches `cat()`, which renders a vector's
values and not what it is labelled with — so a kind no rule knows prints the
same line as it did, under a name, a class, or a dimension.

**Considered options.** Classifying at each consumer of `plan$kind` instead was
rejected: it leaves the plan holding a value whose attributes decide nothing,
puts the same question at every reader present and future, and makes the two
share readers agree by convention. Normalizing the specification object rather
than the plan was rejected because the specification is the caller's object and
this package rewrites none — the amendment *one reading of a recognized nested
argument* above records the specification tree as unnormalized on purpose.
Recording the name without widening what `grouping_kind_name()` strips was
tried on #289's branch and reverted: `unclass()` removes the class and nothing
else, so it does not close the defect for a kind carrying names.

## Amendment: a nested kind read as a name

The amendment above scoped its answer to the one field a plan records a kind in
and left every other `identical()` on a kind field where it stood.
`validate_nested_grouping_units()` is the other one: it decides whether a nested
argument is the composite dimension `rollup()` and `cube()` admit, and it
compared the field as read. So a nested `grouping_set` spelling `set` under an
attribute was refused where the same specification with a bare `set` compiled,
which is the defect. The constraint holding error condition classes, messages,
and detection order fixed is amended by #324, for that guard.

**What the guard compares.** The name, classified where it reads. What this
reader holds is the caller's own nested specification and not a plan, so it
classifies the field it read, as `check_parent_grouping_spec()` does above.

**What the diagnostic renders.** Unchanged, and it reads the parent's field as
read. `abort_marginplyr()` expands a message through `cli::format_inline()`,
which renders a vector's values: it drops names and `dim`, and reaches neither
an `as.character()` nor a `format()` method a class on the kind defines, so
`{.fun {parent$type}}` already spells the name. That is the argument the
amendment above made about `cat()` for the printed line, made about the writer a
diagnostic goes through.

**What moves with the guard.** `resolve_grouping_units()` asserts the same
field one pass later, and it is classified too. The assert was unreachable while
the guard refused this population; admitting the population is what makes it
reachable, and left as read it would refuse exactly that population again — with
a `stopifnot()` message, which is the untyped condition the guard exists to
replace.

**What moves.** A nested specification whose kind spells `set` under any
attribute reads inside `rollup()` and `cube()` as the same specification with a
bare `set` does, at one guard. A non-empty one is accepted where it was refused.
An empty one is refused still, and by the refusal the bare spelling already
receives — `abort_empty_composite()`, the line after the comparison — rather
than by the nesting refusal that used to stand in front of it. That
sub-population is the one place this amendment moves a refusal instead of
removing one, so its message and its position in the detection order both
change; its condition class does not. No other refusal changes in any respect,
and no specification that was accepted is refused. Nothing a constructor builds
is affected: `grouping_set()` stores the string `"set"`.

**Evaluations.** The constraint holding the number and timing of evaluations is
not reached. The guard reads its field once, as every reader above does, and
classifying asks the value's own methods nothing.

**What does not move.** The remaining reads of a kind field.
`abort_empty_grouping_units()` takes one for a message alone, which renders the
name for the reason above; `admitted_nested_kinds()` keys its memo on one, and
`[[` indexes a list by a value, so every spelling of a parent's kind reaches the
same entry; `find_grouping_kind_rule()` classifies already.

**Considered options.** Having the preflight hand the rule the name it already
classified was rejected. `validate_nested` is one contract across the five kind
rules, and `admitted_nested_kinds()` puts each rule to a stand-in specification
built per kind, so the parameter would be a name at one caller and a
specification at the other; the guard also needs the parent specification for
its sentence either way. Classifying where the field is read is what the
amendments above left in place at every other reader, and it is what makes the
two guards over a caller's specification agree by construction rather than by
convention.

A `grouping_kind_is(kind, name)` predicate over the three sites that now spell
`identical(grouping_kind_name(x), <name>)` was rejected. It would not make the
classification unforgettable, which is the only thing that would buy: a site
written later can still compare the field as read, and what stops one is this
ADR and the review that reads it against the code. What it would add is a second
name restating `grouping_kind_name()`'s contract, for three comparisons that are
against different literals, in two files, for three different decisions.

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
surface of this change. The amendment *one reading of a recognized nested
argument* above accepts it, in the narrow form this paragraph describes and
with the counts it changes stated.

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
