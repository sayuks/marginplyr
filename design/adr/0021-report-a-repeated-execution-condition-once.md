# Report a repeated execution condition once, in the caller's names

A Margin verb reports an External condition raised by the caller's summary
expression once, however many grouping sets raised it, and writes its Condition
context in names the caller can act on. Two occurrences are repetitions of one
condition when they agree on class, diagnostic, and the argument they are
attributed to; which grouping set produced an occurrence is never part of that
identity, because it is the part that necessarily differs. The condition itself
— its class, its diagnostic, its cause — is propagated unchanged, as ADR 0015
already required. What this decides is the context around it.

The scope is the `UNION ALL` adapter, and only because that is where the
repetition exists. `summarize_margin_union()` is the one function in the package
that evaluates the caller's own summary expressions more than once: it renames
each grouping column to a `..marginplyr_key_N` column and runs one
`dplyr::summarize()` per grouping set over those names, so dplyr builds every
context it attaches from marginplyr's names rather than the caller's. The native
grouping-sets adapter issues one `summarize()` in total and groups by
`pick(all_of(group_vars))`, so it allocates no key columns and repeats nothing;
it is exempt by construction rather than by exclusion. The nesting verbs and the
share module summarize with expressions marginplyr wrote, not the caller's, so
neither is in scope either.

## The two condition kinds need different mechanisms

They are not one mechanism, and treating them as one was the framing this ADR
had to reject. What dplyr hands over differs:

An **error** arrives as a structured `rlang_error` whose fields are separately
addressable: `$message` holds the argument bullet, `$body` holds the group
bullet as a named character vector, `$call` holds the internal
`dplyr::summarize(.data = .data, ..., .by = dplyr::all_of(.by))` literal, and
`$parent` holds the caller's own original condition, untouched. Rewriting the
first three is a field assignment; `$parent` never moves, which is what keeps
the propagation faithful. Errors also need no deduplication at all: branches run
in sequence, so the first error aborts the operation and no second occurrence is
ever raised.

A **warning** arrives as one condition per branch whose `$message` is a single
pre-rendered string — `$parent` is `NULL` and there is no `$body`. dplyr has
already aggregated every warning from that branch, flattened the bullets into
text, and appended its own count. There is no field to edit and no structure to
read, so the deduplication key can only be computed from the rendered text.

Comparing the rendered messages verbatim does not work, which is why this is
written down: measured on the ticket's own reproduction, a `cube(region, grade)`
produces four warnings whose messages are pairwise distinct, because each embeds
a different key column in its group bullet and a different occurrence count.
Normalising away exactly the parts that necessarily differ — the group bullets,
the leading count sentence, and dplyr's `last_dplyr_warnings()` footer —
collapses those four to one key, while a plan whose branches raise genuinely
different diagnostics still yields one key per diagnostic.

That normalisation reads dplyr's rendered format, which is not a stable
contract, and it is chosen anyway because of which way it fails. If dplyr
changes its wording the patterns stop matching, the keys stay distinct, and
every occurrence is reported — today's behaviour. The failure mode is the
status quo, never a genuinely different rendered warning silently collapsed
into another.

*Rendered* is the bound, and it is dplyr's bound rather than one this adds. A
branch that raises several distinct diagnostics has one of them rendered and
the rest replaced by the pointer at `last_dplyr_warnings()`; the others are
not conditions the caller ever receives, before this change or after. So two
branches whose rendered diagnostic agrees are repetitions here even where what
each hid behind that pointer differs, and the count says how many further
grouping sets reported the diagnostic rather than how many raised something.
Reading past the pointer is not available: what is behind it is a count that
varies with repetition within a branch, which is the one thing an identity may
not depend on. The reported occurrence keeps the pointer, so a caller reading
a report that hides something is told so in the same terms dplyr tells them.

## No rendering decision takes part in the identity

*Rendered* bounds what can be read; it does not license the reading to depend
on how it was rendered. Which grouping set produced an occurrence is excluded
from the identity because it necessarily differs between branches, and the
session a caller happens to be sitting in is excluded for the stronger reason
that it does not differ between branches at all: it is not a property of the
condition. So the identity is computed from the message as it was *written*,
and cli's decisions about how to lay that message out are undone before any
pattern is matched against it.

cli makes three such decisions, and each defeated a pattern on its own. It
**wraps** a line it cannot fit, onto continuations it indents by two spaces, so
a part read off a rendered line is a prefix of it at any narrow width. It
**styles** the markers it writes, so above `cli.num_colors = 1` every pattern
anchored at the start of a line missed, nothing was removed, and every branch's
key differed by its own grouping values. And it **links** the calls it names,
so `cli.hyperlink_run` — which cli sets for itself in a terminal advertising
OSC-8 — replaced the backticks around the `last_dplyr_warnings()` pointer with
an escape sequence carrying a per-branch count, splitting the identity at
`cli.num_colors = 1`.

Only the first was undone when this was written, which is why the property is
now stated as the rule rather than as three repairs (#217). The contract this
ADR states did not hold in a session with colour, and most interactive sessions
have colour; a fix for the styling alone would have left the hyperlink case,
and both were found only because the second was looked for after the first.
Undoing the styling is also not sufficient by itself for the pointer: the
stripped link renders the call without the backticks dplyr writes around it
otherwise, so that pattern admits both spellings.

Evidence: `investigation/dplyr-condition-context-rendering.md`, whose 2026-08-18
revisions section holds the measured report counts, the rendered pointer line, and the
two things measured beside them and found not to need changing — the wrap
indent that survives styling, and the error path that carries no styling at
all.

Two properties bound this. The removal is a *reading* — the key is still
assembled from the lines as they arrived, so two caller diagnostics differing
only by an escape sequence remain two identities, and a caller's own text can
no more be collapsed by the removal than by the reading it feeds. And a green
suite under one rendering says nothing about another, so the fixtures cross the
variables rather than sampling them, and each asserts that the markers it
exists to exercise are actually present before it asserts the collapse.

## Rewriting the names is safe for a reason that does not depend on dplyr

The `..marginplyr_key_N` token is a string marginplyr chose, so finding it in a
rendered message is a search for a planted literal rather than a parse of
dplyr's format. It is planted as a column name, which
`new_margin_internal_names()` allocates clear of the caller's columns; a
grouping *value* spelled the same way is rewritten along with it, as is a
caller's own diagnostic that spells it. Both are the one thing the search
cannot tell apart, and both are left where they are: what it takes to reach
either is writing a name marginplyr allocated for itself. That holds equally in the flat warning string and in the
structured error fields, and it is why this half of the fix carries none of the
fragility the deduplication key does. Substitution runs longest token first:
naively replacing `..marginplyr_key_1` first corrupts `..marginplyr_key_10`,
which a plan of ten or more grouping columns produces.

## Considered Options

**Stop renaming, and group by the original columns.** The cleanest context, and
rejected because it changes what a summary expression can see. The rename exists
so that a dimension omitted from a grouping set stays available to the caller's
expression as a full vector rather than as a per-group scalar. Removing it to
improve a diagnostic would alter the data-mask contract, and the Parent-share
paths read the same branch results.

**Leave it and document it.** The recommendation on #108 for the CRAN
submission, when the fix looked like it required the option above. It no longer
does: the deduplication and the name substitution are both confined to one
function and both degrade to current behaviour, so the risk that justified
deferring is not the risk actually on offer.

**Suppress the context entirely.** Removes the internal names by removing the
information. Rejected: the grouping values are the useful half of the context,
and a caller debugging a summary expression needs to know which group provoked
it.

**Deduplicate by comparing `conditionMessage()` verbatim.** Rejected on
measurement rather than on principle — the messages differ per branch, so it
collapses nothing.

**Read the per-branch conditions from `dplyr::last_dplyr_warnings()`.** It does
return the structured objects the flat message lacks. Rejected: it is a
debugging aid rather than an API, and it is reset per `summarize()` call, so it
holds only the last branch's warnings by the time the loop ends.

**Also restore the caller's spelling of the argument.** dplyr's argument bullet
quotes marginplyr's rewrite — `dplyr::across(dplyr::all_of(c("units")), ...)`
where the caller wrote `c(units)`. Rejected for this decision: restoring it
means deparsing the caller's dots and text-matching them against dplyr's own
deparse of the rewritten ones, which is the fragile-parse problem again, for the
part of the context least likely to mislead. The caller wrote the expression;
seeing it requoted is confusing rather than wrong.

ADR 0022 decides it, on a mechanism this paragraph does not describe: what
dplyr quoted is compared with marginplyr's own rendering of the expression it
handed dplyr, rather than with the caller's spelling. It also measures a cost
this paragraph missed — a rewrite that differs between branches splits one
written expression into one report per branch, so the deduplication key above
is affected and not only the spelling.

## Consequences

Deduplication is observable, not silent: the surviving occurrence says how many
further grouping sets raised it, following the precedent dplyr sets when it
aggregates warnings within one call.

Only eager inputs are covered, and this is a property of the contract rather
than an unfinished part of it. On a `dtplyr`, Arrow, or non-native SQL input,
the branch `summarize()` builds a query without evaluating the caller's
expression; the warning is raised later, inside the caller's own `collect()`,
where no marginplyr frame is on the stack. There is nothing to intercept, and
the Repeated-condition contract says so directly by answering only for what a
Margin verb raises while it runs.

A `message()` is outside this, and is the one External condition kind left
where it was. dplyr attaches no context to one and does not aggregate them, so
a message from a summary expression is already emitted once per group rather
than once per grouping set, in plain dplyr as here; there is no context to
restate and nothing that would make two of them one. The contract is written
over what a verb can identify, which the *External condition* entry enumerates
as warnings and errors.

`dplyr::last_dplyr_warnings()` continues to report the last branch's warnings
under the internal key names. That store belongs to dplyr and is written before
marginplyr sees anything, so the substitution cannot reach it.

A warning still names `dplyr::summarize()` as the call it arose in, where an
error now names the Margin verb. The asymmetry follows from the same difference
the mechanisms do: an error's blamed call is a field, and a warning's is a
clause inside the sentence dplyr rendered before signalling. Rewriting that
clause would be a second parse of dplyr's format, and one that fails the wrong
way — it edits a sentence rather than choosing a key, so a wording change
leaves it silently naming the wrong thing rather than falling back to what
happens today. This is the *Condition context* entry's "a context it cannot
restate in those terms it leaves as it found it".

Because the deduplication key is derived from rendered text, a dplyr release can
change it without failing a test that asserts on structure. The tests therefore
assert on the rendered message, as #108 anticipated, and a plan whose branches
raise different diagnostics is asserted alongside the plan whose branches repeat
one — a test that only checks collapsing would pass just as well if everything
collapsed.
