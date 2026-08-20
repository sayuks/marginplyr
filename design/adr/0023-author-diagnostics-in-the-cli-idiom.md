# Author diagnostics in the cli idiom

Every Package condition marginplyr raises is written as a short refusal plus
`i` bullets, with cli's inline markup naming what each part of the sentence is,
and signalled through `cli::cli_abort()`. `abort_marginplyr()` remains the only
constructor and still owns the class and the blamed call, exactly as ADR 0015
says; what changes is how the message is built, not what carries it. The
adoption is a formatting decision and nothing else.

The boundary of the change is that sentence. `abort_marginplyr()` becomes the
interpolating entry point — it wraps `cli::cli_abort()` with
`.envir = rlang::caller_env()` — so a call site passes an unexpanded template
rather than a string it assembled itself. Three paths keep the flat form they
had: SQL glue templates, which are a different engine that happens to share
`{}`; bare `stop()` invariants, which ADR 0015 separates from Package
conditions and which gain nothing from markup they will never render; and any
sentence built by concatenating a caller's own text, for the reason in *Caller
text is a value* below.

`Config/marginplyr/cran-status` read `unpublished` when this was decided, which
is what made it affordable. A diagnostic's exact text is part of what this
package ships, so after publication these texts harden into something closer to
a contract.

The measurements this rests on are in
`investigation/diagnostic-wrapping-under-rlang-and-cli.md` and, for what was
measured once every site had moved,
`investigation/retrieval-time-formatting-of-a-cli-diagnostic.md`. They are cited
rather than restated because each is a fact about rlang, cli, or testthat, and
ages when one of those moves rather than when `R/` does.

## Amendment: the template is expanded when the condition is raised

[ADR 0024](0024-spell-a-callers-subject-as-the-caller-wrote-it.md) found that
`cli_abort()`'s retrieval-time formatting collapses a run of whitespace inside
an interpolated value as readily as inside the template — so a column the caller
named `a  b` was named `a b` in the refusal. Every sentence below that names
`cli_abort()` as the call, or rests on the wrapping it did, is superseded by it.
What stands is the whole of the idiom: the style table, the injection rule, the
`{?}` rule, the vector defaults, cli as an Import with its floor, and the
structural gate. The template is the same template; what moved is when it is
expanded.

**Every naming of the call** goes: "signalled through `cli::cli_abort()`" and
"it wraps `cli::cli_abort()` with `.envir = rlang::caller_env()`" in the opening
section, and "`cli_abort()`'s `class` argument carries it through" in
*Consequences*. `abort_marginplyr()` expands its template with
`cli::format_inline()` and raises through `rlang::abort()`, whose `class`
argument carries `marginplyr_error` exactly as `cli_abort()`'s did — the
`must_error` hook and its twenty vignette chunks are untouched either way.

**The line conditions** were written against wrapping that no longer happens,
and two of them go with it. Condition 1, that the authored prose of a line fits
in 80 columns, is now style advice rather than a requirement. So is the first
sentence of condition 3, that no pin and no marker spans a wrap point — there
are no wrap points. Condition 2 survives whole and matters more than it did: a
part whose length the caller decides goes alone in an `i` bullet, which without
wrapping is the only thing between a long vector and a very long line. So does
the second sentence of condition 3, that a marker is chosen from a run of
uninterpolated prose, which was never about wrapping — a marker quoting an
interpolated part varies with the data at any width.

**The paragraph in *Consequences* beginning "Wrapping is not the whole of what
retrieval-time formatting costs"** recorded the collapsing and left the trade
open in #230. It is settled: the collapsing is a defect, and ADR 0024 is the
remedy. Two spellings survive neither route and are stated in `?marginplyr` — a
line break and a no-break space inside a name are shown as an ordinary space.

**The normalization *Consequences* asks of `verify-site.R`** is removed with the
wrapping that needed it. The paragraph saying the marker matching "is where a
whitespace-normalizing comparison is needed", and the one citing two site
markers that "only match once normalized", both describe a state that lasted one
commit: rebuilding the site after ADR 0024 shows seven rendered diagnostics with
no wrap in them, and both markers matching raw again. What survives of that
argument is condition 3's second sentence, above.

## Amendment: a template may be split at a space, and the gate says so

The gate admits `paste()` and `paste0()` as of #223's phase 3, on the same terms
it already admitted `c()`: by recursion over their arguments. Two sentences of
*Two rules are gated* below go with that.

"Fail any whose message argument is not a literal in the source" is replaced by
*fail any whose message argument is not authored in the source* — a literal, or
a `c()`, `paste()`, or `paste0()` over arguments that are each authored in turn.
`authored_template()` in `test-diagnostic-authoring.R` is the definition, and
this ADR does not restate it, because a rule about which calls are admitted ages
with the code and not with a decision.

"A `paste0()`-assembled template and an `if`-spelled noun are the same violation
seen from two sides, because neither can be written without computing the
argument" goes whole. Its reasoning was sound and its example is no longer one:
`paste0("a ", "b")` computes nothing a reader cannot see. What survives is the
claim it was making, which the replacement above still supports — an `if`
spelling a noun cannot be written without computing the argument, and neither
can a template splicing caller text.

What the gate refuses is unchanged, because the recursion and not the name of
the call is what enforces both rules — caller-derived text is a symbol rather
than a literal wherever it appears, so `paste0("Unknown column `", columns,
"`.")` is refused exactly as before, and an `if` spelling a noun is refused
inside an admitted call too. Both are fixtures in
`test-diagnostic-authoring.R`.

The reason is a measurement neither this ADR nor #223 had taken, and it binds
every file phase 3 reaches. A template has to be one string literal per message
element, because ADR 0024 expands it with `cli::format_inline()`, whose
`keep_whitespace = TRUE` is the whole of how a caller's spelling survives — so a
source line break inside a template is a line break in the refusal, and glue's
`\` continuation is part of the trimming that flag turns off. Meanwhile the
amendment above demoted condition 1 to style advice, and the shipped sentences
measure 83 to 119 characters before any markup is added. `lintr`'s default
`line_length_linter(80)` therefore has no spelling to accept, and this
repository has no `.lintr`.

The alternatives were all worse, which is why this is the amendment rather than
one of them. A `.lintr` is repository-wide, so raising or excluding the limit
stops it measuring every line of `R/` and `tests/`, for a property only
diagnostics have. A suppression reaches about half the message elements in the
package, and `AGENTS.md` asks each `# nolint` to record a fact about *one*
expression, which a reason repeated eighty times is not. Rewording is what #223
exists to forbid.

What is genuinely lost is small and worth naming: a template is no longer one
literal a reader's eye lands on whole, but a sentence split at a space. It is
still written beside the call that raises it, which is the property *Two rules
are gated* asks a template for — a constant bound elsewhere stays refused.

## Amendment: a branch on a count may choose a subject `{?}` cannot write

`R/margin-label.R`'s collision refusal, re-authored in #223's phase 3, branches
on a count. `length(unique(bad_labels)) == 1L` picks between two
`abort_marginplyr()` calls — one saying `{.val {...}} is already a factor
level`, the other `Margin labels are already factor levels` — and both arms
spell a noun, which *Every singular/plural choice goes through cli's `{?}`*
below forbids an R branch to do.

It is admitted, and what the boundary turns on is what the two arms differ by
rather than what the branch reads. `{?a/b}` chooses between two literal
alternatives, and the alternative it picks is never re-read as a template:
measured on 2026-08-20 with cli 3.6.6,
`{cli::qty(n)}{?{.val {lab}}/Margin labels}` renders its singular arm as the
uninterpolated text `{.val {lab}}`. The two arms here name different subjects —
the one colliding label, which is interpolated, against the words `Margin
labels` standing in for several distinct ones, which is not — so no single
template can write both. The count is how that subject gets chosen; it is not
something being inflected.

So the rule stands where it bites, and `design/architecture.md`'s "`{?}` for
every plural" stays literally true. A noun that inflects still goes through
`{?}`, as `column{?s}` behind `cli::qty()` does in each of the four arms rather
than being branched on. What an R branch may choose is a whole element or a
whole clause, which is what `R/share.R`'s helper-position refusal and
`R/grouping-plan.R`'s two renaming refusals already do; that those two read a
boolean and this one reads a count is not a distinction this ADR draws.

**The inventory in that section** is what goes: "`{?s}` covers the nine sites
that suffix one, `{?is/are}` the one that also inflects a verb, and `{?a/b}`
the two that switch a whole phrase or pick between two different nouns." It is
a census of the corpus taken before any of it was re-authored, it already
undercounted — the collision refusal switches a whole phrase and sits in none
of its three columns — and #223's phase 3 moves the rest of it, since #235
dissolved `abort_selection_rename()`'s noun pair into `{?s}`. Nothing reads the
sentence, so nothing failed either time. It is struck rather than recounted,
because the rule is deliberately stated over the construction and a recount
would be stale again by the next file. Whether anything should count these
sites is left open in [#236](https://github.com/sayuks/marginplyr/issues/236).

The cost is the one those two precedents already pay, doubled, because the
subject and the collision kind are two branches rather than one: four arms,
each repeating the bullet that carries the columns, the noun that inflects
beside it, and the remedy that follows. The structural gate is what leaves no
cheaper spelling — a template hoisted out of the arms is a template bound
elsewhere, which it refuses.

## The migration introduces wrapping, so a line is authored to survive it

`rlang::abort()` wraps nothing. `cli_abort()` wraps at retrieval time, at
whatever width the reader's session has. The diagnostics this package shipped
were therefore unwrapped everywhere they appeared — including in the rendered
vignettes, where `.github/scripts/verify-site.R` matches seven of them as
`fixed = TRUE` substrings of the built HTML.

So this is not a re-formatting whose cost is churn in pinned texts. It puts
line breaks into shipped pages where none existed. Not into the byte-exact
pins: *Consequences* below records the option that keeps the test suite from
seeing a wrap at all. Three conditions are what an author writes against:

1. The authored prose of a line, with interpolation excluded, fits in 80
   columns.
2. A part whose length the caller decides — a vector of column names, a list of
   offered values — does not go in the main line. It goes alone in an `i`
   bullet.
3. No pin and no marker spans a wrap point. A marker is chosen from a run of
   uninterpolated prose.

Condition 2 is what makes the first one hold rather than merely being asked
for. The fixed-`.by` label refusal —

```text
`.margin_label` must not name fixed `.by` columns `region`, `grade`.
```

— cannot be kept short by choosing shorter words, because its length is a
function of how many columns the caller passed. Split into a refusal and a
bullet carrying the columns, the refusal never wraps whatever arrives.

Condition 3 is what keeps `verify-site.R`'s markers meaningful once condition 1
stops being enough — which it does the moment a long interpolated vector pushes
an otherwise short line past the margin.

This is the whole of why the ticket that produced this ADR insisted the
migration succeeds only as re-authoring. Transporting the existing long flat
sentences into `cli_abort()` unchanged would satisfy the type system and break
the site gate.

## Every subject has one inline style

| subject | style | renders as |
| --- | --- | --- |
| column or grouping-dimension name | `{.var}` | `` `region` `` |
| argument name | `{.arg}` | `` `.margin_label` `` |
| code fragment, assignment included | `{.code}` | `` `.duplicates = "error"` `` |
| constructor or function | `{.fun}` | `` `rollup()` `` |
| value the data holds | `{.val}` | `"All"` |

`{.var}`, `{.arg}`, and `{.code}` were measured to render identical bytes for a
character vector. Separating them is still the decision, because the
alternative is not three styles collapsing into one — it is every site reaching
for `{.code}`, which is the one that fits anything, and the table ceasing to
say anything about what a sentence is made of.

The `{.val}`/`{.code}` boundary is the one `R/margin-label.R`'s collision
refusal already drew before there was a table: a spelling the caller types is
`{.code}` — `NA_character_` is an R literal, not a value — while a value the
data holds is `{.val}` — `"All"` is a factor level. Both renderings preserve
the bytes that refusal shipped, which is the evidence the boundary was already
being drawn correctly by hand.

## Caller text is a value, never part of the template

> Caller-derived text is always an interpolated value. It is never concatenated
> into the template, and the template is always a literal in the source: no
> computed template strings.

cli interprets the template and not the values it interpolates, so a caller's
braces are inert wherever this holds. A column named `` `a{b}` `` is legal and
reaches most of the diagnostics in `R/`, so the hazard is not confined to any
one module.

That is why this is written as a rule about a practice rather than as an
exclusion for the restatement machinery of ADR 0021 and ADR 0022. The
restatement path splices an arbitrary caller string, so it is refused by the
rule; stating it as "that module is excluded" would have carved out the one
place the hazard had been noticed and left it live everywhere else.

The same reading puts one line *into* scope that a module-shaped exclusion
would have kept out. `report_branch_warnings()` writes marginplyr's own
sentence — `N further grouping set{?s} raised this warning.` — and the only
value it interpolates is an integer marginplyr counted. It moves to
`cli::pluralize()` inside `rlang::format_error_bullets()`, which was measured
byte-identical to the `sprintf()` and `if` it replaces. It gains no markup and
no width dependence: `pluralize()` does not consult the width. ADR 0021's
identity is computed when a warning is buffered, before this line is appended,
so nothing about that contract moves.

## Every singular/plural choice goes through cli's `{?}`

No R branch spells a noun. `{?s}` covers the nine sites that suffix one,
`{?is/are}` the one that also inflects a verb, and `{?a/b}` the two that switch
a whole phrase or pick between two different nouns.

The rule is stated over the construction rather than over a list of sites,
because the complaint that started this work was that one idiom had been
written out at eight of them. A rule permitting an `if` where `{?}` is
awkward relocates that idiom rather than dissolving it, and leaves every
phase-3 pull request deciding for itself whether its own branch is the
permitted kind.

## cli's vector defaults are adopted unchanged

Serial `and`, `{.or}` where the vector is a list of alternatives, and the
default 20-element truncation. So `` `a` and `b` ``, `` `a`, `b`, and `c` ``,
and `` `a`, `b`, or `c` ``.

Three conventions for joining a vector into a sentence were in use at once — a
bare comma at more than twenty sites, a serial `", or "` in
`format_grouping_constructors()`, and a bare `" or "` in the `.duplicates`
offer beside it — so there was no existing spelling to preserve. Adopting the
defaults converges them, and it is the choice that needs no override written at
every site.

Truncation is the half that loses information, and it is accepted because the
subjects here are grouping dimensions, `.by` columns, and named arguments. A
call reaching twenty of them has a systematic mistake rather than a list to
work through, and the reader still sees the last two and the count. Writing
`vec-trunc = Inf` would put a value in every message for a case no measurement
suggests these diagnostics reach.

## cli becomes an Import, with its own floor

`cli (>= 3.4.0)` moves from Suggests to Imports. After this change every error
path crosses it, which is direct use and not a promotion made to satisfy a
check.

The floor is a deliberate departure from #217, which decided that cli carried
none, and the mechanism is what differs. That floor would have sat in a
Suggests entry, where `marginplyr_suggest_available()` is the only reader and
it is never asked about a package that cannot be absent — so the constraint
really could never bind. An Imports floor is read by the installer. 3.4.0 is
where cli's current `vec_sep`/`vec_last`/`vec_trunc` spellings arrived, and it
is the highest version anything above needs.

dplyr declares `cli (>= 3.6.2)`, so nothing extra installs while that holds.
That is exactly the reason to write our own: `AGENTS.md`'s dependency-metadata
rule is that another package's closure is a property of that package's
metadata and can change without a commit here. The availability paragraph above
`written_message_lines()` stops being true when this lands — cli is no longer
the `DBI = FALSE` case — and is rewritten in the same commit. `DBI` itself
still is that case, so the example in `AGENTS.md` stands.

glue stays where it is and is not a diagnostics dependency. Its three
load-bearing paths are the `across(.names =)` template expansion, rlang's
`"{name}" :=` injection, and `cli::pluralize()` itself, which calls `glue()` at
run time because cli only Suggests it. So "cli replaced glue, drop it" is
answered before it is asked.

## Two rules are gated; the third rests on a gate that already exists

*Caller text is a value* and *every singular/plural choice goes through `{?}`*
are both caught by one structural test, in the shape of `test-query-policy.R`:
walk every `abort_marginplyr()` call in the loaded namespace and fail any whose
message argument is not a literal in the source. A `paste0()`-assembled
template and an `if`-spelled noun are the same violation seen from two sides,
because neither can be written without computing the argument.

The line-length conditions get no gate of their own, and that is a decision
rather than an omission. Their failure mode is a marker that vanished across a
wrap point, and `verify-site.R` already fails a build for a missing marker. A
gate measuring the width of the literal alone would return clean for the case
that actually breaks — a short authored line that a long interpolated vector
pushed past the margin — which is the objection this repository makes to any
assertion that cannot fire where the fault is.

## Considered Options

**Adopt cli's pluralization helper without re-authoring the messages** — the
option #206 weighed, and the one this supersedes. Rejected because it dissolves
the singular/plural idiom while leaving eighty-three long flat sentences that
`cli_abort()` then wraps at retrieval time, putting breaks into rendered
vignettes and pins for no gain the reader can see. The wrapping is not a side
effect to absorb; it is the reason the sentences have to change shape.

**Keep `abort_marginplyr()` taking an assembled string, and call
`cli::format_inline()` at each site** — Rejected. It adds the same boilerplate
at eighty-three sites and leaves each one responsible for pluralizing its own
bullets, which is the shape *every singular/plural choice goes through `{?}`*
exists to remove. It also removes the single point where the injection rule can
be gated.

**Override `vec-last` and `vec-trunc` to preserve the shipped spelling** —
Rejected. There was no single shipped spelling to preserve, and an override
written at every vector-bearing site is a second idiom written out repeatedly,
which is the thing #206 objected to.

**Exclude `R/conditions.R` as a module** — the form the originating ticket gave
exclusion 1. Rejected because the hazard it names is splicing caller text, not
the module's identity, and stating it that way both leaves the hazard
unaddressed in the files that also splice and keeps `report_branch_warnings()`
— which splices nothing — outside a rule it can satisfy exactly.

## Consequences

The byte-exact pins stay byte-exact and stay out of snapshots. Inside
`test_that()`, `local_reproducible_output()` sets `cli.condition_width` to
`Inf` and rlang consults that ahead of `cli.width` and `width`, so
`conditionMessage()` of a `cli_abort()` condition is not wrapped at all in the
test suite, and is therefore deterministic across runs; retrieval-time
formatting varies with a reader's session, not between two runs of the suite.
Snapshots would be skipped under CRAN semantics, which is where
`test-diagnostic-pluralization.R` says its sixteen arms hold, so moving them
there would silently retire the baseline the re-authoring is measured against.

That same option is why the phrase-level pins — the several hundred
`expect_error()` and `expect_match()` patterns — need no normalizing
comparison: a phrase cannot be split by a wrap that does not happen. The one
newline such a pin can still meet is the structural break between a re-authored
message's main line and each of its `i` bullets, and normalization would not
rescue a phrase spanning that either, because the bullet marker sits in the gap.
A pin spanning that boundary is a pin whose sentence the re-authoring has split,
which is what the phase rewriting that file re-pins anyway. `verify-site.R`'s
marker matching is where the normalization is needed, because a vignette is
rendered outside `test_that()` and does wrap; it is needed on top of condition 3
above rather than instead of it.

The first version of this section read `width` rather than
`cli.condition_width` and asked for a test-side normalizing helper on that
basis. The measurements correcting it, and the two site markers that only match
once normalized, are in
`investigation/retrieval-time-formatting-of-a-cli-diagnostic.md`.

Wrapping is not the whole of what retrieval-time formatting costs. The
formatter collapses every run of whitespace in the stored message, including
inside a value the template interpolated, so a subject whose spelling a caller
chose is shown re-spelled: a `.margin_label` dimension named `` `bad  name` ``
is stored with its two spaces and shown with one, where `rlang::abort()` showed
the caller's bytes. Column names, label values, `.id`, and `.key` can all
legally carry such a run, so this reaches the sixty-six sites that splice caller
data — and reaches them identically after re-authoring, since it follows from
signalling through `cli_abort()` rather than from any shim. `format_inline()`
has `keep_whitespace`; `cli_abort()` exposes no equivalent, so preserving the
spelling would mean not signalling through it, which is the whole of this
decision. Whether that trade is right is left open in #230 rather than settled
here, and nothing pins such a name, which is why this is recorded and not
caught.

Until the re-authoring is finished, a fourth path holds the flat form, and it is
not one of the three the opening section names. Every site not yet re-authored
passes through a transitional `abort_marginplyr_flat()`, which hands its
already-assembled string to `abort_marginplyr()` as an interpolated value; cli
interprets the template and not the values, so *Caller text is a value* holds
across the whole corpus from the day the switch lands rather than file by file.
That is also what bounds the gate's reach: it reads `abort_marginplyr()`'s own
argument, so a site still calling the sibling is outside its view, and the
snapshot beside it in `test-diagnostic-authoring.R` is what stops a new one
appearing. The sibling is deleted with the last of those sites, and the gate
then covers every diagnostic this package raises.

`marginplyr_error` survives, so the `must_error` hook and its twenty vignette
chunks are untouched: `cli_abort()`'s `class` argument carries it through, and
the hook reads the class chain rather than the text.

`CONTEXT.md` is unchanged. Package condition, External condition, Condition
context, and Repeated condition already say everything this decision needed to
be written in, and an inline style table is an implementation decision rather
than a term.

The rules are enforced where *Two rules are gated* says and reviewed
everywhere else. In particular, nothing checks that a re-authored diagnostic
chose the right style from the table — `{.var}` and `{.code}` render alike, so
a wrong choice is invisible until cli styles them differently, which is the
respect in which this table is a convention held up by review.
