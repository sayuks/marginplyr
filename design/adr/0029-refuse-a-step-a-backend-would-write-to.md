# Refuse a step a backend would write to

A Margin verb refuses a Mutable step — a dtplyr step whose root was built with
`dtplyr::lazy_dt(immutable = FALSE)` — and refuses it before any branch is
built. The refusal is a Package condition naming where the input came from, why
a Margin verb cannot use it, and the one rewrite that fixes it:
`dtplyr::lazy_dt(immutable = TRUE)`.

`CONTEXT.md` defines *Mutable step*. What the refusal answers is measured in
#451: over such an input `expand_with_margins()` returned a result in which
every row carried the margin label, and left the caller holding a table whose
column *names* had been permuted while its column *data* had not — so
`dtx$region` afterwards held the integers that had been `dtx$year`. A
`select()` in the input's own pipeline lost the caller a column outright.

## Why this is marginplyr's to refuse

The hazard is dtplyr's. `union_all(mutate(l, region = "T"), l)` over a mutable
step returns the same wrong answer with no marginplyr involved, so a
multi-branch query over a step data.table may write to is broken wherever it is
written. That report is filed separately and this refusal does not wait on it.

What marginplyr adds is the part a caller cannot see. A Margin verb builds one
branch per grouping set from the one step it was handed, so a caller who wrote
no union at all gets one; the branches then write to the same table by
reference and the second overwrites the first before the union runs. The caller
asked for a rollup and received a corrupted table, and the only thing marginplyr
said about it was a `data.table` shallow-copy warning it passed along.

## Why the input is refused rather than copied or documented

[ADR 0025](0025-refuse-a-summary-a-backend-would-absorb.md) is the precedent,
and this differs from it in what is at stake rather than in the disposition.
0025 refuses because a backend would **read more than the caller needs** — the
result is right and the cost is unasked-for. This refuses because a backend
would **destroy what the caller holds**. So 0025 had to argue that the caller
loses nothing by being asked; here there is no comparison to make, because the
alternative is a wrong answer over a damaged input.

The rest follows from ADR 0020's rule that when the caller's data is touched is
the caller's to decide, read at its strongest: a verb that writes to the
caller's table has not merely read early, it has changed what the caller can
read at all.

## The line is drawn at the root, and it over-refuses

The refusal is decided by walking `$parent` to the root and asking whether that
`dtplyr_step_first` carries `implicit_copy = TRUE`, which is `lazy_dt()`'s
`immutable = FALSE` recorded under the opposite name.

That question is deliberately coarser than the damage. `lazy_dt(d, immutable =
FALSE) |> filter(...)` returns a correct result today and leaves the table
intact, and it is refused. This is accepted rather than worked around, because
what makes it safe is a property of the query dtplyr generated for that
derivation and not anything this package can promise across dtplyr versions —
and, measured in #451, a `filter()` and a `select()` over one mutable root
carry the same `implicit_copy` value and fall on opposite sides. No step below
the root separates them.

Nor does the verb separate them. `summarize_with_margins()` returned a correct
result over the same input while `expand_with_margins()` did not, and
`nest_with_margins()` corrupted the caller's table while returning a correct
result — three verbs, three different consequences, one preparation path. So
"the verbs that build more than one branch", which is how #451 first framed the
scope, is not a boundary either: every verb reaches the same place, and whether
the reference write is destructive is decided by the query.

**A field that cannot be found lets the input through.** A future dtplyr that
renames or removes `implicit_copy` would otherwise make this refuse on a field
that no longer means what it did, and a Package condition raised on a
misreading is worse than the hazard, because the rewrite it names would not
help. The tests are what report such a release: they pin both fields in both
directions and on a derived step, so the change fails CI rather than silently
switching the refusal off.

## Where it is raised

On the one preparation path every verb taking `.grouping` shares —
`prepare_grouping_plan()`, which `prepare_margin_operation()` and
`inspect_grouping()` both reach — immediately after `grouping_backend()` has
resolved the kind.

After rather than before, because `grouping_backend()` checks the dtplyr
version floor: a dtplyr below it is answered by the floor rather than by a
field that version may spell differently. Nothing above that point has read a
row, which is what ADR 0005 requires, and no `.grouping` grammar error loses
its precedence — those passes sit further up the same function and are not
hoisted.

## Considered options

**Warn and copy the input.** Rejected. It is the option that looks like a
service and is not one: `data.table::copy()` on a caller's table is a
materialization of exactly the size they asked not to pay for, chosen by a verb
they did not ask to choose it — the shape ADR 0020 exists to forbid, arriving
under a warning most callers will not act on. It also answers the wrong
question, since the caller's `immutable = FALSE` was a deliberate statement
about their memory budget and copying overrides it silently.

**A per-step predicate, refusing only the derivations that destroy.** Rejected
on the measurement above: `filter()` and `select()` are indistinguishable by
step class and by field value. Building one would mean reading dtplyr's
generated expression to decide, which is a second implementation of dtplyr's
translation whose failure mode is the silent corruption this refuses.

**Document the hazard and leave the input accepted.** Rejected, and it is worth
naming because it is what the package did. Nothing in `R/`, `man/`, or
`vignettes/` mentioned `immutable` before this decision, so "leave it
documented" was never the status quo it sounds like — and a documented hazard
whose symptom is a silently permuted table is one a reader has to have already
hit to recognise.

**Refuse in the union adapter, where the branches are built.** Rejected. It is
where the damage happens and not where it can be prevented: by then metadata
has been acquired and, for the verbs that read one, the plan has been compiled
against the input. ADR 0005 puts a local refusal in front of both.

## Documentation consequences

The database-backends vignette shows the refusal in a `must_error:
marginplyr_error` chunk beside the other dtplyr sections, and
`.github/scripts/verify-site.R` gains the matching marker. That chunk is behind
`has_dtplyr` and renders nothing where dtplyr is absent, so the marker is
quoted from the unconditional prose introducing it rather than from the
diagnostic — the placement `verify-site.R`'s own comment on the entry states,
and the one the Arrow refusal beside it already takes.

`?marginplyr` is unchanged: what a caller catches is the `marginplyr_error`
that page already promises.

The reference pages are unchanged too, and that is decided rather than passed
over (#470). A Mutable step is a dtplyr step, so `nest_with_margins()`'s
`@param .data` — *A local data frame or a `dtplyr` step* — is broader than
what is accepted, and it is left broader. This is where the parallel with ADR
0025 stops. There the page had to carry what a caller could not otherwise know,
which summaries Arrow absorbs being Arrow's to decide and moving with its
version; here the refusal is reached only by a caller who wrote `immutable =
FALSE` themselves, against `lazy_dt()`'s default, and the diagnostic names the
one rewrite. A clause would have to go on every `@param .data` definition in
the package, the refusal being raised for every verb `verbs_taking(".grouping")`
returns, and nothing would hold those copies in step — an amendment here would
leave the ones it did not reach standing and wrong.

The other pages #470 names describe an input that was accepted: when a
`dtplyr` result is collected to be made row-wise, the integers a step returns
for a grouping identity, and what establishes the share source rules on one.
None of them claims what may be passed, so none of them is what this decision
would qualify.

## Test strategy

The tests sit where the backend-kind contracts do and are guarded by
`skip_if_suggest_absent("dtplyr")`. They call `data.table::` — `lazy_dt()`
refuses `immutable = FALSE` for anything that is not already a data table, so
the input cannot be built without it — and take no guard on data.table, which
is the one-backend-per-test rule holding rather than being bent. dtplyr
declares `Imports: data.table`, so data.table is never the reason one of these
tests could fail, and a guard on it would instead skip the whole set in the
dtplyr coverage configuration, where `verify-suite-coverage.R` hides every
other member of `optional_backends()`. The test states this where it calls it.

The last assertion below is the one that is not about behaviour:

- Every verb `verbs_taking(".grouping")` returns refuses a mutable root, each
  pinned by snapshot. The set is derived rather than listed, so a seventh verb
  fails here instead of arriving unrefused, and one snapshot per verb is what
  pins the part that differs between them — the call the refusal blames.
- The caller's table is unchanged after a refusal, in names, columns, and rows.
  That is what the refusal is for, and a refusal raised after the damage would
  otherwise pass every other assertion.
- A grouped mutable step reaches this refusal rather than the fixed-key
  rejection its groups used to earn, the refusal sitting above key resolution.
  That is a consequence of the placement above and is pinned rather than left
  to be rediscovered.
- The pair that proves the predicate reads the root: a `filter()` over a
  mutable root is refused, a `mutate()` over an immutable one is accepted. Both
  steps sit one level from their root, so a predicate reading the step it was
  handed passes neither.
- Both dtplyr fields, pinned in both directions and on a derived step. A dtplyr
  that renames either makes the refusal stop firing, and a refusal that stopped
  firing reads exactly like an input nothing is wrong with — which is the
  silence this decision exists to remove.

## Related decisions

ADR 0025 is the precedent and the contrast above. ADR 0020 is the rule this
serves. ADR 0005 is what places the refusal before any backend read. ADR 0015
is the boundary it is placed against — a caller reaching it has a one-line
rewrite, which is what makes it a Package condition. ADR 0023 is the idiom it
is authored in and ADR 0024 the rule it spells `.data` by.

Evidence: #451, whose triage comment holds the table of eight inputs the root
predicate was chosen from.
