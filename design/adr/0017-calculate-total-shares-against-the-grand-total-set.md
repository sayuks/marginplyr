# Calculate Total shares against the grand total set

`share_of_total()` is a second contextual summary helper for
`summarize_with_margins()`. It divides one preceding named numeric scalar
summary by the same summary on the Grand total set. Its source contract, its
`across()` grammar, its output naming, its value rules, and its backend
boundaries are `share_of_parent()`'s, unchanged and recorded in ADR 0010.
Only the denominator differs. This decision records what that one difference
implies, because in four places it implies something ADR 0010 did not settle.

## Decision

A Total share's denominator is the value of the same source summary on the
Grand total set within the row's fixed `.by` partition. A row of the Grand
total set has a Total share of one. Missing numerator, missing denominator,
and zero denominator produce `NA_real_`; every other result is an unclamped
double. Those are ADR 0010's rules and they are not restated here.

### It is supported wherever the plan contains a Grand total set

ADR 0010 closes by deferring `cube()` and arbitrary grouping sets "until an
explicit parent-selection model exists". That deferral is about selecting a
parent, and it does not reach this helper.

A parent is ambiguous outside a rollup because a grouping set can have
several strictly less detailed sets in the same plan and nothing in the plan
says which of them is *the* parent. A Grand total set is not selected at all:
it is the set in which every variable grouping dimension is omitted, and a
plan either contains it or does not. There is nothing for a selection model
to decide.

`share_of_total()` is therefore supported for any Grouping plan containing a
Grand total set. `rollup()` and `cube()` always produce one. `grouping_sets()`
produces one only when it includes an empty `grouping_set()`, and a
specification without one raises a Package condition naming that fix.

The asymmetry this creates between the two helpers — one accepts `cube()`,
the other does not, in the same module over the same machinery — is a
property of their denominators and not an inconsistency to be tidied away.
Making `share_of_parent()` accept `cube()` still needs the parent-selection
model ADR 0010 deferred, and this decision does not supply one.

### Fixed keys partition the denominator

The denominator is the Grand total set *within* each fixed `.by` partition,
exactly as ADR 0010 defines for Parent shares.

A denominator pooled across `.by` partitions was rejected on an invariant
both helpers already keep: a share's denominator is a row of the result the
caller can see. Fixed keys appear in every grouping set of a plan, so no row
of any result represents a cross-partition total. Dividing by one would
divide by a number the result never displays, that `.id` cannot reach, and
that no caller can check the result against. It would also need a second
aggregation of the input rather than a second read of the staged result,
which is the cost the shared staging exists to avoid.

A caller who wants a cross-partition denominator moves that column out of
`.by` and into `.grouping`. The pooled total then becomes a row of the
result, and the invariant is kept rather than excepted.

### Duplicate grand total occurrences are interchangeable

Which Grand total occurrence supplies the denominator is not specified.

Duplicate occurrences arise only under `.duplicates = "keep"`. Two Grand
total sets are two occurrences of the same grouping mask, so `"error"`
rejects the plan and `"drop"` removes the second. When they are kept, both
aggregate the same rows and therefore hold the same values, so every
candidate denominator is the same number. Naming one of them — "the first in
plan order" — would put plan order into the public contract in exchange for
nothing. Grouping set identifier is already defined as position-dependent and
unstable across plan changes, so a denominator rule that depended on it would
contradict the vocabulary.

Every row of the Grand total set is itself one, duplicates included.

This differs from Parent shares, where duplicate occurrences are *skipped*
because a parent must be strictly less detailed. Here they are equivalent
rather than skipped, and documentation states the two rules separately rather
than sharing one sentence.

### Total shares get their own denominator mapping

Total shares reuse the module's staging, source validation, conditions, and
adapter dispatch. They do not reuse its parent mapping.

`build_lazy_parent_mapping()` filters the staged result once per child
occurrence and unions the results, because it must tag each child with the
denominator row of a *different* parent. That is linear in the number of
grouping sets, which for a rollup is one per dimension.

For a Total share every occurrence has the same denominator, so the same
construction would union one relation to itself once per non-root occurrence
— thirty-one branches for `cube()` over five dimensions, each rescanning the
staged result — and would synthesize a join key column per dimension whose
value is `NULL` on both sides, since the Grand total set contains no
dimension to match on. A Total share's denominator depends on `.by` and
nothing else, so its mapping is the Grand total rows reduced to one row per
fixed partition, joined on the fixed keys with the same missing-safe
identity.

The query shape is not part of the public contract and this could have been
left to a later optimization. It is decided here because the simpler mapping
is also the one that states the rule: the denominator is a function of `.by`.

### Arrow rejects Total shares, in the caller's terms

ADR 0010's amendment rejects Parent shares on Arrow because Arrow's schema
seam cannot prove scalar cardinality and the mechanisms that would validate
inside the query erase the Package condition's class and the caller's call.
Every one of those reasons is about the numerator's source summary, which
this helper shares unchanged, so the rejection transfers verbatim and Arrow
rejects Total shares at the same admission point.

The message names the helpers the caller actually wrote rather than a fixed
`share_of_parent()`. A caller who wrote only `share_of_total()` is otherwise
told to remove a function that is not in their call.

### The name

`share_of_total()`, symmetric with `share_of_parent()`.

"Total" is also the default `.margin_label`, and that collision was weighed
and dismissed: the glossary's Margin label entry already lists "total value"
as a term to avoid, so the package's own vocabulary does not call a Margin
label a total. The collision is with the default value of an option, which a
caller can change, and not with a canonical term.

## Considered options

**`rollup()` only, symmetric with `share_of_parent()`.** Rejected: the
restriction on Parent shares follows from parent ambiguity, which a Grand
total set does not have, so copying it would restrict this helper for a
reason that does not apply to it. It would also refuse the most common
reason to reach for a cube — asking what fraction of the whole each cell is.

**`rollup()` and `cube()` only**, the specifications that always contain a
Grand total set. Rejected: it would reject a `grouping_sets()` plan that does
contain one, which is a supported denominator refused on the grounds of how
the caller spelled the specification.

**A denominator pooled across `.by` partitions**, or an argument selecting
between pooled and partitioned. Rejected above; an argument would make every
caller decide something the invariant already answers.

**Naming the first Grand total occurrence in plan order.** Rejected above.

**Erroring when a plan has more than one Grand total occurrence.** Rejected:
it refuses a case whose value is unambiguous, and the caller has no way to
read the refusal as anything but arbitrary.

**One helper, `share_of(x, .of = "parent" | "total")`.** Rejected: the
`across()` grammar requires `.fns` to be the bare helper, so a mode argument
could not be supplied there at all, and column-wise shares are the form that
motivated `across()` support in ADR 0010.

**`share_of_grand_total()` and `share_of_root()`.** Rejected: the first is
unambiguous and unusable at the length callers actually type; the second
names the plan's internal vocabulary rather than the reader's, and "root" is
the word the glossary now lists under Grand total set as one to avoid.

**Reusing the parent mapping unchanged**, leaving the union to a later
optimization. Rejected above. It was the prototype's implementation and it is
correct; it is not what the rule says.
