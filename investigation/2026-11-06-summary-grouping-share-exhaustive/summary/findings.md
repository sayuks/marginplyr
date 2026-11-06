# Ordinary Margin summary cast-dispatch investigation

Investigated: 2026-11-06
Repository snapshot: `9ffb24614a71512170298a3827bc11c80354e742`

This lane used the source checkout through `pkgload::load_all()` with
`TESTTHAT_PARALLEL=false`. No package implementation or package test changed.
The summary reference's *Relationship to dplyr summaries*, ADR 0016's ordinary
column-behavior delegation, ADR 0019's Contextual helper boundaries and ADR
0028's unnamed local expansion decision supplied the contracts. The November
5 greedy and November 6 ordinary/deep summary lane notes supplied the earlier
negative matrices and the separate positive-row list-conversion finding.

## Confirmed: unnamed local frames bypass their original frame-level casts

An ordinary closure returned a one-row data-frame subclass expressing one
metre for one group, and another compatible subclass expressing 100
centimetres for the next group. Their vctrs common type was metres and their
frame-level cast converted centimetres to metres. Direct `vctrs::vec_c()` and
ordinary `dplyr::summarise()` both produced `distance = c(1, 1)`.
`summarize_with_margins()` produced `distance = c(1, 100)`.

Two input rows and a fixed key sufficed: no variable margin, duplicate set,
share, naming template or selection rewrite was needed. Naming the summary
explicitly retained normal packing and the correct converted values. A
homogeneous metre-frame control agreed. A closure consuming `pick(x)` instead
of `x` reproduced the same wrong values, including the Assigned summary name
path.

The fixture's same-class casts were identities; only the valid cross-class
unit conversion changed values. It was a deliberately small custom class,
not a claim about a particular third-party data-frame package. The defect
concerned expanded numeric values rather than preservation of an arbitrary
outer class or attribute.

### Red-capable feedback loop and discriminating evidence

[frame-casts.R](frame-casts.R) provided the minimal public-interface witness,
direct-vctrs oracle and named/homogeneous controls. Its observation mode
asserted the dated defect and exited zero. Its `--assert` mode required the
ordinary result and exited one at
`identical(actual$distance, expected$distance)`; the full transcripts were
[observation](frame-casts.log) and [contract](frame-casts-contract.log).

The desired-contract mode did not require the dated wrong value first. Its
`--assert --named-control` path used the public explicitly named summary,
then expanded the packed result for the same value assertions; it exited zero.
[The green-control transcript](frame-casts-green-control.log) established that
the assertion could pass for a correct public result. No implementation was
changed to produce that control.

After the first differential, the hypotheses were ranked as follows:

1. The carrier discarded original frame-level cast dispatch. This predicted
   loss only when multiple original frame classes needed value conversion,
   and preservation when the same output was explicitly named and packed.
2. Grouping-set union alone changed the common type. This predicted that a
   single grouping-set branch with fixed keys would agree.
3. Selection rewriting changed the expression's values. This predicted a
   dependence on the `pick()` spelling.

The minimized fixed-key call refuted the second alternative, the bare-column
call refuted the third, and the named/homogeneous controls supported the first.
`new_local_summary_value()` retained the original prototype as metadata while
putting its stored columns into a `margin_summary` frame.
`vec_ptype2.margin_summary.margin_summary()` combined the original prototypes,
but `vec_cast.margin_summary.margin_summary()` used `vctrs::df_cast()` on the
carrier instead of casting the original frame value to that common original
prototype. Its ordinary columns therefore needed no unit conversion and the
original frame-level methods did not determine the expanded result.

[frame-cast-matrix.R](frame-cast-matrix.R) crossed three eager inputs (plain
data.frame, tibble and data.table), three plan roles (fixed key, included
Grouping set and rollup), and bare-column/selected-frame expressions. All
18 unnamed cases produced the wrong expanded value and all 18 explicitly
named controls retained the ordinary result. [The CSV](frame-cast-matrix.csv)
and [transcript](frame-cast-matrix.log) retained expected and observed values.
The rollup oracle separately summarized the detailed and Grand total branches
with ordinary dplyr, then combined their expanded columns.

### Duplicate comparison and ticket recommendation

The all-state GitHub Issue and PR queries `"vec_cast" "summary"` returned no
matches. The all-state Issue queries `"unnamed" "class"` and `"cast" "frame"`
returned surrounding reports; their titles and bodies were checked for this
boundary. The complete bodies and comments of
[#1020](https://github.com/sayuks/marginplyr/issues/1020) and
[#1365](https://github.com/sayuks/marginplyr/issues/1365) were read.
The former concerned accepting incompatible frame/non-frame mixtures; the
latter concerned extra positive-row `as.list()` dispatch. Both repairs were
present at the investigated SHA. Neither reported bypassed cross-frame
`vec_cast()` value conversion. No matching report was established by those
queries.

Recommended independent ticket: **[P2] Preserve original frame casts when
expanding unnamed local summaries**. It needed no blocker.

- Unnamed compatible frame-subclass values requiring frame-level conversion
  must expand to the values ordinary dplyr produces within one branch.
- Cover bare-column and selected-frame expressions, eager input classes,
  fixed keys and included/rollup Grouping specifications.
- Preserve explicit-name packing, homogeneous frames, unspecified-value
  expansion and the established incompatible-type refusals.
- Evaluate the caller's summary once per group; retain public-interface
  regression coverage of the cross-class conversion.

The coordinator owned independent replay, latest-main confirmation, final
all-state deduplication and Issue publication.

## Additional independent boundary comparisons

| Probe | Finite scope | Observation |
| --- | --- | --- |
| [dtplyr special columns](dtplyr-special-controls.R) | Thirty input measure names crossed pronoun, bare, pick and across reads: 120 comparisons against ordinary dtplyr summaries. | No unmatched acceptance or accepted value result. |
| [Local control flow](control-flow-controls.R) | Twenty ordinary expression wrappers crossed three value forms and three eager input classes: 180 comparisons. | No unmatched acceptance or accepted value result; nine pairs were refused by both. |

The dtplyr names included data.table special spellings, numbered dots,
ellipsis, data-frame constructor formal names, data.table argument names,
pronouns and ordinary function/helper names. The local wrappers included
identity/force/invisible, braces, local evaluation, visibly evaluated
language, conditional/switch, condition handlers, promises/defaults, early
returns, loop returns, on-exit cleanup and do.call. The paired-error
comparisons established no equality of condition classes, messages or causes.
The [dtplyr transcript](dtplyr-special-controls.log),
[CSV](dtplyr-special-controls.csv), [control-flow transcript](control-flow-controls.log)
and [CSV](control-flow-controls.csv) retained their zero-difference verdicts.

## Scope and remaining boundaries

These finite witnesses established neither arbitrary custom-method
side-effect equality nor equivalence for every custom class. They made no
claim about remote backends, other dependency versions or platforms,
concurrency, interruption, or arbitrary code introspecting evaluation frames.
The complete suite was not used as a hunting loop. Every owned R execution
reached terminal completion before handoff. The source file's `--assert`
failure was an intentional dated-contract failure, not an implementation
repair or a Review-ready outcome.
