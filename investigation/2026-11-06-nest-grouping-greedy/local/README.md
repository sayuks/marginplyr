# Local multiple-key and logical-control boundaries

Investigated: 2026-11-06
Tested package SHA: `952a0d9b18fd3ff4631f153432f6024ad62fcac2`

This track investigated both nesting Margin verbs without implementation or
permanent-test changes. `CONTEXT.md`, the nesting reference and ADRs 0016 and
0018 supplied the source-cell, factor and Margin order contracts. The November
5 [structured-key track] and [boundary hunt] supplied the previously explored
frontier. The new matrix crossed multiple dimension types together rather than
varying one dimension in isolation.

## Confirmed: an accepted logical check flag bypassed collision admission

`logical-check-dispatch.R` constructed a logical scalar storing `TRUE` with a
class whose logical-negation method returned `TRUE`. Both `isTRUE(flag)` and
ordinary `if (flag)` read the flag as true. The four Margin verbs accepted it
as `.check_margin_label` and returned two indistinguishable `Total` rows for
one source row whose grouping value was already `Total`. Each result was
identical to the explicit plain-`FALSE` control. Plain `TRUE` raised the
expected Package condition about the observed Margin label collision.

The inherited parameter contract described `.check_margin_label` as a logical
scalar controlling observed collision checks. The normalization retained this
classed scalar after its successful `isTRUE()` guard. The later negated gate
then dispatched the caller's method. This affected both Nest verbs and the
expansion and summary consumers of their shared validation boundary. It did
not require a malformed source, source mutation, stateful method or alternate
Margin label.

The separating hypotheses were that the flag survived validation and changed
the negated gate, that its class changed the source or label comparison, or
that option preparation substituted a different value. The constant negation
method, unchanged source, plain-TRUE refusal and plain-FALSE equality isolated
the first. Inspection of the scalar guard, option normalization and observed
collision gate established the specific dispatch route.

The reverse direction was not established for a constant negation method. A
stored `FALSE` with a method returning `FALSE` failed the scalar guard, because
base `isFALSE()` itself negated its argument. It was not an accepted false flag
that unexpectedly read the source. The note makes no such claim.

An accepted stored-TRUE `.keep` control retained the original keys, because
that branch read the flag directly. Named, matrix-shaped and ordinary AsIs
TRUE scalars still received the collision refusal. These controls distinguished
custom logical negation from ordinary scalar attributes. No claim about
`.check_share_source` was made.

Observation mode completed with exit 0. Desired-behavior mode (`--assert`)
completed with exit 1 at `inherits(stored, "marginplyr_error")`, providing a
small deterministic failing command. The parent investigator published [#1360]
after an independent replay, as an independent ticket with no blockers.

### Duplicate search

All-state GitHub Issue searches used:

- `"logical" "class"`
- `"check_margin_label" "class"`
- `"logical scalar" "check_margin_label"`
- `"check_margin_label" "dispatch"`

An all-state PR search used `"check_margin_label" "class"`. No matching logical
negation report was returned. The complete bodies and comments of [#1309],
[#516], [#109] and [#371] were read. #1309 concerned stored character strings
and the fixed vocabulary of Option arguments. #516 concerned explicit opt-out
remediation, #109 the refusal of NULL, and #371 the per-dimension label form;
none covered an admitted TRUE flag bypassing collision admission.

## Independent multiple-key controls

`multikey-oracle.R` compared every ordered pair drawn from twelve key families:
character, integer, double with NA/NaN/infinities/signed zero, unordered factor,
factor with a declared NA level and separate missing codes, ordered factor,
list, packed frame, matrix, Date, POSIXct and difftime. Each source also carried
a fixed character key containing a missing value, an integer source-row
identifier, and a packed payload containing a factor with an NA level.

The four sets of a two-dimension cube were independently enumerated. For every
set, `vctrs::vec_group_loc()` over the original fixed/included keys supplied the
expected source-row memberships. Every returned cell had to retain the original
row order, the required column order and source values selected by a vctrs
slice. Serialized input equality detected source mutation.

For explicit Margin orders, a separate key was built from visible fixed keys,
Grouping bits derived from the independently enumerated sets, row-vector
missingness, visible values and the public Grouping set identifier. Native
dplyr ordering over that key had to leave the returned row order unchanged.
This checked the order itself, extending the prior structured-key track's
membership-only sorting controls.

Both verbs, both `.keep` choices and all three `.sort` choices gave 1,728
matching calls with typed-missing labels on both dimensions. `--mixed` replaced
the first dimension's label with `Total` and retained a typed-missing label on
the second. Its first dimension ranged over the nine scalar families that
admitted a string display; the second ranged over all twelve. Its 1,296 calls
also matched. No source changed in either matrix. The CSV files retained one
row per completed comparison.

Existing `.drop = FALSE` grouped-input behavior was inspected separately: the
shared reference explicitly refused it, and the grouping-interface tests
asserted that refusal. It was therefore not promoted as a missing-empty-group
finding. These matrices did not claim coverage of that refused input.

## Replay and limits

Run one command at a time from the tested checkout root. The optional first
argument selects another source checkout; the optional second argument selects
an existing CSV output directory. Without an output argument the matrices
write to R's temporary directory.

```sh
evidence=investigation/2026-11-06-nest-grouping-greedy/local
Rscript "$evidence/logical-check-dispatch.R"
Rscript "$evidence/logical-check-dispatch.R" --assert
Rscript "$evidence/multikey-oracle.R" . "$evidence"
Rscript "$evidence/multikey-oracle.R" --mixed . "$evidence"
```

The logical observation invocation completed directly with exit 0; its
assertion invocation completed directly with exit 1. The original typed-missing
matrix retained execution session 88077 through exit 0. The original mixed
matrix retained execution session 65564 through exit 0. The final script
accepted `--mixed` explicitly instead of requiring a temporary edited copy;
its final replays retained sessions 76877 and 81718 through exit 0, with 1,728
and 1,296 matches respectively.
All executions were direct R processes without owned child work. This track
started no suite, build, render or Review-ready invocation.

The environment was R 4.6.1 on aarch64-apple-darwin23, dplyr 1.2.1, tidyr 1.3.2,
tibble 3.3.1, rlang 1.3.0, vctrs 0.7.3 and pkgload 1.5.3. These finite checks
covered the listed local key combinations and accepted flag. They did not
establish behavior for every S3/S4 method, every possible grouping tree,
resource limits, other R/dependency versions, or hosted backends.

[structured-key track]: ../../2026-11-05-nest-grouping-adversarial-followup/local/README.md
[boundary hunt]: ../../2026-11-05-nesting-grouping-spec-hunt/README.md
[#1309]: https://github.com/sayuks/marginplyr/issues/1309
[#516]: https://github.com/sayuks/marginplyr/issues/516
[#109]: https://github.com/sayuks/marginplyr/issues/109
[#371]: https://github.com/sayuks/marginplyr/issues/371
[#1360]: https://github.com/sayuks/marginplyr/issues/1360
