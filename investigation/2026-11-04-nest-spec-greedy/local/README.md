# Local nesting: factor encodings and composed extension-vector keys

Investigated: 2026-11-04
Baseline: `78ebbc36200bdc5cef70838dd4e772c5c4a1401c` (`main`)

## Outcome and contract

One new defect was confirmed and published as [#1215](https://github.com/sayuks/marginplyr/issues/1215): a valid factor subclass's character method changed
ordinary nonmissing factor values to missing during package factor encoding and
reconstruction. The defect reached both nesting Margin verbs, and the shared
route reached expansion and summary too. No package implementation, permanent
regression test, dependency or earlier investigation note changed.

The nesting reference promised one cell per source group and original source
values under `.keep`. Its inherited *Result class and attributes* section and
ADR 0016's *The promise boundary* placed factor reconstruction on the package's
promised side. ADR 0012 distinguished factor values, declared levels and missing
codes. This finding concerned changed values, rather than requiring the
subclass or its arbitrary attributes to survive.

The November 3 [local investigation](../../2026-11-03-nest-spec-greedy/local/README.md)
and its root record, and the November 2 [composition investigation](../../2026-11-02-nest-grouping-hunt/local/README.md),
supplied earlier findings and unexplored boundaries. October 30/31 equality
records supplied source-equality controls. None of their counts was counted as
an execution on this baseline.

## Confirmed: factor character methods broke package-owned value reconstruction

The final witness was a factor with ordinary levels `a` and `b`, extended with
an `as.character()` method returning `[a]` and `[b]`. The method preserved empty
length and missing values. Compatible same-class vctrs common-type and cast
methods made ordinary local union preserve the source values. The factor's
source values remained its stored codes and declared levels; its character
method only provided a different representation.

Both nesting verbs with a single included `grouping_set(g)` and the default
label returned missing outer factor values in place of `a` and `b`. No omitted
branch, rollup, duplicate set or Margin order was necessary. The list-column
cells still identified the right source rows; a local `.keep = TRUE` copy also
retained the original factor values. A typed-missing label with no NA level
avoided the package encoding route and preserved those values.

Three hypotheses distinguished the boundary:

1. The source was not a valid dplyr grouping vector. Native vector grouping,
   local tidyr nesting and immutable dtplyr grouped summaries retained both
   original values and groups, refuting this explanation.
2. Ordinary union or collection destroyed the values before marginplyr could
   answer. Local same-class union, native immutable dtplyr collection and
   dtplyr union retained codes `1, 2` (repeated for union), refuting that
   explanation. The dtplyr union's separate loss of an unused NA level was the
   already known upstream trigger for the package's encoding route.
3. The package encoded with a subclass character method but decoded against
   undecorated declared levels. `encode_factor_for_margin()` dispatched
   `as.character()` and `reconstruct_factor_vector()` matched those strings
   against the stored levels. `[a]` and `[b]` matched neither `a` nor `b`,
   turning nonmissing source values into missing codes. The controls and role
   changes supported this hypothesis.

`factor-display.R` made 288 nesting calls across tibble/plain-frame/immutable
dtplyr sources, absent/present unused NA levels, empty/populated data,
dimension/fixed-key/payload roles, nonmissing/NULL labels, both `.keep`
settings and both verbs. Sort choices cycled across the calls. There were
44 calls with changed values and 244 matches: 8 changed-value calls for each
local representation and 28 for dtplyr. All 144 empty calls completed without
value-bearing rows; they were controls, not evidence of value preservation.

Locally, corruption required a nonmissing label on the subclass dimension.
Under dtplyr, a declared NA level also selected encode-and-rebuild for fixed
keys, payloads and `.keep` copies, so those values changed even with a NULL
Margin label. All source serialization checks matched. The eight-call
included-only companion panel reached both nesting verbs, expansion and summary
on local and immutable dtplyr inputs; all eight changed the ordinary factor
values. One shared encoding/reconstruction repair ticket, #1215, was published with
no blockers.

Observation mode returned terminal exit 0 after recording these dated defects.
`--assert` completed all controls and comparisons, then returned terminal exit 1
for the desired-behavior assertion. These observation outcomes were not passing
regression tests. The independent backend and coordinating investigations
validated the public boundary separately.

`factor-remapping.R --assert` measured three other representations through
the same mismatch: swapping the two existing levels, mapping both values to
one existing level, and mapping both values to the chosen Margin label.
All 24 local/dtplyr, checked/unchecked, two-verb calls silently changed the
outer keys while source memberships and native local nesting/union controls
remained correct. The Margin-label case returned `Total` in both detail rows
even with explicit collision checking. Factor columns were excluded from the
observed scan after their declared levels had passed validation; the generated
encoding then introduced that apparent collision. These were consequences of
the same encoding mismatch, not additional repair tickets. The desired-mode
execution was session 2278, terminal exit 1 after every comparison completed.

## Additional finite oracles

`temporal-membership.R` made 4,608 calls over twelve source-key families:
lubridate Duration, Period and Interval; zoo yearmon/yearqtr; normal and sample
distributional vectors; integer64 values above double precision; hms, difftime,
Date and POSIXct with a non-UTC time zone. The calls crossed empty/populated
sources, dimension/fixed-key roles, tibble/plain-frame/data.table/grouped-tibble
representations, label choices, `.keep` choices and both nesting verbs.
Rollup, cube and irregular grouping-set order were independently enumerated
for dimension scenarios. Fixed-role and grouped scenarios used compatible
narrower plans; their plan-label iterations also cycled the three sort choices,
so the count does not imply three different Grouping plans in those scenarios.

An independent source-group oracle matched every admitted cell membership and
all 60,480 cell-column value projections. Output column order and rowwise group
names matched, and source serialization stayed unchanged. This oracle exercised
sorting for interference with membership; it was not an independent full
Margin-order oracle. Arbitrary attributes were not promoted into guarantees:
projection comparisons accepted an attribute-only difference under ADR 0016.

`empty-converters.R` measured nine empty character converters through both
nesting verbs. Duration/Period/Interval/yearmon/yearqtr/distributional/integer64/
difftime/hms all returned successful zero-row character displays, including the
hms converter whose raw `as.character()` still returned `logical(0)`. This
confirmed the package boundary repaired after the previous investigation.
`factor-equality-controls.R` combined a case-insensitive NA-level factor fixed
key with a separate rollup dimension; every source cell retained its two rows.

## Duplicate search and prior reports

All-state Issue and PR searches through `gh` used `"factor" "as.character"`,
`"factor" "subclass"`, `"factor" "encoding"`, and `"factor" "decorated"`.
The latter searches found no decorated-factor report. JSON search results are
retained beside this note, along with the complete bodies and comments read for
[#1088](https://github.com/sayuks/marginplyr/issues/1088) / [PR #1118](https://github.com/sayuks/marginplyr/pull/1118),
[#408](https://github.com/sayuks/marginplyr/issues/408) / [PR #413](https://github.com/sayuks/marginplyr/pull/413),
[#415](https://github.com/sayuks/marginplyr/issues/415),
[#421](https://github.com/sayuks/marginplyr/issues/421),
[#491](https://github.com/sayuks/marginplyr/issues/491), and
[#56](https://github.com/sayuks/marginplyr/issues/56) / [PR #59](https://github.com/sayuks/marginplyr/pull/59).

#1088 repaired source-equality membership through private identities; the new
fixture retained membership but lost public factor values. #408/#415/#421
routed columns around a data.table union that dropped NA levels; this witness
exposed a different encoding/decoding mismatch, including a single local
included-only set. #491 concerned deferred helper lookup rather than the
factor character method called by the helper. #415's triage comment explicitly
separated changed values from the arbitrary-class/attribute boundary. No
existing report was modified or reopened in this track. The coordinating agent
owned current-main confirmation and Issue publication.

## Replay, exclusions and provenance

Run from the investigated checkout root. Entry points load that source with
`pkgload::load_all()`; output locations are configurable or use R's temporary
directory. The measured runs used a separate `/private/tmp` archive of the
baseline, with `TESTTHAT_PARALLEL=false`, one R execution at a time and no owned
child processes. Every yielded handle was retained through terminal completion.

```sh
Rscript investigation/2026-11-04-nest-spec-greedy/local/factor-display.R
Rscript investigation/2026-11-04-nest-spec-greedy/local/factor-display.R --assert
Rscript investigation/2026-11-04-nest-spec-greedy/local/factor-remapping.R --assert
Rscript investigation/2026-11-04-nest-spec-greedy/local/temporal-membership.R
Rscript investigation/2026-11-04-nest-spec-greedy/local/empty-converters.R
Rscript investigation/2026-11-04-nest-spec-greedy/local/factor-equality-controls.R
```

The temporal oracle's final execution was session 74541, terminal exit 0. The
factor matrix observation was session 22833, terminal exit 0; its desired-mode
final execution was session 12824, terminal exit 1. Adjacent logs and CSV files
retain the observations. The final factor witness strengthened its converter to
preserve the character type of a length-zero result as well as missingness; it
retained the same matrix counts as the earlier observation. A first temporal
harness passed an explicit empty `.by` to a
grouped source and reached the documented refusal; the final harness omitted
`.by` for grouped sources. An initial empty distribution-sample constructor
failed before target calls; that constructor was excluded from the converter
panel, while supported distribution-sample values were used in the completed
membership oracle.

The exploratory `extra-vector-exploration.R` also observed timeDate rejected by
tibble admission and Surv rejected by ordinary grouped summarization with a
recursion error. Those failed native controls were not marginplyr findings.
A first factor subclass without compatible cast/common-type methods let native
union lose values too; it did not supply the final witness's separating union
control. No claimed passing comparison depended on that fixture. The final
subclass supplied the compatible methods and every native control passed.

Logs recorded R 4.6.1 on macOS Tahoe 26.6.2, dplyr 1.2.1, vctrs 0.7.3,
tibble 3.3.1, dtplyr 1.3.3, data.table 1.18.6.1, tidyr 1.3.2, hms 1.1.4,
lubridate 1.9.5, zoo 1.9-0, distributional 0.8.1 and bit64 4.8.6. These
finite probes established neither all S3/S4 behavior, all source graphs, another
R/dependency/OS version, remote nesting support nor unbounded resource behavior.
No full package suite or Review-ready invocation ran in this track; publication
verification belonged to the coordinating agent.
