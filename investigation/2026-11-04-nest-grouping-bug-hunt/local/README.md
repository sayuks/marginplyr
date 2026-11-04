# Local nesting: factor source values and display dispatch

Investigated: 2026-11-04
Baseline: `78ebbc36200bdc5cef70838dd4e772c5c4a1401c` (`main`)

## Scope and result

This track investigated both nesting Margin verbs at the boundary between
original factor values, character display methods and factor reconstruction.
It confirmed one independently repairable defect: a factor subclass's
character rendering replaced or erased the original factor values in outer
detail keys. Source grouping memberships and retained source cells remained
correct, which made checking only cell contents insufficient to detect it.
No package implementation, permanent regression test, dependency, generated
documentation or prior investigation note changed.

The November 3 [local display investigation](../../2026-11-03-nest-spec-greedy/local/README.md)
and [coordinating record](../../2026-11-03-nest-spec-greedy/README.md), the
November 2 [composition audit](../../2026-11-02-nest-grouping-hunt/local/README.md),
and the October 30/31 local source-equality investigations supplied the
selection frontier. Their historical counts were not counted as executions
on this baseline. ADRs 0012, 0016 and 0018 supplied the factor-value,
restoration and order boundaries. The nesting reference promised original
values in `.keep` copies and one cell for each original source group; the
shared display reference promised factor reconstruction on declared levels.

## L1: a character rendering became a different factor value

The minimized fixture in `factor-display.R` was one local source row. Its
factor stored code 1 on the single declared level `A`. A lawful subclass
implemented `as.character()` as the display `A!`, and compatible same-class
vctrs prototype and cast methods made ordinary unions retain its factor
values. Each nesting Margin verb with a single included `grouping_set(g)` and
the default non-missing Margin label returned a factor outer key whose code
was missing, even though its levels remained `A` and `Total`. Its original
source code and the `.keep = TRUE` cell's code were still 1. `.keep` made that
comparison visible but was not necessary to trigger the wrong outer value.
No omitted dimension, branch union, NA level, actual missing source code,
sorting or output identifier was necessary.

Three renderer variants established the same boundary:

- Appending `!` erased ordinary outer values into missing factor codes.
- Swapping the displays `A` and `B` silently changed each detail key to the
  other already declared factor level, without introducing missing values.
- Returning `Total` for a nonmissing source value turned the detail key into
  the synthetic Margin-label level, although that dimension was present in
  the grouping set.

These were one finding, rather than three tickets, because all three followed
the same factor encoding and reconstruction path. Fixing only a collision
predicate would leave the first two corruptions intact. No arbitrary factor
subclass restoration was requested: a plain output factor carrying the source
values on their declared levels was sufficient. The defect was replacement of
value content by package-owned reconstruction.

## Hypotheses and separating observations

The ranked hypotheses were:

1. The factor encoder dispatched the display method and reconstruction then
   matched the display against original levels. A rendering outside the
   domain would become missing, while a rendering equal to another declared
   level would become that level. A typed-missing label or fixed-key request
   would bypass this particular conversion.
2. Ordinary branch union or slicing discarded the factor subclass and lost
   its values. A direct ordinary dplyr union would then show the same loss.
3. Native grouping could not handle the subclass. Ordinary dplyr nesting
   would then fail or return different source groups too.

The controls supported hypothesis 1. Direct dplyr union retained the original
codes, and native nesting retained both the original outer keys and `.keep`
copies. Plain-factor, typed-missing-label and fixed-key controls retained
their original values. Caller-owned sources remained byte-identical. The
public target kept the correct source cells while its outer keys acquired the
wrong codes, separating the defect from grouping equality and native casting.

`encode_factor_for_margin()` called `as.character(x)` and carried that answer
as its factor encoding. `reconstruct_factor_vector()` matched it against the
declared source levels plus the synthetic label. Those are distinct questions
for a factor subclass with its own rendering. The private nesting identities
preserved original source equality, so they did not protect the reconstructed
outer value. The single-set witness reached the public defect without any
union between grouping sets.

An initial exploratory subclass omitted compatible vctrs cast methods. A
direct union of that fixture also lost its values, so that experiment was
excluded as a native cast limitation. The retained reproductions registered
compatible methods and asserted native union controls before the target.

## Completed finite matrix

`factor-display-matrix.R` completed 6,912 target calls. It crossed three
renderers, ordinary/ordered factor subclasses, absent/present declared NA
levels, empty/populated sources, tibble/plain-frame representations,
dimension/fixed/payload roles, included-only/rollup plans, string/NULL labels,
both keep settings, all three sort choices and both nesting verbs.

The independent oracle enumerated the selected sets and grouped original
source keys with `vctrs::vec_group_loc()`. It compared the resulting ordered
source-row sequences inside cells while treating unspecified outer group
order as irrelevant. It checked cell column order and every retained source
column's values. For the target factor, it compared stored codes rather than
calling the custom display method again. Original native union codes and
serialized source objects were checked separately.

There were 6,336 calls with correct outer factor values and 576 calls with
wrong outer values. Every failure had a populated factor-dimension input and
a non-missing label; there were 192 failures for each renderer. All source
group memberships, cell projections, fixed-key values and source-immutability
checks matched. There were no skips, unclassified errors or incomplete cases
in the final matrix. This was value-content verification, not a promise to
restore arbitrary column attributes or classes, nor an independent complete
Margin-order oracle.

## Duplicate search and repair boundary

Five all-state Issue and PR query pairs were run through `gh`, each with a
100-result limit: `factor display`, `factor encoder`, `"as.character"`,
`factor reconstruction`, and `factor subclass`. `duplicate-queries.txt` and
the paired `issue-search-*.json` / `pr-search-*.json` files retained the
queries and their complete results. No result reached that limit.

Full bodies and comments were read and retained for
[#1088](https://github.com/sayuks/marginplyr/issues/1088),
[#1143](https://github.com/sayuks/marginplyr/issues/1143),
[#427](https://github.com/sayuks/marginplyr/issues/427),
[#421](https://github.com/sayuks/marginplyr/issues/421),
[#1177](https://github.com/sayuks/marginplyr/issues/1177),
[#56](https://github.com/sayuks/marginplyr/issues/56),
[#343](https://github.com/sayuks/marginplyr/issues/343), and
[#702](https://github.com/sayuks/marginplyr/issues/702), plus the bundled
[#495](https://github.com/sayuks/marginplyr/issues/495), together with related
[PR #1118](https://github.com/sayuks/marginplyr/pull/1118),
[PR #1168](https://github.com/sayuks/marginplyr/pull/1168), and
[PR #955](https://github.com/sayuks/marginplyr/pull/955).

#1088 and its repair concerned source equality identities, which remained
correct in this witness. #1143 concerned caller bindings in generated dtplyr
helper lookup; the local witness used the intended source-class method, with
no masked helper. #421 concerned dtplyr unions dropping NA factor levels;
this witness needed neither dtplyr, an NA level nor a union. #427 concerned
omitted-branch scalar overwrites. #1177 concerned an empty non-factor display
returning the wrong type. #702 concerned reserved pronoun lookup. The
attribute and backend-scope decisions in #56/#343 did not permit replacing
source values during factor reconstruction. #495's factor item removed an
unnecessary assignment and lint suppression; it did not concern value encoding.
No duplicate or prerequisite
repair was identified by these searches.

A repair ticket could independently require both nesting verbs to retain
factor source values despite the three display variants, with original
declared levels, ordered-factor semantics, NA-level/missing-code distinction,
synthetic label placement, `.keep`, fixed/payload roles and source immutability
preserved. The same shared factor encoder was reached by expansion and
summary, but this track's completed counts belonged only to the two nesting
verbs. Publication and current-main reassessment were owned by the
coordinator.

## Replay, execution outcomes and limits

Run from the investigated checkout root:

```sh
Rscript investigation/2026-11-04-nest-grouping-bug-hunt/local/factor-display.R
Rscript investigation/2026-11-04-nest-grouping-bug-hunt/local/factor-display.R --assert
Rscript investigation/2026-11-04-nest-grouping-bug-hunt/local/factor-display-matrix.R
```

Development execution used a disposable archive of the baseline at
`/private/tmp/marginplyr-nest-local-20261104/source`. Observation mode returned
terminal exit 0 after asserting the dated defect and controls; it was not a
passing repaired regression test. Desired-behavior mode returned terminal
exit 1 with `A detail factor must retain its original source value.` Both
logs were retained adjacent to the script. The matrix was collected through
its original shell session `56262` to terminal exit 0; its output was retained
in `factor-display-matrix.txt`. The scripts started no subprocesses or
parallel test workers, and no owned execution remained active. The disposable
archive copy was removed after terminal completion; the committed source
revision and replay scripts supplied reconstruction of the experiment.

Logs recorded R 4.6.1 on `aarch64-apple-darwin23`, dplyr 1.2.1, vctrs 0.7.3
and tibble 3.3.1. These finite probes did not establish all lawful S3/S4
methods, remote backend nesting, arbitrary class/attribute restoration, every
other dependency/R/OS version or resource-exhaustion behavior. No full package
suite or Review-ready check ran in this track; publication verification was
owned by the coordinating agent. The user requested an investigation only,
so no fix or post-fix claim was attempted.
