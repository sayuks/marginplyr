# Grouping specification selection and capture boundaries

Investigated: 2026-11-06
Tested package SHA: `547d77095c2923573216d002f50bef21778247f9`
Environment: R 4.6.1, aarch64-apple-darwin23, macOS Tahoe 26.6.2

This track investigated all five Grouping specification constructors and
`inspect_grouping()`. The November 5 [product investigation] and November 6
[family investigation] supplied previous failures, controls and the independent
set-family calculation. ADRs 0005, 0008, 0013 and 0026 supplied admission,
evaluation-budget, inspection and nested-name contracts. No implementation or
permanent regression test was changed, and this track established no new
confirmed defect.

## Completed finite coverage

| Probe | Executed coverage | Outcome |
| --- | --- | --- |
| [Selection oracle](selectors-oracle.R) | 52 selectors, populated/empty input, fixed-key selection and all five constructor kinds under all three duplicate policies | 1,664 exact complete-plan or stable expected-refusal matches |
| [Capture controls](capture-controls.R) | Six constructor-call spellings crossed with all five parent constructors, plus numbered-dot forwarding under zero through six pairs of parentheses | 37 matching plans and one-read controls |
| [Variable suffix controls](variable-suffix-controls.R) | Three public consumers, local/unsafe dtplyr input and four product suffix shapes | 24 observations matched the recorded boundary; zero upstream callbacks |

The selection oracle first applied each captured expression to the real input
with direct `tidyselect::eval_select()`, then independently calculated the
expected set families, duplicate removal, dimension order, Grouping bits and
Grouping identifiers. It called no marginplyr compiler, expansion or mask
helper to calculate expected values. Successful inspection tibbles were compared
with `identical()`. Expected duplicate and empty-dimension refusals required the
Package condition and a stable identifying message fragment. The family/plan
calculation was adapted from the earlier family investigation; the direct typed
selection oracle and varied expressions were added for this investigation.

The 52 selectors exercised literal names, ranges, exclusions, set operations,
name helpers, type predicates, value-dependent local predicates, direct selection
data/name lookups, closures and defaults, variadic forwarding, lexical pronouns,
identity aliases, explicitly qualified parentheses, parenthesized callees, and
compositions of base operations. The adjacent [selector list](selectors.txt)
retained the exact expressions. Value-dependent selections on populated local
input and their empty-input variants were separate observations.

The capture controls used a constructor-shaped caller function
whose second invocation would raise. It returned the ordinary selection `a`
through a bare name, parenthesized callees, qualified whole-call parentheses,
and qualified constructor lookup. All 30 parent/spelling combinations selected
`a` and invoked that function once. The remaining seven cases forwarded an
actual nested specification through numbered dots with increasing redundant
parentheses. No caller binding or source specification was rewritten.

## Variable-size suffix: observation without a new bug claim

The candidate hypothesis was that a duplicate product prefix could require
extra typed selection work after #1342. The separating prediction was: a
fixed-size union suffix should reject without a predicate call, whereas an
unresolved rollup/cube suffix might still need types. This was observed.

Both fixed suffixes rejected the known duplicate before the predicate or dtplyr
metadata acquisition. A typed rollup/cube suffix evaluated the predicate on all
three local columns and then reported duplicates. On unsafe derived dtplyr
input it instead reported the metadata-safety refusal, with zero upstream
callbacks. The CSV retained the complete conditions and counts. These were
observations of the remaining boundary, not 24 assertions of a new defect.

The relevant distinction from the earlier confirmed #1342 witness was that its
union suffix had two grouping-set occurrences independently of selection. The
rollup/cube suffix's occurrence count depended on how many dimensions its
predicate selected. The exact duplicate occurrence positions could not be
established from the unresolved prefix alone, and an all-empty answer would
instead make the suffix invalid. Consequently this investigation did not prove
that these calls violated the locally decidable, position-consistent diagnostic
contract. Treating every duplicate prefix as an early rejection would require
establishing how that diagnostic should handle unknown positions and earlier
suffix validity, rather than silently assuming a new precedence rule.

All-state `gh issue list` and `gh pr list` searches used
`repo:sayuks/marginplyr "product" "suffix"`, with limit 100. The saved
[Issue results](suffix-issues.json) contained three entries and the
[PR results](suffix-prs.json) contained three entries, so neither response
reached the limit. Full bodies/comments for [#1266](issue-1266.json),
[#1342](issue-1342.json), and [PR #1352](pr-1352.json), including its review
record, were retrieved. The #1342 evidence explicitly depended on fixed suffix
cardinality and known occurrence positions. The candidate belonged to that
existing product-preflight family and did not establish a separate independently
specified repair. No duplicate ticket was recommended.

## Replay, execution identity and limits

Run one command at a time from the tested package root. Each script supports
`MARGINPLYR_HUNT_SOURCE` for another source copy and `MARGINPLYR_HUNT_OUTPUT`
for outputs; the default output directory is temporary.

```sh
MARGINPLYR_HUNT_OUTPUT=/path/to/output Rscript investigation/2026-11-06-nest-spec-exhaustive/spec/selectors-oracle.R
MARGINPLYR_HUNT_OUTPUT=/path/to/output Rscript investigation/2026-11-06-nest-spec-exhaustive/spec/capture-controls.R
MARGINPLYR_HUNT_OUTPUT=/path/to/output Rscript investigation/2026-11-06-nest-spec-exhaustive/spec/variable-suffix-controls.R
```

The final selection-oracle run retained terminal session `96110` through exit
0. Capture controls completed directly at exit 0. The final variable-suffix
run retained terminal session `92416` through exit 0. Every execution completed
before another reproduction began. The oracle log retained the R environment;
relevant versions were dplyr 1.2.1, tidyselect 1.2.1, rlang 1.3.0, tibble 3.3.1,
vctrs 0.7.3, dbplyr 2.6.0, dtplyr 1.3.3 and data.table 1.18.6.1.

These finite checks did not cover every selector, arbitrary class/method
behavior, every query graph, hosted databases, unbounded specification depth or
resource use, other library/OS versions, or every interruption boundary.
No complete suite, build, installation or render was run by this track. A new
claim about variable-size suffix admission would need a specified diagnostic
and precedence for unknown occurrence positions and unresolved suffix validity.
No fix or post-fix success was claimed.

[product investigation]: ../../2026-11-05-nest-grouping-adversarial-followup/spec/README.md
[family investigation]: ../../2026-11-06-nest-grouping-greedy/spec/README.md
