# Grouping specification capture and algebra investigation

Investigated: 2026-11-02
Baseline: `978d7bef4247bf1348af5b90bfab47924db25500` (`origin/main`)

This track established no new independently actionable defect. It investigated
public capture with injection, the five Grouping specification kinds and their
composition, duplicate policies, fixed columns, exact list-format inspection,
Grouping set identifiers, Grouping bits, and Grouping identifiers. The October
30 and October 31 expansion investigations supplied context and prior findings;
their executions were not counted as new observations. ADRs 0007, 0008, 0009,
0013 and 0026 supplied the capture, grammar, identity and inspection contracts.
No package implementation or regression test was changed.

## Hypotheses and observations

| Hypothesis | Distinguishing observation | Result |
| --- | --- | --- |
| Injected selections lose an inner captured environment, or resolve at a different capture phase | Compare ordinary public `enquo()` followed by typed tidyselect with the same written expression in `.by` and every constructor | 4,488 comparisons: 4,284 matches and 204 required empty rollup/cube refusals; no unexplained difference |
| Nested unions, products, overlapping dimensions or composite dimensions disagree with the family algebra | Independently construct each family and compare exact sets, omissions, named bits, bit masks and occurrence IDs | 3,000 comparisons: 2,758 matches and 242 required duplicate refusals |
| A quosure in a constructor call head repeats the caller's selection function | Count invocations and assert the first of two alternating answers | Three injected-head forms retained `a` after exactly one invocation |
| The 31-dimension mask boundary loses fixed-column exclusion or integer precision | Compare inspection with a bare contextual `grouping_id()` and a mathematical mask sequence | Dimension counts 0, 1, 30 and 31 agreed; 32 and 40 retained complete inspection bits with missing IDs and refused the contextual helper as documented |

The injection differential used seed `2026110201`. Its 374 expressions combined
fourteen injection leaves with selection unions, intersections, differences,
ranges, complements and negation. Leaves included literal vectors, injected
symbols, injected quosures, a typed predicate, missing/empty selections, nested
splicing, and an inner environment whose external vector disagreed with the
outer environment. It tested a populated frame and an empty frame through six
positions. The oracle and package both captured the original written AST at a
public function boundary; an unprocessed tidyselect AST was not substituted for
that boundary. The comparison checked selected columns or External-condition
class, with the documented empty-dimension refusal distinguished explicitly.

The first generator combined a unary `!` directly with an injected `!!` AST,
forming top-level `!!!`. That has different capture semantics for the oracle's
single formal and a constructor's dots. Its 220 apparent differences were
therefore oracle mismatches rather than package findings. The generator was
corrected to preserve parentheses around unary operands and all 4,488
comparisons were rerun. `injection-initial-oracle.log` retained the initial
classification; `injection-differential.log` and `injection-results.csv` held
the completed corrected run.

The family oracle used seed `2026110202` to generate 500 valid trees of depth
at most three, bounded to 64 family occurrences. It included the empty
`grouping_set()` and identity `grouping_spec()` explicitly. It represented
union and Cartesian product independently, generated cube subsets from bit
patterns sorted by cardinality and subset positions, and normalized membership
in first-encountered dimension order. Input names included `.data`, Japanese
characters, and comma/equals separators. Every tree ran with three duplicate
policies and with/without a fixed `.by` column. The oracle compared exact
list-format vectors rather than parsing the display-only text format.

The first family oracle attached an empty `names` attribute to zero-length
Grouping bits, whereas the documented answer was bare `integer()`. All 614
initial mismatches belonged to zero-dimension families. The oracle was corrected
to the public contract and all 3,000 comparisons were rerun.
`family-initial-oracle.log` retained the initial classification; the completed
corrected run was in `family-algebra.log` and `family-results.csv`.

The capture-head probe used an injected bare name, an injected parenthesized
name, and an injected qualified call. Each caller function returned `a` first
and `b` subsequently, and the qualified case used the captured environment's
namespace operator. All three assertions required the first answer and exactly
one call. The identifier probe placed a fixed key beside each dimension count,
so the 31-column case also tested its exclusion from the default mask budget.
An initial zero-column fixture used `paste0()` on `integer()` and accidentally
created one name for zero vectors; correcting that fixture preceded the
completed boundary run.

## Replay and limits

Run from the repository root:

```sh
Rscript investigation/2026-11-02-expand-grouping-greedy/spec/injection-differential.R
Rscript investigation/2026-11-02-expand-grouping-greedy/spec/family-algebra.R
Rscript investigation/2026-11-02-expand-grouping-greedy/spec/capture-probe.R
```

All final commands returned exit zero. `TESTTHAT_PARALLEL=false` was explicit;
no full test suite, package build, install, or site render was run by this
track. The adjacent environment record pinned R and dependency versions.
The issue query `gh issue list --state all --limit 100 --search
'Grouping specification' --json number,title,state` was read as prior-report
context; no new candidate required a publication or duplicate disposition.
The tested main status was the pinned baseline, rather than a claim about
later main.

The observations did not enumerate arbitrary caller code, custom specification
field methods, active binding combinations, unbounded capture depth or family
size, all diagnostic messages or warning behavior, all backend types, other
operating systems, or dependency versions. The family oracle checked inspection,
not physical expansion row values or execution order. Successful comparisons
established only the stated finite fixtures and contracts.
