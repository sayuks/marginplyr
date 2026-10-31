# Grouping specification selection and expansion correspondence

Investigated: 2026-10-31
Baseline: `339315c387f31deace77566f5d3294b15b323865` (`main`)

This track investigated Grouping specification construction, selection capture,
preflight, nested-name ambiguity, cyclic references, plan normalization and
public inspection. It read the recent October 29 and October 30 expansion,
nesting and summary investigations to distinguish known findings and their
accepted evaluation budgets. ADRs 0007, 0008, 0009, 0013, 0015, 0019 and 0026
supplied the capture, kind, identity, inspection, condition and nesting rules.
No implementation or package regression test was changed. No new independently
actionable Grouping specification defect was established.

## Independent plan and expansion oracle

`plan-expansion-correspondence.R` independently represented each grouping-set
family as a list of character vectors. It enumerated every pair of the sixteen
subsets of four dimensions: 256 families. The dimensions included `.data`, a
name containing comma and equals separators, and Japanese characters. Three
duplicate policies, empty/two-row input, and explicit `.by` versus pre-grouped
input produced 3,072 inspection/expansion call pairs.

Of these, 3,008 pairs matched exact set membership, fixed keys, occurrence
identifiers, named Grouping bits, Grouping identifiers, expanded row counts,
source-row membership, all source dimension/payload values, and typed-missing
omitted dimensions. The remaining 64 pairs required duplicate-policy refusals
and received Package conditions from both APIs. The oracle ordered the result
by explicit occurrence and source-row identifiers for comparison; it did not
claim that `.sort = "none"` promised physical result order.

An initial fixture included an input column named `..1`. Ordinary
`dplyr::select()` refused that input with
`vctrs_error_names_cannot_be_dot_dot`, before the tested Grouping specification
semantics were reached. It was excluded as an unsupported input-name control
and replaced with an ordinary non-syntactic name before the completed run.
This did not establish a new package finding.

## Typed selection differential

`selection-differential.R` used seed `2026103101` to generate 1,530 selection
expressions from thirty leaves and selection-operator combinations. It used
two frames, one with populated character/integer/logical/Date columns and one
with empty integer/character/factor/list columns. Direct typed
`tidyselect::eval_select()` supplied the selection or External-condition-class
oracle for `.by` and all five constructor positions. One direct constructor
spelling was excluded from the selection comparison because the nested
position deliberately reads it as a specification. The remaining 1,529
expressions produced 18,348 executed frame/expression/position comparisons.

There were 778 raw differences. Every one contained double-bang or triple-bang
syntax whose evaluation phases differed: direct `eval_select()` received the
unprocessed AST while public capture performed injection. All 99 expressions
containing that syntax, including their matching observations, were therefore
excluded as incomparable: 1,188 comparisons. The remaining 17,160 comparisons
comprised 16,596 matches, 554 required empty-dimension refusals and ten required
nested-specification spelling refusals. No difference remained unexplained.
The latter refusals came from a caller function returning a specification
inside a position whose spelling did not admit it, as the constructor
reference specifies; `.by` propagated the ordinary selection diagnostic.

Leaves and compositions included column names, numeric and empty selections,
name helpers, typed predicates, value-dependent predicates, helper arguments
reading `peek_data()`, plain and computed/qualified heads, closures, ordinary
pronoun subscripts, formulas, refused arithmetic/logical operators and
external-vector bindings. The comparison checked selected first-set membership
or condition class, with explicit documented refusals. It did not establish
full diagnostic-message identity, warning behavior or injection correctness.

The initial output recorder expanded multi-line deparses into several CSV
rows. Its display table therefore counted display rows rather than distinct
comparisons. The retained raw run log records that table; the final CSV and
`selection-classification.log` normalize by frame, expression and position,
joining the source lines after asserting every other recorded field agrees.
The reporter was corrected to record each deparse as one string and to
classify the injection cases explicitly. The completed normalization replay
used `--classify-existing`; it did not rerun public calls or add them to the
executed count. Both original execution and normalization ended with exit zero.

## Replay and limits

Run from the repository root:

```sh
Rscript investigation/2026-10-31-expand-grouping-greedy/spec/plan-expansion-correspondence.R
Rscript investigation/2026-10-31-expand-grouping-greedy/spec/selection-differential.R
```

The adjacent log recorded exit zero and the R/dependency environment. The
measurements were finite observations over the stated cases. They did not
prove arbitrary caller code, arbitrary custom field methods, unbounded
specification trees, exponentially large plans, other dependency versions,
other operating systems or remote backend behavior. Earlier investigations
were read as context and were not counted as new execution.
