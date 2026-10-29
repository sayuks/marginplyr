# Grouping specification grammar, capture and identity evidence

Investigated: 2026-10-29
Baseline: `4c63bb917ed7ed31ff6aadf060a5eae7a32cd004` (`main`)

This track established no new independently actionable defect. It inspected the
five public constructors, the structural preflight, retained selection values,
the kind registry, nested-name ambiguity, cycle detection, plan normalization,
inspection formatting and Grouping identifier computation. It compared the
contracts in `CONTEXT.md` and ADRs 0007, 0008, 0013, 0019 and 0026 with the
implementation and earlier investigation evidence.

Recent nesting, expansion and summary hunts were read for deduplication.
Closed issues #825, #828, #873, #889, #897, #924, #946, #975 and #995 already
covered the known typed-selection, retained-value, injection and cyclic-reference
boundaries; this track did not relabel those repairs as new findings. The
function-shaped specification that also succeeds as a predicate was considered
separately: ADR 0008's amendment for a specification stored as a function
explicitly recorded that unreached shape, and ordinary public constructors
returned lists. It was not a new defect established here.

## Reproduction and measured coverage

Run each script from the repository root, or pass the repository path as its
first argument. The scripts loaded that working tree with `pkgload::load_all()`
and required the installed development dependencies plus dtplyr.

```sh
Rscript investigation/2026-10-29-nesting-grouping-deep-hunt/spec/algebra-special-names.R
Rscript investigation/2026-10-29-nesting-grouping-deep-hunt/spec/evaluation-boundaries.R
Rscript investigation/2026-10-29-nesting-grouping-deep-hunt/spec/wide-identity.R
```

The adjacent `.log` files recorded terminal success for each script. The
algebra log also recorded `sessionInfo()`: R 4.6.1 on macOS arm64, marginplyr
0.1.0, dplyr 1.2.1, dtplyr 1.3.3, data.table 1.18.6.1, tidyselect 1.2.1,
rlang 1.3.0 and vctrs 0.7.3.

| Experiment | Completed comparisons and observed result |
| --- | --- |
| Special-name algebra | Seed `20261029`; 600 generated specification ASTs, local and immutable dtplyr inputs, three duplicate policies, 3,600 inspections. An independent ordered family oracle matched exact included/omitted columns, fixed keys, bit-vector names and values, identifiers, occurrence order and stable deduplication. Default duplicate refusal agreed with the oracle. |
| Evaluation boundaries | 540 observed public-consumer comparisons across all five constructors, local/dtplyr input, inspection and both nesting interfaces, 0/1/4 parenthesis pairs, and name-only/typed nested children. Recognized nested names, ordinary non-column selections from alternating active bindings, and constructor-shaped ordinary functions each ran once. Constructor arguments were forced once. |
| Ambiguity | 60 comparisons across five constructors, two backends, three consumers and 0/3 parenthesis pairs. A colliding name bound to a composite was read once and refused where the parent admitted it; `grouping_set()` took the column and read the binding zero times. |
| Promise reuse | Six consumer calls to one captured specification containing the same wrapper promise twice. The first call forced it once; the five later calls forced it zero times. Captured caller meaning remained correct. |
| Width and identity | 80 checks across eight widths (0, 1, 30, 31, 32, 33, 64 and 65) and two backends. Inspection retained every bit and occurrence. Numeric IDs remained exact through 31 dimensions and became `NA_integer_` above that limit. Explicit helper selections of at most 31 dimensions agreed with independent masks, implicit helpers above 31 refused, fixed-column bits were zero, and both nesting interfaces retained one source row per fixed partition and deduplicated grouping set. |

The algebra generator used set union, hierarchical prefixes, subsets and
Cartesian products over character-vector families. It did not call the package
compiler to create its expected values. It included overlapping composite
dimensions, empty sets, empty products, duplicate occurrences, numeric position
selections, explicit name selections, bare selected names and redundant
parentheses. Names included `.data`, `.env`, commas, equals signs, backticks,
Japanese characters and an accented character. The separate width experiment
used `.env` and `.data` as actual dimensions and exercised their helper names
without asking the caller environment for their values.

## Harness corrections and limits

The first algebra fixture wrote `.data` and `.env` as bare symbols. Direct
`tidyselect::eval_select()` rejected those same spellings as pronoun objects,
so the generator used explicit name or position selection for them. This was
an upstream selection rule, not a package finding. The empty-bit oracle was
normalized to an unadorned `integer()` before the completed run. An extra closing
parenthesis in the initial width script was corrected before its successful
run; no package source was changed for any correction.

This bounded search did not establish correctness at arbitrary expression
depth, exponentially large cubes/products, arbitrary custom S3/S4 field
methods, every remote backend or every dependency version. The width nesting
checks asserted cell cardinality and outer partition cardinality; they did not
claim an additional general source-membership oracle for heterogeneous keys.
The investigations from earlier dates retained their own evidence and were not
rewritten. No package implementation or permanent test was edited by this track.
