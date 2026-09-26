# Preserve ordinary across argument evaluation for dynamic unpack values

## What to build

**Priority:** P2

Keep ordinary local across argument evaluation consistent with ordinary dplyr when the unpack argument is an expression or binding. Preserve its evaluation phase and group scope as well as its invocation count.

A pure function factory capturing `dplyr::n()` exposes two failures. With TRUE unpacking and groups containing one and two rows, ordinary dplyr and literal-TRUE Margin calls return 2 and 32; dynamic TRUE returns 2 and 31 because the first group's function is reused. With FALSE unpacking and one input value 1, the ordinary and literal controls return 1; dynamic FALSE returns 2 because the factory runs in a different data-mask phase. String unpacking shares the TRUE defect.

The TRUE case passed before #699 and regressed in its repair. The FALSE phase defect already existed; #693 fixed the factory invocation count without covering the context it observes. Both manifestations belong to the same wrapper boundary. ADR 0019 leaves ordinary functions and n() to their ordinary evaluation; ADR 0028 still requires actual-value frame-name checks.

Evidence: finding 1 and the `unpack_true` / `unpack_false` probes in the companion investigation. The corrected 360-case matrix includes 168 mismatches and 192 matching controls across local frame classes, unpack spellings, group shapes, result shapes, and unrelated shares.

## Acceptance criteria

- [ ] Compare dynamic FALSE, TRUE, and string unpacking with ordinary dplyr and equivalent literal Margin controls. Assert actual values, output names/shapes, callback counts, and the group context observed by ordinary function arguments.
- [ ] On the investigated dependency versions, the pure TRUE reproduction returns 2 and 32; the one-row FALSE reproduction returns 1. Assertions should use the corresponding ordinary dplyr call as the behavioral oracle where evaluation details can vary by supported dependency version.
- [ ] Cover expression, ordinary-bound, delayed, and active-binding arguments, scalar-returning and frame-returning factories, one function and function lists, unequal detail-group sizes, and multiple Grouping-set occurrences.
- [ ] Cover tibble, data.frame, and eager data.table inputs, with and without an unrelated Total share. The cache must neither move an ordinary expression into a different evaluation context nor share a value across groups when dplyr would reevaluate it.
- [ ] Retain dynamic-frame collision refusals for public grouping keys, allocated internal keys, requested identifiers, and previously defined share sources. Existing #683/#685/#693 regressions continue to pass.
- [ ] Keep caller-named packed summaries and invalid-unpack diagnostics consistent with the supported ordinary path. Introduce no lazy-input read.
- [ ] Add focused public-API regression tests and pass the repository's Review-ready check before review.

## Blocked by

None (can start immediately).
