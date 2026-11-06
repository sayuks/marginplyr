# Independent deferred-copy confirmation

Investigated: 2026-11-07
Baseline: `e1dabaabb392a3791177e5c48e983f780c7c93ec`

The coordinator exported a separate archive after fetching main again; both
the investigation baseline and fetched revision named the SHA above.
`copy-witness.R` used two different source rows, `east/13` and `west/29`, and
constructed an included-only expansion before installing a selective caller
namespace dispatcher. Ordinary collection, identity mutation and source union
returned the expected payload with zero callbacks. Expansion invoked the
dispatcher once and returned `-99, -99` with unchanged keys. Source serialization
was unchanged, and removing the dispatcher restored the expected expansion.

Observation mode exited zero. `--assert` intentionally exited one on the
desired result and zero-callback assertion, after its successful controls.
The parent execution handle `36793` collected both child R processes and
ended at zero after checking those two expected statuses. Captured logs retained
the actual output. These were confirmations of an unfixed defect rather than
passing package regressions.

Run from an archived package root:

```sh
Rscript /absolute/path/to/copy-witness.R
Rscript /absolute/path/to/copy-witness.R --assert
```

The environment matched the lazy lane: arm64 macOS, R 4.6.1, dplyr 1.2.1,
dtplyr 1.3.3 and data.table 1.18.6.1. Testthat parallel execution was disabled;
no package test or external service ran. The lane's 216-case matrix and this
independent payload-corruption witness were distinct measurements.
