# Local Parent-share benchmark

This benchmark records the effect of issue #27 on representative local
rollup execution. It is developer evidence, not a package test, and has no
wall-clock assertion.

## Workload

The script in `dev/benchmark-parent-share-local.R` uses 100,000 rows, four
integer dimensions with cardinalities 50, 20, 10, and 5, a five-Grouping-set
rollup, and one double summary with one Parent share. Each measurement follows
one warm-up execution.

## Result

Measurements were taken on 2026-08-02 in the same development environment.

| Implementation | Revision | Elapsed seconds | Median |
|---|---|---|---:|
| Before | `ca27c14` | 0.516, 0.342, 0.318 | 0.342 |
| After | Issue #27 working tree | 0.335, 0.189, 0.194 | 0.194 |

The observed median was 43% lower after scalar validation was integrated into
summary execution and the Grouping-set-proportional full-input rescans were
removed. The result is descriptive only; timings vary by machine and session.

## Running it

Load the package from the checkout, then source the script:

```r
devtools::load_all()
source("dev/benchmark-parent-share-local.R")
```
