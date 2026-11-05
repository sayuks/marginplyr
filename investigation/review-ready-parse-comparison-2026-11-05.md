# Review-ready parsed-expression comparison cost

Investigated: 2026-11-05

## Inputs and environment

The measurement used source SHA
`66529b4a7f0b2e5b0cf8778ddbecc10e51654ffb`, R 4.6.1 (2026-06-24),
and Darwin 25.6.0 arm64. `R/` contained 30 R files (939,707 bytes),
and `tests/` contained 263 (2,923,983 bytes): 293 files and 3,863,690 bytes
in total. Recursive discovery selected filenames ending in `.R` or `.r`.

Each source was paired with a separate file containing the same lines preceded
by one ordinary explanatory comment. Every comparison parsed both sides anew
with `keep.source = FALSE` and required `identical()` to return true. No package
was loaded and no package tests, build or Review-ready invocation were run.

## Measured elapsed time

Values below are seconds, expressed as median [minimum, maximum]. Each row
used ten samples. Single-file samples averaged twenty comparisons per sample.

| Source selection | Parse and compare | Read, parse and compare |
| --- | ---: | ---: |
| Normal R file, 14,237 bytes | 0.000650 [0.000550, 0.000750] | 0.000650 [0.000600, 0.000700] |
| Largest test file, 168,782 bytes | 0.009225 [0.008250, 0.009450] | 0.009825 [0.008850, 0.011000] |
| Largest ten files, 1,155,571 bytes | 0.0720 [0.0680, 0.0880] | 0.0775 [0.0740, 0.0820] |
| All 293 files | 0.2760 [0.2670, 0.2880] | 0.3075 [0.2880, 0.4480] |

The normal file was `R/contextual-helpers.R`; the largest test was
`tests/testthat/test-static-expression-analysis.R`.

Three additional samples included starting and waiting for a new
`Rscript --vanilla` process. The single-file median was 0.099 seconds
[0.093, 0.105]; the all-file median was 0.413 seconds [0.400, 0.446].
These were fresh processes, not cold-disk measurements: OS caches were not
cleared. The measurement did not bound performance on other machines.

Ten single-file samples also included `git show <SHA>:<path>` acquisition of
the baseline, reading the comment-edited file, parsing both and comparing.
Their median was 0.013 seconds [0.012, 0.015], excluding a new R process.

## Reproduction and retained output

The measurement scripts and raw results were saved under
`/private/tmp/marginplyr-parse-timing-87c628e581d1/`:

```sh
Rscript --vanilla /private/tmp/marginplyr-parse-timing-87c628e581d1/benchmark.R
Rscript --vanilla /private/tmp/marginplyr-parse-timing-87c628e581d1/git-acquisition.R
```

The directory also held `results.csv`, `git-acquisition.csv`, `inventory.csv`,
`environment.txt` and `manifest.rds`. These temporary artifacts were not
committed; rerunning those exact scripts depended on their retention and the
unchanged source files in `/private/tmp/marginplyr-1292`.

The original benchmark execution session `70915` was retained until terminal
exit code 0; the separate Git-acquisition measurement also returned exit code 0.
