# Review-ready roxygen generation cost

Investigated: 2026-11-05

## Inputs and method

The measurement used source SHA
`66529b4a7f0b2e5b0cf8778ddbecc10e51654ffb`, R 4.6.1, roxygen2 8.1.1
and macOS arm64. Each of three sequential samples extracted a fresh source
copy from a Git archive and started a new `Rscript --vanilla` process to run
`roxygen2::roxygenise()`. OS caches were not cleared.

The parent timed the complete child process; the child separately timed
`roxygenise()`. A before/after inventory compared the names and content hashes
of all source files, including additions and deletions. Inventory comparison
was outside the timed child process.

## Results

| Sample | R startup and generation, seconds | Generation alone, seconds | Changed files |
| --- | ---: | ---: | ---: |
| 1 | 2.100 | 1.899 | 0 |
| 2 | 1.440 | 1.272 | 0 |
| 3 | 2.145 | 1.932 | 0 |

The process median was 2.100 seconds, with range 1.440 to 2.145 seconds.
Every child returned exit code 0 without warnings. `man/` plus `NAMESPACE`
contained twelve files totaling 261,662 bytes. Their contents and DESCRIPTION
were unchanged, as was the full source inventory.

This measured generation, not Rd validation, example execution or a package
build/check. It did not establish timing bounds on other hosts or revisions.

## Retained evidence

Temporary scripts and results were saved under
`/private/tmp/marginplyr-roxygen-timing-a1702bfc2753/`: `benchmark.R`,
`run-roxygen.R`, `summary.csv`, `results.rds`, the three run logs and
`session-info.txt`. Running `Rscript --vanilla <directory>/benchmark.R`
recreated the measurement in a new temporary directory while the source
repository and pinned SHA remained available. These temporary artifacts
were not committed.

The original execution session `47579` was retained until terminal exit code
0; its three sequential child R processes also completed with exit code 0.
