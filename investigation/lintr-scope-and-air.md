# lintr scope and Air replacement

Investigated: 2026-11-04
Baseline SHA: `f8a2a964b50ff4d7d42e3c5ba3ea917a70147203`

## Checks at the baseline

- Review-ready invoked `jarl check .` and then ran
  `pkgload::load_all('.', quiet = TRUE)` followed by
  `lintr::lint_package()`, with `LINTR_ERROR_ON_LINT=true`. The `Lint` step
  in CI used the same R sequence. CI's `etiennebacher/setup-jarl@v0.1.1`
  step pinned Jarl 0.6.0 and used the action's default
  `check --output-format github` against the workspace. The action installs
  and invokes Jarl in the same step.
  [Review-ready source steps](../tools/review-ready-check-lib.R),
  [lint workflow](../.github/workflows/lint.yaml),
  [action inputs](https://github.com/etiennebacher/setup-jarl/blob/v0.1.1/action.yml),
  [action implementation](https://github.com/etiennebacher/setup-jarl/blob/v0.1.1/src/setup-jarl.ts).
- There was no tracked `.lintr` and local configuration discovery returned
  none. The installed lintr 3.4.0 supplied 26 default rules. These covered
  layout and style, such as `indentation_linter()` and
  `object_name_linter()`, as well as checks such as
  `equals_na_linter()`, `vector_logic_linter()` and
  `object_usage_linter()`. The last uses `codetools::checkUsage()` and uses
  the package namespace as the parent of its checking environment when
  available. Loading marginplyr lets that check resolve internal helpers.
  lintr supports authoring R-based linters, and the baseline used the
  upstream defaults.
  [Default linters](https://lintr.r-lib.org/reference/default_linters.html),
  [object usage reference](https://lintr.r-lib.org/reference/object_usage_linter.html),
  [namespace lookup source](https://github.com/r-lib/lintr/blob/v3.4.0/R/object_usage_linter.R),
  [creating linters](https://lintr.r-lib.org/articles/creating_linters.html).
- Jarl's defaults were extended with `DPLYR` and `expect_not` in
  `jarl.toml`. Its package-specific rules resolve the origins of relevant
  calls from source, package metadata and a short R query for available
  package versions and exports. Its `unused_function` rule covers unused
  package functions, and the opt-in `unused_object` rule covers assignments.
  Jarl 0.6.0 has no `object_usage` rule. It linted R chunks in `.qmd` and
  `.Rmd` by default, with limitations on inline R and automatic fixes.
  [Repository rule selection](../jarl.toml),
  [Jarl package-specific rules](https://jarl.etiennebacher.com/howto/package-specific),
  [Jarl rule index](https://jarl.etiennebacher.com/rules),
  [Jarl R Markdown and Quarto support](https://jarl.etiennebacher.com/howto/rmarkdown-quarto).

## What Air can cover

Air 0.13.0 was the latest release when checked through the official GitHub
release API. The CLI implements `format` and `language-server`.
`air format . --check` parses the selected R source, formats the syntax
tree and compares the resulting text with the input. It fails for files that
would change and reports parse or configuration errors. Air's documented
configuration controls formatting, including width, indent, assignment
style, and file or expression exclusions. The CLI has no lint rule interface
or loaded package namespace input. Posit provides `posit-dev/setup-air@v1`
and a CI example that runs the same format check.
[Release API](https://api.github.com/repos/posit-dev/air/releases/tags/0.13.0),
[CLI arguments](https://github.com/posit-dev/air/blob/0.13.0/crates/air/src/args.rs),
[format implementation](https://github.com/posit-dev/air/blob/0.13.0/crates/air/src/commands/format/source.rs),
[configuration](https://posit-dev.github.io/air/configuration.html),
[GitHub Actions](https://posit-dev.github.io/air/integration-github-actions.html).

The CLI's default inclusion selects `.R` and `.r` files, including when a
file is passed directly. `--force` bypasses path filtering and still uses the
R parser on the whole input. For Quarto, the VS Code and Positron integration
passes R code cells to Air through the Quarto extension. Positron also
documents editor formatting in R Markdown. The editor integration does not
give the CLI check coverage of the five Quarto vignettes or `README.qmd`.
Of those six sources, `lint_package()` reaches the five under `vignettes/`
because it searches the standard package directories. Jarl checks R chunks
in the project tree.
[Air default inclusion source](https://github.com/posit-dev/air/blob/0.13.0/crates/workspace/src/settings/default_include_patterns.rs),
[Air 0.9.0 direct path change](https://github.com/posit-dev/air/releases/tag/0.9.0),
[Quarto editor integration](https://posit-dev.github.io/air/editor-vscode.html#quarto),
[Positron editor documentation](https://positron.posit.co/tutorial-migrate-from-rstudio.html#format-your-code-automatically-with-air),
[lintr package scope](https://lintr.r-lib.org/reference/lint.html),
[Jarl R Markdown and Quarto support](https://jarl.etiennebacher.com/howto/rmarkdown-quarto).

## Local feature probes

The Apple Silicon binary from the 0.13.0 release matched the SHA-256 digest
published by GitHub. The binary and fixtures were run in a disposable
directory. Air used `air format --no-configuration --check <file>`, Jarl
used `jarl check <file>` with defaults, and lintr 3.4.0 used `lint()`
with configuration discovery disabled. The object-usage probe selected
`object_usage_linter()` explicitly. The test of an undefined function used
a standalone file, and the package namespace behavior above was established
from lintr's implementation and marginplyr's loading sequence.

| Fixture | Air 0.13.0 | Other check |
| --- | --- | --- |
| Formatted function calling `missing_helper()` | Passed | lintr reported no visible global function definition. Jarl 0.6.0 passed. |
| Formatted function returning `x == NA` | Passed | Jarl flagged `equals_na`. |
| Formatted `camelCase` function returning a single-quoted string | Passed | lintr flagged `object_name_linter` and `quotes_linter`. |
| `g<-function(){1}` in an `.R` file | Exit 1, would reformat | |
| The same line in an R code fence in `.qmd` and `.Rmd` | Exit 0 for each, skipped | |

For example, the undefined function fixture was

```r
f <- function() {
  missing_helper()
}
```

Air can own a formatting policy for R source. It overlaps lintr's layout
rules, with a different style contract. Replacing package-aware lintr with
Air, even alongside Jarl, would lose the demonstrated undefined-function
check and other lintr diagnostics. A format adoption can be evaluated
separately from any decision about the required lint coverage.

## Package scope and local timings

The local runner archived the committed tree, including `investigation/`,
before starting its source checks. lintr itself searched only the seven
package directories named by `lint_package()`. The root `investigation/`
directory was never passed to the file walker. Its exclusion from a package
tarball through `.Rbuildignore` was a separate boundary. The local archive
and Jarl's repository-root scan were separate operations.
[Runner source archive](../tools/review-ready-check-lib.R),
[package build exclusions](../.Rbuildignore),
[lintr package scope](https://lintr.r-lib.org/reference/lint.html).

An exit trace at lintr 3.4.0's file-selection boundary
(`drop_excluded()`) captured the absolute file list before linting. Two
calls to `lint_package()` produced identical vectors in identical order.
One used the default exclusions and the other added `"investigation"` while
retaining `"R/RcppExports.R"`.

| Directory | Files |
| --- | ---: |
| `R/` | 29 R |
| `tests/` | 254 R |
| `inst/` | 1 R |
| `data-raw/` | 1 R |
| `vignettes/` | 5 Quarto |
| `demo/`, `exec/`, `investigation/` | 0 |
| Total | 290 |

For each row below, a fresh R 4.6.1 process on the Apple Silicon Mac
loaded the same baseline SHA archived to a disposable directory, using
pkgload 1.5.3 and lintr 3.4.0. `system.time()` measured package loading,
lintr namespace initialization and the lint call separately. Both lint
calls returned zero findings under `LINTR_ERROR_ON_LINT=true`. These were
one observation per condition on a shared machine. The identical file
vectors establish unchanged lint coverage independently of timing noise.

| lintr call | Package load (s) | lintr init (s) | Lint elapsed (s) | Lint CPU (s) |
| --- | ---: | ---: | ---: | ---: |
| `lint_package()` | 0.899 | 0.017 | 61.230 | 53.180 |
| `lint_package(exclusions = list("R/RcppExports.R", "investigation"))` | 0.846 | 0.017 | 73.766 | 64.524 |

An elapsed-time `Rprof()` run at a 10 ms sampling interval used the default
call and measured 63.838 s of lint elapsed time. Across 5,340 samples,
source-expression construction appeared in 24.59%, XML node searching
(`xml_find_all_`) in 24.55%, and `codetools::checkUsage()` in 13.78%.
These inclusive stack shares can overlap. The installed implementation
selected and checked files serially through `lint_dir()`, whose file
loop used `lapply()`. The profile located the cost inside the per-file
work after package loading.

For reproduction, extract the baseline with `git archive`, switch to
its source root, and run the following in fresh R processes, replacing
the lint call with the exclusion form for the comparison.

```r
system.time(pkgload::load_all(".", quiet = TRUE))
system.time(loadNamespace("lintr"))
system.time(result <- lintr::lint_package())
stopifnot(length(result) == 0L)
```

## Cache correctness probe

The measured commands used lintr's default `cache = FALSE`. The local
runner also assigned a new source path and private cache directory for
each invocation. In the installed lintr 3.4.0, the cache file lookup used
the source path, and the result key included the checked file's contents
and the linters. Neither lookup tracked the rest of the loaded package
namespace. A disposable two-file package made the consequence observable.
In separate R processes, each starting with `pkgload::load_all()`:

1. `lint_package(cache = <persistent directory>)` passed when
   `R/caller.R` called `fixture_helper()` defined in `R/helper.R`.
2. Renaming the definition in `R/helper.R` left the caller unchanged.
   Repeating the cached call still returned zero findings.
3. `lint_package(cache = FALSE)` reported
   `[object_usage_linter] no visible global function definition` in
   `R/caller.R`.

Changed-file-only lint would also miss the unchanged caller in this fixture.
A cache or narrower check would need to account for cross-file dependencies,
the effective lint rules and the loaded environment.
[lintr cache argument](https://lintr.r-lib.org/reference/lint.html),
[object usage reference](https://lintr.r-lib.org/reference/object_usage_linter.html).

## Two-worker prototype and Jarl timings

A disposable R prototype loaded marginplyr in the parent, then sent the
same 290 absolute paths to `parallel::mclapply(files, lintr::lint,
mc.cores = 2L)` with the default linters and cache disabled. It required
a `lints` result from every file. Package load took 0.920 s and lint took
37.125 s elapsed, against 61.230 s in the serial baseline, about 39% less
time in this one comparison. Both returned zero findings. On the renamed
helper fixture above, serial package lint and the two-worker approach
returned the same lint, including its location and diagnostic fields.
This was a Mac prototype. It used per-file settings discovery and the
observed inventory, so adoption would need to preserve package-level
configuration, discovery, exclusion and diagnostics, and verify worker
failure propagation and cancellation. The Review-ready baseline runs
before the heavy slot, so measurements with concurrent invocations would
be needed before choosing a worker count.
[Review-ready baseline and heavy admission](../design/agents/local-checks.md),
[R parallel worker behavior](https://stat.ethz.ch/R-manual/R-devel/library/parallel/html/mclapply.html).

On the same archive, Jarl 0.6.0 passed `jarl check . --with-timing` in
1.039 s elapsed and `jarl check . --with-timing --exclude=investigation/`
in 0.725 s. Each was measured once, with the exclusion run second. Jarl
walks the repository root and supports an explicit exclusion, which
changes the scope of that check. The measured difference was small
relative to lintr's elapsed time.
[Jarl file selection](https://jarl.etiennebacher.com/reference/cli).

The local evidence supports evaluating bounded lintr parallelism before
changing rule or file coverage. Air can provide a separate formatting
check if a formatting policy is desired.
