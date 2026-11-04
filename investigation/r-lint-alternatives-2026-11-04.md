# Alternatives to lintr, Jarl, and Air

Investigated: 2026-11-04

## Scope and conclusion

This was a primary-source survey with release-binary feature and adoption
probes, not a performance benchmark or adoption decision. No repository configuration
changed. Arity and Raven were extracted and executed only under `/private/tmp/`.
The accompanying investigation used R 4.6.1, lintr 3.4.0, Jarl 0.6.0, and Air
0.13.0 as its local reference versions. Upstream documentation and source below
were read on the investigation date; development-source capabilities were not
assumed to exist in every published binary.

The strongest additional candidates found were **Arity**, for consolidating
formatting and linting, and **Raven**, for package-aware name resolution. Neither
had evidence here establishing a drop-in replacement for marginplyr's checks.
flir's author had shifted lint development to Jarl. styler and formatR answered
formatting needs but could not supply semantic lint coverage. The tool called
`rflint` that could be verified was for Robot Framework, not R.

## Comparison

| Candidate | Verified capability and coverage | CI and adoption implications |
|---|---|---|
| **flir** | R API over ast-grep, with custom pattern matching and rewriting. The author explicitly limited built-in lint maintenance and stopped adding rules in favor of Jarl. Directory discovery matched `.R` only; selecting a vignette directory therefore did not establish Rmd/Qmd chunk coverage. | Useful for custom refactors rather than replacing maintained standard lint rules. `lint()` returned findings; CI must explicitly turn findings into failure. Package directory discovery did not establish loaded-namespace analysis. [Project](https://flir.etiennebacher.com/), [discovery source](https://github.com/etiennebacher/flir/blob/main/R/utils.R), [lint source](https://github.com/etiennebacher/flir/blob/main/R/lint.R). |
| **styler** | R formatter supporting R, Rmd, Rmarkdown, Qmd, Rnw, and roxygen examples. Style transformations were customizable. The site reported 1.11.0; its news included curly-curly comment fixes and webr chunk support. | `dry = "fail"` provided a non-writing gate. Useful when custom formatting or document coverage matters; no replacement for object-usage checking. Token-level transformations require more care than whitespace-only formatting. [API](https://styler.r-lib.org/reference/style_file.html), [news](https://github.com/r-lib/styler/blob/main/NEWS.md). |
| **formatR** | R-source formatting through `tidy_source()` and directory formatting through `tidy_dir()`. The inspected development page reported 1.14.1 and described comment-preservation limitations. | A small R-native formatter, with no non-base Imports in the inspected DESCRIPTION. Whole-document Rmd/Qmd support and a built-in fail-on-diff gate were not established by the inspected API; a chunk extraction/comparison wrapper would add work. No semantic lint coverage. [Author's documentation](https://yihui.org/formatr/), [DESCRIPTION](https://github.com/yihui/formatR/blob/master/DESCRIPTION). |
| **codetools / R package checks** | `checkUsagePackage()` checked closures in a loaded package's internal namespace; it was not a general source-tree formatter. R's tools also checked documentation, replacements, and S3 methods against package/import environments. | Preserves useful R-aware checks with existing R infrastructure. `checkUsage()` used a report callback defaulting to `cat`, so a standalone failure gate needs deliberate handling. These APIs did not establish equivalent scanning of scripts, tests, or document chunks, nor replacement of style rules. [codetools API](https://stat.ethz.ch/R-manual/R-devel/library/codetools/html/checkUsage.html), [tools QC API](https://stat.ethz.ch/R-manual/R-devel/library/tools/html/QC.html). |
| **Arity** | Rust formatter, linter, and language server; `arity.toml` and package-symbol indexing. Release v0.29.0 was published 2026-10-30. Inspected CLI discovery recognized R files and package DESCRIPTION, not Rmd/Qmd. | One executable can simplify formatting/linting. `arity lint` failed on findings, and `arity format --check` failed on formatting differences. Namespace-aware rules still depend on symbol metadata. Native Rmd/Qmd CLI coverage would be lost unless another tool or extraction layer supplies it. [README](https://github.com/jolars/arity), [release](https://github.com/jolars/arity/releases/tag/v0.29.0), [CLI](https://github.com/jolars/arity/blob/main/docs/src/reference/cli.md), [discovery](https://github.com/jolars/arity/blob/0badabcaa16a7761a2b995ac983498397af263c7/src/file_discovery.rs). |
| **Raven** | Static analyzer and language server with R/Rmd/Qmd checking, cross-file scope, package R-directory visibility, NAMESPACE/roxygen imports, and testthat helpers. GitHub's latest release endpoint reported v0.21.1, published 2026-09-17; inspected development docs described further behavior. | Potential name-resolution replacement candidate, not established equivalence to `object_usage_linter`. `raven check` failed above a configurable severity threshold; style findings at information level passed its default threshold. A metadata database adds maintenance when exact dependency versions matter. [Package mode](https://github.com/jbearak/raven/blob/075b6003c312f4534d92d4aa85fb6da0c9e1bcfe/docs/r-package-dev.md), [CI](https://github.com/jbearak/raven/blob/075b6003c312f4534d92d4aa85fb6da0c9e1bcfe/docs/ci.md), [CLI](https://github.com/jbearak/raven/blob/075b6003c312f4534d92d4aa85fb6da0c9e1bcfe/docs/cli.md), [release](https://github.com/jbearak/raven/releases/tag/v0.21.1). |

`rflint` was excluded: its owning repository explicitly installed the
`robotframework-lint` Python distribution and an executable named `rflint` to
check Robot Framework files. Searches for an R-specific project of that name
did not establish one. This finding did not establish that no obscure project
with that name existed. [Owning repository](https://github.com/boakley/robotframework-lint).

## Speed evidence and its limits

Jarl's author reported one 20-rule dplyr comparison: Jarl 0.131 seconds,
flir 4.5 seconds, lintr 18.5 seconds, or 9 seconds with caching. That was an
upstream result for its corpus and selected rules, not a marginplyr measurement
or proof of semantic equivalence. It supplied no speed reason to migrate from
Jarl to flir. [Jarl's comparison](https://jarl.etiennebacher.com/).

Arity published a reproducible harness and data dated 2026-09-16 on Linux/x86_64,
AMD Ryzen 9 7900, with Arity 0.23.0, Air 0.11.0, Jarl 0.5.0, lintr 3.3.0.1, and
styler 1.11.0. Its tidyr project formatter result was 23.72 ms for Arity versus
36.14 ms for Air. On `tidyr/pivot-wide.R`, its linter result was 30.87 ms for
Arity, 10.62 ms for Jarl, and 902.92 ms for lintr. The harness used ordinary
tool invocations, not matched semantic rule sets. These numbers made Arity
worth evaluating; they did not predict this repository's speed or establish
that Arity linting beat Jarl. [Recorded versions](https://github.com/jolars/arity/blob/main/docs/src/guide/benchmarks_meta.md),
[results](https://github.com/jolars/arity/blob/main/docs/src/guide/benchmarks_results.md),
[method](https://github.com/jolars/arity/blob/main/docs/src/guide/performance.md).

The same method disabled styler's cache and skipped its project timing, claiming
there was no check-only directory mode. styler's documented `dry = "fail"`
contradicted that general claim: its directory API also exposed `dry`. Therefore
the missing styler project result was a harness limitation, not evidence that
styler could not gate CI. No matched marginplyr speed result was obtained for
styler, formatR, Raven, or codetools. Rust implementation alone established no
performance ordering. [styler directory API](https://styler.r-lib.org/reference/style_dir.html).

## Name-resolution risks worth testing before any replacement

Arity's inspected `undefined-symbol` implementation deliberately suppressed the
whole file when an attached package was not indexed; otherwise an unknown
export could produce a false positive. It also treated captured or opaque
package-local arguments conservatively. A green result with incomplete metadata
could therefore mean a skipped class of checks. [Pinned rule implementation](https://github.com/jolars/arity/blob/0badabcaa16a7761a2b995ac983498397af263c7/src/linter/rules/correctness/undefined_symbol.rs).

Raven documented that it did not implement full lintr coverage, including
`object_usage_linter`, and acknowledged limitations such as dynamically
registered native-routine bindings. Its downloaded package-name database could
differ from installed dependency versions; `raven packages freeze` produced
project-specific metadata. These were material differences from analyzing
loaded R closures. [Limitations](https://github.com/jbearak/raven/blob/075b6003c312f4534d92d4aa85fb6da0c9e1bcfe/docs/limitations.md),
[package database workflow](https://github.com/jbearak/raven/blob/075b6003c312f4534d92d4aa85fb6da0c9e1bcfe/docs/r-package-dev.md).

Raven's cross-file documentation included a concrete missing-helper example:
`add_tax()` resolved from `utils.R`, while misspelled `add_taxes()` produced an
undefined-variable diagnostic. It also documented global definitions being
visible inside function bodies regardless of definition order. Combined with
its package mutual-visibility model, this made it a credible candidate for
detecting a missing internal helper. Arity's rule implementation contained both
standalone and cross-file unresolved-name paths. These sources suggested that
`f <- function() missing_helper()` belonged within both analyzers' intended
scope; the release probes below confirmed that narrow behavior, without
establishing parity for other codetools checks. [Raven's examples and scope](https://github.com/jbearak/raven/blob/075b6003c312f4534d92d4aa85fb6da0c9e1bcfe/docs/cross-file.md),
[Arity's implementation](https://github.com/jolars/arity/blob/0badabcaa16a7761a2b995ac983498397af263c7/src/linter/rules/correctness/undefined_symbol.rs).

## Release-binary probes

Both projects published macOS ARM64 release archives and separate SHA-256
files, permitting temporary extraction without Homebrew or a Rust build.
Initial sandboxed downloads failed; scoped escalated `gh release download`
requests succeeded. Both archives matched the separately published SHA-256
files before extraction. `--version` confirmed Arity 0.29.0 and Raven 0.21.1.
No installer script or package-index download was used.
[Arity assets](https://github.com/jolars/arity/releases/tag/v0.29.0),
[Raven assets](https://github.com/jbearak/raven/releases/tag/v0.21.1).

| Asset | Verified SHA-256 |
|---|---|
| `arity-aarch64-apple-darwin.tar.gz` | `5ccb2b6d8c356c0747e5fe1b93d0e217bbd03e9186b28cd8e46166f1cd6d907a` |
| `raven-macos-arm64.zip` | `58205a60c2f69bc4ad4b194e81a8defb185e2f13b5075a93c6cf8d8c1b7b3aba` |

The temporary workspace was `/private/tmp/r-lint-candidate-probes/` on the
same macOS host as the accompanying investigation. Files were written with R.
The standalone directory contained only `probe.R`:

```r
f <- function() missing_helper()
```

The package fixture had `Package: lintprobe`, `Version: 0.0.0`, a title,
description, `License: MIT`, and `NAMESPACE` containing `export(f)`. Its two R
files contained:

```r
# R/helper.R
helper <- function() 1

# R/f.R
f <- function() helper() + missing_helper()
```

Each binary ran from the respective fixture root, with default rules and no
custom configuration. Package discovery itself mattered here: the commands
selected `R/`, while the working directory contained `DESCRIPTION`.

| Fixture and command | Arity 0.29.0 | Raven 0.21.1 |
|---|---|---|
| Standalone: `arity lint probe.R` / `raven check probe.R` | `undefined-symbol` for `missing_helper`; also `unused-binding` for `f`; exit 1 | `undefined-variable` for `missing_helper`; exit 1 |
| Package: `arity lint R/` / `raven check R/` | Only `missing_helper` reported; exit 1 | Only `missing_helper` reported; exit 1 |
| Package after changing `f` to `function() helper()`; same commands | No findings; exit 0 | No findings; exit 0 |

Thus both published binaries distinguished a valid cross-file package helper
from a nonexistent helper in this small case. No external Imports, NSE,
testthat helpers, Qmd/Rmd, suppression directives, or unsupported dynamic
behavior were exercised. Command wall times from these tiny runs were not a
performance measurement. Installed dependency metadata versus downloaded symbol
indexes would need to be fixed before a representative comparison.

## Read-only repository adoption probe

Each candidate also ran once with its own default rules on a disposable archive
of marginplyr `fc7f17b16789864a1efa460123e3ad0c7e7b9120`. The selected directories
were the existing members of lintr 3.4.0's `lint_package()` list: `R`, `tests`,
`inst`, `vignettes`, and `data-raw`; `demo` and `exec` did not exist. This matched
directory intent, not each tool's discovered file set. Arity did not discover
Qmd files. No fix or format command ran. Each `processx::run()` had a 30-second
timeout, wrote JSON stdout and stderr under `/private/tmp/r-lint-candidate-probes/`,
and reached a terminal exit without timing out. The commands, from the archive
root, were:

```sh
arity lint --output json R tests inst vignettes data-raw
raven check --format json --workspace "$archive_root" R tests inst vignettes data-raw
```

| Candidate | Terminal outcome | Findings by directory | Important categories |
|---|---|---|---|
| Arity 0.29.0 | Exit 1, 377 diagnostics | R: 71; tests: 305; inst: 1 | 82 `undefined-symbol`, all outside R; 91 `unused-binding`; other style/correctness rules |
| Raven 0.21.1, default config | Exit 1, 457 diagnostics | tests: 52; vignettes: 405 | 456 `undefined-variable`; 1 `namespace-member-not-found` |

An initial Raven run inherited the overlap experiment's temporary `.lintr`
from this disposable archive, producing 498 diagnostics and an unsupported
`seq_linter` warning. That run was not the default-configuration result. After
removing only that temporary file, Raven was rerun once and produced the 457
diagnostics above, with empty stderr. Arity did not consume `.lintr`.

These counts were adoption obstacles, not counts of confirmed package defects.
For example, Raven flagged `is_fact`, `revenue`, `region`, `store`, and `product`
inside the data-masked `summarize_with_margins()` / `rollup()` example in
`vignettes/completing_keys.qmd`. It also flagged the deliberately quoted
`stats::share_of_total` in `test-contextual-helpers.R`'s rejected-qualifier test.
Arity flagged `test_that` and `expect_identical` in the sourced fixture
`tests/testthat/fixtures/dtplyr-scalar-missing-contracts.R`. Resolving fixture
scope, custom NSE, and intentional failures would require explicit configuration
or improved tool modeling; blanket suppression would risk hiding real defects.

Raven's published binary help explicitly described extracting R chunks from
Rmd/Rmarkdown/Qmd while ignoring prose and non-R chunks. The real Qmd findings
confirmed discovery and analysis, but did not establish correct custom NSE
modeling. No dedicated Rmd fixture was executed. No wall-clock comparison was
made from these adoption runs, whose rules and file coverage differed.

A follow-up comparison would need pinned release binaries, the same file set,
document chunks, cold/warm metadata states, and planted failures: unknown names,
unused locals, package internals/imports, NSE, helper visibility, malformed
syntax, and formatting differences. It should assert both diagnostic presence
and nonzero CI exits. Matching rule names or a clean run on already-clean code
would not prove replacement coverage. That experiment, rather than adding all
candidates to CI, was the smallest next step suggested by this survey.

## Measured lintr/Jarl overlap reduction

The accompanying [overlap experiment](lintr-jarl-overlap-2026-11-04/README.md)
compared all 26 default lintr rules with a 22-rule prototype, retaining
package-aware loading and the same 290 files. Three fresh-process runs per
variant gave median lint times of 32.725 s and 30.391 s, a 7.1% reduction.
The prototype removed the active analogues for equals-NA, sequence, T/F
symbols, and vector logic using an ordinary `.lintr` configuration.

Differential fixtures established lost checks for three of those four rules.
The experiment therefore did not recommend adopting the four-rule removal.
Its directory retained the runnable benchmark, prototype configuration,
complete 26-rule correspondence table, 32 feature probes, and raw timings.
These results narrowed the earlier idea of removing overlapping rules to
a modest measured gain with specific coverage costs.
