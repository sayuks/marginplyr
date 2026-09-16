# Running lintr in the CRAN preflight

Investigated: 2026-09-16

This investigation asked how marginplyr's local CRAN preflight could run
`lintr` against the exact source candidate, how the candidate namespace must be
prepared, how the tool dependencies should be declared, and what a focused
implementation issue should require.

## Conclusion

The CRAN preflight can add a package-aware `lintr` step immediately after it
has verified and unpacked the source tarball. The step should lint
`identity$package_path`, not the repository checkout or the pre-build archive.
That path is the only already-available tree whose contents have been proved to
match the retained tarball.

Calling `lintr::lint_package(identity$package_path)` alone is insufficient for
this repository. lintr's default set contains executing linters, including
`object_usage_linter()`, and lintr says that the target package and its
dependencies must be installed or loaded for those linters to work as
intended. The namespace must describe the unpacked candidate, not merely an
installed marginplyr package with the same version number. The appropriate
sequence is therefore:

```r
pkgload::load_all(identity$package_path, quiet = TRUE)
lints <- lintr::lint_package(
  identity$package_path,
  cache = FALSE,
  show_progress = FALSE
)
```

The default `load_all()` behavior matters here. marginplyr's lint scope
includes `tests/`, and its test files rely on testthat functions and helper
files. Disabling helper loading or testthat attachment leaves those names
unresolved and creates false `object_usage_linter()` findings. The existing
routine lint workflow already uses the correct default `load_all()` behavior.

A nonempty `lints` result should be reported as a candidate defect and map to
preflight exit code 1. An error while loading or linting is a tool failure and
maps to exit code 2. The preflight should derive those classifications from
the returned object and caught conditions rather than rely on
`LINTR_ERROR_ON_LINT=true`, whose documented behavior is to terminate the R
process when findings are printed. Direct classification preserves the
preflight's evidence finalization and its distinction between candidate and
tool failures.

No hand-constructed lint finding is needed in the verifier. It would test a
mock representation of lintr output rather than the candidate boundary. The
existing structural verifier can assert the invocation, ordering, dependency
set, and evidence contract without introducing another synthetic “test of the
test.”

## Evidence

### The preflight already owns the exact unpacked candidate

On 2026-09-16, `run_preflight_pipeline()` performed these operations:

1. checked all declared Suggests and `Config/Needs/preflight`, the installed
   marginplyr version, Quarto, Pandoc, and TeX;
2. archived clean `HEAD` into an external workspace and ran `R CMD build`;
3. required exactly one tarball, retained it in the evidence bundle, and
   checked its name, top-level directory, `DESCRIPTION` identity, and SHA-256;
4. unpacked it below `candidate-source/` and returned the normalized package
   path as `identity$package_path`;
5. ran spelling on that unpacked path;
6. ran checktor on the retained tarball;
7. ran one `R CMD check --as-cran` on the retained tarball; and
8. conditionally diagnosed URL findings and checked `cran-comments.md`.

`build-workspace/` and `candidate-source/` were temporary external directories
deleted when the pipeline returned. The retained tarball and its digest were
the canonical artifact. This makes the appropriate insertion point
unambiguous:

```text
clean HEAD
   |
   v
R CMD build -> retained tarball -> verify identity -> unpacked candidate
                                                     |        |
                                                     |        +-- spelling
                                                     |        +-- lintr (new)
                                                     v
                                            checktor and R CMD check
```

Running lint before `R CMD build` would inspect files that `.Rbuildignore`
removes, such as `data-raw/`, `tools/`, and `.github/`. Running it on the
repository root would also permit uncommitted or ignored files to affect the
answer. Running it on `identity$package_path` instead answers the release
question: whether the source tree actually present in the candidate tarball
passes the repository's package lint policy.

Writing R Extensions describes a source package tarball as the result of
`R CMD build` cleanup and says that `.Rbuildignore` controls which source-tree
files are excluded
([Writing R Extensions, building package tarballs](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Building-package-tarballs)).

### lint_package accepts a package directory and covers shipped R surfaces

lintr documents `lint_package(path)` with `path` as the package base directory.
It searches the package's `R/`, `tests/`, `inst/`, `vignettes/`, `data-raw/`,
`demo/`, and `exec/` directories, using the directories that actually exist
under that path
([lintr `lint_package()` reference](https://lintr.r-lib.org/reference/lint.html),
[lintr `lint.R` source](https://github.com/r-lib/lintr/blob/main/R/lint.R)).

The unpacked tarball is therefore a supported input. Because the built package
does not contain ignored `data-raw/`, repository workflows, or `tools/`, this
preflight lint is deliberately narrower than the routine checkout lint. It
complements that routine gate; it does not replace it.

The preflight should pass `cache = FALSE` explicitly. That is lintr's documented
default for a single lint operation, and spelling it at this boundary records
that no result from an earlier checkout lint may contribute to the verdict on
the unpacked candidate
([lintr `lint()` reference](https://lintr.r-lib.org/reference/lint.html)).

### Executing linters require the candidate namespace

lintr classifies `object_usage_linter()`, `object_name_linter()`,
`object_length_linter()`, `namespace_linter()`, and related checks as executing
linters. Its documentation warns that they evaluate parts of the linted code
and may require the target package and its dependencies to be available. For a
package, this includes loading the package with `pkgload::load_all()` or
installing and attaching it
([lintr executing-linters reference](https://lintr.r-lib.org/reference/executing_linters.html)).

The lintr continuous-integration guide repeats the warning specifically for
`object_usage_linter()` before recommending `lintr::lint_package()`
([lintr continuous-integration guide](https://lintr.r-lib.org/articles/continuous-integration.html)).
The official r-lib/actions package lint example installs `lintr` and the local
package, then runs `lint_package()` with `LINTR_ERROR_ON_LINT=true`
([r-lib/actions lint example](https://github.com/r-lib/actions/blob/v2-branch/examples/lint.yaml)).

pkgload says that `load_all(path)` sources the package's R files into an
environment that behaves like a regular namespace, loads its imports, and
handles an already-loaded namespace by unregistering it and registering the
new development namespace. It also loads testthat helpers by default for a
package that uses testthat
([pkgload `load_all()` source and documentation](https://github.com/r-lib/pkgload/blob/main/R/load.R)).

This last behavior is important because `preflight_prerequisites()` called
`requireNamespace("marginplyr")` while checking the installed candidate
version. That loads the installed namespace before the proposed lint step.
Matching only the version is not sufficient evidence that its source matches
the retained tarball; two commits may have the same package version during
development. Loading `identity$package_path` replaces the registered namespace
used for lint resolution with the unpacked candidate's code.

### Repository measurement confirmed the namespace and helper requirement

In fresh R sessions on 2026-09-16, using lintr 3.4.0 against the inspected
marginplyr checkout produced:

| Preparation | Findings | Finding class |
|---|---:|---|
| no marginplyr load | 70 | all `object_usage_linter` |
| `load_all(..., helpers = FALSE, attach_testthat = FALSE)` | 69 | all `object_usage_linter` |
| default `load_all(..., quiet = TRUE)` | 0 | none |

The second run still reported testthat expectations and marginplyr test-helper
functions as invisible globals. The default load loaded the package namespace,
attached testthat, and sourced test helpers, matching the existing
`.github/workflows/lint.yaml` command and eliminating the false findings.

This measurement establishes a marginplyr-specific condition, not a universal
claim that every package must load test helpers before linting. It also shows
why using `attach = FALSE` or disabling helpers is not a safe simplification
for this package's current lint scope.

### Findings and tool failures need different handling

`lint_package()` returns an object of class `lints`; a zero-length object means
no findings
([lintr `lint_package()` reference](https://lintr.r-lib.org/reference/lint.html)).
lintr's CI guide says `LINTR_ERROR_ON_LINT=true` makes a build fail and warns
that it terminates the R process when a lint is printed
([lintr continuous-integration guide](https://lintr.r-lib.org/articles/continuous-integration.html)).

That environment variable is appropriate for the standalone routine lint
job, but not for an in-process multi-step preflight that must still write its
summary, machine result, step table, before/after worktree records, and final
exit code. The preflight should instead:

- load the candidate and call `lint_package()` inside `tryCatch()`;
- print or format each returned lint so its relative file, line, column,
  linter, and message are visible without invoking lintr's process-killing
  collection printer under an inherited error-on-lint setting;
- record `passed` when `length(lints) == 0L`;
- record `failed` and `record_problem(state, "candidate")` for nonempty lints;
  and
- record `unavailable` and `record_problem(state, "tool")` for a load or lint
  error.

This matches the preflight's existing public exit contract: candidate defects
produce 1, tooling failures produce 2, and an interruption produces 130. The
lint step should not return early merely because it found a candidate defect;
the existing spelling step likewise records the defect and allows the
remaining release evidence to be collected.

### Dependency declarations should be complete per task

pak defines `Config/Needs/*` values as named extra R-package dependency sets
that callers request explicitly
([pak dependency types](https://pak.r-lib.org/reference/package-dependency-types.html)).
`r-lib/actions/setup-r-dependencies` maps `needs: preflight` to
`Config/Needs/preflight` and resolves it with pak
([setup-r-dependencies action source](https://github.com/r-lib/actions/blob/v2-branch/setup-r-dependencies/action.yaml)).

The agreed repository dependency model treats each task field as a complete
set of its direct R-package tools and permits intentional overlap. Applied to
the proposed code:

```dcf
Config/Needs/preflight:
    checktor (== 0.1.0),
    lintr,
    pkgload,
    rcmdcheck,
    spelling,
    urlchecker

Config/Needs/review:
    lintr,
    pkgload,
    rcmdcheck,
    spelling,
    testthat
```

The `Config/Needs/review` field and derivation of
`review_ready_prerequisites()` from it were agreed but had not yet been
implemented in the tree inspected on 2026-09-16. The proposed preflight change
does not alter that decision. Repeating `lintr`, `pkgload`, `rcmdcheck`, and
`spelling` across task groups is intentional: either task can be provisioned
without knowing the other's dependency set. Keeping `spelling` in `Suggests`
and also naming it as a direct preflight/review tool is likewise permitted by
this task-complete model.

On the investigation date, the preflight already combined every Suggest with
`Config/Needs/preflight` and removed duplicate package names. Therefore the
overlap does not install two copies. The existing verifier assertion that the
preflight set and standard package dependencies have an empty intersection
would need to be replaced with assertions about the exact task sets.

The local CRAN preflight installs nothing. Its documented preparation remains:
install full Suggests, request `Config/Needs/preflight`, and install the
external Quarto/TeX tools. No routine CI job ran the full preflight on the
investigation date; `.github/scripts/verify-cran-preflight.R` was a base-R
contract verifier. Adding lintr to the real local preflight therefore does not
justify silently turning that verifier into a full dependency-installing lint
job.

### Existing evidence machinery can record the new tools without another artifact

`run_preflight_pipeline()` built `state$tool_versions` from the packages named
by `Config/Needs/preflight` and wrote the values to both `results.dcf` and the
human summary. Once `lintr` and `pkgload` are in that field, their installed
versions are recorded automatically. The prerequisite table likewise records
whether each declared version requirement was satisfied.

For findings, the minimal design can follow spelling's established policy:
print actionable findings to the command output, record a `lintr` row in
`steps.tsv`, and avoid a second lint TSV or synthetic report beside the
canonical tarball. A successful run has no findings to retain; a failed run is
remediation input rather than approved release evidence. If durable lint output
later becomes a release requirement, that should be an explicit evidence
policy change rather than an incidental by-product of this integration.

## Findings

### The installed same-version package is necessary elsewhere but insufficient here

The preflight's installed marginplyr version check exists because Quarto
vignette building runs in a child process. It does not establish source
identity. lintr reads the unpacked candidate files but resolving package
symbols against a stale same-version namespace can produce a false result in
either direction. `load_all(identity$package_path)` binds both halves of the
lint operation to the candidate.

### Default load_all behavior is part of the lint environment

For marginplyr, loading only `R/` is not enough because `lint_package()` also
checks `tests/`. The testthat attachment and helper sourcing are inputs to
`object_usage_linter()` resolution. The implementation should make this
dependency visible in its comment rather than “optimizing” it away with
`helpers = FALSE`.

### The tarball lint and routine lint answer different questions

The routine lint job answers whether the committed repository source passes
before review. The preflight lint answers whether the exact cleaned source
tarball candidate passes before release. Some duplication of computation is
deliberate because the artifact boundary differs. The release check should not
be used as a reason to remove the faster routine CI signal.

### No synthetic finding is required

The proposed adapter has only three observable outcomes: empty lints,
nonempty lints, and a condition. A hand-built `Lint` or `lints` object would
duplicate lintr's representation and formatting contract. It would also repeat
the artificial-finding pattern already questioned for spelling without proving
that the unpacked candidate, namespace loading, helper loading, and real linter
participate together.

The verifier should instead make structural drift loud: exactly one lintr
invocation, after tarball identity, using `identity$package_path`, with the
declared dependency and tool-version evidence. If an end-to-end negative
demonstration is desired during implementation, a temporary real source lint
can be used manually; it need not become a permanent artificial fixture.

## Boundary with issue #562

[Issue #562](https://github.com/sayuks/marginplyr/issues/562) concerns a
routine CI gate for **spelling**. It records that `tests/spelling.R` could skip
under ordinary check conditions or print findings without failing, while the
release-only preflight was the only effective spelling gate.

The proposed issue is different on all three axes:

- tool: lintr rather than spelling;
- artifact: the verified source tarball rather than a package test or routine
  checkout; and
- timing: local release preflight rather than routine pull-request CI.

The lintr-preflight issue should not remove `tests/spelling.R`, redesign the
routine spelling gate, or claim to satisfy #562. Conversely, #562 should not
be expanded to own lintr or the tarball-candidate namespace. They may both
touch dependency metadata, so intentional `Config/Needs/*` overlap should be
stated rather than treated as duplicate cleanup.

## Recommended implementation shape

1. Add `lintr`, `pkgload`, and the already-direct `spelling` tool to the exact
   `Config/Needs/preflight` set, preserving the checktor pin and the rcmdcheck
   and urlchecker entries.
2. Derive prerequisite checking and recorded versions from that field, as the
   preflight already does.
3. After `tarball-identity` and the existing spelling step, load
   `identity$package_path` with default `pkgload::load_all()` behavior and lint
   that same path with `cache = FALSE` and progress disabled.
4. Report individual lint details without relying on the collection printer's
   `LINTR_ERROR_ON_LINT` process termination.
5. Map nonempty findings to a candidate failure and load/lint conditions to a
   tool failure, while allowing later evidence steps to run.
6. Record the step in the ordinary step table and rely on the existing tool
   version summary; do not add a lint-specific report or synthetic finding
   fixture unless evidence policy is separately changed.
7. Update `tools/cran-preflight.md`, the release playbook's audit summary, and
   the verifier's exact dependency/order assertions.

## Proposed issue acceptance criteria

- The preflight runs `lintr` exactly once, after source-tarball identity has
  been verified, against the unpacked `identity$package_path`, with
  `cache = FALSE`.
- The unpacked candidate is loaded with `pkgload::load_all()` in the same lint
  environment, retaining the testthat/helper behavior required by
  marginplyr's `tests/` lint scope.
- The lint step inspects only the candidate tarball contents; it does not lint
  the repository checkout or the pre-build archive.
- Zero findings record a passed `lintr` step. Nonzero findings print their
  relative file and location, record a failed step, and make the final
  preflight exit 1. A package-load or linter error records `unavailable` and
  makes the final exit 2.
- The preflight completes its ordinary summary, machine result, steps table,
  and before/after worktree evidence even when lints are found; it does not
  delegate classification to `LINTR_ERROR_ON_LINT` process termination.
- `Config/Needs/preflight` is the complete direct set
  `checktor (== 0.1.0), lintr, pkgload, rcmdcheck, spelling, urlchecker`.
  Intentional overlap with `Suggests` and the agreed complete
  `Config/Needs/review` set is accepted rather than rejected by the verifier.
- The human and machine summaries record the installed `lintr` and `pkgload`
  versions through the existing tool-version mechanism.
- The local preflight documentation and release playbook name lintr among the
  candidate checks and retain the rule that the command installs nothing.
- The contract verifier asserts the real invocation path, ordering, exact
  dependency set, and evidence fields. It does not introduce a hand-built
  lint-finding fixture.
- The change neither implements nor closes issue #562; routine spelling CI
  remains that issue's scope.

## Uncertainties and limits

- The investigated repository had no `.lintr` file, so both the routine job
  and the proposed tarball lint used lintr's defaults. If a repository-only
  `.lintr` file is added later and excluded from the source tarball, the project
  must decide explicitly whether release lint policy follows that external
  configuration or only configuration shipped in the candidate.
- `load_all()` executes package code and test helpers. lintr explicitly warns
  that executing linters should not be used with untrusted code. The candidate
  here is the repository's own clean, reviewed `HEAD`; this recommendation is
  not a general design for linting untrusted tarballs.
- pkgload approximates installed-package loading and documents differences
  from `library()`. It is appropriate here because the source identity matters
  and is the mechanism lintr recommends, but it does not replace the later
  `R CMD check` installation and execution of the tarball.
- The local measurement used the checkout that was inspected on 2026-09-16.
  The official lintr/pkgload contracts and the preflight's candidate-path
  design are the durable evidence; exact finding counts may change as source
  and tool versions change.
