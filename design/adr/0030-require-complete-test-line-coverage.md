# Require complete test line coverage

The package requires 100% of its measured R source lines to execute in the
test suite, with no coverage exclusions. The CI coverage job owns this
measurement; the local Review-ready check runs without coverage. The local
check remains the required boundary before review, while coverage failures are
discovered in CI. The pull-request coverage job remains a visible, optional
signal and does not restrict merging. The `main` ruleset has no required CI
status checks. The CRAN release process still requires the successful coverage
job and Codecov report for the exact candidate SHA.

The Full local source-tarball `R CMD check --as-cran` sets `NOT_CRAN=true` and
requires every optional Suggest registered by the test suite. It runs the full
suite, including snapshot expectations, once alongside package metadata,
examples, and vignettes ([#941](https://github.com/sayuks/marginplyr/issues/941)).
The common CLI also selects Repository for supported repository prose and runs
spelling, both linters, and mapped artifact verification while omitting package
build/check ([#1099](https://github.com/sayuks/marginplyr/issues/1099)).
Documentation retains the package build and `R CMD check --as-cran --no-tests`
for supported documentation inputs, with separately registered documentation
assertions and generated-artifact comparison
([#1100](https://github.com/sayuks/marginplyr/issues/1100)). The exception avoids
executing unrelated tests while retaining examples, vignettes, Rd and codoc
checks. Unknown inputs and R edits remain Full.
[Local checks](../agents/local-checks.md) owns selection and evidence;
Documentation and Repository success do not assert full-suite or snapshot
execution. CRAN-emulating CI jobs
retain their existing environment.

A reachable branch gets a test asserting its behavior or diagnostic; a
meaningful internal backstop gets an abnormal-state test; a branch with no
useful state to guard is removed. Instrumentation drift is a CI coverage
finding. Exclusions would turn 100% into a configurable denominator, so the
coverage command rejects them. The formal CRAN preflight does not repeat
coverage: the release playbook verifies the candidate's CI evidence after
that audit.

Line coverage does not assert that every outcome of a condition was exercised.
Tests still state the observable contract they protect, and the release matrix
still proves optional backend execution independently.
