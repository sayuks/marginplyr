# Require complete test line coverage

The package requires 100% of its measured R source lines to execute in the
test suite, with no coverage exclusions. The local Review-ready check and the
pull-request coverage job use the same strict line-coverage command. The local
check is the required boundary before review; the pull-request job remains a
visible, optional signal and does not restrict merging. The `main` ruleset has
no required CI status checks. The CRAN release process still requires the
successful coverage job and Codecov report for the exact candidate SHA.

The local coverage run enables snapshot expectations and replaces the separate
full `testthat::test_local()` run. The source-tarball `R CMD check --as-cran`
still runs the tests under CRAN semantics, where snapshots are skipped, and
checks package metadata, examples, and vignettes. This leaves two full-suite
runs before review while retaining both kinds of evidence. Instrumentation
drift remains a blocking finding at the local boundary. A reachable branch
gets a test asserting its behavior or diagnostic; a meaningful internal
backstop gets an abnormal-state test; a branch with no useful state to guard is
removed. Exclusions would turn 100% into a configurable denominator, so the
gate rejects them. The formal CRAN preflight does not repeat coverage: the
release playbook verifies the candidate's CI evidence after that audit.

Line coverage does not assert that every outcome of a condition was exercised.
Tests still state the observable contract they protect, and the release matrix
still proves optional backend execution independently.
