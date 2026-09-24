# Require complete test line coverage

The package requires 100% of its measured R source lines to execute in the
test suite, with no coverage exclusions. The local Review-ready check and the
pull-request coverage job use one verdict; a strict required check prevents a
pull request from merging when it fails. The CRAN release process also requires
the successful coverage job and Codecov report for the exact candidate SHA.

This spends another full test run before review and makes instrumentation drift
a blocking finding. That cost buys an early, reproducible answer about every
retained source line. A reachable branch gets a test asserting its behavior or
diagnostic; a meaningful internal backstop gets an abnormal-state test; a branch
with no useful state to guard is removed. Exclusions would turn 100% into a
configurable denominator, so the gate rejects them. The formal CRAN preflight
does not repeat coverage: the release playbook verifies the candidate's CI
evidence after that audit.

Line coverage does not assert that every outcome of a condition was exercised.
Tests still state the observable contract they protect, and the release matrix
still proves optional backend execution independently.
