# Local DuckDB JSON test availability

Status: accepted, 2026-11-09. The maintainer confirmed the complete design after
the Q1–Q11 interview.

## Accepted scope

Make DuckDB JSON tests executable through ordinary `devtools::test()` and
`testthat::test_local()` calls, Review-ready `full`, and local CRAN preflight.
The preparation must support all of these entry points rather than only one
command or one R session.

## Accepted preparation boundary

Allow automatic preparation and reuse a valid cached extension without network
access. Retain an explicit preparation command for operators who want to prepare
the environment separately. Individual test cases do not acquire or install
extensions.

Review-ready `full` and the outer preflight entry prepare before the entire test
suite begins. Ordinary direct `devtools::test()` and `testthat::test_local()`
prepare immediately before the first case in a selected JSON test file. Other
test files may already have run in those direct calls. A focused selection
excluding JSON test files does not acquire the extension.

If required JSON support cannot be prepared, report a nonzero preparation
failure and do not run its JSON cases. Report the cause separately from a
package test failure; do not turn missing required coverage into a successful
skipped run.

## Accepted formal preflight boundary

Preserve formal preflight's non-installing contract. An outer entry point may
prepare the extension automatically and then invoke formal preflight as one
operator action. Direct invocation of the existing preflight only validates the
prerequisite and explains how to prepare it when missing.

## Accepted storage scope

Store extensions in a persistent user-level cache shared across worktrees.
Reuse only an extension matching the running DuckDB version and platform.
Test cases read the prepared extension; they do not install or update it.

## Accepted repair and retention

When a matching managed cache entry is damaged, automatic preparation may
reacquire and validate it once. Publish only a validated replacement; if repair
fails, report preparation failure. An invalid explicitly supplied external path
is an error, not permission to change that file or select a different one.

Keep older version/platform entries until an explicit cleanup command removes
them. Persistent extension storage is separate from Review-ready's seven-day
run-artifact retention. Keeping these entries permits offline reuse by older
environments at the cost of retaining their disk usage.

## Accepted required and optional execution

Ordinary local executions that select JSON tests require usable DuckDB and JSON
support. A missing DuckDB R package is a preparation failure; automatically
installing that R package is outside this change.

CRAN checks, explicitly minimal-dependency runs and other-backend-only CI may
skip with a reason when DuckDB or optional JSON support is unavailable. The
execution entry declares the coverage it promises; absence alone is not proof
of an intentionally minimal configuration. Preserve errors for invalid explicit
extension paths rather than masking a broken promised setup as a skip.

## Accepted acquisition and coordination

Acquire JSON only from DuckDB's official HTTPS extension distribution at
`extensions.duckdb.org`, preserving TLS and binary signature verification. Add
only this download host to the existing matching Codex and Claude network
policies and their verifier.

Serialize preparation for the same version/platform; a waiting caller rechecks
the published cache before acquiring anything. Preparation for other keys is
independent. Interrupted preparation must not publish an incomplete file or
destroy a valid entry. Explicit cleanup refuses an entry while it is in use.

## Verification requirements

Exercise real JSON cases without the previous missing-extension skips, fresh
offline reuse, invalid explicit paths, managed-cache repair and failure,
concurrent preparation, interruption-safe publication, in-use cleanup refusal,
and retained optional behavior. A non-JSON focused selection must not acquire
the extension. Verify that full-check children receive the prepared absolute
path and that direct formal preflight never installs an extension.
