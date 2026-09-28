# Recover a local Review-ready check after a native rendering crash

Decision: accepted by the maintainer on 2026-09-28 for
[#712](https://github.com/sayuks/marginplyr/issues/712).

The operational authority is [Local review-ready checks, One retry after a
native rendering crash](../agents/local-checks.md#one-retry-after-a-native-rendering-crash).
This specification records the implementation and verification obligations
for that policy. It does not extend the CI or formal release-preflight gates.

## Investigation disposition

End further root-cause investigation for #712 after preserving the evidence.
The immediate null callback in Deno's SQLite initialization was identified,
but the historical failure mechanism remained unconfirmed. A formal ordering
counterexample and bounded native non-reproduction did not establish a fix.

After implementing and verifying the accepted retry contract, close #712 with
the explicit disposition that root-cause investigation ended and operational
recovery was provided. Do not describe the cause as resolved or the crash as
prevented. Closure depends on implementation verification, not on a later
check happening to pass.

## Implementation boundary

Keep one Review-ready invocation and one built tarball. The source-tarball
boundary owns at most two complete `R CMD check --as-cran` attempts, each
with its own directory, console log, terminal result, and NOTE report. Do not
repeat the prior source checks or build. The source SHA, tarball identity,
and Quarto/Deno identities bind the attempts to the same input and tools.

Determine retry eligibility from a failed attempt's returned check result and
its own vignette-rebuilding diagnostics. Initially support only the macOS
Quarto shell-launcher SIGSEGV form naming the Deno command it executed. The
classifier does not claim to recognize a native stack, SQLite defect, or all
native crashes. A nonzero outer status, arbitrary occurrence of `SIGSEGV`, or
an unrelated `.ips` cannot authorize another attempt.

The initial supported envelope is exit status 1, no timeout, one ERROR,
zero WARNINGs, no test failure, and one failed HTML `.qmd` rebuilding segment.
It requires the Quarto R 1.5.1 wrapper measured with a synthetic launcher
failure: the CLI error ends in failed cli interpolation of `QUARTO_DENO` and
`object 'QUARTO_DENO' not found`. Other successful vignette segments are
allowed. The fixture established a wrapper form, not a real native crash.
Unknown forms remain ineligible until corresponding evidence and fixtures
justify extending the classifier.

Compare canonical Quarto/Deno executable paths and versions, plus content
hashes of the tarball, Quarto launcher, its `quarto.js`, and Deno executable,
before the first attempt and immediately before retry.

Retain the first failure before any retry and make retention completeness
observable. A printed bundle path alone does not prove that available files
were copied. Keep failed-attempt evidence after workspace cleanup, including
when the retry passes. Tool and tarball identity failures, ambiguous evidence,
independent check failures, and timeouts follow the refusal rules in the
operational contract.

Report an ordinary first-attempt pass separately from `passed-after-retry`.
The latter is an overall successful gate with a recorded failed first
attempt, subject to the final attempt's ordinary NOTE disposition. Any failed
retry ends the invocation with a nonzero status and both failures preserved.

## Verification obligations

Use deterministic checker fixtures to verify recovery; a real Deno crash is
not required. Cover these boundaries without running repeated full gates:

- Eligible native failure followed by a clean complete check: exactly two
  calls, identical tarball bytes and check options, different check
  directories, retained first failure, and `passed-after-retry` output.
- Eligible failure followed by any failure, exception, or timeout: exactly
  two calls, terminal failure, and separate diagnostic evidence.
- Crash text plus an independent ERROR or WARNING, an ordinary Quarto error,
  an unidentified process, unsupported platform, incomplete or ambiguous
  output, unsupported wrapper, multiple failed vignette segments, initial
  timeout, and a pre-result exception: no retry. Successful vignette segments
  alongside the single eligible failure do not prevent retry.
- Failure to preserve any available required diagnostic, or changed or
  unavailable tarball/Quarto/Deno identity: no retry and an explicit reason.
- Missing native report with sufficient portable evidence: eligibility does
  not depend on asynchronous OS crash-report creation.
- Existing first-attempt success, NOTE classification, earlier-stage failure,
  and cleanup behavior remain covered. The public CLI distinguishes recovery
  from a first-attempt pass and returns failure for an exhausted retry.

Run the focused verifier, required repository checks, and the normal
Review-ready gate for the implementation commit. Record the evidence before
closing #712; do not claim that fixture verification proved crash prevention.
