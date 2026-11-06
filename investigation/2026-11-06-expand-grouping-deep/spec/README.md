# Forwarded Grouping selections inside qualified parentheses

Investigated: 2026-11-06
Tested package SHA: `547d77095c2923573216d002f50bef21778247f9`

This lane examined constructor capture, nested-argument normalization,
selection classification and product expansion after reading the Grouping
contracts and recent investigations. The previous November 5 selection
investigations, November 6 pattern-proxy investigation and November 6 nesting
family investigation supplied exclusions; their execution counts were not
credited here. Only standalone evidence was added. No package code, package
test, dependency declaration or generated help was edited.

## G1: normalization erased an ordinary qualified selection boundary

[The minimized replay](injected-qualified-minimal.R) used a public embracing
wrapper that returned ``grouping_sets(base::`(`({{ column }}))``. Its one-row
source had character column `a` and integer column `b`, and lexical `a` held
the name `"b"`. Ordinary typed
tidyselect selected `b`; Grouping inspection silently selected `a`.

The successful separating controls selected `b` through the same qualified
call without embracing, an identity wrapper with embracing, and an embraced
fixed-key selection. Syntactic parentheses with embracing selected `a`, as
tidyselect's selection walk required. Thus this was a change from ordinary
qualified-call evaluation to selection-walk evaluation, rather than a claim
that syntactic parentheses should change the selected column.

The initial hypotheses were a wrong captured environment, a names-only
metadata-classifier error, and wrapper erasure during nested-argument
normalization. The identity and fixed-key controls retained the captured
environment. The unembraced qualified selector used the same source and
classifier and selected `b`. Inspection of `nested_arg_quosure()` then showed
the remaining distinction: it found the child quosure through qualified
parentheses and replaced the whole argument with that child. The original
ordinary-call boundary never reached selection classification or tidyselect.
The same normalization replaced a qualified wrapper around a forwarded
numbered dot with that dot's captured argument.

This violated the ordinary qualified-selection reading established by closed
[#1315] and [PR #1330], and the retained captured-environment behavior in
[ADR 0008]. [ADR 0013] supplied the correspondence between inspected plans
and Margin operations. Closed [#897] established transparent syntactic
parentheses around injected nested specifications; it did not establish that
ordinary qualified selection calls could be discarded. Closed [#1288]
concerned retaining the value of a constructor-shaped call with a qualified
parenthesis in its callee, rather than a forwarded column selection inside
the argument.

The expected repair was one independently verifiable slice: preserve the
qualified selection boundary and the captured environment while normalizing
forwarded arguments. The observations did not establish a need for separate
per-consumer repairs or prerequisite refactoring. No repair was attempted.

## Measured scope

| Probe | Executed comparisons | Result |
| --- | ---: | --- |
| [Injected-selection matrix](injected-qualified-matrix.R) | 1,400 | 400 silent wrong results, 1,000 successful controls, no errors or excluded fixtures |
| [Public-forwarding matrix](forwarded-qualified.R) | 200 | All 200 qualified forwarded results differed from the independent literal-selection result |
| Additional refusal witnesses | 2 | A valid union falsely refused duplicate sets; a valid fixed-key/dimension pair falsely refused overlap |

The first matrix crossed the five Grouping constructors, all five consumers
(inspection and four Margin verbs), local and immutable dtplyr-root sources,
bare-name and combining-call quosures, and lexical name and positional
selection values. Seven wrapper forms comprised public `base::` parentheses
at depths one and three, private `base:::` parentheses at the same depths,
syntactic parentheses, an identity call, and an unembraced ordinary qualified
call. Only the two public-qualified injected forms failed. The `base:::` forms
were successful controls; no equivalence of that private spelling's static
recognition was claimed.

Every non-inspection comparison checked the complete materialized public
result against the same constructor over the literal selected column. The
inspection comparisons checked the complete ordinary tibble. No ordering was
requested, and no Margin-order guarantee was inferred from these comparisons.

The second matrix used public wrappers with embracing, explicit `!!enquo()`,
`..1`, and `(..1)`, each checked against a direct typed tidyselect reference.
It crossed the same constructors, consumers and two input kinds. These were
additional forwarding spellings of G1, not additional unique bugs. The union
control requested sets `b` and `a`, while the faulty wrapper instead produced
two `a` sets. The fixed-key control retained `a` and grouped on `b`, while the
faulty wrapper instead claimed `a` for both roles.

The first matrix initially assumed both qualification operators would fail.
The complete observations instead showed the private operator preserved its
boundary. That assertion was corrected and the whole final matrix reran;
only the final complete run supplied the reported counts.

## Existing coverage and duplicate disposition

The existing qualified-parenthesis package tests exercised unforwarded whole
ordinary calls and forwarded ordinary calls inside `c(...)`. Neither shape
normalized away the whole nested argument as this witness did. The forwarded
tests also covered literal strings and explicit lexical pronouns; those values
could still name the same column after losing the qualified wrapper, whereas
the bare symbol in this witness distinguished the two readings.

All-state snapshots supplied by the coordinator contained 695 issue
titles/bodies and the latest 300 PR titles/bodies. This lane searched both for
an injection,
embracing or quosure term together with qualified-parenthesis or `base::`
terms. The relevant result was merged PR #1330. The full bodies and comments
of #1315, #897, #1288, #1280, #1214 and #1234, and the full body/comments of
PR #1330 were retrieved. PR #1330's accepted review finding concerned a forwarded argument
inside ``c(base::`(`({{ col }}), b)``, not the whole nested
argument normalization reached here. #1214 and #1234 concerned transparent
syntactic parentheses around numbered dots and the distinction from source
aliases, rather than retaining ordinary-call evaluation around a forwarded
selection. No equivalent issue was found in those snapshots.
Live `gh ... --search` requests from this lane encountered a
sandbox network rejection for `cafe.github.com`; the coordinator owned the
publication-time live duplicate and current-main refresh.

## Replay and environment

Run from the investigated package root:

```sh
Rscript investigation/2026-11-06-expand-grouping-deep/spec/injected-qualified-minimal.R
Rscript investigation/2026-11-06-expand-grouping-deep/spec/injected-qualified-minimal.R --assert
Rscript investigation/2026-11-06-expand-grouping-deep/spec/injected-qualified-matrix.R
Rscript investigation/2026-11-06-expand-grouping-deep/spec/forwarded-qualified.R
```

`MARGINPLYR_HUNT_SOURCE` selected another package source copy, and
`MARGINPLYR_HUNT_OUTPUT` selected the matrix CSV destination. The minimal
observation mode checked the dated wrong selection and all separating
controls and exited zero. Its desired-contract `--assert` mode exited one
with `identical(actual, expected) is not TRUE`. The final matrix retained
terminal session `65711` through exit zero; the forwarding matrix retained
session `78656` through exit zero. No owned execution remained active when
this lane finished. These were defect demonstrations, not a passing package
regression suite or Review-ready result.

The environment was R 4.6.1 on aarch64 macOS, dplyr 1.2.1, dbplyr 2.6.0,
tidyselect 1.2.1, rlang 1.3.0, vctrs 0.7.3, tibble 3.3.1, dtplyr 1.3.3,
data.table 1.18.6.1 and pkgload 1.5.3. `TESTTHAT_PARALLEL=false` was explicit.
No package suite, source-package build, rendering, hosted service or live
database was used in this lane. Arbitrary callers, unlimited plans, other
platforms and other dependency versions remained outside these observations.

[#1315]: https://github.com/sayuks/marginplyr/issues/1315
[PR #1330]: https://github.com/sayuks/marginplyr/pull/1330
[#897]: https://github.com/sayuks/marginplyr/issues/897
[#1288]: https://github.com/sayuks/marginplyr/issues/1288
[ADR 0008]: ../../../design/adr/0008-centralize-grouping-specification-kind-rules.md
[ADR 0013]: ../../../design/adr/0013-inspect-grouping-plans-as-ordinary-tibbles.md
