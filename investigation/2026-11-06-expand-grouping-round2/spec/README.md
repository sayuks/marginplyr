# Grouping specification forwarding, retention and family probe

Investigated: 2026-11-06
Tested SHA: `9ffb24614a71512170298a3827bc11c80354e742`

This lane read the Grouping specification vocabulary, ADR 0008's complete
kind/capture amendments, ADR 0026, ADR 0013, ADR 0015 and the relevant static
parenthesis decisions in ADR 0019. The November 5 adversarial and greedy
investigations and the two November 6 expansion investigations supplied prior
coverage and exclusions. In particular, the baseline included the #1383
forwarded-qualified-parenthesis repair. Prior measurements were not credited
as executions of this lane.

The hypotheses were that additional callable-parenthesis shapes might still
lose a forwarded selection's evaluation boundary; that retaining a
constructor-shaped call's answer might repeat evaluation or force ignored
arguments through one of those shapes; and that nested composite families
might disagree with independent finite enumeration. The observations below
refuted those hypotheses for the measured fixtures. No new implementation
issue was established in this lane.

## Forwarded selection boundaries

[forward-grid.R](forward-grid.R) compared ordinary typed tidyselect with the
first included set from each of the five Grouping constructors. Its one-row
source held character `a`, integer `b` and logical `c`, while lexical `a`
held `"b"`. An injected quosure carrying `a` separated ordinary lexical
evaluation from the selection walk.

Eight outer forms crossed nine inner forms (the same eight and no wrapper):
public-qualified parentheses, private-qualified parentheses, syntactic
parentheses, one and two pairs around the qualified callable head, a pair
around the private callable head, explicit identity, and a pair around the
unqualified parenthesis head. All 360 comparisons matched the direct
selection oracle; there were no failed or excluded fixtures. This directly
challenged the new normalization boundary rather than repeating only the
previous bare qualified-parenthesis witness. It established selected-column
agreement on this local source, without asserting lazy metadata-query counts
or result ordering.

## Retained ordinary answers

[retained-grid.R](retained-grid.R) crossed five parent constructors, five
shadowed constructor names, eight wrapper forms and plain versus injected
quosures: 400 comparisons. Each shadow returned `"b"`, counted its own
invocation and ignored an argument that raised if forced. The direct typed
selection and Grouping inspection both selected `b` in every comparison.
Every inspection called the shadow exactly once and left the ignored argument
unforced. This checked the one-reading contract from ADR 0008 through plain,
syntactic, public/private-qualified, identity, nested-parenthesis and
parenthesized-qualified-callee forms.

An initial fixture wrapped an already captured bare quosure in a second
quosure before handing it to direct tidyselect. Tidyselect rejected that
formula-shaped test input before the comparison. The oracle was corrected to
hand a top-level quosure through unchanged, and the full final 400-case matrix
then completed. The preliminary fixture failure was not counted as a package
defect or an additional observation.

## Composite family enumeration

[composite-oracle.R](composite-oracle.R) reused the independently enumerating
algorithm from the November 5 composite-dimension investigation with new seed
906173 and 3,000 generated shapes. It combined repeated selections, reordered
multi-column composite dimensions, unions, products, rollups, cubes and nested
products at depth four. The finite oracle enumerated left-major products and
size/combination-ordered cube subsets itself, then normalized included names
to plan dimension order.

Of those shapes, 2,642 were Grouping specifications within the 500-occurrence
fixture cap. All exact included-set and set-order comparisons passed under
both `keep` and `drop`, giving 5,284 policy comparisons. The other 303 shapes
were top-level selectors rather than specifications and 55 exceeded the
fixture cap; neither category counted as a passing comparison. The original
algorithm capped cube units at ten. No resource-bound or unbounded-family
claim followed from this finite run.

## Rejected malformed-object lead

[invalid-shapes.R](invalid-shapes.R) put nine non-quosure element shapes into
the `args` list of a manually forged specification. Eight reached rlang's
`quo must be a quosure` error; the missing-marker shape reached a base missing
argument error naming an internal capture variable. These observations did
not establish an actionable bug: ADR 0015's *Considered Options* explicitly
excluded forged specifications that the documented interface cannot produce.
ADR 0008's separate field-reader hardening did not establish a promise to
validate every forged argument element. The lead was discarded rather than
turned into a hardening ticket. No valid constructor call was shown to
produce these objects.

## Known open namespace-call defect (#1396)

A publication-time tracker refresh found open [#1396], whose complete body,
labels and empty comment list were read. [known-1396.R](known-1396.R)
independently replayed its bound-specification form against the same archived
baseline: lexical `s` held a Grouping set for `a`, while the caller namespace
operator for the authored public-qualified parenthesis returned a function
selecting `b`. Direct typed tidyselect selected `b` after one lookup. Grouping
inspection instead selected `a` after zero lookups. The corresponding private
qualification selected `b` after one lookup; a genuine base-qualified identity
wrapper correctly selected `a`.

This was a confirmed existing defect, not a new finding or ticket. The earlier
360/400-case matrices used genuine base namespace operators: they established
preservation of ordinary forwarded selection and retained-call boundaries in
different fixtures, and did not establish correct evaluation of caller-authored
namespace operations surrounding a specification-valued binding. The independent
observation command exited zero after checking both controls and the dated
wrong result. Its `--assert` mode deliberately exited one on the desired-plan
assertion. No implementation was changed.

[#1396]: https://github.com/sayuks/marginplyr/issues/1396

## Replay and limits

The first Review-ready invocation reaching lint flagged private-qualified and
multiply parenthesized language objects inside quoted fixtures. Those fixture
heads were subsequently constructed with `parse_expr()` from their literal
spellings. Every generated form was identical to the original quoted object.
The 360 forwarding and 400 retained-call comparisons were replayed against a
disposable copy of `e600e060`, whose package code was unchanged from the tested
main. Both passed and both CSVs were byte-identical to their original final
results. The [replay record](lint-fixture-replay.log) retained terminal handles
and output; these reruns were not added to the coverage counts.

Run from an archive of the tested package SHA. The source can also be chosen
with `MARGINPLYR_HUNT_SOURCE`; CSV output defaults to the R temporary
directory and can be directed to an existing directory with
`MARGINPLYR_HUNT_OUTPUT`.

```sh
Rscript <absolute-evidence-directory>/forward-grid.R
Rscript <absolute-evidence-directory>/retained-grid.R
Rscript <absolute-evidence-directory>/composite-oracle.R 3000
Rscript <absolute-evidence-directory>/invalid-shapes.R
Rscript <absolute-evidence-directory>/known-1396.R
Rscript <absolute-evidence-directory>/known-1396.R --assert
```

The first three commands asserted the stated comparisons and exited zero.
The invalid-shapes command printed the forged-object observations and exited zero;
it was an exclusion probe, not a promised-behavior test. CSVs, short terminal
logs and [environment.txt](environment.txt) were retained. The composite log
transcribed the retained execution's final counts, command, source SHA and
exit status; the other logs captured the shipped scripts' stdout directly.
All owned executions were collected: forwarding session 8145, family session
6811, retention session 57337, and the shipped-script replay session 78515
all reached exit zero. The two known-1396 invocations completed directly with
exit zero and the intentional assertion exit one. No package suite, build, rendering or live service was
used. `TESTTHAT_PARALLEL=false` was explicit.

The environment was R 4.6.1 on arm64 macOS, dplyr 1.2.1, dbplyr 2.6.0,
rlang 1.3.0, tidyselect 1.2.1, vctrs 0.7.3, tibble 3.3.1 and pkgload 1.5.3.
These observations did not cover arbitrary caller classes, all possible
capture syntaxes, unbounded plans, other dependency versions or platforms.
Only dated standalone evidence was added; package code, tests and contracts
were unchanged.
