# Grouping specification capture and authored continuation hunt

Investigated: 2026-11-07
Tested package SHA: `e1dabaabb392a3791177e5c48e983f780c7c93ec`

This lane investigated the public Grouping constructors and plan inspection,
including the authored namespace and typed-preflight continuation behavior
merged for #1396. It read the vocabulary, ADR 0008 (including its capture,
retention and authored-namespace amendments), ADR 0026, ADR 0013, and the
November 6 expansion and nesting constructor investigations. Earlier runs
were inputs to hypothesis selection, not executions credited to this lane.
No package implementation or package regression test was changed.

## Authored namespace boundaries: 1,500 comparisons matched

[namespace-grid.R](namespace-grid.R) crossed seven outer wrappers, eight inner
forms (including no inner wrapper), three payloads and all five public
constructors. The wrappers included public and private namespace parentheses,
syntactic parentheses, parenthesized public/private callee heads, two pairs
around a public callee, and a parenthesized unqualified parenthesis head.
Payloads were a bound specification, a constructor call and an injected
quosure. The caller's namespace operators returned a selector that selected
`b` only in the faithful character metadata context for column `a`.

The 750 admissible fixtures agreed with direct typed tidyselect on the first
included grouping set, and their complete local `expand_with_margins()`
results agreed with literal-column specifications: 1,500 result comparisons.
Both inspection and expansion independently matched the reference's exact
namespace-lookup count. The retained CSV recorded those counts separately.

Ninety constructor combinations were excluded because their syntactic-only
expressions were refused by direct tidyselect while nested Grouping grammar
could read the specification. Comparing those different contracts would be an
invalid oracle. An initial run reported these as differences; they were
classified as fixture exclusions before the final complete execution. No
excluded fixture was counted as a pass. The direct reference signalled the
ordinary external-vector deprecation once; this was not a Package condition
or a warning-identity test.

## Constructor capture: 1,331 checks matched

[capture-grid.R](capture-grid.R) enumerated every ordered triple from eleven
argument shapes. They included plain selections, ordinary captured quosures,
a captured quosure containing an injection that raised if reprocessed, empty
and populated splices, a splice of quosures, a combining selection, NULL and
an empty argument.

For the 1,000 triples without the protected injection witness, the captured
argument list was exactly identical to independent ordinary `enquos()`
capture through a variadic wrapper, including quosure environments and empty
positions. The other 331 triples asserted that each occurrence of the
already captured witness remained identical and its injection never ran.
Ordinary re-capture was deliberately not the oracle for that promised opaque
boundary. Preliminary attempts to call `enquos()` directly with arbitrary
arguments and to recapture that opaque injection were discarded as invalid
references; neither supplied a package finding or a counted successful check.

## Nested continuation: 400 comparisons matched

[continuation-grid.R](continuation-grid.R), seed 61107, generated 200 bounded
nested union/product shapes at depth at most three, each with one to three
children per internal node. Each leaf crossed a caller-authored namespace
operation and returned either an ordinary selection or a Grouping set. Its
operator asserted faithful typed character metadata before recording its
identity. The reference replaced each leaf with the literal Grouping set.

Under both `keep` and `drop`, every complete inspection tibble matched the
reference, including occurrence order, set identifiers, dimensions and bits.
All 986 leaf callback observations occurred exactly once per leaf occurrence,
in the authored depth-first order. These observations refuted lost retained
answers, resumed-subtree rereads and reordered pending siblings for these
finite shapes. They did not prove arbitrary family algebra or unbounded
continuations.

## Disposition and limits

No new actionable defect was established in this lane. A separate exploratory
probe of `signalCondition()` with an error-class condition did not establish a
selection-correspondence defect: direct tidyselect itself converted that
signal into its chained error before an outer calling handler could resume.
That probe was excluded from all comparison counts. Manually forged
specification objects were not used to infer new public validation promises.
The all-state Issue snapshot was retrieved with `gh issue list --state all
--limit 2000 --json number,title,body,url` to identify existing capture,
forwarding and namespace work; no new candidate needed publication or a
candidate-specific duplicate disposition.

The observations used local inputs, finite shapes and one provisioned library.
They did not cover every caller class, every dispatch method, arbitrary
capture syntax, backend metadata-query counts, live hosted databases,
warning identity, other dependency versions or platforms.

## Replay and execution

From the investigated package root:

```sh
Rscript investigation/2026-11-07-expand-grouping/spec/namespace-grid.R
Rscript investigation/2026-11-07-expand-grouping/spec/capture-grid.R
Rscript investigation/2026-11-07-expand-grouping/spec/continuation-grid.R
```

`MARGINPLYR_HUNT_SOURCE` optionally selected a package source copy;
`MARGINPLYR_HUNT_OUTPUT` optionally selected an existing CSV destination,
otherwise R's temporary directory was used. The final recorded runs selected
this evidence directory for output. Short stdout/stderr logs, complete CSVs
and [environment.txt](environment.txt) were retained. `TESTTHAT_PARALLEL=false`
was explicit. Final namespace session 49317, capture session 7938 and
continuation session 10989 reached terminal exit zero. Every owned execution
was collected; no subprocess, database connection or server was started.

The environment was R 4.6.1 on aarch64 macOS, dplyr 1.2.1,
tidyselect 1.2.1, rlang 1.3.0, vctrs 0.7.3, tibble 3.3.1 and pkgload 1.5.3.
No package suite, snapshot suite, source-package build/check or rendering ran.
These development probes did not supply Review-ready evidence.
