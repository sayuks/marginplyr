# Ordinary Margin summary boundary hunt

Investigated: 2026-11-05
Repository snapshot: `0403c9150394cbae21b59f34c75791d0873b8451`

This lane investigated local summary evaluation and ordinary dtplyr summary
construction. The user requested research and issue publication without repairs.
The November 5 summary/Grouping/share investigation, its #1310–#1312 reports,
the merged #1338 repair, and the November 3–4 summaries guided the boundary
selection. All research executions used the source checkout above through
`pkgload::load_all()` with testthat parallel execution explicitly disabled.
No package implementation or package regression test was edited.

## Confirmed finding: generated naming glue reached caller namespace lookup

An unpartitioned plain data frame held one numeric column x with values one
and three. An ordinary unnamed across summed x and used an unpack argument
that evaluated to FALSE through identity. A caller binding for the internal
namespace operator intercepted only the package's private name-checking helper,
returning the name `intercepted`.

Ordinary dplyr returned x = four and invoked that binding zero times. The
Margin call returned intercepted = four and invoked it once. The user had
written no namespace access to marginplyr's private helper. Removing the
interception restored the ordinary name. Literal FALSE, TRUE and string
unpacking controls retained their ordinary results without calling the
interceptor; equivalent unpacking values obtained from a call exposed it.

The public effect was a successful result with a wrong output name. Later
code selecting the caller's intended summary name could consequently fail.
An interceptor returning a protected grouping name was separately probed:
the later grouping-output collision check still refused it. The evidence did
not establish silent grouping-key corruption or an overwrite-check bypass.

### Contract and discriminating hypotheses

ADR 0019's rule and its *isolate generated namespace operations* amendment
required package-generated operations to execute the selected helper while
caller-authored namespace expressions retained ordinary lookup. The summary
interface also delegated ordinary name-value pairs to dplyr. These contracts
made a private generated naming callback attributable to the package rather
than to authored user code.

After the red differential was established, three alternatives were considered:

1. Generated naming glue still resolved its namespace head in the caller's
   environment. This predicted a helper-specific callback invocation, wrong
   names with dynamic unpacking, and no change when that callback delegated.
2. Dynamic unpacking itself changed dplyr's output schema. This predicted a
   difference without the namespace binding.
3. Earlier across selection or aggregation had chosen different values.
   This predicted changed summary values or failure of a literal-unpacking
   control independent of name interpolation.

The 108-case interception matrix and its 108 delegation controls supported
only the first alternative. `local_checked_unpack_arg()` and
`local_checked_across_names_arg()` produced strings containing the qualified
private helper call; glue evaluated that string in its naming environment.
Those strings were outside the generated-AST function-value isolation added
for #1311. The observed numbers stayed correct while the names changed.
The `authored-namespace.R` control additionally verified that a namespace
expression deliberately written in the caller's name template still used the
caller binding in both verbs: each returned x_17 = six, with two total authored
callback invocations.

### Matrix and replay

Run from the tested source root:

```sh
Rscript investigation/2026-11-05-summary-grouping-share-greedy/summary/glue-namespace.R
Rscript investigation/2026-11-05-summary-grouping-share-greedy/summary/glue-namespace.R --assert
Rscript investigation/2026-11-05-summary-grouping-share-greedy/summary/authored-namespace.R
```

Observation mode asserted the dated defect, successful literal controls and
correct delegating controls; it exited zero. Contract mode required the
ordinary name and zero generated-callback invocations and intentionally exited
one. It was a research signal, not an installed regression test.

`glue-matrix.csv` recorded 108 intercepted combinations: three eager input
classes (plain data.frame, tibble and data.table), three plan roles (no key,
fixed key and rollup), six unpack spellings (three dynamic and three literal),
and default/custom names. All 54 dynamic cases produced wrong names; all
54 literal controls agreed. Repeating all 108 combinations with the helper
binding delegated to its actual namespace function agreed exactly with the
ordinary-dplyr branch oracle. The oracle for a rollup combined ordinary
per-key and unpartitioned summaries, adding the typed-missing margin key.
These were finite overlapping examples, not independent random samples.

### Duplicate comparison

The all-state issue queries were `"local_checked_glue_name"` and
`"namespace" "glue"`; the all-state PR query was `"namespace"`. The private
helper query returned no issues. The namespace/glue query returned unrelated
#41 and #223. The PR query included merged #1338 and the earlier dtplyr ratio
repair #1219. Results were limited to 30 per query. Issue #1311's complete body
and comments and #1338's body and comments were read.

[#1311](https://github.com/sayuks/marginplyr/issues/1311) and merged
[#1338](https://github.com/sayuks/marginplyr/pull/1338) concerned generated
selection captures, runtime selection heads and nested-mask thunks. Those
repairs were present at the tested SHA and their regression tests used AST
calls carrying selected function values. The distinct remaining witness was
a generated glue string evaluated later for a dynamic unpack/name template.
The proposed follow-up therefore covered runtime naming and preservation of
caller-authored template namespace expressions. It needed no blocker.
The parallel share lane investigated a different late dtplyr ratio boundary;
this lane made no claim about that implementation.

## Additional bounded negative observations

| Probe | Completed scope | Observation |
| --- | --- | --- |
| `discover.R` | 42 local pairs | Frame/scalar/unspecified combinations, packed and empty values, matrices, lists, dates, classes, across and pick forwarding produced no unmatched successful result/error outcome. |
| `names-probe.R` | 312 local pairs | Thirteen name-template shapes crossed four function forms, three unpack values and named/unnamed summaries; no unmatched result/error outcome. |
| `special-names.R` | 380 local pairs | Thirty-eight measure names crossed five reading/selection forms and keyed/unkeyed calls; 378 pairs agreed. Two bare share-helper symbol uses hit the documented reserved-spelling refusal and were excluded from findings. Explicit pronoun and selection reads stayed valid. |
| `dtplyr-probe.R` | 116 live dtplyr pairs | Twenty-nine expression forms crossed named/unnamed and keyed/unkeyed calls. No unmatched collected result/error outcome. |

The 850 discovery comparisons treated paired errors as matching outcomes and
did not assert error-message/class identity. Successful discovery values were
compared without arbitrary attribute equality, consistent with ADR 0016's
promise boundary. Their purpose was to find semantic disagreements, not to
certify every diagnostic or custom class. The naming finding's dedicated
matrix instead required exact result equality for successful controls.

The probes covered only the written expressions, finite input data and
installed dependency versions. They established no guarantee about arbitrary
closures, custom vctrs methods, hosted backends, all syntax, concurrency,
interruption, or other dependency versions. The complete package suite was
not used as a hunting loop. Research scripts deliberately exercised errors
and typed/empty outcomes without changing implementation.

## Environment and disposition

`environment.txt` recorded R and package versions for this lane. Every owned
R execution reached a terminal exit before handoff. The coordinator owned
current-main confirmation, independent replay, final issue publication and
the repository's Review-ready gate. The standalone probes and this note were
research artifacts only; they asserted neither repaired behavior nor a full
package-check result.
