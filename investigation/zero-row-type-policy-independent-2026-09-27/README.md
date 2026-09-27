# Independent reassessment of zero-row type guarantees

Investigated: 2026-09-27
Baseline: 9af58d87ca8c49ff267df66d368e1059885f5b38
Status: revised recommendation; not an adopted implementation decision

## Why the comparison was reopened

The user asked for an independent product judgment that disregarded their
earlier maintenance preference. The previous recommendation to withdraw all
three zero-row promises was reassessed against a fair test-maintenance
counterfactual and an actual storage consumer.

The revised recommendation was to retain `.id` and share empty types and extend
numeric empty types to outputs wholly determined by a direct Grouping helper,
including supported across expansions of that same result. Backend-specific
integer/double representations remained acceptable. Arbitrary surrounding
summary expressions and later lazy transformations remained delegated.

## Test consolidation did not require withdrawing the guarantee

The previous -119-line test draft combined three changes: reorganizing tests,
removing zero-type assertions, and reducing matrix cases. Treating all 119
lines as the benefit of withdrawing the promise was an unfair comparison.

| Alternative retaining the existing promises | Net test lines removed |
| --- | ---: |
| Same reorganized cases, restore zero-type checks | 106 |
| Also restore previously exercised code locations | 99 |
| Also retain label/order/plan matrices | 81 |

The final 92-line consolidated SQLite test retained the original label and sort
matrices, one-set/rollup expansions, direct and computed empty-type checks,
populated defaults, a raw-driver control, and the empty-input grand total.
The nonempty all-missing share-prefix tests remained. All focused tests passed;
the two modified files exercised the same 1,414 covr probes as the original
pair. Probe identity did not prove complete behavioral equivalence; shared
fixtures still differed in some details. `test-audit.md` records those limits.

Thus at least 81 lines of the earlier reduction were achievable while retaining
the promises and the original matrices. In the same reorganized case layout,
restoring the actual type assertions cost only 13 lines. The original
implementation draft's -14 remained measured, but adding it to -119 and calling
all 133 lines a reason to withdraw the guarantee was not supported.

`preserved-tests.patch`, `comparison-counts.json`, and
`coverage-comparison.json` preserve the competing draft and small results.

## Empty schemas affected later nonempty values

`schema-use.R` produced SQLite Margin results with `sid` and `mask`, where
`mask = grouping_id(g, h)` took nonempty values 0, 1, and 3. The empty result's
mask was logical at the baseline. Only `sid` and `mask` were saved, so ordinary
summary-column inference did not contribute to the experiment.

The script wrote an empty Parquet file and a populated Parquet file, then read
the directory as one Arrow Dataset. With the empty fragment first and inferred
schema from that fragment, the empty logical column established Boolean type.

| Empty mask stored as | Dataset read with first-fragment schema | Read with schema unification |
| --- | --- | --- |
| bool | FALSE, TRUE, TRUE | Error merging bool and int32 |
| int32 | 0, 1, 3 | 0, 1, 3 |

The second row was an explicit integer conversion of the empty column after
collection. The populated rows were unchanged before writing. The baseline's
`sid` stayed int32 because its existing empty guarantee supplied integer(0).
Running the earlier relaxed implementation also made empty `sid` Boolean;
reading the populated identifiers 1, 2, 3 then produced TRUE, TRUE, TRUE under
the same inference conditions. Both transcripts are saved alongside the probe.

This was a conditional interoperability failure, not a claim that every Arrow
workflow lost data or that marginplyr changed those populated values directly.
Supplying an explicit Arrow schema avoided the problem. The relevant product
cost was requiring callers to repair known package-created column types before
otherwise ordinary storage operations.

Arrow 25.0.1's [open_dataset reference][arrow-dataset] documented that directory
and file-path inputs defaulted to inspecting only the first fragment unless
schema unification was requested. The experiment matched that documented
inference rule. The Boolean casts were measured behavior, not inferred solely
from documentation.

## What the earlier reasoning overstated

- Direct-result scope still had value after collection: R selection and file
  storage consumed those returned types. Delegating later lazy verbs did not
  remove that benefit.
- A guarantee for package-created identity columns remained useful even when
  arbitrary ordinary summaries had delegated types. The storage probe used
  exactly those identity columns.
- The earlier dynamic-name failure showed a defect in predicting output names
  separately from actual expansion. It did not establish that the desired
  guarantee intrinsically required that defective approach.
- Line-count reduction alone did not measure net maintenance: some complexity
  moved to callers, and substantial test simplification was available to both
  choices.

The existing SQLite collector/materializer remained necessary for nonempty
missing shares, source types, and ordering under either policy. This left no
large subsystem that would disappear in exchange for the weaker interface.

## A different helper implementation worked, with a real cost

`bounded-marker.patch` marked only results wholly determined by a direct helper
or a literal across lambda returning one. It read the names dbplyr had actually
expanded, then removed the marker before SQL translation. It did not evaluate
names a second time. General enclosing expressions were left unmarked because
attributes or wrapper calls could otherwise change what an R expression saw.

The probe covered 56 SQLite queries: empty/populated inputs, seven expression
shapes, two sort settings, and presence/absence of an identifier. Column names
and name-expression evaluation counts matched the baseline in all cases. In the
dynamic mixed across case, both evaluated `bump()` three times and the actual
helper column `v_h_3` received an integer declaration. All 28 populated cases
retained direct values/types and computed values; all 56 finite zero-row
collections retained the declared types. Enclosing arithmetic, character
conversion, and local inspection were not assigned helper output types.

This was not a smaller or finished implementation. The measured diff was
104 inserted and 12 deleted lines, net **+92 R lines**, before minimization or
formal checks. It coupled provenance handling to dbplyr's expanded select
representation and added expression-position classification. Activating the
existing collector also changed `tbl_df` to `data.frame` in two sort/plan cases;
that behavior still needed resolution. These were real maintenance costs and
prevented treating the draft as ready to ship. The evidence only established
that the previous independent-name-prediction bug was avoidable.

For scale, the earlier withdrawal draft removed 14 R lines, keeping the existing
promises required no production diff, and this broader helper draft added 92 R
lines. Those were three different scopes. The helper draft's eventual tests and
class correction were not included in 92. Test consolidation was available to
all three policies and was not a saving unique to withdrawal.

## Product judgment and scope

The recommendation was to preserve the existing contract and extend helper
outputs at the package-owned result boundary. A row-count exception would make
a known numeric output's type depend on whether a filter happened to match a
row. The measured storage consequence and the limited fair maintenance savings
outweighed the implementation savings in this independent judgment.

The proposed scope covered `.id`, contextual shares, and columns whose entire
summary result was a direct Grouping helper, including corresponding supported
across lambdas. An enclosing conversion such as
`as.character(grouping_id())` retained its own semantics and was not to be
converted back to numeric. No all-backend integer normalization, arbitrary
summary type inference, synthetic row, or unrequested input query was proposed.

The general rationale agreed with [vctrs' type-stability design][stability]:
callers could reason from input types instead of inspecting data values.
[DBI's fetch specification][fetch] also expected typed zero-row fetches.
Neither source by itself established what marginplyr had to implement; the
package-specific measurements supplied the tradeoff here.

This recommendation superseded the preference-weighted withdrawal recommendation
in the preceding note. The maintainer had not yet adopted either policy, and
the production branch remained unchanged.

## Verification and reproduction

Run `Rscript schema-use.R <baseline-checkout>` from a repository checkout with
the existing optional-dependency guard and Arrow available. The script uses
synthetic data, in-memory SQLite, and temporary Parquet files. `schema-use.txt`
is the baseline result; `schema-relaxed.txt` used the earlier relaxed draft.
Both finished with exit status 0. RSQLite 3.53.3 and Arrow 25.0.1 were measured.

The test-only patch was checked for application to the baseline. The focused
test comparisons passed under covr 3.6.5.9001. This was not a full package check
or evidence that the helper extension was ready to ship.

For the helper probe, apply `bounded-marker.patch` to a disposable copy of the
baseline, then run `Rscript probe.R <baseline-checkout> <patched-copy>/R`.
`syntax-probe.R` accepted the same arguments and checked a `substitute()`-using
local wrapper. The captured `probe.txt` came from the same probes before their
machine-specific paths were replaced by arguments.

[arrow-dataset]: https://arrow.apache.org/docs/r/reference/open_dataset.html
[stability]: https://vctrs.r-lib.org/articles/stability.html
[fetch]: https://dbi.r-dbi.org/reference/dbFetch.html
