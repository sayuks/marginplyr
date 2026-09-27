# Zero-row type policy: evidence index

Investigated: 2026-09-28
Baseline: `9af58d87ca8c49ff267df66d368e1059885f5b38`

Three comparisons on 2026-09-27 on a throwaway branch preceded the maintainer's adoption.
This index retained their evidence and distinguished superseded interpretations
from measured results. The runnable scripts, transcripts, and competing patches
were preserved outside main at the pinned snapshots below.

## Evidence sequence

| Investigation | What it established | Interpretation later corrected |
| --- | --- | --- |
| [Initial comparison][initial] | Withdrawing empty promises removed 14 R lines; a direct-call-only extension added 3; both retained populated values/types in the measured matrix, while the extension exposed a class change | The small extension did not cover across, and the preference for withdrawal did not fully assess downstream schemas |
| [Follow-up][followup] | A reorganization removed 119 test lines; an across extension added 50 R lines but separately predicted dynamic names incorrectly | The 119 lines included savings available without withdrawing promises; the naming defect was specific to that prototype |
| [Independent reassessment][independent] | An empty Boolean schema affected later populated identifiers; 81 test lines could be removed while retaining guarantees and original matrices; actual expanded names avoided the prior defect | The withdrawal recommendation was reversed; none of the prototypes became a production implementation |

The independent archive also added dated revision pointers to its preceding
reports. Historical recommendations remain readable as the conclusions reached
then; adoption is recorded separately in ADR 0033.

## Decisive measurements and their limits

The storage probe selected only `.id` and `grouping_id()` from SQLite Margin
results. With an empty Parquet fragment providing the inferred Boolean schema,
Arrow read populated masks `0, 1, 3` as `FALSE, TRUE, TRUE`. Under the relaxed
identifier prototype, populated `.id` values `1, 2, 3` similarly became
`TRUE, TRUE, TRUE`. Schema unification failed on bool/int32 instead. Converting
the empty columns to integer before saving preserved both sets of values.
[Arrow's Dataset reference][dataset] described first-fragment inference as the
default for directory inputs. The measured cast behavior came from the probe,
not from an inference about the documentation.

This required the specified schema-inference conditions; it was not a claim
that every Arrow workflow lost values. An explicit schema avoided the problem.
Ordinary empty-plus-populated `bind_rows()` succeeded, so a general claim that
empty logical helpers break binding or joins was not supported.

The fair test comparison retained existing `.id`/share guarantees and original
label/order/plan matrices while removing 81 lines. The focused tests passed and
executed the same 1,414 covr probes as the original pair. Probe identity did not
prove all behavioral conditions equivalent; the archive recorded fixture
differences. In the same reorganized case layout, type assertions cost 13 lines.
The earlier 133-line implementation/test reduction therefore was not wholly
attributable to withdrawing the type promise.

The bounded helper-provenance draft added 104 and removed 12 R lines, net 92.
Across 56 SQLite query cases, actual output names and name-evaluation counts
matched baseline. All 28 populated cases retained direct and computed column
values/types; all 56 finite zero-row collections retained declared types. Two
cases changed the collected class from tibble to data.frame, which remained an
implementation gap. Neither production-ready line count nor final test cost was
established. Backend-specific numeric helper representations remained acceptable;
the experiments did not justify a universal R-integer cast.

## Reproduction and validation scope

The [independent archive][independent] contains `schema-use.R`, both storage
transcripts, the retained-guarantee test patch and audit, and the bounded-marker
patch with query and syntax probes. Its README gives replay arguments. All
patches are throwaway alternatives against the stated baseline. Read its
limitations before choosing a mechanism.

Measured versions included RSQLite/SQLite 3.53.3, Arrow 25.0.1, dbplyr 2.6.0,
and covr 3.6.5.9001. Both repository linters passed on the evidence branch.
These investigations did not run the full package Review-ready check or
establish cross-version or cross-platform compatibility.

[initial]: https://github.com/sayuks/marginplyr/blob/881fecd02d8c520c9ef4e68ff846a85d6d556ceb/investigation/zero-row-type-policy-2026-09-27/README.md
[followup]: https://github.com/sayuks/marginplyr/blob/bfb717a811ffa4f2fdd1440d6ef473b94b990481/investigation/zero-row-type-policy-followup-2026-09-27/README.md
[independent]: https://github.com/sayuks/marginplyr/blob/0cb2fc7a5b81574790571120da6dc726391e5562/investigation/zero-row-type-policy-independent-2026-09-27/README.md
[dataset]: https://arrow.apache.org/docs/r/reference/open_dataset.html
