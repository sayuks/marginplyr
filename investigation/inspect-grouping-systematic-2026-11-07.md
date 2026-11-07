# Systematic Grouping inspection contract investigation

Investigated: 2026-11-07
Tested main: `a79def28240a5d213d3b9c1a19da93c313820b51`

A new Git worktree and branch started from main after fetching origin. The
fetched revision matched local main. This investigation exercised
`inspect_grouping()` and its shared Grouping specification preparation, with
independent ordered-set expectations and native selection controls. It found
one previously unreported DuckDB selection defect, filed as [#1481][new]. No
package implementation or package tests were changed. Reproduction programs
and execution output were kept in temporary investigation directories.

## Contract and prior evidence

The public references for [inspection][inspection], [constructors][constructors]
and [Grouping identity][helpers] supplied the expected behavior. Relevant
decisions included ADR 0002 (one typed snapshot), 0005 (local errors before
reads), 0007 (captured expressions), 0008 (kind rules and evaluation budgets),
0009 (occurrence and absence identity), 0013 (ordinary local tibble), 0020
(lazy-input reads), 0026 (ambiguous nested names), 0029 (Mutable steps) and
0032 (SQL identifiers). The [identity guide][guide] supplied the distinction
between Grouping-plan order and result order.

The recent [inspection investigation][prior], [receiver-dispatch
investigation][receiver] and [Grouping proxy investigation][proxy-prior],
plus the earlier [identity-boundary investigation][identity-prior], supplied
case families. Their historical counts were not counted as executions here.
The tested main already included merged [PR #1479][integration]. Full related
Issue bodies and comments were read, including #1277, #1434, #1439, #1461,
#1462, #1463, #1464, #1465 and #1469; the five immediate inspection findings
had no comments and were closed.

| Historical candidate | Main replay and disposition |
| --- | --- |
| #1461: DuckDB dynamic PIVOT evaluated rows during metadata preparation | The prior contract replay passed. The dynamic query received a Package condition before the sequence advanced; static and name-only controls passed. Fixed. |
| #1462: DuckDB metadata wrapper changed captured column identities | Dimension, fixed-key and overlap witnesses received Package conditions for the schema mismatch. Fixed. |
| #1463: a tibble names method was skipped | Arrow, DuckDB and SQLite direct/wrapped receiver controls agreed. Fixed for that names-method witness. |
| #1464: exponential duplicate rejection exceeded a 96 MiB heap | The prior contract replay passed under the same heap bound. Twenty repeated cube units and twenty product factors raised duplicate Package conditions in 0.016 and 0.055 seconds respectively; small keep/error and large drop controls passed. Fixed for the measured families. |
| #1465: Arrow reader normalization displaced invalid Option arguments | Both invalid arguments received their argument-specific Package condition and the reader retained its two rows. Fixed. |
| #1469: dtplyr's data.table selection proxy was skipped | The report and merged repair concerned the data.table receiver. The new tibble receiver witness below still reproduced after that repair. It was separated rather than reopening or duplicating that ticket. |

## Confirmed new defect: a DuckDB selection proxy was skipped

The minimal fixture had two dummy rows: character a held east and west;
integer b held 1 twice. It was copied into a temporary table in an in-memory
DuckDB connection. A scoped, registered `tidyselect_data_proxy.tbl_df` method
returned the same tibble with b before a. It preserved column names, values,
types and rows and depended only on its input. No changing answer or call-stack
test was involved.

Native tidyselect on the corresponding zero-row tibble chose b for the first
active name, `tidyselect::peek_vars()[1L]`. The public inspection results were:

| Position | Expected | Observed direct form | Wrapped control |
| --- | --- | --- | --- |
| `.by`, with list output | fixed b | fixed a | `identity()` around the same selector fixed b |
| A Grouping dimension beside fixed a | included b | overlap refusal naming a | matched literal b |

The false dimension refusal reproduced through all five constructors. Wrapped
and literal b beside fixed b retained the genuine-overlap refusal.
Four first-name spellings covered `peek_vars()` and
`names(tidyselect::peek_data())`, each with `[` and `[[`. Of 48 DuckDB
inspection calls, all 24 direct forms disagreed with the native expectation
and all 24 wrapped forms matched it. This changed results downstream:
fixed-key count summarization returned two a groups with counts 1 and 1 for
the direct spelling, versus one b group with count 2 for the wrapped spelling.

The names-only classification bypassed proxy dispatch on the actual tibble
receiver. The dtplyr classifier already checked registered proxy methods;
the tibble classifier checked names methods. This was evidence for the
boundary, not a proposed implementation requirement.

Inspection in this witness did not read source rows. A sequence expression in
the lazy source remained uncalled through inspection, every observed tibble
proxy had zero rows, and an explicit subsequent sequence call returned 1,
calibrating that witness. Source bytes and the temporary table stayed
unchanged. An independent replay from an archive of the tested main confirmed
the mismatch and all five constructor controls. Observation mode exited 0;
the desired-contract assertion exited 1 on the incorrect fixed key.
The independent minimal audit also recorded zero owned metadata statements
for the direct spelling and one for the wrapped spelling. Sent queries were
treated as construction records, not proof of database receipt.

[#1481][new] records expected behavior, reproduction conditions, actual
observations, acceptance criteria and dependencies. The failing case requires
marginplyr and its Imports, plus DBI and DuckDB; `pkgload` is a source-loading
development dependency only. Arrow, RSQLite, dtplyr and data.table were controls,
not prerequisites for that failure. The ticket is one independently verifiable
behavior slice, labelled `bug` and `ready-for-agent`, with no blockers or
required prefactoring. The user's delegation covered its split and publication.

All-state Issue and PR searches included `inspect_grouping`,
`"tidyselect_data_proxy"`, `"proxy" "tbl_df"`, `"peek_vars"`, and proxy/context
and proxy/Grouping combinations. The closest reports were #1463's names
dispatch and #1469's dtplyr proxy dispatch. Their complete bodies, comments
and PR #1479 established the different receiver boundaries. A final open
Issue/PR refresh found no matching report before publication. The created
Issue was read back as open with both labels.

## Finite coverage and results

Each row below describes its own measurements; they are not interchangeable
test or query counts, and overlapping controls are not claimed to be unique
guarantees.

| Probe | Completed evidence |
| --- | --- |
| Independent ordered-set algebra | 998 families combining sets, unions, overlapping/composite rollups and cubes, and products; all three duplicate policies and both formats on populated, empty and grouped inputs. Each carrier produced 4,516 exact full plans and 1,472 expected duplicate refusals: 17,964 comparisons total. All six output columns, ordinary ungrouped tibble class, occurrence order, fixed keys, bits and identifiers matched. |
| Local selection and admission boundaries | 231 assertion groups, including 176 native selection comparisons over 22 selectors and tibble/data.frame/data.table/empty input in fixed-key and dimension positions. Empty specifications and zero-column inputs, widths 0/1/2/30/31/32/64/96, invalid options, constructor arguments, nested ambiguity, renaming, overlap, missing columns, rowwise and `.drop = FALSE` refusals passed. |
| Symbolic width and exact names | Seven complete plans at widths 0/1/30/31/32/64/96, with 32 occurrences, independently checked full inclusion, first/middle/last omission and the empty set. Named bits stayed complete beyond 31 dimensions while numeric IDs became `NA_integer_`. Three additional occurrences preserved comma, equals, backtick, newline, Japanese, parentheses, pronoun-like and slash names in list output. |
| Ordinary selector equivalence | 22 selectors in all five constructors and `.by`: 132 public calls matched native tidyselect selection or refusal. Included positions, negation, helper arguments, ordering, pronouns, same-name selections, predicates, parentheses, unusual names and context-name lookups. |
| Plan/execution identity correspondence | 288 comparisons across 12 specifications, drop/keep, populated/empty data, tibble/data.frame/data.table and grouped/explicit-fixed input. Output `.id` and bare `grouping_id()` matched the inspected occurrence, fixed bits were zero, independent distinct-key row counts and occurrence-weighted source counts matched. |
| Additional root admission controls | 24 invalid Option argument values and six unsupported/grouping-input controls produced the expected Package conditions; grouped inputs stayed unchanged. |
| Typed backend matrix | Zero/three-row input and six selection cases across local, immutable/Mutable dtplyr, Arrow, DuckDB and SQLite: 60 exact plans and 12 expected SQLite predicate refusals. SQLite recorded no owned SQL for those refusals; DuckDB acquired one metadata statement in each of its 12 cases. |
| Derived dtplyr metadata | Five type predicates over root, filter, projection, rename, relocation, order, grouping, arithmetic, alias and cached steps: 49 exact plans and one expected overlap refusal. The upstream row callback ran zero times; Mutable source bytes were unchanged. |

Grouping specification objects and caller inputs retained their serialized
bytes in the algebra and local/execution probes. Duplicate policies were
compared as ordered occurrences, not merely as sets of sets. Text output was
checked as display; only list output was required to preserve exact separator
names. Empty source rows did not remove the structural Grand total set from
inspection. Grouping-plan order was not used to assert an unspecified physical
order of a Margin result.

## Rejected hypotheses and corrected controls

The final proxy carrier matrix completed 144 calls with 24 confirmed direct
DuckDB mismatches and 120 expected matches or genuine-overlap refusals. The
provisional proxy carrier matrix used b as a uniform expectation across
DuckDB, Arrow and RSQLite. That was wrong for RSQLite: its canonical receiver
was the lazy table itself, not a tibble, so the scoped tibble method did not
apply. Its 48 calls correctly selected a or refused genuine overlap; a
registered lazy-table proxy control made native, direct and wrapped selection
agree on b. Arrow's 48 calls agreed on b with its schema proxy. The provisional
72-failure count was discarded; the confirmed defect comprised DuckDB's 24
direct calls only.

A stateful local proxy alternating its column order was called a different
number of times by inspection and native selection. It was not filed: no
general arbitrary S3 callback-count guarantee was established, and the method
did not return a stable answer. That exploratory result did not establish a
violation of the specified captured-argument budgets.

An early dtplyr oracle explicitly collected the original Mutable graph and
changed the source through native backend behavior. The final oracle collected
an isolated deep clone instead. Native tidyselect's refusal of binary
subtraction, zero-length fixture name generation and tibble's refusal of a
`..1` fixture were also separated from package defects. Only corrected,
terminally completed probes supplied the final counts.

## Environment, execution and limits

The environment was arm64 macOS, R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
tidyselect 1.2.1, rlang 1.3.0, tibble 3.3.1, vctrs 0.7.3, dtplyr 1.3.3,
data.table 1.18.6.1, DBI 1.3.0, DuckDB 1.5.5, RSQLite 3.53.3 and
Arrow 25.0.1. Every database used was created for these probes with dummy
data; no existing user database or hosted store was queried. Main was fetched
again before Issue publication and still named the tested SHA.

Temporary programs and logs were kept under
`/private/tmp/marginplyr-inspect-plans-20261107`,
`/private/tmp/marginplyr-inspect-systematic-backends`,
`/private/tmp/inspect-selection-systematic` and
`/private/tmp/inspect-systematic-evidence`. The final algebra, edges, width,
backend, ordinary-selector, identity and observation runs exited 0. The
desired-contract reproduction exited 1 deliberately. All original execution
handles were collected to terminal completion; no in-progress run was used as
a result. These paths are temporary evidence, not repository replay assets.
For identification, the final minimized `duckdb-proxy-repro.R` SHA-256 was
`408ceefc62efd7c07eacabc6511d4821c26615f76626494d3573715a8961612a`.
The Issue's stepwise fixture and public calls record the minimal reproduction
without committing the temporary programs.

Other operating systems and dependency versions, live PostgreSQL/SQL Server,
hosted Arrow datasets, arbitrary receiver methods, unbounded/deep expression
graphs, all possible competing condition fields, general memory ceilings and
arbitrary value-dependent SQL preparation effects were not established. The
96 MiB duplicate witnesses were finite regressions, not a general resource
bound. No security-verification claim was made. One confirmed unreported
defect was filed; no other hypothesis was promoted to a defect.

## Submission effects

The branch adds this dated note only. Its consumers are investigation readers;
it introduces no executable package/check consumer. Package code, tests,
examples, vignettes, generated documentation, installed files, dependencies,
build rules and verification tooling are unchanged. The cumulative comparison
baseline is the merge-base with fetched main, the tested SHA above. These
effects justify Review-ready `source`; no ancestor's full result is inherited.
Package build/check, complete suite and snapshots are omitted. A research-only
record adds no user-visible behavior change and requires no NEWS entry.
The submitted SHA, current-main compatibility, mandatory checks and separate
Standards/Spec review records belong on the PR. CI monitoring was outside the
requested work.

[new]: https://github.com/sayuks/marginplyr/issues/1481
[integration]: https://github.com/sayuks/marginplyr/pull/1479
[inspection]: ../R/inspect-grouping.R
[constructors]: ../R/grouping-spec.R
[helpers]: ../R/grouping-context.R
[guide]: ../vignettes/grouping_identity.qmd
[prior]: inspect-grouping-bughunt-2026-11-07.md
[receiver]: expand-grouping-receiver-dispatch-bughunt-2026-11-07.md
[proxy-prior]: summary-grouping-share-contracts-2026-11-07.md
[identity-prior]: grouping-plan-identity-boundaries-2026-10-01.md
