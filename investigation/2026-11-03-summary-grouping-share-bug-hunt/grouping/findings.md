# Grouping selection warnings converted to errors passed the read boundary

Investigated: 2026-11-03
Repository snapshot: `4b34fdecb2d8a41034b3d9b5d5af8e51592555e1`
Scope: Grouping specification constructors, structural preflight, selection resolution, and inspection

This lane established one new residual preparation defect at the assigned main
snapshot. It changed no package implementation or package regression test and
performed no tracker write. The retained R scripts loaded that checkout through
`pkgload::load_all()`. The observations below were focused research, not a
complete-suite or Review-ready result. Later-main confirmation and publication
were left to the coordinator.

## Confirmed finding: `warn = 2` failures were suppressed until after metadata

Suggested classification: P2, one shared-preparation ticket. The same cause
affected all five constructors and all five public consumers. It caused an
unnecessary DuckDB metadata query or replaced the original selector error with
a dtplyr refusal asking the caller to collect otherwise valid source data.

The minimal DuckDB trigger was:

```r
options(warn = 2)
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
remote <- dplyr::copy_to(con, data.frame(a = 1L), "source")
inspect_grouping(remote, .grouping = grouping_set(one_of("absent")))
```

Expected: tidyselect's converted-warning error before contacting the input.
Observed: one actual `DBI::dbSendQuery()` invocation with
`SELECT * FROM source LIMIT 0`, followed by:

```text
ℹ In argument: `one_of("absent")`.
Caused by error:
! (converted from warning) Unknown columns: `absent`
```

No typed selector or second Grouping argument was necessary for the DuckDB
failure. The retained reproducer used table name `warning_preflight_minimal`
and traced the actual DuckDB DBI method. It did not infer execution from a Sent
query record. It disconnected the connection and removed its trace on exit.

A supported derived dtplyr input exposed the condition replacement:

```r
data <- tibble::tibble(a = 1L, b = "x")
step <- dtplyr::lazy_dt(data) |> dplyr::mutate(b = "z")
inspect_grouping(
  step,
  .grouping = grouping_set(one_of("absent"), where(is.numeric))
)
```

With `warn = 2`, this reported `Can't safely determine column metadata` and
advised collection. Direct typed tidyselect and the local public call instead
reported the converted-warning error. Name-only dtplyr inspection used its
existing no-snapshot path and retained that error; the four Margin verbs did
not have that inspection fast path, so they also demonstrated the replacement
with a names-only later selection.

### Contract and cause

- `base::options()`'s installed help, section `warn`, specified that zero was
  the default and that two converted warnings to errors. The inspected excerpt
  was retained in `options-warn.txt`.
- `design/adr/0005-reject-local-errors-before-backend-reads.md` required
  preparation errors decidable from the call and local metadata before backend
  contact. This selection was already classified as names-only.
- `design/adr/0015-separate-package-conditions-from-internal-invariants.md`
  required External conditions to propagate unchanged. `CONTEXT.md`'s
  *External condition* and `R/marginplyr-package.R`'s *Errors and warnings*
  supplied the same condition boundary.
- `R/grouping-plan.R`'s `prepare_grouping_plan()` wrapped both discarded
  preflight forms in `suppressWarnings()`. That prevented R's default warning
  handler from performing its `warn = 2` conversion. The later canonical
  selection ran only after `grouping_selection_proxy()` or
  `dtplyr_selection_proxy()`.
- The same module's `discarded_pass_verbosity()` preserved a caller's
  `lifecycle_verbosity = "error"` specifically to keep a locally decidable
  deprecation failure before the read. `warn = 2` was not accounted for by
  either that helper or the suppression around it.

The diagnostic hypotheses were separated as follows:

| Hypothesis | Distinguishing prediction and disposition |
| --- | --- |
| Discarded-pass warning suppression erased the early failure | Confirmed: names-only classification was true, ordinary warning modes succeeded, `warn = 2` failed after the proxy, and the separately preserved lifecycle error failed before it |
| The selector was wrongly classified as type-dependent | Refuted: `preflight_grouping_spec()` returned `name_only = TRUE` for both demonstrated first selections |
| The mismatch was a dtplyr-only metadata limitation | Refuted: actual DuckDB query invocation and local proxy instrumentation demonstrated the shared ordering boundary |
| The old selector was invalid in ordinary warning modes | Refuted: default `warn = 0` and `warn = 1` succeeded with the ordinary warning; the same fixed `.by` selection under `warn = 2` failed without a query |

The supported deprecation spelling `.data$a`, with
`lifecycle_verbosity = "warning"` and `warn = 2`, reproduced the same failure.
It was not a separate ticket: `quiet` withholding and warning suppression both
removed the warning-induced failure from the discarded pass.

### Completed matrices

`warning-preflight.R` crossed two warning-producing selections, names-only
versus typed later selections, five constructors, and local/dtplyr/DuckDB
inputs. Each local/dtplyr shape used inspection and all four Margin verbs;
DuckDB used inspection, summarization and expansion. SQL nesting was excluded
because input admission refused it before Grouping preparation.

| Completed public calls | Result |
| --- | --- |
| 100 local calls | All returned the original converted-warning error |
| 60 DuckDB calls | All invoked one input metadata query before that error |
| 10 names-only dtplyr inspections | Original error, using the existing no-snapshot path |
| 90 other derived dtplyr calls | Original error replaced by the metadata refusal |
| 12 fixed/variable controls, two warning modes and three inputs | All fixed-key `warn = 2` failures invoked no query; ordinary warning-mode controls succeeded |

The 260-call matrix therefore demonstrated 150 affected calls and 110 matching
controls, plus the separately counted twelve controls. These were finite,
overlapping research cases rather than a claim about all possible inputs.

`warning-mode-controls.R` completed twelve local instrumented calls. It verified
default `warn = 0`, immediate `warn = 1`, conversion under `warn = 2`, and
lifecycle `quiet`/`warning`/`error` modes. Default-mode unknown-column selection
succeeded and warned; warning conversion reached the typed proxy first;
`lifecycle_verbosity = "error"` failed before that proxy in all three R warning
modes. The proxy trace was evidence about invocation order on a local input,
not a claim of database contact. Only the separate actual DBI trace established
the latter. Nonmuffling observers could see more than one lifecycle condition
signal; those signals were not classified as duplicate package warnings.

An initial matrix used `a` as the later names-only selection, which duplicated
the `.data$a` dimension in three constructor forms. That harness-only
structural-error interference was removed by using distinct column `b`.
The retained completed matrix had no such duplicate-family errors.

## Duplicate search and relation to prior work

The full body/comments of #346 and PR #353, and #1090 and PR #1120, were read.
The selected all-state issue search also included #342, #825, #889, #995,
#1039, #1139 and #1144. The relevant historical investigation notes were
`summary-spec-share-bughunt-2026-10-29.md`, the October 29 exhaustive Grouping
lane, `summary-spec-total-share-bughunt-2026-10-30.md`,
`summary-grouping-total-share-bughunt-2026-10-31.md`, and
`summary-grouping-share-bughunt-2026-11-02.md` with its Grouping lane.

The 2026-11-02 fixed-key-overlap finding was already repaired at the tested
snapshot: `validate_leading_selections()` received `.by` and applied
`validate_grouping_overlap()`. It was not counted again.

This finding differed from #1090's invalid leading selectors and #1139's
already-known fixed-key overlap: the ordinary selection was valid and warned,
and R's caller-selected warning conversion made it fail. It also differed from
#346/#353's deprecation-mode fix. PR #353's published review explicitly
identified and fixed a lifecycle `"error"` version of the same ordering
obligation; that repair remained present and passed the instrumented controls.
The unhandled R `warn = 2` route was the residual boundary.

The recorded all-state queries were:

```text
gh issue list --state all --limit 500 --search 'grouping selection' --json number,title,state,url,body
gh issue list --state all --limit 100 --search 'warn=2' --json number,title,state,url
gh issue list --state all --limit 100 --search 'discarded warning' --json number,title,state,url
gh issue list --state all --limit 100 --search 'one_of warning metadata' --json number,title,state,url
gh issue list --state all --limit 100 --search 'warning preflight' --json number,title,state,url
gh pr list --state all --limit 100 --search 'warn=2 grouping' --json number,title,state,url
```

`selection-issue-search.json` and `duplicate-warning-search.json` retained
queries and matching issue/PR titles. The closest matching reports were the
closed work above; none of the inspected reports described this R warning-mode
trigger. #754 and PR #759 concerned later summary errors competing with
buffered-warning replay, after execution rather than Grouping preparation.
No tracker mutation was performed by this lane.

## Replay and limits

Run from the tested checkout root:

```sh
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/grouping/reproduce-warning-preflight.R
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/grouping/reproduce-warning-preflight.R --assert
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/grouping/warning-preflight.R
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/grouping/warning-preflight.R --assert
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/grouping/warning-mode-controls.R
```

Both report modes exited zero. Both `--assert` modes intentionally exited one
on the unfixed snapshot; the minimal one failed on `converted(typed) is not
TRUE`, and the matrix failed on `all(results$converted) is not TRUE`. These
assertions required the original error and no DuckDB input metadata query, so
they could distinguish a repaired boundary. All owned executions were
collected at terminal completion before another reproduction started.
Adjacent CSVs and logs retained the completed outcomes. `jarl check` on this
artifact directory passed.

The environment was R 4.6.1 (2026-06-24), aarch64 macOS; dplyr 1.2.1,
tidyselect 1.2.1, rlang 1.3.0, dtplyr 1.3.3, data.table 1.18.6.1,
DuckDB 1.5.5, dbplyr 2.6.0, DBI 1.3.0 and pkgload 1.5.3. `environment.txt`
retained the platform and package versions. No live hosted database, different
dependency version, arbitrary warning-handler transfer, concurrency,
interruption, unbounded specification, build, rendering, or complete package
suite was established. The examples installed no outer warning handler that
muffled the warning; such a handler could deliberately prevent its conversion
and was outside the demonstrated failure.
