# Focused mutation testing of margin semantics

Investigated: 2026-09-12

## Question

Can the existing marginplyr test suite detect meaningful implementation errors
in Grouping-plan expansion, denominator mapping, and share value rules? The
campaign sought useful survivors, not the highest possible mutation score.

Production code and the durable test suite were not changed. Tooling, mutated
copies, JSON reports, and temporary validation tests lived under
`/private/tmp`.

## Repository and baseline

The source state was commit `bc2d33ff7251d7c03fdacd066cc2097c1ca23f96`
with a clean working tree. The observed environment was:

- R 4.6.1 on Darwin 25.6.0 arm64;
- testthat 3.3.2, pkgload 1.5.3, dplyr 1.2.1, and dbplyr 2.6.0;
- Arrow 25.0.1, dtplyr 1.3.3, DuckDB 1.5.5, and RSQLite 3.53.3.

`DESCRIPTION` selects testthat edition 3. `tests/testthat.R` runs
`testthat::test_check("marginplyr")`. The development boundary in
`tools/review-ready-check-lib.R` runs:

```r
testthat::test_local(".", reporter = "summary", stop_on_failure = TRUE)
```

The same command passed before mutation. Its elapsed time was 79.356 seconds.
The repository additionally separates source-tree tests from source-tarball
`R CMD check`, depends-only checks, optional-backend jobs, and structural suite
coverage in `.github/workflows/R-CMD-check.yaml` and
`.github/workflows/release-matrix.yaml`. A green local suite therefore does not
replace the release matrix, but it is the correct baseline for this campaign.

## Approach

### Tool choice

The primary-source comparison is recorded separately in
[`investigation/r-package-mutation-testing-tools.md`](r-package-mutation-testing-tools.md).

The campaign used `muttest` 0.3.0, installed only in
`/private/tmp/marginplyr-mutation-lib`. It was not added to `DESCRIPTION`.
`muttest_plan()` allows source files and operators to be named explicitly, and
the resulting data-frame plan can be restricted by source location before any
mutant runs. `PackageCopyStrategy(symlink = FALSE)` gives every mutant its own
disposable package copy, and the JSON reporter preserves source location,
operator, replacement, and result.

`mutator` was evaluated but not selected. Its coverage-guided subprocess model
and independent hang result are useful for broad exploratory sampling, but its
public interface generates a fixed mutator set across package files and then
narrows by excluded files or sampling. That is a weaker fit for the requirement
to declare a small semantic scope first and account for every mutant in it.
Its result path also treats several non-test errors as killed, whereas this
campaign needed ERROR to remain separate.

### Mutation scope

The selected code was:

- `R/grouping-plan.R`: `compile_grouping_spec_impl()`,
  `expand_grouping_family()`, `resolve_grouping_set()`,
  `expand_grouping_sets()`, `resolve_grouping_units()`, `expand_rollup()`,
  `expand_cube()`, and `expand_grouping_product()`;
- `R/share.R`: `grand_total_occurrence_ids()`, `total_set_ids()`, and
  `parent_set_ids()`;
- `R/share.R`: the R-side value expression built by
  `apply_joined_shares()`.

These functions own grouping-set membership and order, duplicate handling,
Grouping masks, Grand total set discovery, strictly-less-detailed parent
selection, own-denominator rows, and missing/zero share behavior. ADR 0008,
ADR 0010, and ADR 0017 make these observable semantics rather than incidental
implementation details.

The operators were:

- comparison replacement (`==`/`!=`, `<`/`<=`/`>`, `>`/`>=`/`<`);
- logical replacement (`&&`/`||`, `|`/`&`);
- condition negation;
- numeric literal increment and decrement;
- unary-negation removal where the target block contained ordinary negation.

Unary-negation removal was deliberately excluded from the tidy-eval-heavy share
value block because it also matches each `!` in `!!` injection. Those mutants
mostly test parser/metaprogramming artifacts, not plausible Boolean errors.
Index mutators were not layered on: numeric-literal mutations already exercised
the consequential `[[1L]]` boundaries, while a broad index pass would mostly
produce out-of-range errors rather than semantic alternatives.

### Isolation and test execution

Each mutant was written only into a deep disposable package copy. The original
file was never rewritten, so interruption required no restoration step.

The first pass used existing tests related to the mutated module:

- Grouping-plan mutants:
  `grouping-plan`, `grouping-interface`, and
  `metamorphic-margin-semantics`;
- share mutants: `share`.

A focused-test kill is necessarily a full-suite kill. Every focused SURVIVED or
ERROR was then rerun against all of `tests/testthat`. The final SURVIVED label
therefore means the complete baseline suite passed with the mutant.

Four workers were used. The intended focused timeout was 60 seconds and the
full-suite timeout 150 seconds. `muttest` starts a mirai timeout when a task is
submitted rather than when a queued worker begins it, so the first large batch
reported 19 queue-wait timeouts. The affected mutants were rerun in a smaller
batch with queue allowance; all received ordinary outcomes. A timeout was never
accepted as a mutation result.

## Results

| Result | Count |
|---|---:|
| Total mutants | 53 |
| KILLED | 50 |
| SURVIVED | 3 |
| Final TIMEOUT / ERROR | 0 |
| TEST GAP | 1 |
| EQUIVALENT MUTANT | 2 |
| IRRELEVANT / UNREACHABLE | 0 |
| INCONCLUSIVE | 0 |

The reference mutation score is 50 / 53 = 94.3%. It is not the quality
judgment: one of the three survivors exposed a real documented-contract gap,
while the other two cannot be usefully killed.

## Survivor triage

### `R/grouping-plan.R:910`, `> 0L` to `>= 0L`

Classification: **EQUIVALENT MUTANT**.

The only newly entered state is `length(dimensions) == 0L`. In that state
`masks` has zero columns, and assigning the zero-length value
`as.integer(!dimensions %in% normalized[[i]])` to each zero-column row is a
no-op. The original and mutant matrices are identical. Empty
`grouping_set()` and empty `grouping_spec()` are valid and compile to the empty
grouping set; ADR 0008 and `test-grouping-plan.R` establish that contract.
Nothing observable differs, so a test written merely to execute the mutant
branch would pin an implementation detail.

### `R/share.R:3044`, `candidates > i` to `candidates >= i`

Classification: **EQUIVALENT MUTANT**.

Before this filter, every candidate already satisfies
`length(parent) < length(child)`. The current set at index `i` has the same
length as itself and can therefore never be in `candidates`. Including or
excluding `i` gives the same vector for every valid plan. ADR 0010 requires the
immediate strictly less detailed set and requires duplicate occurrences to be
skipped; the earlier strict-length predicate is the part that enforces that
contract. The mutated filter is redundant under its established invariant.

### `R/share.R:2685`, first missingness `|` to `&`

Classification: **TEST GAP**.

The original inner condition is equivalent to:

```r
is.na(source) | is.na(denominator) | denominator == 0
```

The mutant uses `&` between the first two terms. For `NA_real_`, ordinary R
division still produces `NA_real_`, which hid the mutation. For a local `NaN`
numerator and a finite nonzero denominator, however, the original selects the
explicit `NA_real_` branch while the mutant evaluates `NaN / denominator` and
returns `NaN`. `is.na()` accepts both, but `is.nan()` and `identical()` expose
the contract change.

ADR 0010 says a missing numerator produces `NA_real_` and explicitly says that
local `NaN` is treated as missing. The `share_of_parent()` reference in
`R/share.R` repeats the same value contract. The mutant is therefore observable
and contract-relevant.

## Confirmed test gap

### Local `NaN` numerator is not asserted to normalize to `NA_real_`

Severity: **Medium**. The numerical value remains missing to `is.na()`, but the
package explicitly promises normalization and callers can distinguish `NaN`
from `NA_real_`. The difference can propagate into serialization, reporting,
and downstream `is.nan()` logic.

Minimal public reproducer:

```r
result <- summarize_with_margins(
  data.frame(group = c("a", "b")),
  total = if (dplyr::n() == 1L) NaN else 2,
  share = share_of_parent(total),
  .grouping = rollup(group),
  .margin_label = NULL
)

detail <- result$share[!is.na(result$group)]
```

The unmutated package returned:

```r
c(NA_real_, NA_real_)
```

The mutant returned:

```r
c(NaN, NaN)
```

A temporary assertion
`expect_identical(detail, c(NA_real_, NA_real_))` passed on the original and
failed on that mutant.

The closest existing test, “Parent-share values handle missing, zero,
negative, and empty summaries”, constructs `NA_real_` rather than `NaN` and
uses `expect_true(is.na(...))`. Both choices admit the mutant. Other exact
missing-value assertions cover missing and zero denominators, not the local
`NaN`-numerator normalization.

Recommended durable test: add one local Parent-share case that produces a
`NaN` source only on detail rows while keeping the parent source finite, then
assert the detail vector with `expect_identical(..., NA_real_)` and optionally
assert `!any(is.nan(...))`. Put it beside the existing Parent-share value-rule
test. ADR 0017 reuses ADR 0010's value rules for Total shares, but the expression
is shared; a duplicate Total-share test would add little defect-detection value
unless a future adapter separates the paths.

## Execution and backend artifacts

- Nineteen initial TIMEOUT results were worker-queue artifacts, not mutant
  hangs. Smaller reruns classified every one.
- Four focused runs produced reporter-level RuntimeError results from mutants
  that disrupted the narrow test run. Full-suite reruns classified all four as
  KILLED; none remained an execution error.
- `FileTestStrategy` in muttest 0.3.0 failed in a mirai worker because its
  internal `.escape_regex()` helper was not resolved after serialization. A
  campaign-local strategy using an explicit filter was required.
- Printing a `muttest_result` backed only by `JSONMutationReporter` attempted
  to call a missing reporter `print()` method. The JSON file itself was valid.
- All optional backends named above were installed for the baseline. No final
  survivor was attributable to an absent backend. Arrow rejects shares by
  contract before this value expression, so the confirmed gap is specifically
  local R behavior, not an Arrow coverage claim.

## Code deliberately outside this campaign

- Backend adapters, dialect probing, query audit, and lazy execution were
  excluded. Mutating them safely requires the release matrix's one-backend
  environments and Sent-query assertions; a local all-backends run would mix
  genuine gaps with dialect and availability artifacts.
- Static expression analysis and diagnostic construction were excluded from
  this first semantic pass. Their large syntax surface needs tailored
  call-shape operators; generic mutation would create many malformed-language
  and message-only mutants.
- Margin order, Margin label collision handling, factor restoration, and
  condition deduplication remain meaningful separate campaigns. Including them
  here would make the survivor set span unrelated contracts.
- Export wrappers, constructors that only capture quosures, package data,
  generated documentation, and test/helper code were excluded because they are
  not the production decision points this campaign was designed to assess.

## Recommended next actions

1. Add the confirmed local-`NaN` normalization test. It has high
   defect-detection value, a two-row fixture, and low maintenance cost.
2. Run a second focused campaign for Margin order and Margin label semantics,
   then a separately isolated backend campaign using the generated
   one-backend matrix. Do not infer backend coverage from this local campaign.
3. The 2026-09-12 evidence did not justify a permanent mutation-testing check.
   `muttest` found a real gap quickly, but the observed campaign took roughly
   seven minutes beyond setup and required workarounds for worker filtering,
   queue timeout accounting, and JSON-result printing. Pinning 0.3.0 plus a
   repository harness would itself become maintenance surface.
4. Reconsider a manual or scheduled CI job after the tool issues are resolved,
   or after two more focused campaigns show recurring unique findings. Do not
   add `muttest` to package Imports or Suggests; install it as quality-tooling
   infrastructure if it becomes permanent.

## Complete mutant disposition

`K` means KILLED and `S` means SURVIVED before semantic triage.

| Block | Location | Mutations and result |
|---|---|---|
| Plan normalization | `R/grouping-plan.R:864:15` | numeric `1L` to `0` K; to `2` K |
| Plan normalization | `R/grouping-plan.R:866:38` | `|` to `&` K |
| Plan normalization | `R/grouping-plan.R:868:7,27` | negate condition K; `&&` to `||` K |
| Plan normalization | `R/grouping-plan.R:882:56` | numeric `1L` to `0` K; to `2` K |
| Plan normalization | `R/grouping-plan.R:898:7` | negate condition K |
| Plan normalization | `R/grouping-plan.R:899:13` | remove `!` K |
| Plan normalization | `R/grouping-plan.R:910:7,26` | negate condition K; `>` to `<` K; `>` to `>=` S |
| Plan normalization | `R/grouping-plan.R:912:32` | remove `!` K |
| Set expansion | `R/grouping-plan.R:975:13` | negate condition K |
| Unit expansion | `R/grouping-plan.R:990:13` | negate condition K |
| Unit expansion | `R/grouping-plan.R:999:13,26` | negate condition K; `==` to `!=` K |
| Unit expansion | `R/grouping-plan.R:1008:7,21` | negate condition K; `==` to `!=` K |
| Rollup expansion | `R/grouping-plan.R:1019:11,13` | negate condition K; `==` to `!=` K |
| Cube expansion | `R/grouping-plan.R:1038:11,25` | negate condition K; `==` to `!=` K |
| Product expansion | `R/grouping-plan.R:1045:7,30` | negate condition K; `==` to `!=` K |
| Product expansion | `R/grouping-plan.R:1050:19` | negate condition K |
| Share value | `R/share.R:2666:11,30` | negate condition K; `==` to `!=` K |
| Share value | `R/share.R:2675:13` | numeric `1` to `0` K; to `2` K |
| Share value | `R/share.R:2676:13` | numeric `1` to `0` K; to `2` K |
| Share value | `R/share.R:2683:11` | numeric `1` to `0` K; to `2` K |
| Share value | `R/share.R:2685:52` | first `|` to `&` S |
| Share value | `R/share.R:2686:59` | second `|` to `&` K |
| Share value | `R/share.R:2687:54,57` | `==` to `!=` K; numeric `0` to `-1` K; to `1` K |
| Grand total mapping | `R/share.R:3010:39` | `==` to `!=` K |
| Total denominator | `R/share.R:3021:7,31` | negate condition K; `==` to `!=` K |
| Total denominator | `R/share.R:3024:10` | remove `!` K |
| Parent candidate | `R/share.R:3040:24,40` | `<` to `<=` K; `<` to `>` K; `&&` to `||` K |
| Parent candidate | `R/share.R:3042:15` | numeric `1L` to `0` K; to `2` K |
| Parent candidate | `R/share.R:3044:41` | `>` to `<` K; `>` to `>=` S |
| Parent candidate | `R/share.R:3045:9,28` | negate condition K; `>` to `<` K; `>` to `>=` K |
