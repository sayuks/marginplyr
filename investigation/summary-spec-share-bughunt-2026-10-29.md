# Summarize, Grouping specification, and contextual-share bug hunt

Investigated: 2026-10-29
Repository snapshot: `59d55a7867ffc0255f75be32a233d9d475bcb7fb`

The investigation started from local `main` in a newly created worktree on
`codex/margins-bug-hunt-2026-10-29`. Local `main` and `origin/main` resolved to
that snapshot when the worktree was created. Four investigators divided
Grouping specification compilation, local summary semantics, contextual shares,
and backend execution, then independently replayed the candidate findings.

The requested deliverable was investigation and tickets. All committed changes
were confined to this dated note and its excluded investigation artifacts. No
package repair, package regression test, or generated reference correction was
made. The tickets own the requested behavior. The user delegated ticket
boundaries and publication, so `to-tickets`'s interactive breakdown checkpoint
was omitted.

Five independent tickets were published with `bug` and `ready-for-agent`
labels. None blocked another.

| Finding | Ticket | Priority | Established effect |
| --- | --- | --- | --- |
| F1 | [#998](https://github.com/sayuks/marginplyr/issues/998) | P2 | An independent `across()` output inherited a sibling's alias dependency and was refused as a share source |
| F2 | [#999](https://github.com/sayuks/marginplyr/issues/999) | P2 | dtplyr fixed `.` and unsigned dot-dot keys failed at different share stages |
| F3 | [#1000](https://github.com/sayuks/marginplyr/issues/1000) | P2 | Restoring an active `sf` geometry summary key invalidated its geometry metadata |
| F4 | [#1001](https://github.com/sayuks/marginplyr/issues/1001) | P3 | The public reference overstated helper rewriting of literal parsed language |
| F5 | [#1002](https://github.com/sayuks/marginplyr/issues/1002) | P2 | Signed and whitespace dot-dot names bypassed generated-read protection |

## F1: across sibling dependencies contaminated an independent source

```r
data <- tibble::tibble(g = c("a", "b"), x = c(1, 3))
summarize_with_margins(
  data, prior = sum(x),
  dplyr::across(c(x, prior), sum, .names = "tot_{.col}"),
  fraction = share_of_total(tot_x),
  .grouping = rollup(g)
)
```

The call raised `marginplyr_error`, saying `tot_x` depended on earlier summary
alias `prior`. The ordinary output `tot_x` depended only on x; the sibling
`tot_prior` was the output that selected prior. Without the share, the selected
source values were 1, 3, 4. Splitting the one `across()` into separate calls for
x and prior made the same share succeed with 0.25, 0.75, 1.

A second minimal variation selected only x and used
`list(plain = sum, alias = ~sum(.x) + prior)` as `.fns`, with names
`"{.col}_{.fn}"`. A share of `x_plain` was rejected because the sibling
`x_alias` read prior. Parent shares and Total shares reproduced both forms.

The source contract in `share_of_parent()`'s *Eligible source summaries*
explicitly admitted a statically named output from a preceding `across()`.
ADR 0010 required that source to be self-contained. These literal selected
columns and literal functions exposed each output's reads; they did not require
reflection or guessing a dynamic binding.

Three hypotheses were separated: a genuine source dependency, an alias
introduced by another generated output, and an inability to determine output
names. Successful split-call and no-share controls excluded the first and
third. `analyze_ordinary_summaries()` computed dependencies over the complete
`across()` and assigned the same set to every output record. Its available
column/function provenance did not narrow those dependencies.

The existing dependency-refusal test exercised an output that actually read an
earlier alias. It did not include an independent sibling. Closed #105 concerned
duplicate-definition accounting, and #899 concerned selector-source
eligibility; neither described this per-output false dependency.

Replay: `reproduce-shares.R --assert --only=F2` in the artifact directory
below. The script's F2 label predated this note's ordering. It observed all four
helper/form failures and exited 1 against the expected successful call.

## F2: dtplyr fixed-key references failed at two share stages

```r
data <- tibble::tibble(
  . = c("f", "f"), g = c("a", "b"), x = c(1, 3)
)
source <- dtplyr::lazy_dt(data)
summarize_with_margins(
  source, value = sum(x), fraction = share_of_total(value),
  .by = dplyr::all_of("."), .grouping = rollup(g)
)
```

Both share helpers raised `simpleError: object '.SD' not found` during
construction. The same Margin summary without a share succeeded. Ordinary
`dplyr::summarise(source, value = sum(x), .by = all_of("."))` also succeeded.
Local, DuckDB, and SQLite share controls returned 0.25, 0.75, 1.

The hypotheses were a cardinality-guard failure, a raw fixed-key expression in
window grouping, and an upstream restriction on the caller's `.by` selection.
Tracing isolated the dot failure to `apply_windowed_shares()` handing a raw
`.` grouping expression to dtplyr. `margin_safe_column_reads()` did not alias
that spelling. The successful ordinary tidyselect call ruled out an upstream
restriction on the user's spelling; bare `group_by(.)` was a different,
package-generated spelling that itself failed upstream.

Fixed `..0`, `..00`, and `..01` exposed a second site. Query construction
succeeded after window grouping safely aliased those keys. Collection failed
in the earlier `assert_dtplyr_grouped_keys()` call, which still received raw
fixed-key symbols. Both helpers reproduced all three names: six failed
execution cases. The no-share Margin controls succeeded. The ticket grouped
these sites into one verifiable delivery: supported fixed-key names surviving
every dtplyr share stage.

Fifteen other data.table-related fixed spellings passed the focused dot sweep.
Closed #952 repaired named safe grouping expressions for `.N`, `.I`, `.SD`,
`.GRP`, `.NGRP`, and `.BY`; the missing dot and the unprotected guard read were
separate remaining boundaries. DuckDB's ordinary refusal of unsigned dot-dot
grouping names was excluded from this ticket.

Replay: `reproduce-shares.R --assert --only=F1` and
`reproduce-dt-dotdot.R --assert`. Both exited 1 against successful-call
expectations. Report modes retained ordinary-summary and no-share controls.

## F3: restoring an sf summary key left its active geometry name stale

```r
source <- sf::st_as_sf(
  data.frame(value = c(1, 3), x = 1:2, y = 3:4),
  coords = c("x", "y")
)
summarize_with_margins(source, total = sum(value), .by = geometry)
```

The call failed with `simpleError: internal error: can't find sf column`.
Ordinary `dplyr::summarise()` with the same geometry fixed key succeeded,
returning the two point keys and totals 1, 3. The zero-row counterpart failed
in the Margin verb while ordinary dplyr returned valid empty geometry.

The candidate causes were unsupported geometry grouping, an `sf` summary
restriction, and stale metadata after package-owned aliases. The first two
were excluded by the ordinary fixed-key control. A minimal alias probe copied
the active geometry under `private_geometry`, summarized with that copy as its
fixed key, and restored its name through `rlang::set_names()`. The public
column became geometry while `sf_column` still named private_geometry.
`sf::st_geometry()` then independently reported that its active-geometry
attribute did not point at a geometry column.

The portable summary adapter's restoration of included private keys used that
same generic name setter. Its next selection encountered the invalid
intermediate object. ADR 0016 delegated result classes to dplyr/vctrs; the
finding requested preserving an intermediate object's existing invariants,
without promising an outer `sf` class or arbitrary attribute restoration.
Closed #949/#951 addressed nesting and column reordering, not this summary-key
restoration.

The separate ordinary `sf` refusal asking for an explicit geometry aggregate
when `.by` selected a non-geometry key was reproduced and excluded. Supplying
that aggregate allowed both ordinary and Margin rollups on populated and empty
inputs. That upstream restriction did not explain the active-geometry case.

Replay: `reproduce-sf.R` printed the populated/empty ordinary controls and stale
metadata. Its `--assert` mode exited 1 on the first Margin error.

## F4: the reference overstated rewriting of literal parsed language

```r
source <- data.frame(g = "a")
summarize_with_margins(
  source, bit = eval(quote(grouping_bit(g))),
  .grouping = grouping_set(g)
)
# bit = 0L
summarize_with_margins(
  source, bit = eval(str2lang("grouping_bit(g)")),
  .grouping = grouping_set(g)
)
# `grouping_bit()` can only be used inside `summarize_with_margins()`.
```

The *Relationship to dplyr summaries* reference said that every preceding rule
applied to evaluated language, and included text parsed from a literal among
the statically followed forms. The literal parsed helper instead reached the
runtime context guard. Binding `grouping_bit <- function(...) 99L` in the
caller made that parsed form return 99L, whereas the visibly quoted form kept
its statically rewritten bit.

Qualified `base::eval(base::str2lang(...))`, `parse(text = ...)`, and
`rlang::eval_tidy()` reproduced the distinction. Parsed
`dplyr::cur_group_id()` still triggered the reserved-helper refusal, proving
that literal parsing was followed for searching. Quoting the helper rather
than parsing it made rewriting succeed; qualification did not fix the parsed
form.

`rewrite_evaluated_language()` explicitly documented this intentional boundary:
searches could recover parsed text, but rewriting did not write it back.
Existing tests separated parsed-language analysis from capture rewriting.
The confirmed finding was therefore the reference's false universal claim,
classified P3 and ticketed as a documentation correction. No extension of
runtime language rewriting was assumed necessary.

Replay: `reproduce-eval-contract.R` printed the controls and exited 1 when it
asserted the public reference's claimed bit behavior. That final assertion
exposed the false claim; it was not proposed as the acceptance test for a
mandatory new runtime feature.

## F5: signed dot-dot names bypassed internal read protection

```r
source <- tibble::tibble(g = c("a", "b"), x = c(1, 3))
summarize_with_margins(
  source, `..-1` = sum(x), fraction = share_of_total(`..-1`),
  .grouping = rollup(g)
)
```

Ordinary dplyr summary and the Margin summary without a share accepted that
output name. Its values under the rollup were 1, 3, 4. Adding a share failed
when a generated denominator projection read `.data[["..-1"]]`, with
`indexing '...' with non-positive index -1`. Local and dtplyr inputs reproduced
both share helpers. dtplyr construction remained lazy and its failure occurred
on materialization.

`..-0`, `..+1`, `..+0`, and `.. 1` reproduced the same source-read boundary.
A local input fixed key with each spelling also succeeded under ordinary
`.by = all_of(name)` but failed in the Margin verb's private-key construction,
even without a share. The focused reproduction executed 20 source/helper
cases and five local fixed-key cases, each with successful ordinary controls.

The source's scalar validity and the caller's ability to name the output were
verified before attributing the failure to a generated read. Direct data-pronoun
reads reproduced R's special dot-dot lookup; the ordinary selection/output
interfaces accepted the names. `margin_dot_dot_names()` recognized unsigned
digits only, so these admitted spellings bypassed the existing alias boundary.

The repository's intentional upstream restriction on `...` and positive `..j`
input names was retained. The ticket concerned admitted names and generated
references. F2 was separate because it reproduced unsigned names at dtplyr
fixed-key stages; this finding already reproduced locally and through
source-summary lookup.

Replay: `reproduce-signed.R` printed all 25 cases and controls. `--assert`
exited 1 on the first failed successful-call expectation.

## Exploration and negative findings

The retained artifacts were saved under
[`2026-10-29-summary-spec-share-hunt`](2026-10-29-summary-spec-share-hunt).
These counts described bounded feedback loops, not exhaustive input spaces.
Random seeds were 6539 for Grouping specifications, 20261029 for local summary
forms, 72531 for the share oracle, and 193061 for backend summaries.

| Harness | Executed scope | Established result |
| --- | --- | --- |
| `grouping-oracle.R` | 2,000 generated trees across all five constructor kinds, nesting, composites, overlap, identity products and duplicate occurrences | Independent ordered-set and identifier oracle passed |
| `grouping-selection.R` | 100 name layouts × 16 tidyselect forms = 1,600 comparisons | Public list-format inspection matched direct tidyselect values or external error classes |
| `grouping-backends.R` | 500 plans × dtplyr, Arrow Table, simulated PostgreSQL and live DuckDB = 2,000 comparisons | Complete list-format inspection matched local results |
| `dt-metadata.R` | Nine arithmetic metadata probes | Typed proxy and actual dtplyr types agreed |
| `summary-one-set.R`, `summary-rollup.R` | 93 one-set and 744 two-level rollup comparisons, including 12 key representations | Ordinary dplyr oracle found no additional mismatch |
| `summary-helpers.R` | 984 helper-expression/key/input comparisons | 972 agreed; 12 literal-parsed cases were the same F4 scope discrepancy |
| `share-oracle.R` | 40 fixtures × three Grouping specifications × four inputs = 480 checks | Independent structural denominator and backend differential oracles passed |
| `backend-differential.R` | 30 fixtures × five plans × two fixed-key policies × three sorts × four lazy inputs = 3,600 comparisons | All 270 raw differences had empty source inputs; SQL empty-sum and delegated empty ordinary-output types explained the observed differences |
| `backend-names.R` | 30 spellings × four roles × two sorts × two duplicate policies × four inputs = 1,920 comparisons | Eight raw failures were DuckDB unsigned dot-dot grouping failures also reproduced in ordinary dplyr |
| `backend-types.R` | Ten key representations × empty/populated × fixed/dimension × three sorts × two labels × four inputs = 960 comparisons | 404 raw signals were retained; source-import types, backend string casts, factor availability/order and ordinary empty-output types were separated from package guarantees |
| `backend-derived.R` | Ten input transformations × two sorts × two duplicate policies × four inputs = 160 comparisons | 56 raw failures were ordinary Arrow coalesce translation, dtplyr coalesce type rules, or documented metadata refusal; isolated controls established those boundaries |
| Share name sweeps | 324 exploratory cases and a retained 324-case valid-name sweep | The retained sweep returned 312 successes and 12 raw failures; focused controls separated F2/F5 from ordinary DuckDB constraints |

A cube-order assumption in the first mathematical oracle was corrected to the
specified descending subset-size order before the reported 2,000-plan run.
SQL sums over all missing values were not assumed equal to R sums. The share
oracle explicitly coalesced its ordinary sum to zero before comparing ratios.
The backend differential's empty ordinary-output differences did not assert a
new type promise for arbitrary summaries.

Specific exclusions and controls included:

- `...` and positive `..j` input restrictions already retained by the
  repository; ordinary tidyselect succeeding on an unrelated column did not
  remove those restrictions.
- DuckDB ordinary `.by`/`group_by()` failures for `..0` and `..00`, separated
  from the successful portable controls in `control-sql-dotdot.R`.
- DuckDB `copy_to()` importing an integer64 fixture as its double storage
  representation before any Margin call; the original key's missing code and
  order were already lost at import. That was not attributed to Margin order.
- Backend-specific character casts of logical, Date, timestamp and infinite
  values, and factor-order guarantees limited to backends holding factor
  metadata. Original-local-column equality was not a universal SQL oracle.
- `coalesce(sum(...), 0L)` failing in ordinary Arrow summaries, as established
  by `control-arrow-derived.R`; sum-only Margin summaries over a union input
  succeeded.
- dtplyr refusing metadata-unsafe head, distinct and aggregated inputs under
  ADR 0020, rather than executing upstream source rows to answer selections.
- Both contextual shares' documented Arrow refusal. Inspection comparisons
  on PostgreSQL used a simulator and established no live PostgreSQL execution.

No new confirmed defect in Grouping specification expansion or inspection was
established. That negative result was limited to the generated trees, selection
forms, data types, versions and backend shapes actually exercised.

## Replaying the evidence

Run from the repository root. The committed harnesses loaded that checkout
through pkgload; the source scripts were made independent of the original
absolute worktree path. DuckDB drivers in the saved scripts used
`shared_home = FALSE`. Numeric literal spelling and an intentional redundant
parentheses probe were expressed in linter-compatible forms without changing
the cases being measured.

```sh
Rscript investigation/2026-10-29-summary-spec-share-hunt/grouping-oracle.R
Rscript investigation/2026-10-29-summary-spec-share-hunt/grouping-selection.R
Rscript investigation/2026-10-29-summary-spec-share-hunt/grouping-backends.R
Rscript investigation/2026-10-29-summary-spec-share-hunt/summary-helpers.R
Rscript investigation/2026-10-29-summary-spec-share-hunt/share-oracle.R
Rscript investigation/2026-10-29-summary-spec-share-hunt/reproduce-shares.R
Rscript investigation/2026-10-29-summary-spec-share-hunt/reproduce-dt-dotdot.R
Rscript investigation/2026-10-29-summary-spec-share-hunt/reproduce-signed.R
Rscript investigation/2026-10-29-summary-spec-share-hunt/reproduce-sf.R
Rscript investigation/2026-10-29-summary-spec-share-hunt/reproduce-eval-contract.R
```

Report scripts checked their successful controls and printed the captured
failures. The three share reproduction scripts and the sf reproduction also
accepted `--assert`, which intentionally exited 1 on this investigated code.
`reproduce-eval-contract.R` always ended with the contradicted public-claim
assertion and therefore exited 1. These red outcomes were investigation
signals, not failed package regression tests.

`replay-status.json` recorded terminal exit codes for the saved-script replay;
its adjacent logs recorded stdout/stderr. The `backend-*.log` files retained
initial scanner output, including raw differences requiring the classifications
above. Those broad scans were diagnostic reports, not all-green tests. The
manifest covered retained artifact bytes.

## Environment and local publication checks

R 4.6.1 (2026-06-24), aarch64 macOS; dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3,
data.table 1.18.6.1, DuckDB 1.5.5, RSQLite 3.53.3, Arrow 25.0.1, bit64 4.8.6,
sf 1.1.2, and pkgload 1.5.3 were used. Package implementation was loaded from
the snapshot above, not inferred from an installed package of the same version.

This was a Repository-only change under `design/agents/local-checks.md`:
`investigation/` was excluded from the source package and drove no generation.
The Package-affecting Review-ready check was therefore not required. Jarl and
package-aware lintr were run before publication, with pkgload loading the
namespace before lintr. The context-budget and maintained-document reference
verifiers were also run; their terminal results were retained with the
artifacts. `git diff --check` checked the committed prose and evidence layout.
No CI monitoring was requested or performed.
