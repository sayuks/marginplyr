# Contextual-share source and lexical-scope investigation

Investigated: 2026-11-03
Repository snapshot: `4b34fdecb2d8a41034b3d9b5d5af8e51592555e1`

This lane investigated local and dtplyr source eligibility, lexical dependency
analysis and structural denominators. It changed no package implementation,
package regression test, dependency metadata or generated documentation. The
scripts loaded the recorded checkout through `pkgload::load_all()` and set
`TESTTHAT_PARALLEL=false`. One reproduction execution ran at a time, and each
yielded execution was collected through its original handle before the next.

The environment was R 4.6.1 (2026-06-24) on aarch64 macOS, dplyr 1.2.1,
rlang 1.3.0, dtplyr 1.3.3, data.table 1.18.6.1 and pkgload 1.5.3. The first
discovery log recorded these versions; the coordinator retained the wider
environment record. No full-suite, package build/check or formal review result
is claimed by this lane.

## S1: nonliteral list2env values retained a stale lookup proof

ADRs 0010 and 0017, and `R/share.R`'s *Eligible source summaries*, required a
self-contained share source and refused ordinary summaries that read an
earlier contextual share. `expression_alias_dependencies()` and
`reflective_lookup_symbols()` recorded the conservative rule for undecidable
lookups: report possible alias reads rather than silently admit one. The
decisions in issues #130 and #173 recorded the same safety model.

A directly written base setter still bypassed both refusals when its `x`
argument was not a top-level `list()` call. Only one brace pair distinguished
the minimal failure from a correctly refused control:

```r
input <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
outside <- new.env(parent = baseenv())
summarize_with_margins(
  input, value = sum(x), prior = share_of_total(value),
  derived = {
    base::list2env({list(outside = environment())}, envir = environment())
    sum(get("prior", envir = outside))
  },
  .grouping = rollup(g)
)
```

Expected: a Package condition refusing the ordinary summary's dependency on
the preceding share. Observed: displayed `prior = .25, .75, 1`, but
`derived = 100, 300, 400` from the old same-named input payload. Removing only
the braces around the list produced the intended refusal. Replacing the
setter with canonical `assign()` also refused. The reverse direction admitted
a share source depending on an earlier ordinary alias and returned source
values one, three and four instead of refusing that dependency.

`list2env-unresolved-reproduction.R` retained both public calls and the
literal-list control. Report mode exited zero. Its `--assert` mode required
both missing Package refusals and exited one on the investigated SHA.

`list2env-unresolved-matrix.R` established 72 missing refusals and 168 passing
controls. It crossed data.frame, tibble and data.table inputs; both helpers;
both dependency directions; no fixed keys versus fixed keys including a
missing partition; and three `x` forms: a brace-wrapped literal list,
`identity(list(...))` and a literal-name `setNames(list(...), "outside")`.
Controls covered the corresponding no-share evaluations, canonical assign
and direct literal-list refusals, stable external lookups and unrelated
literal-key writes. The captured external environment's identity, binding and
parent stayed unchanged. This separated local binding replacement from parent
replacement or caller-owned environment mutation.

The owning boundary was `expression_rebound_names()` in `R/share.R`. It
recognized the base `list2env` mutation and extracted names only when `x` was a
direct base `list` call. Every other `x` yielded no assigned names rather
than an unresolved mutation marker. `lookup_environment_value()` then reused
the captured external `outside` value, despite the setter visibly replacing
that binding before the lookup. No arbitrary callback needed to be inspected
or executed to recognize the base setter or to decline that stale proof.

The bounded repair scope was conservative invalidation for a recognized
`list2env` mutation with an explicit non-NULL target and unreadable keys.
Acceptance should require both dependency refusals, preserve direct literal
key precision and unrelated-key controls, and retain constructor calls with
omitted/NULL `envir`. Planning must not evaluate `x` to discover its keys.
The brace-wrapped literal and the runtime-built names distinguish readable
syntax from conservative unknown-key handling.

## S2: dtplyr input aliasing rewrote list2env-owned lexical values

`CONTEXT.md`'s Contextual helper entry and ADR 0019 retained ordinary lexical
lookup for every non-contextual name. `rewrite_dtplyr_share_measure()`'s own
contract retained lexical scopes while aliasing special input measures. A
visible base `list2env` lexical binding fell outside its binding reader.

```r
input <- data.frame(g = c("a", "b"), .BY = c(1, 3), x = c(2, 6))
summarize_with_margins(
  dtplyr::lazy_dt(input), value = sum(x),
  independent = local({
    base::list2env(list(.BY = c(5, 7)), envir = environment())
    sum(.BY)
  }),
  fraction = share_of_total(value), .grouping = rollup(g)
) |> dplyr::collect()
```

Expected: lexical `independent = 12, 12, 12` if supported, or an appropriate
refusal. Observed: successful `independent = 1, 3, 4`, read from the otherwise
unused input `.BY` column. The local Margin call and changing only that unused
input column's name returned 12 throughout. Ordinary dtplyr and the no-share
Margin call refused with `cannot change value of locked binding for '.BY'`;
the claim is that adding shares converted refusal into a wrong successful
answer, not that every ordinary function shape had been supported upstream.

`dtplyr-list2env-reproduction.R` retained the query and those successful
controls. Its report mode exited zero; `--assert` required correct lexical
values or a Package refusal and exited one. The generated data.table call
left `list(.BY = c(5, 7))` unchanged but rewrote its later lexical read to
`sum(..marginplyr_dtplyr_column_1)`, isolating the value change to aliasing.

`dtplyr-list2env-matrix.R` established 112 wrong lexical results and 224
passing local/renamed controls. It crossed seven input spellings (`.BY`,
`.SD`, `.GRP`, `.NGRP`, `.SDall`, `.xSD`, `.iSD`), both helpers, direct versus
ordinary across sources, the lexical expression as the share source versus
as an independent ordinary summary, and `local()` versus an inline function.
Fixtures included fixed and missing partitions. All caller tables remained
unchanged. `dtplyr-list2env-controls.R` independently observed zero source
rows visited at construction and four only after explicit collection, while
retaining ordinary/no-share refusals and source isolation.

The owning boundary was `dtplyr_share_binds_measure()` in `R/share.R`. It
consulted `lookup_assignment_names()` for assignments but did not recognize
the literal-key `list2env` setter. `rewrite_dtplyr_share_measure()` consequently
treated the later bare symbol as an input read. A direct `<-` lexical-binding
control kept its value. This was independent of S1: the plain literal-list
form already covered by S1's correctly refused dependency control reproduced
S2, and S2 needed no reflective lookup or preceding alias dependency.

The bounded repair scope was preserving ownership under recognized
multi-binding setters at the special-measure aliasing seam. Acceptance should
require correct lexical values or a targeted refusal for these public forms,
both helper kinds, source and independent roles, direct/across sources and
all seven already supported special measures. Preserve the supported input
measure sums, scalar/type validation, lazy construction, caller-table
isolation, and the existing direct/formal/string-assignment ownership cases.
No expansion of dtplyr's upstream function grammar is required.

## Duplicate comparison

All-state `gh issue list` and `gh pr list` searches used the following queries
on the investigation date, with `--limit 100` and exact matching results read:

- `repo:sayuks/marginplyr list2env`
- `repo:sayuks/marginplyr "share" "environment"`
- `repo:sayuks/marginplyr "dtplyr" "lexical"`
- `repo:sayuks/marginplyr "special" "measure"`

The retained `duplicates-*.json` files recorded the focused search results.
The complete bodies and comments of #1140, #1153, #1033, #1049, #1047, #130 and
#173 were read. `issue-1140.json` and `issue-1049.json` retained the closest
comparisons.

- [#1140](https://github.com/sayuks/marginplyr/issues/1140) explicitly required
  visible literal-key `list2env` mutations. Its direct-list repair worked in
  S1's control. S1 concerned a recognized setter whose list keys were not read
  at that top-level shape, requiring conservative invalidation rather than a
  return to the already repaired direct-list case.
- [#1153](https://github.com/sayuks/marginplyr/issues/1153) concerned binding
  replacements carried inside `eval(quote(...))`. Its traversal repair was
  present. S1 used a direct base-qualified setter and no eval boundary.
- [#1033](https://github.com/sayuks/marginplyr/issues/1033) repaired formal and
  direct assignment ownership in special-measure aliasing;
  [#1049](https://github.com/sayuks/marginplyr/issues/1049) covered six string
  and indirect assignment forms. S2 used a different visible multi-binding
  setter and reproduced after those fixes. Their correct direct/formal
  controls were not described as newly failing.
- [#1047](https://github.com/sayuks/marginplyr/issues/1047) concerned supported
  aggregate inputs under six additional data.table special names. S2's
  original input measure was unused, and the wrong read belonged to an
  expression-local binding instead.

The matching PR searches returned merged #1148/#1169 for lookup proofs and
#1035/#1063 for lexical special-measure protection. No matching new report
covered the measured residual forms. These were two candidate P2 follow-ups,
with neither depending on the other. The coordinator owned publication and
any final issue-granularity decision; this lane published no issue or comment.

## Other hypotheses and bounded negative evidence

`lexical-discovery.R` explored 16 setter/lookup shapes across both helpers
and dependency directions. The finalized script's 64 rows were discovery
evidence only. Constructor-style local calls and direct environment
replacement syntax encountered their own ordinary errors. rlang `env_bind`
and `env_poke` showed similar effects, but inference of those ordinary
functions was not needed for either candidate's repair scope and was not
claimed as a separate defect.

`dtplyr-lexical-discovery.R` explored eight lexical forms over seven special
input names and four control modes (224 rows). It found the list2env seam,
confirmed direct-binding controls, and retained paired upstream failures for
reflective reads. A first draft's named-list substitution and a first lexical
discovery invocation's protected-language call were harness mistakes; their
discarded outputs were not included in any count. The retained scripts and
logs were the corrected final executions.

`typed-denominator-oracle.R` independently selected structural denominator
rows from the visible occurrence identifiers and typed keys. It covered
16 key types as fixed partitions and variable dimensions, on local and
dtplyr inputs: 64 public cases. Forty-nine succeeded and all 888 Parent/Total
ratios agreed; fifteen had paired no-share/backend errors. Types included
factor missing levels, NaN versus NA, dates/times/durations, integer64,
packed frames, matrices, arrays, list, complex and raw. The paired refusals
were not treated as package share failures. No additional numeric denominator
defect was established.

No live SQL, Arrow, hosted service, arbitrary callback, other-version,
concurrency or interruption guarantee followed from these bounded probes.

## Replay

Run from the recorded checkout root, one command at a time:

```sh
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/share/list2env-unresolved-reproduction.R
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/share/list2env-unresolved-reproduction.R --assert
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/share/list2env-unresolved-matrix.R
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/share/dtplyr-list2env-reproduction.R
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/share/dtplyr-list2env-reproduction.R --assert
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/share/dtplyr-list2env-controls.R
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/share/dtplyr-list2env-matrix.R
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/share/typed-denominator-oracle.R
```

Each report/matrix/control command completed with exit zero. Both `--assert`
commands completed with exit one against the missing behavior. Adjacent CSVs
and logs retained the individual outcomes. No tests were skipped by these
standalone scripts; paired backend refusals were recorded explicitly.
