# Nesting identity and Grouping specification greedy bug hunt

Investigated: 2026-10-31
Baseline: `339315c387f31deace77566f5d3294b15b323865` (`main`)

## Scope and outcome

This investigation created a fresh worktree from main on
`investigation/nest-spec-greedy-2026-10-31`. The fetched origin main matched
the audited commit. Four investigators examined `nest_with_margins()`,
`nest_by_with_margins()`, the five Grouping specification constructors,
`inspect_grouping()`, and Grouping identity helpers. The October 30
[nesting equality/lifecycle hunt](../2026-10-30-nest-spec-greedy/README.md),
October 29 [exhaustive hunt](../2026-10-29-nest-spec-exhaustive/README.md),
[dispatch follow-up](../2026-10-29-nest-spec-dispatch-followup/README.md), and
[deep hunt](../nesting-grouping-deep-hunt-2026-10-29.md) informed the additional
boundaries. Their measurements were not counted as runs on this baseline.

Two independently reproduced defects became implementation tickets through
`to-tickets`:

| Finding | User-visible failure | Ticket | Blocking edges |
| --- | --- | --- | --- |
| F1: NA-level factor identity encoding discarded source equality | One source group became two one-row nested cells | [#1088](https://github.com/sayuks/marginplyr/issues/1088) | None |
| F2: dtplyr display extraction resolved caller helpers | Correct cell payloads acquired silently incorrect NA outer keys | [#1089](https://github.com/sayuks/marginplyr/issues/1089) | None |

Both Issues received `bug` and `ready-for-agent`. Each repair formed a complete,
independently verifiable behavior change; neither required a prefactoring
ticket or blocked the other. The user delegated granularity and publication,
so the skill's breakdown interview was omitted. No parent Issue was modified.
No package implementation or permanent regression test was changed. The fix
and post-fix phases of `diagnosing-bugs` were outside the requested scope.

## F1: factor identity encoding split a source equality group

The minimal local fixture used a valid vctrs factor subclass whose equality
proxy folded `A` and `a` together. Same-class prototype and cast methods made
the fixture compatible with ordinary vctrs combination. Its two rows used
those two levels, and its domain also declared an unused NA level.

Ordinary `vctrs::vec_group_loc()` and `dplyr::nest_by()` returned one two-row
group. Ordinary `dplyr::union_all()` retained the subclass and one equality
group over the combined rows. Both nesting Margin verbs with a single
`grouping_set(g)` instead returned two one-row cells. No omitted dimension,
inter-set union, observed missing value, or non-missing Margin label was
necessary. Removing the unused NA level or selecting `g` as a fixed `.by`
key restored one cell. A NULL label still reproduced the defect.

The [local record](local/README.md) retained three ranked hypotheses and their
controls. The controls supported loss of the equality proxy in the package's
factor identity encoder, rather than an upstream union failure or a source
type ordinary nesting could not group. `execute_margin_nest()` selected
`identity_encoded` for the NA-level factor and copied it through
`encode_factor_for_margin()` into character values. The fold then grouped by
those character identities instead of the original source equality.

The coordinating investigator independently defined a second factor whose
proxy folded `low-1` and `low-2` together. Both verbs and both label routes
reproduced the split after the same ordinary-grouping/union controls passed.
This was source membership constructed by marginplyr, not a request to restore
arbitrary factor-subclass attributes under
[ADR 0016](../../design/adr/0016-delegate-result-class-and-attributes-to-dplyr.md).
The one-cell-per-source-group promise in the nesting reference supplied the
expected result; [ADR 0012](../../design/adr/0012-distinguish-factor-na-levels-from-missing-margin-values.md)
supplied the NA-level/missing-code distinction that a repair also had to keep.

Closed [#1073](https://github.com/sayuks/marginplyr/issues/1073) prevented
displayed values from refining an intact source identity. This fixture lost
equality before that fold, and its typed-missing-label route failed too.
Closed [#620](https://github.com/sayuks/marginplyr/issues/620) protected distinct
keys with coinciding display strings. Their full bodies and empty comment
lists were read; neither was an open duplicate of the factor-encoding cause.

## F2: caller helpers changed dtplyr outer keys

A two-row immutable dtplyr input contained character `g = c("a", "b")` and
integer row identifiers. With a single `grouping_set(g)`, a root-local binding
`min <- function(...) 0L` made both nesting verbs return two NA outer keys.
The two cells still contained the correct separate source rows, and no warning
or error reported the wrong keys. Callers supplied no summary expression.

The [backend minimal reproduction](backend/reproduce-display-helper.R)
constructed the lazy nesting result before installing that binding. Its
initial collection returned `a` and `b`; installing the binding changed the
same result to NA keys, and rebinding only that function to `base::min`
restored the keys. Each affected verb invoked the caller callback twice.
Ordinary source collection, grouped dtplyr summary, tidyr nesting, fixed-key
nesting, and nesting the explicitly collected source all kept the correct keys
and invoked the callback zero times. Source serialization stayed identical.

The [backend record](backend/README.md) retained the ranked hypotheses and
controls. They supported late lexical lookup in the representative display
expression, rather than source changes or an incorrect Grouping plan.
`nest_expanded_margins()` wrote unqualified `seq_len(min(..., 1L))` into the
dtplyr expression. Collection resolved those calls in the root environment.
An independently authored global-binding reproduction confirmed the same
failure through both public verbs. The matrix also established the analogous
`seq_len` binding failure.

This involved package-written expressions and the visible grouping keys that
identify cells, not user-written summary semantics or arbitrary attribute
restoration. Closed #1073 introduced the representative extraction but did not
make its helpers independent of the dtplyr root environment. The all-state
searches also read the full bodies/comments of
[#76](https://github.com/sayuks/marginplyr/issues/76),
[#948](https://github.com/sayuks/marginplyr/issues/948),
[#890](https://github.com/sayuks/marginplyr/issues/890), and
[#424](https://github.com/sayuks/marginplyr/issues/424).
Those concerned source-column/internal-value shadowing, backend-special
column bindings, omitted-key classes, or converter formals. No duplicate
caller-helper display ticket was found. A masked `list` candidate also broke
ordinary upstream grouped/nesting controls and was excluded from F2.

## Completed search coverage

The rows below belonged to distinct oracles and were not combined into one
passing-assertion total. Expected refusals were counted separately from
successful nesting.

| Evidence | Completed observations |
| --- | --- |
| [Local equality](local/README.md) | 576 calls: 480 faithful and 96 wrong source memberships, all populated NA-level factor dimensions. Crossed a package-version second dimension, structured payload, fixed/dimension roles, both verbs/keep modes, all sort choices, label routes, empty/populated sources and single-set/rollup. There were 1,536 exact cell projections and no source mutation. |
| Local simultaneous names | 480 calls over 32 Unicode, control-character, backend-special, dot-dot and private-prefix spellings: 372 admitted source-membership matches with 1,298 exact projections, 18 documented rowwise `.rows` refusals, and 90 source dot-dot refusals reproduced by ordinary dplyr selection. |
| Local reference-bearing list keys | 432 calls using shared/distinct environments, closures, formulas, pairlists, expressions and recursive reference objects: source memberships and 936 retained-key projections matched, with no source mutation. |
| [dtplyr display extraction](backend/README.md) | 864 comparisons with root-local `min` or `seq_len` bindings: 576 populated calls returned wrong keys and 288 empty controls matched. Every unmasked baseline matched explicitly collected-input local nesting; character/integer/factor dimensions, both verbs, keep modes, labels, sort choices and set families were crossed. No warning, unexpected error or source mutation occurred. |
| [Closure-mediated selections](spec/README.md) | 9,600 direct-tidyselect comparisons across 20 bodies, four closure/default forms, three helper-head parenthesis depths, eight typed inputs and five constructors: 7,008 exact plans, 1,440 matching External-condition classes, 480 renaming refusals and 672 empty rollup/cube refusals. |
| Grouping algebra and identity | 300 generated ASTs with three duplicate policies supplied 808 exact plans and 92 expected duplicate refusals. All 2,748 summary rows matched independent Grouping bits, full identifiers, and independently reordered/subselected identifiers. |
| [Remote admission and metadata](coordinator/remote-boundaries.R) | 50 calls per SQL engine over cross/anti/semi joins, ranked filtering and empty relations. SQLite sent no statements; DuckDB sent ten, all `LIMIT 0`. Unsupported nesting and locally invalid inspection sent none and left empty recording rows. Twenty Arrow calls preserved all five one-shot readers and all 15 original rows. |

The remote harness initially used Arrow's scalar `filter(FALSE)` spelling,
which the installed Arrow rejected before the target calls. The retained run
used a supported column predicate for the empty relation. The failed harness
attempt was not a package finding or an extra completed case.

## Replay, publication checks, and limits

Run from the investigated checkout root. Each R entry point loaded that
checkout with `pkgload::load_all()`. Track READMEs named their complete matrix
commands and exclusions. The minimal desired-behavior commands intentionally
returned terminal exit 1 on this unfixed baseline:

```sh
Rscript investigation/2026-10-31-nest-spec-greedy/local/factor-equality.R --assert
Rscript investigation/2026-10-31-nest-spec-greedy/backend/reproduce-display-helper.R --assert
Rscript investigation/2026-10-31-nest-spec-greedy/coordinator/independent-findings.R --assert
Rscript investigation/2026-10-31-nest-spec-greedy/coordinator/independent-findings.R --assert-dtplyr
```

Observation modes exited zero while asserting the dated wrong results; those
were not passing regression tests. Adjacent logs retained the final outcomes.
The [environment record](backend/environment.log) recorded R/platform and
core dependency versions; the coordinating environment log recorded the remote
dependencies too. Trailing whitespace was removed from captured text logs for
repository whitespace checks. `MANIFEST.sha256` identified the retained evidence.

Only dated investigation material was added under the existing source-package
exclusion. This was Repository-only under
[the local-check classification](../../design/agents/local-checks.md).
The explicitly requested common `marginplyr-review-ready` invocation was
nevertheless included in publication validation. Its checked SHA and terminal
outcome, the linters, context-budget/reference checks, and the review record
belonged on the pull request. No earlier investigation note was amended.
CI monitoring was outside the request.

These finite probes did not establish freedom from all bugs, every S3/S4
method or evaluation boundary, another dependency/R/OS version, unbounded
ASTs/graph depth, resource-exhaustion behavior, or support for nesting SQL and
Arrow inputs. The custom factor established a concrete equality boundary,
not the prevalence of that subclass in downstream packages. Remote checks
established the stated inspection and refusal boundaries, not successful
remote nesting.
