# Nesting and Grouping specification bug hunt

Investigated: 2026-10-25

Source snapshot: `b6f9faba946521aeb92fcf2c5655a2c3bf608ec8` (`main`).
Scope: both nesting verbs, the five Grouping specification constructors,
Grouping-plan inspection, and their shared preparation/finalization boundaries.

## Outcome and ticket boundaries

Six defects were independently reproduced: four arose from this hunt, and
two related existing tickets were confirmed during the duplicate search.
Three new tickets were split by public behavior that a fix could restore and
verify independently. No ticket blocked another; no prefactoring was needed.

| Finding | Observed impact | Ticket |
|---|---|---|
| F1: named scalar options | Duplicate policy, inspection format, Margin order, or factor-label position silently ignored | [#830](https://github.com/sayuks/marginplyr/issues/830) |
| F2: named scalar output names | Identifier renamed accidentally; sorting failed; empty row-wise nesting lost its required row | [#831](https://github.com/sayuks/marginplyr/issues/831) |
| F3: cyclic specifications | Ordinary constructor calls exhausted R's stack instead of producing a Package condition | [#828](https://github.com/sayuks/marginplyr/issues/828), reused |
| F4: grouped derived dtplyr steps | Metadata-preserving rename/grouping composition refused as unsafe | [#837](https://github.com/sayuks/marginplyr/issues/837) |
| F5: array display labels | Higher-dimensional arrays escaped scalar-label validation and failed in generated conversion | [#834](https://github.com/sayuks/marginplyr/issues/834), reused |
| F6: type-dependent helper arguments | Fixed-key selection disagreed with dplyr and split one nested group into two | [#825](https://github.com/sayuks/marginplyr/issues/825), reused |

During the duplicate search another investigation published #828 against the
same snapshot. Its body and comments were read; the independently reproduced
cycle finding was linked to that issue rather than published again. Existing
#825 and #834 supplied the inputs for the additional confirmations below;
those findings were not claimed as independent discoveries. The three
new issues were labelled `bug` and `ready-for-agent`. Their acceptance criteria
covered complete public behavior and regression evidence, without artificial
blocking edges.

No implementation, package-test, generated-document, or configuration change
was made. The defects remained reproducible at the source snapshot.

## Runtime and method

Experiments loaded the main-based worktree with `pkgload::load_all()`.
The runtime was R 4.6.1 (2026-06-24), marginplyr 0.1.0, dplyr 1.2.1,
tidyr 1.3.2, dtplyr 1.3.3, data.table 1.18.6.1, dbplyr 2.6.0,
RSQLite 3.53.3, DuckDB 1.5.5, Arrow 25.0.1, rlang 1.3.0,
tidyselect 1.2.1, tibble 3.3.1, and vctrs 0.7.3.

Public-API reproductions were combined with independent source-membership
and specification-algebra oracles, local/dtplyr differential comparisons,
and controls changing one relevant input at a time. Suspected backend limits
were replayed without marginplyr before classification. Exploratory harnesses
were temporary; the decisive minimal reproductions were retained below.
Each R block can be run independently from the source root. Assertions of
the required behavior failed at the investigated snapshot.

Contracts read included the nesting/inspection references, `CONTEXT.md`'s
Nested specification position, Option argument, Package condition, and Mutable
step, and ADRs 0007, 0008, 0012, 0013, 0015, 0016, 0018, 0020, 0026, and 0029.
Those artifacts owned decisions and behavior; this note recorded observations.

## F1: R names changed accepted Option values

`match_margin_choice()` accepted a named character scalar and returned its
attributes. Consumers comparing it with a plain string using `identical()`
then chose another branch. The unnamed equivalent behaved as documented.

```r
pkgload::load_all(quiet = TRUE)
x <- tibble::tibble(g = factor(c("b", "a")), value = 1:2)
actual <- c(
  dropped_sets = nrow(inspect_grouping(
    x, .grouping = grouping_sets(g, g),
    .duplicates = c(label = "drop")
  )),
  text_format = is.character(inspect_grouping(
    x, .format = c(label = "text")
  )$included),
  first_label = identical(levels(nest_with_margins(
    x, .grouping = rollup(g),
    .margin_label_position = c(label = "first")
  )$g), c("Total", "a", "b"))
)
print(actual)
# dropped_sets text_format first_label
#            2           0           0
stopifnot(identical(unname(actual), c(1L, 1L, 1L)))
```

Additional public controls established:

| Named scalar's argument/value | Required behavior | Observed behavior |
|---|---|---|
| `.duplicates`: `error` | Refuse repeated sets | Repeated occurrences returned, including by both nesting verbs |
| `.duplicates`: `drop` | Stable deduplication | Both occurrences retained |
| `.format`: `text` | Character display columns | List columns |
| `.sort`: `first` | Margin rows first | Margin rows last |
| `.sort`: `none` | Same result as unsorted call | Fixed-key rows sorted |
| `.margin_label_position`: `first` | Synthetic factor level first | Synthetic level last |

The ranked explanations were attribute retention, consumer comparison, and
argument reevaluation. A read-only matcher probe established retained R names;
removing those names restored its branch comparison. All observations used
accepted public inputs, as can arise by indexing a named settings vector
with `[`. No forged object was needed. One shared Option-value ticket (#830)
covered the independent fix, including the nesting duplicate-policy violation.

## F2: R names changed scalar output-name semantics

`.id` and `.key` described string values naming output columns. Their
validators accepted R names, but two consumers interpreted those attributes
as behavior.

```r
pkgload::load_all(quiet = TRUE)
x <- tibble::tibble(g = c("a", "b"), value = 1:2)
empty <- tibble::tibble(value = integer())
named <- nest_with_margins(
  x, .grouping = rollup(g), .id = c(alias = "set")
)
print(names(named))
# [1] "g" "alias" "data"
cardinality <- c(
  unnamed_key = nrow(nest_by_with_margins(empty, .key = "data")),
  named_key = nrow(nest_by_with_margins(
    empty, .key = c(alias = "data")
  )),
  unnamed_id = nrow(nest_by_with_margins(empty, .id = "set")),
  named_id = nrow(nest_by_with_margins(empty, .id = c(set = "set")))
)
print(cardinality)
# unnamed_key named_key unnamed_id named_id
#           1         0          1        0
stopifnot(
  identical(names(named), c("g", "set", "data")),
  identical(unname(cardinality), rep(1L, 4L))
)
```

Both nesting interfaces returned `alias` instead of `set`. Either requested
Margin order then failed with `Column set not found` in the sorting expression.
Expansion reproduced the shared-finalizer failure. R names matching a grouping
column or `.key` passed collision validation based on value `set`, then failed
later with duplicated output names.

`normalize_margin_id()` retained names. The finalizer passed its result to
`all_of()` in a projection, which tidyselect interpreted as a rename; sorting
still addressed the original value. Separately, the empty-input fallback
compared unnamed column names with `c(.id, .key)` using `identical()`. An R
name on either argument suppressed reconstruction of the required empty cell.
An `.id` whose R name equaled its value isolated this second defect from the
accidental rename.

The nesting reference promised one empty nested row for row-wise nesting with
no grouping keys. `dplyr::nest_by()` retained it with the same named `.key`.
The symptoms were grouped in #831: one scalar-output-name fix had to preserve
meaning through admission, projection, sorting, and empty reconstruction.
It did not depend on the Option-value ticket.

## F3: deferred nested names could form a cycle

```r
pkgload::load_all(quiet = TRUE)
s <- grouping_sets(s)
error <- tryCatch(
  inspect_grouping(data.frame(a = 1), .grouping = s),
  error = identity
)
print(class(error))
# "nodeStackOverflowError" "stackOverflowError" "error" "condition"
cat(conditionMessage(error), "\n")
# node stack overflow
stopifnot(inherits(error, "marginplyr_error"))
```

Separate R processes reproduced this failure in approximately 0.7 seconds.
Both nesting interfaces also reached `nodeStackOverflowError`. A practical
trigger extended a valid binding: `s <- grouping_set(a)` followed by
`s <- grouping_sets(s, b)`. Its captured nested name resolved to the replacement
specification. A two-binding cycle and a product constructor also failed.

The ranked explanations were a missing preflight ancestor boundary,
family-expansion recursion, promise forcing, and a data/selection interaction.
All constructor families reached the same recursive preflight; fresh-name and
injected-value controls over the same data compiled normally. The evidence
located nontermination in `preflight_grouping_spec()`, which visited resolved
children before applying parent nesting grammar and tracked no active ancestry.

Finite controls passed for fresh-name composition, injection of the previous
value during same-name extension, reuse in two sibling union positions with
duplicates retained, and reuse in two product positions. A repair needed to
distinguish active ancestry from legitimate sibling reuse and preserve nested
arguments' one-reading behavior. It did not need to infer that rebinding meant
the previous value. The actionable-refusal gap fell under ADR 0015 and existing
issue #828. No claim was made about arbitrary acyclic depth or R resource limits.

## F4: metadata-preserving dtplyr grouping composition was refused

```r
pkgload::load_all(quiet = TRUE)
x <- dtplyr::lazy_dt(tibble::tibble(g = c("a", "b"), value = 1:2)) |>
  dplyr::rename(group = g) |>
  dplyr::group_by(group)
result <- tryCatch(nest_with_margins(x), error = identity)
if (inherits(result, "error")) cat(conditionMessage(result), "\n")
# Can't safely determine column metadata for `.data` from this dtplyr step.
# ... collect the input ...
stopifnot(!inherits(result, "error"))
```

A root, a renamed input with explicit fixed keys, and a grouped root succeeded.
Rename-then-group failed in both nesting interfaces. Grouping a root and then
renaming only a payload also failed: normalizing with `ungroup()` introduced
the rejected wrapper. Expansion and summarization shared the refusal.
Explicit collection followed by local nesting produced the expected rows.
Name-only inspection succeeded; typed inspection was refused while its
explicit-fixed-key counterpart succeeded.

The grouping-step safety branch required a non-NULL internal `name` even for
a grouping wrapper over an admitted metadata-preserving parent. Derived
wrappers had NULL `name`, although grouping added no columns or types. This
localized the refusal to representation rather than unsafe type inference.
A read-only isolated-proxy diagnostic returned the faithful renamed schema.

A public row-predicate probe was not evaluated during successful explicit-key
construction, nor during the refused grouped construction. Explicit collection
subsequently evaluated the probe on all three source rows, proving the probe
was live. Caller-owned data.tables remained identical to deep copies throughout.
Ticket #837 required accepting safe grouping compositions while preserving
ADR 0020's source-read boundary, ADR 0029's Mutable-step refusal, and refusals
for genuinely row-dependent metadata.

## F5: scalar labels for higher-dimensional arrays escaped validation

The related existing ticket #834 was read with its comments before checking
both nesting interfaces independently:

```r
pkgload::load_all(quiet = TRUE)
x <- tibble::tibble(g = array(1:8, c(2, 2, 2)), value = 1:2)
error <- tryCatch(nest_with_margins(x, .grouping = rollup(g)), error = identity)
cat(conditionMessage(error), "\n")
# In argument: g = as.character(.data[["g"]]).
# g must be size 2 or 1, not 8.
stopifnot(inherits(error, "marginplyr_error"))
```

Both nesting verbs produced this external mutate error from package-generated
conversion with the default scalar label; neither returned the intended
schema-based Package condition. Both succeeded with `.margin_label = NULL`,
returning three outer rows (two detail groups and one Grand total set).
The local type/membership matrices had used typed-missing labels for arrays,
so their passing results had not exercised this failing default-label boundary.
No second issue was needed for the shared validation defect.

## F6: a helper argument needing types was answered from names alone

Existing ticket #825 was also read before independent confirmation through
inspection and both nesting interfaces:

```r
pkgload::load_all(quiet = TRUE)
x <- tibble::tibble(a = c("x", "y"), b = c(1, 1))
selected <- dplyr::select(
  x, dplyr::all_of(names(Filter(is.numeric, tidyselect::peek_data())))
)
print(names(selected))
# [1] "b"
result <- nest_with_margins(
  x, .by = dplyr::all_of(names(Filter(is.numeric, tidyselect::peek_data())))
)
print(names(result))
# [1] "a" "b" "data"
print(nrow(result))
# [1] 2
stopifnot(identical(names(result), c("b", "data")), nrow(result) == 1L)
```

The intended numeric fixed key was only `b`; the result should have held one
group with both original `a` values in its cell. Both nesting verbs instead
fixed on `a` and `b`, returned two groups, and removed `a` from their payload.
Inspection reported the same wrong fixed keys. Treating `all_of()` as settled
from names ignored its argument's dependence on `peek_data()` types. The
cross-backend plan comparisons used simple helper arguments and therefore
had not tested this counterexample. Existing #825 covered the selection
boundary; no duplicate ticket was published.

## Search coverage and controls

These were completed experiments, not additional permanent package tests.
Counts name different units and were not combined into a single test total.

| Experiment | Completed coverage and result |
|---|---|
| Existing relevant suite | Seven test files passed: nest-operation, grouping-plan, grouping-interface, inspect-grouping, grouping-backends, dtplyr-special-nesting, metamorphic-margin-semantics |
| Specification algebra | 1,200 generated ASTs, seed 20261025; independent set-union/prefix/subset/product oracle; included/omitted sets, masks, occurrence order, keep/drop and fixed keys all matched |
| Nested grammar | 150 parent-kind/child-kind/spelling/collision combinations passed; additional lexical quosure, splicing, missing-position, and empty-resolution controls passed |
| Local types | 720 calls: 20 types in fixed/dimension/payload roles, both verbs, both keep modes, all sort choices; source cardinality and exact kept projections passed |
| Local membership | 768 calls: all 64 pairs of eight dimension types, cube, missing fixed keys, both verbs/keep modes/all sort choices; independent `vctrs::vec_group_loc()` source-membership oracle passed |
| dtplyr names | 572 local/dtplyr comparisons across 29 names and five roles; no mismatch |
| dtplyr types | 360 comparisons; 256 matched; 104 differences reproduced as upstream grouping/union/root-conversion limits below |
| dtplyr plans | 6,000 comparisons, seed 8675309: 100 inputs of 0-9 rows, ten plans, two keep modes, three sort modes; all matched, including requested outer order |
| dtplyr derivations | 800 completed comparisons; 764 matched and 36 exposed F4; caller-table mutations: zero |
| Cross-backend plans | 179 exact inspections across local, dtplyr, Arrow, SQLite, DuckDB; simple name/position/helper selections and supported numeric predicates matched |
| Grouping helper identity | Five backend comparisons: `grouping_id()` and `grouping_bit(a)` agreed with the inspected plan indexed by `.id` |

The existing-suite command was:

```sh
Rscript -e 'devtools::test(filter="(nest-operation|grouping-plan|grouping-interface|inspect-grouping|grouping-backends|dtplyr-special-nesting|metamorphic-margin-semantics)", reporter="summary")'
```

Generated ASTs used at most three arguments per node and depth three over
three column names. Their independent oracle used no package compiler or
expansion function. Local types included precision-colliding doubles,
NA/NaN/infinities, complex/raw, Date/zoned POSIXct/difftime, unused and ordered
factor levels, distinct NA levels and missing codes, list/packed frame/matrix/
3D array, and zero-column structured vectors. Kept cell values were compared
with independently sliced source rows. The local membership oracle distinguished
an NA factor level from an actual missing code.

The dtplyr plan loop included duplicate source rows, missing fixed/dimension/
payload values, factors with NA or unused levels, optional `.id`, NULL/default
labels, empty/single/composite sets, rollups, cube, unions, and products. The
name/type/derived loops compared typed outer-key and cell row multisets;
only the 6,000-plan loop also retained requested physical outer order. They
did not promise equal backend-specific cell classes. Both-error outcomes in
the name loop meant matching refusal, not identical diagnostics.

The derived loop covered roots, filter/select/reorder/rename/relocate/arrange,
grouping, safe copy/arithmetic mutation, and ordinary joins/set operations.
Its first run stopped after 720 completed comparisons when a fixture tried to
rename a column to `..1`, which tidyselect itself refused. That invalid fixture
was excluded; a separate run completed the remaining 80 valid comparisons.
The algebra harness's initial NULL-versus-empty-vector oracle error was
corrected before its full run. Neither harness correction was a package bug.

## Candidates not promoted and limits

- data.table itself refused complex/raw or list grouping keys. Its union route
  also refused complex/raw payloads across two ordinary dtplyr branches.
  `lazy_dt()` itself flattened packed frames/matrices before marginplyr.
  These controls exposed limits in the attempted type comparisons; no new
  nesting preservation promise was inferred. They did not establish every
  immediate error cause: [#833](https://github.com/sayuks/marginplyr/issues/833)
  separately tracked a package-generated omitted-list expression in expansion.
  Its construction-time parser failure also preceded unsupported list grouping
  in a nesting rollup. Ordinary list payloads nested successfully; the upstream
  list-grouping limit did not refute the separate expansion defect.
- dplyr/tidyselect rejected `...` and `..1` as column names. Documented
  row-wise-input and grouped `.drop = FALSE` refusals, Mutable-step refusal,
  and data.table's inability to retain rows without columns were not bugs.
- A bare external-vector active binding was read in preflight and selection.
  No wrong valid-plan result was established, and tidyselect deprecated that
  spelling; the read counter alone was not promoted to a finding.
- This bounded search did not establish correctness for arbitrary custom S3/S4
  methods, unbounded specification size/depth, every remote dialect, or every
  dependency version. Arrow CPU-cache `sysctlbyname` diagnostics were emitted
  in the sandbox; the cross-backend process nevertheless finished successfully.

## Publication checks

Only this dated investigation note was added, a Repository-only change under
`design/agents/local-checks.md`; the package-affecting Review-ready check was
not required. `jarl check .`, package-aware `lintr::lint_package()` after
`pkgload::load_all()`, and `verify-context-budget.R` passed. The context closure
was 20,008 bytes against its 22,005-byte baseline. CI monitoring was outside the
requested work.
