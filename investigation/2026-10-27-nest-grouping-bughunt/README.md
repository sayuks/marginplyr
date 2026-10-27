# Nesting and Grouping specification bug hunt

Investigated: 2026-10-27
Base: `13dd7c30ac38fe3fc82e74c561d8795a6c4b6d17` (`main` and `origin/main` after fetch)

## Scope and outcome

The investigation examined `nest_with_margins()`, `nest_by_with_margins()`,
`grouping_set()`, `grouping_sets()`, `rollup()`, `cube()`, `grouping_spec()`,
and `inspect_grouping()`. The shared compilation, metadata, labeling, and
Grouping-identity paths were exercised where they affected these interfaces.
Three independent defects were reproduced and published:

| Issue | Observed defect | Priority | Blocked by |
| --- | --- | --- | --- |
| [#923](https://github.com/sayuks/marginplyr/issues/923) | A typed-missing raw Margin dimension became an ordinary zero byte | P2 | None |
| [#924](https://github.com/sayuks/marginplyr/issues/924) | A foreign qualified selection function was misclassified by its export name, losing fixed-key partitions | P2 | None |
| [#925](https://github.com/sayuks/marginplyr/issues/925) | Parenthesized empty tidy forwarding changed omitted-argument semantics and leaked internal missing-argument errors | P2 | None |

Each ticket covered one cause and a complete public-interface regression path;
shared symptoms across verbs were kept together. All three received `bug` and
`ready-for-agent`. No implementation, package tests, generated documentation,
or dependency metadata was changed by the investigation.

## Reproduction bundle

Run from the repository root with the recorded dependencies available:

```sh
Rscript investigation/2026-10-27-nest-grouping-bughunt/reproduce.R raw
Rscript investigation/2026-10-27-nest-grouping-bughunt/reproduce.R selectors
Rscript investigation/2026-10-27-nest-grouping-bughunt/reproduce.R missing
```

Each command asserted the intended public behavior and exited **1** on the
investigated snapshot. The `selectors` mode built and installed a disposable
local fixture package under a temporary directory; it downloaded no package
and removed its fixture after execution. The retained combined output was
[reproduce.txt](reproduce.txt):

```text
raw: checks 50 failures 48
selectors: checks 48 failures 40
missing: checks 46 failures 35
TOTAL: checks 144 failures 123
```

The successful checks were controls. Multiple failing checks represented
variants of the same three defects, not 123 separate findings. The raw mode
accepted either a genuine missing result or an explicit Package-condition
refusal, so it caught the silent ordinary-value substitution without requiring
a missing representation that raw vectors cannot provide. These scripts were
standalone evidence under the package-excluded investigation directory.

[environment.txt](environment.txt) recorded R and dependency versions. Execution
used R 4.6.1 on aarch64 macOS, dplyr 1.2.1, tidyselect 1.2.1, rlang 1.3.0,
vctrs 0.7.3, dtplyr 1.3.3, data.table 1.18.6.1, DuckDB 1.5.5, RSQLite
3.53.3, and Arrow 25.0.1. Every probe loaded the investigated working tree with
`pkgload::load_all()`.

## Finding 1: an omitted raw key was nonmissing

A one-row tibble with `g = as.raw(1L)` and a payload, nested with `rollup(g)`,
`.margin_label = NULL`, and `.id = "set"`, returned outer keys `01, 00`
with set identifiers `1, 2`. The Grand total set's `00` was nonmissing under
both `is.na()` and `vctrs::vec_detect_missing()`. A real source key could
also contain `00`.

The result contradicted the inherited *Display labels and grouping identity*
contract: NULL and NA labels represented omitted dimensions with typed missing
values. Raw had no missing representation. Both `x[NA_integer_]` and
`vctrs::vec_init(x, 1L)` produced an ordinary zero byte for raw input.

Three explanations were compared: prototype construction, label normalization,
and collision checking. NULL versus `NA_character_` and enabled versus disabled
collision checks produced the same nonmissing key. A character display label
on an atomic raw dimension worked. Raw fixed keys and raw dimensions included
in every set retained their source values. These controls isolated the
omitted typed-missing path.

Inspection of `margin_column_info()` in `R/backend-metadata.R` and
`label_margin_branch()` in `R/margin-label.R` explained the measurement:
the former built a supposed missing prototype with a missing subscript for
atomic columns or `vec_init()` for structured columns; the latter inserted it
without checking that the value was missing. Atomic raw, a raw matrix, and a
packed data-frame dimension containing raw all reproduced the defect. Both
nesting verbs and the shared expansion and summarization paths were affected.
The original raw values inside `.keep = TRUE` cells remained intact.

The ticket requested an actionable Package condition when an omitted dimension
could not represent the promised typed missing, preserving fixed keys,
included-only dimensions, supported explicit labels, and structured dimensions
that did have a missing representation. Its shape followed the existing
label contract and ADR 0015's Package-condition boundary.

## Finding 2: a foreign helper's name changed its result

A disposable package exported `choose_character()`, which selected character
columns through `tidyselect::peek_data()`, and assigned the same function
object to the ten optimized leaf-helper names:
`all_of`, `any_of`, `starts_with`, `ends_with`, `contains`, `matches`,
`num_range`, `everything`, `last_col`, and `one_of`.

For `tibble(a = c("x", "y"), b = c(1, 1))`, ordinary dplyr selection,
the ordinary qualified export, and a local alias all selected `a`.
The genuine `tidyselect::starts_with("a")` agreed. In contrast, each foreign
qualified leaf-helper spelling selected no fixed key in `inspect_grouping()`.
Both nesting verbs consequently returned one cell holding both rows, rather
than two cells keyed by `a`. In `rollup()`, the same spellings falsely raised
`requires at least one dimension`.

The competing explanations were export-name classification, captured-environment
loss, and a limitation of the selection function. Identical function objects
under an ordinary export and a local alias worked, as did direct dplyr
selection. The failure followed the qualified helper name. The
`is_name_only_expr()` leaf-helper branch in `R/grouping-plan.R` checked
`static_call_name()` and the arguments, but did not establish the qualifier
or actual callee. Its names-only proxy held numeric columns, so the ordinary
function's character-column predicate selected nothing. The `.by` path retained
that wrong selection as its resolved fixed keys.

Local, dtplyr, and DuckDB inspection separated the foreign spelling from the
working controls. Both local and dtplyr nesting interfaces showed the silent
partition loss. SQLite was excluded from that comparison: its alias control
also lacked typed column metadata, so it did not isolate this defect.

The ticket requested typed selection for foreign helpers whose name-only
semantics were unproven, while preserving genuine-helper early errors,
caller evaluation budgets, and ADR 0020's restrictions on source-row reads.

## Finding 3: redundant parentheses exposed missing markers

This natural wrapper reproduced an internal missing-argument error:

```r
wrap <- function(x) grouping_sets(({{ x }}), a)
inspect_grouping(data.frame(a = 1), .grouping = wrap())
```

The error said `argument "expr" is missing, with no default` and blamed
`nested_arg_quosure(arg)`. Without the redundant parentheses, the error was
`marginplyr_error`, identified argument 1 of `grouping_sets()` as empty, and
blamed the public verb. Explicit `enquo()` followed by parenthesized injection
also reached that intended refusal.

The failure was broader than diagnostic quality. A wrapper forwarding an
omitted argument directly to `.by = ({{ x }})` or
`.grouping = ({{ x }})` failed, whereas the unparenthesized spelling
successfully used the documented absent/default meaning. The matrix measured
10 successful plain controls and 10 parenthesized false refusals across five
public consumers and the two arguments. Constructor positions leaked internal
errors in all 25 combinations of five constructors and five consumers.

The hypotheses were a lost missing value during capture, an evaluation-environment
change, and incomplete empty detection after unwrapping. Comparing the captured
shapes and controls supported the last: `is_empty_argument()` did not open a
quosure inside parentheses. Constructor preflight checked emptiness before
`nested_arg_quosure()` opened it; a later loop iteration then bound the
missing expression to the local `expr`. The public `.grouping` path similarly
bound a missing result to `grouping_spec`. The `.by` path reached tidyselect
after marginplyr missed its own absent-argument interpretation.

Direct dplyr selection also rejected one parenthesized missing shape. That was
kept separate from the evidence above: marginplyr's constructor error arose
before tidyselect, and its documented Nested specification position promised
parenthesis transparency and a constructor/position refusal. The earlier
[#897](https://github.com/sayuks/marginplyr/issues/897) fixed nonempty injected
specifications; its body and the inspected tests did not cover missing payloads.

## Breadth of the hunt

The strategy combined an independent specification model, source-row membership
oracles, direct tidyselect comparisons, backend differential runs, and explicit
admission/nonmutation probes. It did not use the implementation's compiled
Grouping plan as the sole oracle for that plan.

Retained broad probes:

| Command, after the directory prefix below | Measured scope and result |
| --- | --- |
| `spec-algebra.R` | Seed 94813; 3,000 random ASTs of all five kinds, composite dimensions, identity products, duplicate keep/drop, and plan order; all agreed with an independent list/set/product model |
| `identity.R` | Seed 271027; 107 cases including 0/1/2/3/30/31/32 dimensions, duplicate sets, and inspection/summary mask correspondence; all passed |
| `remote-masks.R` | Seed 102727; 80 specifications x 2 duplicate policies x DuckDB/dtplyr/Arrow = 480 comparisons of summary masks to inspection, with Margin order; all passed |

Run each as `Rscript investigation/2026-10-27-nest-grouping-bughunt/<name>`.
The adjacent `.txt` files retained their completed output, with trailing
whitespace removed. Arrow printed denied `sysctlbyname` cache-size probes in
this sandbox; the remote-mask assertions still completed with exit 0. These probes
required the dependencies they exercised; they did not install them.

Additional exploratory probes were run from temporary scripts. Their scope and
outcomes were recorded here, but those one-off scripts were not retained:

| Probe | Measured scope and outcome |
| --- | --- |
| Local nesting membership | 2,554 cases: 504 ordinary types, 156 NA/NaN/complex/integer64 variants, 466 accepted special-name cases, 456 empty cases, 972 fixed/grouped/data.table cases; no membership/value/name/count defect beyond the separately asserted raw-label finding |
| Selection-expression differential | Seed 141422; 2,000 generated expressions over mixed types, selection operators, predicates, helpers, pronouns, external vectors, and renaming; all matched direct tidyselect behavior |
| Backend payload/name differential | 420 comparisons over 10 payload shapes, 21 list-column names, and both keep settings; 228 candidate differences were traced to upstream input conversion or unsupported union types |
| Typed inspection differential | Three specification shapes x 11 selections x dtplyr/DuckDB/Arrow = 99 comparisons against local inspection; all agreed |
| Random backend nesting | Seed 527; 160 inputs of 0-12 rows, mixed keys, four specification shapes, both nesting interfaces, keep/label/sort variants; no difference or caller-table mutation |
| dtplyr metadata shapes | 19 root/filter/select/rename/arrange/mutate/join/set shapes x 3 typed selections = 57 comparisons; five unsafe transformations were refused before evaluation |
| Admission and input preservation | 12 SQL/Arrow nesting refusals; reader nonconsumption during inspection/refusal; mutable root/filter/select/rename refusal and inspection nonmutation; all passed |
| integer64 boundary | 18 label/keep/sort variants around 9007199254740992 and 9007199254740993, negatives and NA; no additional difference observed |

The existing focused test command also completed with exit 0:

```sh
Rscript -e 'devtools::test(filter = "^(nest-operation|nest-rows|grouping-interface|grouping-plan|inspect-grouping|injected-nested-grouping|cyclic-grouping|dtplyr-.*nesting)$", reporter = "summary", stop_on_failure = TRUE)'
```

It ran nine files: cyclic grouping, dtplyr AsIs nesting, dtplyr special nesting,
grouping interface, grouping plan, injected nested grouping, inspection,
nesting operation, and nesting `.rows`. No failure or warning was reported.

## Rejected candidates and limits

- Complex missing values exposed a difference between `dplyr::group_by()` and
  `.by`. Marginplyr agreed with `.by` and `vctrs::vec_group_loc()`, so this
  was not filed as a marginplyr defect.
- Matrix, packed, array, and POSIXlt input could already be transformed or
  invalidated by `lazy_dt()` input conversion. Raw/complex payload unions
  also failed under bare `dplyr::union_all()`. These were separated from
  marginplyr's raw-dimension missing-value construction.
- `...` and `..1` were rejected by upstream name rules. Unrequested row order
  and arbitrary input attributes/subclasses were outside the promises in
  ADR 0016 and ADR 0018.
- Bare or parenthesized-head selection helpers still resolved from tidyselect's
  own context when the caller bound that name. This rejected the simple
  caller-masking hypothesis; the foreign qualified-export defect remained.
- Early harness errors included zero-length `paste0()` name construction,
  R's precedence of `!` in a mask oracle, matrix slicing with base indexing,
  and generated `!!` being interpreted as tidy injection. Their oracles were
  corrected before the reported comparison runs; they were not findings.
- No external PostgreSQL server, other remote SQL dialect, alternate OS,
  minimum supported dependency versions, or unbounded specification size was
  exercised. Finite property runs provided evidence for their sampled cases,
  not a proof that all remaining inputs were correct.

CI monitoring was outside the requested work. This dated note recorded
observations and ticket recommendations; the referenced API contracts, ADRs,
and implementation remained the authority for subsequent behavior.
