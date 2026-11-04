# lintr 3.4.0 / Jarl 0.6.0 overlap evidence

Investigated: 2026-11-04

This compared installed lintr 3.4.0 function bodies with Jarl 0.6.0 tag
`7ac3b8d0b510abbb51102d74263e639c2ea2aad6`, and ran 32 small independent
fixtures. The fixtures only established those examples, not universal parser
or semantic equivalence. No repository configuration was changed.

The tested Jarl selection was its defaults plus `DPLYR,expect_not`, matching
the repository's `jarl.toml`. A tempfile outside the package avoided package
cross-file analysis; the explicit extension selection supplied the repository
rule selection. None of these examples executed user R expressions.

## Conclusions for benchmark interpretation

- `assignment`, `pipe_consistency`, and `quotes` existed in Jarl 0.6.0 but
  were **disabled by default** and were not enabled by `DPLYR,expect_not`.
- Jarl had no counterpart to lintr's default implicit-return enforcement.
  `pipe_return` detects `return()` on the RHS of `%>%`; it does not enforce
  implicit returns at the end of functions.
- Removing `equals_na_linter`, `seq_linter`, and `vector_logic_linter`
  demonstrably loses findings. A benchmark removing them measures a policy
  reduction, not equivalent de-duplication.
- `T_and_F_symbol_linter` was the closest narrower removal candidate. The
  12 T/F examples found no lintr-only finding and one Jarl-only finding
  (`x ~ I(T)`). The source comparison likewise found Jarl's formula exclusion
  narrower than lintr's. This supports consideration after file/suppression
  scope is checked, not a claim of universal equivalence.
- The 26-to-22 experiment removing the four preceding active analogues must
  be described as a non-equivalent configuration. Removing assignment or
  return as well would have no basis in the enabled Jarl policy.

## All 26 default linters

`None` means no rule with this contract in Jarl's tagged registry, rather than
proof that no unrelated rule could ever report the same expression.

| lintr default | Jarl 0.6.0 counterpart under repository selection | Disposition |
|---|---|---|
| `assignment_linter` | `assignment` exists but disabled | Keep; lintr also covers `<<-`, `->>`, `%<>%` |
| `brace_linter` | None | Keep; formatting policy |
| `commas_linter` | None | Keep; formatting policy |
| `commented_code_linter` | None | Keep; detects executable-looking comments |
| `equals_na_linter` | `equals_na`, enabled | Partial overlap; keep for both-sides-NA cases |
| `function_left_parentheses_linter` | None | Keep; formatting policy |
| `indentation_linter` | None | Keep; formatting policy |
| `infix_spaces_linter` | None | Keep; formatting policy |
| `line_length_linter` | None | Keep; line-width policy |
| `object_length_linter` | None | Keep; name-length/S3-generic-aware policy |
| `object_name_linter` | None | Keep; naming/S3-generic-aware policy |
| `object_usage_linter` | No equivalent; `unused_function` is enabled, `unused_object` disabled | Keep; codetools lexical usage/unbound-name checks require package environment |
| `paren_body_linter` | None | Keep; space between closing parenthesis and body, not redundant parentheses |
| `pipe_consistency_linter` | `pipe_consistency` exists but disabled | Keep; enabled form also has deliberate non-equivalent-pipe exceptions |
| `pipe_continuation_linter` | None | Keep; multiline pipe layout |
| `quotes_linter` | `quotes` exists but disabled | Keep |
| `return_linter` | None; `pipe_return` is a different contract | Keep |
| `semicolon_linter` | None | Keep; no Jarl semicolon rule |
| `seq_linter` | `seq` and `seq2`, enabled | Partial overlap; keep wider patterns |
| `spaces_inside_linter` | None | Keep; formatting policy |
| `spaces_left_parentheses_linter` | None | Keep; formatting policy |
| `T_and_F_symbol_linter` | `true_false_symbol`, enabled | Closest candidate; bounded probes/source suggest Jarl covers tested lintr findings |
| `trailing_blank_lines_linter` | None | Keep; formatting policy |
| `trailing_whitespace_linter` | None | Keep; formatting policy |
| `vector_logic_linter` | `vector_logic`, enabled | Partial overlap; keep expectations and subsetting checks |
| `whitespace_linter` | None | Keep; formatting policy |

## Differential findings

The runnable script was saved as `probes.R`;
raw result table was `probes.tsv`. It uses
`processx` to preserve Jarl exit status, accepts statuses 0/1, requires empty
JSON parser errors, and applies one selected lintr rule per example.

| Example | lintr | Enabled Jarl selection |
|---|---|---|
| `x = 1`, `1 -> x`, `x <<- 1`, `x %<>% f()` | assignment finding | None |
| `x == NA`, `x %in% NA` | equals-NA finding | `equals_na` |
| `NA == NA`, `NA_character_ != NA_real_` | equals-NA finding | None |
| `1:length(x)`, `seq(length(x))` | sequence finding | `seq`, `seq2`, respectively |
| `length(x):1`, `1:n()`, `1:.N` | sequence finding | None |
| `seq_len(length(x))`, `unlist(lapply(x, seq_len))` | sequence finding | None |
| `f <- function(x) { return(x) }` | return finding | None |
| `if (x & y) z` | vector-logic finding | `vector_logic` |
| `expect_true(x & y)`, `subset(x, a && b)`, `filter(x, a && b)` | vector-logic finding | None |
| `x <- T`, `T <- 1`, `x[T]`, `foo(x = T) ~ y`, `function(T) T`, `f <- function(T) T`, `f <- function(x = F) x` | T/F finding | `true_false_symbol` |
| `T[x]`, `x ~ T`, `x$T`, `foo(T = 1)` | None | None |
| `x ~ I(T)` | None | `true_false_symbol` |

Source explanations matched these observations. Jarl `equals_na()` explicitly
returns without a finding when both operands are recognized NA spellings.
Its `seq`/`seq2` accept only length/nrow/ncol/NROW/NCOL with the inspected
operator/call shape, while lintr additionally recognizes `n`, `dim`, `.N`,
reverse colon sequences, `seq_len(length(...))`, and concatenated sequences.
Jarl vector logic only inspects `&`/`|` at the top of an if/while condition;
lintr includes testthat scalar expectations and rejects `&&`/`||` in
subset/filter expressions. `DPLYR` supplied no missing vector-logic finding
for the tested `filter()` expression.

## Scope and suppression caveats

Jarl's pinned documentation says `.R`, `.Rmd`, `.rmd`, `.qmd` are discovered,
with generated-file and default filename exclusions. lintr's installed
`lint_package()` selected `R`, `tests`, `inst`, `vignettes`, `data-raw`, `demo`,
and `exec`, with a broader case-insensitive extension pattern including
Rnw/Rhtml/Rrst/Rtex/Rtxt. Therefore a rule-level comparison alone does not
establish equal file coverage. Jarl also excluded `# Generated by` files,
`cpp11.R`, `extendr-wrappers.R`, and `import-standalone-*.R`, among others;
lintr's default package exclusion explicitly named `R/RcppExports.R`.

Jarl documentation explicitly distinguished its suppression syntax from
lintr's `# nolint`. A future removal must inspect which findings are
suppressed independently, rather than translating names mechanically.

Separate tiny `.qmd` and `.Rmd` fixtures contained both `Value `r T`.` inline
text and an R chunk with `x <- T`. Both lintr 3.4.0 and Jarl 0.6.0 reported
only the chunk's T on line 4. Jarl's documentation explicitly excludes inline
R; these two examples do not establish document-parser equivalence generally.

## Primary sources

- [Jarl 0.6.0 rule registry](https://github.com/etiennebacher/jarl/blob/7ac3b8d0b510abbb51102d74263e639c2ea2aad6/crates/jarl-core/src/rule_set.rs): enabled/disabled metadata and complete inventory.
- [Jarl equals_na implementation](https://github.com/etiennebacher/jarl/blob/7ac3b8d0b510abbb51102d74263e639c2ea2aad6/crates/jarl-core/src/lints/base/equals_na/equals_na.rs).
- [Jarl seq implementation](https://github.com/etiennebacher/jarl/blob/7ac3b8d0b510abbb51102d74263e639c2ea2aad6/crates/jarl-core/src/lints/base/seq/seq.rs), [seq2 implementation](https://github.com/etiennebacher/jarl/blob/7ac3b8d0b510abbb51102d74263e639c2ea2aad6/crates/jarl-core/src/lints/base/seq2/seq2.rs).
- [Jarl vector_logic implementation](https://github.com/etiennebacher/jarl/blob/7ac3b8d0b510abbb51102d74263e639c2ea2aad6/crates/jarl-core/src/lints/base/vector_logic/vector_logic.rs).
- [Jarl true_false_symbol implementation](https://github.com/etiennebacher/jarl/blob/7ac3b8d0b510abbb51102d74263e639c2ea2aad6/crates/jarl-core/src/lints/base/true_false_symbol/true_false_symbol.rs) and sibling `options.rs`: no default skipped functions.
- [Jarl pipe_return documentation](https://github.com/etiennebacher/jarl/blob/7ac3b8d0b510abbb51102d74263e639c2ea2aad6/docs/rules/pipe_return.md).
- [Jarl configuration file reference](https://github.com/etiennebacher/jarl/blob/7ac3b8d0b510abbb51102d74263e639c2ea2aad6/docs/reference/config-file.md), [document support](https://github.com/etiennebacher/jarl/blob/7ac3b8d0b510abbb51102d74263e639c2ea2aad6/docs/howto/rmarkdown-quarto.md), [suppression compatibility](https://github.com/etiennebacher/jarl/blob/7ac3b8d0b510abbb51102d74263e639c2ea2aad6/docs/howto/suppression-comments.md#compatibility-with-lintr).
- Installed lintr 3.4.0 exported function bodies; the corresponding upstream
  [v3.4.0 source tree](https://github.com/r-lib/lintr/tree/v3.4.0/R).
  Tag lookup returned `ae8cda850cbbd07191ed7fbd12d7e967353ebe45`.


## Package benchmark

The prototype was stored in `reduced.lintr` and copied to the root of a
disposable Git archive. It used the official `linters_with_defaults()` API
to remove `equals_na_linter`, `seq_linter`, `T_and_F_symbol_linter`, and
`vector_logic_linter`. The baseline had 26 default linters, the prototype
22. The repository's root configuration and CI/local gate definitions were
unmodified by the experiment.

The archived SHA was `fc7f17b16789864a1efa460123e3ad0c7e7b9120`.
Its package sources matched the preceding investigation's
`f8a2a964b50ff4d7d42e3c5ba3ea917a70147203`; the intervening commit added
only the earlier research note. R 4.6.1, lintr 3.4.0, pkgload 1.5.3 and
Jarl 0.6.0 were installed on the Apple Silicon Mac.

Each measurement started a fresh R process, called `pkgload::load_all()`,
and called `lintr::lint_package(cache = FALSE)`. All 290 discovered
files remained eligible, including the five Quarto vignettes. The process
asserted zero findings and used `LINTR_ERROR_ON_LINT=true`.
No parallelism or test suite was involved.

Three measurements per variant ran sequentially in the order
baseline, reduced, reduced, baseline, baseline, reduced. This balanced
some order effects, but the shared machine was not isolated from other
work. The raw timings were retained in `timings.csv`, and versions in
`environment.txt`. Earlier one-off measurements in this conversation
were taken under different machine load and were not used as controls.

| Variant | Lint elapsed observations (s) | Median lint elapsed (s) | Median lint CPU (s) | Median whole R process (s) |
| --- | --- | ---: | ---: | ---: |
| Default 26 rules | 33.393, 32.725, 32.309 | 32.725 | 32.643 | 33.357 |
| Reduced 22 rules | 30.391, 30.811, 30.184 | 30.391 | 30.312 | 31.021 |

The median lint reduction was 2.334 s, approximately 7.1%.
All six processes passed with zero findings on the unchanged package.
Jarl's rule selection and inputs were held constant, so the table measured
the lintr portion of the combined workflow. It did not measure total
Review-ready or hosted CI turnaround.

Run from this worktree root, using a new external output directory:

```sh
mkdir /private/tmp/marginplyr-overlap-reproduction
Rscript investigation/lintr-jarl-overlap-2026-11-04/benchmark.R /private/tmp/marginplyr-overlap-reproduction
Rscript investigation/lintr-jarl-overlap-2026-11-04/probes.R /private/tmp/marginplyr-overlap-probes.tsv
```

The harness archives the invoking worktree's HEAD and records its SHA.
To reproduce the historical measurement, use the recorded baseline SHA
with the harness and configuration from this experiment.

## Benefits, risks, and disposition

The prototype used a short official configuration, retained package loading
and `object_usage_linter()`, and reduced measured lint runtime by about
two seconds. It added no worker management or cache invalidation machinery.

The three lost contracts in the differential table affected real bug
patterns, including empty reverse sequences and scalar operators used for
row filtering. Full-default versus full-reduced probes also confirmed that
the other 22 linters did not recover `NA == NA`, `length(x):1`, or
`filter(x, a && b)`. Every one had one default finding and zero reduced
findings. These were also missed by the enabled Jarl selection.

The investigation therefore did not recommend adopting the four-rule
removal for speed. The narrower T/F-only candidate had no lost findings in
12 examples but did not have an independent package timing measurement.
Its file-discovery and suppression differences would still need a policy
decision. A general claim that Jarl made these four lintr rules redundant
was not supported.
