# Contextual-share probes

Investigated: 2026-11-05
Repository snapshot: `f158df41168bbf0b64bce01518fcff1bd88cb11f`

ADR 0010 requires a self-contained source and refuses an ordinary summary
that depends on a preceding share. ADR 0017 gives Total shares the same
contract. The public call in the dependency matrix summarized two groups,
a and b, with x = 1/3 and a source column prior = 100/300. The forward
direction defined source = sum(x), prior = share_of_total(source), then the
ordinary observed summary. The reverse direction defined prior = sum(x),
observed as a share source, then ratio = share_of_total(observed). Both
directions were repeated with share_of_parent. The same visible values
applied to the corresponding Parent shares on this one-dimensional rollup.

## A. Literal frame depth changed the dependency decision

A minimal expression for observed was:

```r
(function(prior) {
  (function() (function() rm("prior", envir = parent.frame(2)))())()
  sum(prior)
})(1)
```

The forward call returned observed = 100/300/400 and prior = .25/.75/1.
The reverse call returned observed = 1/3/4 and ratio = .25/.75/1. Both
should have reported the documented dependency Package condition. Changing
only `2` to `2L` produced the refusal. Literal `parent.frame(3L)` through
three direct invocations and `sys.frame(sys.parent(1L))` and
`sys.frame(sys.parent(2L))` also bypassed the checks. Omitted
`sys.parent()` gave the refusal.

`static_parent_frame_depth()` recognized one as a double or integer, two
only as an integer, and `sys.parent()` only with its argument omitted.
The matching issue #1230 explicitly bounded its acceptance criteria to
the reproduced forms, which were `parent.frame(2L)`, omitted
`sys.parent()`, and literal inheritance. PR #1251 reported regressions
for that bounded set, and the two prior frame repros here passed on main.
#1224 covered the immediate enclosing frame and #1269 covered implicit
`local()`. Neither covered these literal call forms.

## B. A named literal closure read a binding at invocation

```r
(function(prior) {
  read_prior <- function() sum(prior)
  rm(prior)
  read_prior()
})(1)
```

The closure's written body was analyzed while the formal was bound.
Its invocation was analyzed as a function lookup without rewalking that
body. It read the data mask after removal during actual evaluation,
producing the same 100/300/400 and 1/3/4 outcomes as A. Defining the closure
after removing the binding correctly refused, and invoking it before
removal gave one in every group. A literal formal default containing the
closure had the same issue. A named local literal closure could itself
remove the enclosing formal via `parent.frame()` without detection.
Writing the latter closure as a direct inline invocation correctly refused.

The #1224 brief excluded separately defined functions and bounded its
repair to directly invoked nested inline functions. The present closures
were defined and invoked in the same visible expression. #1232 and PR
#1251 classified literal functions for call-head lookup and shadowing of
`rm()`. They did not carry the lexical reads or effects of this named
closure to the invocation. #1042 and PR #1064 covered removed bindings
in nested expressions and explicit reparenting.

## C. Known data-mask environments lost direct field reads

```r
sum(parent.env(environment())$prior)
```

The direct field read bypassed both dependency rules and returned the same
values as A. The equivalent base `[[ "prior" ]]` access and inheriting
`rlang::env_get(rlang::current_env(), "prior", inherit = TRUE)` agreed.
Base `get("prior", envir = parent.env(environment()))` correctly refused
both forbidden dependencies. Field access and `get()` constrained to a
separate environment with prior = 7 both returned seven for every row.

The ordinary field path in `scoped_data_symbols()` discarded the
literal member name without establishing the target was outside the mask.
The supported reflective primitive path recognized base `get()` and
classified its environment. The rlang accessor did not reach that path.
#872 covered explicit mask lookup through the demonstrated
`get()`, `get0()`, `exists()`, and `mget()` forms. #101 preserved the
opposite case, an ordinary object's field with the same spelling as an
alias. Its external-object control remains valid here.

## D. A written NULL became omission in contextual across

```r
summarize_with_margins(
  data.frame(g = c("a", "b"), x = c(1, 3)), value = sum(x),
  dplyr::across(value, share_of_total, .names = "{.col}_share", .unpack = NULL),
  .grouping = rollup(g), .sort = "last"
)
```

The call returned value_share = .25/.75/1. Replacing the literal with a
variable holding NULL or the expression `(NULL)` refused with the intended
Package condition. A NULL injected as a literal reproduced the acceptance.
Omission and FALSE correctly succeeded. TRUE correctly refused. ADR 0010
admits omission and FALSE. The argument parser returned the same unpack
value for an omitted argument and a present NULL, although the retained
argument position distinguished them. The share validator inspected only the
value. #350 and PR #356 covered an empty or injected .names, a different
formal with its own rule.

## Replay and duplicate search

Run from the investigated checkout. Each script loads that checkout using
pkgload. All four contract modes exited one at the missing refusal.
Observation mode completed with expected findings and controls. The
dependency matrix tested 216 direction comparisons over three eager input
classes and two helpers. There were 48 misses for A, 36 for B and 36 for C,
alongside 60 correct refusals and 36 legitimate successes. The unpack
matrix compared 56 calls across those eager classes and dtplyr, with 16
missing refusals, 24 correct refusals and 16 legitimate successes.

```sh
Rscript share-dependencies.R
Rscript share-dependencies.R frames --assert
Rscript share-dependencies.R closures --assert
Rscript share-dependencies.R mask --assert
Rscript share-unpack-null.R
Rscript share-unpack-null.R --assert
Rscript share-volatile-sql.R
```

The SQL loop compared each displayed volatile source against its matching
visible denominator across two in-memory backends, two Grouping
specifications, three sort options and two aggregation options. All 24 calls
matched. The loop compared both helpers for rollups and Total shares for
Grouping-set unions. It checked ratios, with two fixed partitions, and did
not independently assert order or test SQL nonfinite numbers.

All-state issue and PR searches used `parent.frame in:body`,
`"enclosing" "removal" in:title`, `"share" "closure" in:title`,
`"late" "closure" "share"`, `"sys.parent" "share"`,
`"current_env" "share" "alias"`, `"as.list" "share" "alias"`,
`"share" "environment" "env_get"`, `"unpack" "NULL" "share"` and
`"share" "literal" "NULL" in:title`. The matching scope comparisons
are described above. No other result described the same public behavior.

R 4.6.1 on aarch64 macOS, pkgload 1.5.3, dplyr 1.2.1, rlang 1.3.0,
tidyselect 1.2.1, vctrs 0.7.3, tibble 3.3.1, data.table 1.18.6.1,
dtplyr 1.3.3, DBI 1.3.0, RSQLite 3.53.3, dbplyr 2.6.0, DuckDB 1.5.5.
No package tests or implementation files were added or changed.
