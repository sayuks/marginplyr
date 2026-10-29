# Contextual-share scope and denominator investigation

Investigated: 2026-10-29
Repository snapshot: `4c63bb9`

This sub-investigation read the always-loaded context, `CONTEXT.md`, ADRs
0010, 0014, 0017 and 0019, the investigation-note conventions, and the recent
2026-10-28 and 2026-10-29 summary/share bug-hunt notes before probing the
snapshot. It changed no package implementation or existing package test.

Two independently actionable dependency-guard defects were established. The
scripts below exercised public `summarize_with_margins()` calls. Their report
modes exited 0 after recording incorrect behavior and verifying controls;
`--assert` required the documented refusal and exited 1. These deliberate
investigation failures were not failing package regression tests.

## S1: nested removal preserved a binding that no longer existed

The source contract in ADR 0010 required a share source to be self-contained,
without a dependency on an earlier ordinary summary alias. The same decision
required a later ordinary summary to be refused when it used an earlier share.
ADR 0017 applied both rules to Total shares. Both directions were bypassed
when a temporary lexical binding was removed inside another evaluated call.

```r
d <- tibble::tibble(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
summarize_with_margins(
  d,
  prior = sum(x),
  value = { prior <- 0; identity(rm(prior)); sum(prior) },
  fraction = share_of_total(value),
  .grouping = rollup(g)
)
```

The call was accepted and returned `value = 1, 3, 4` and
`fraction = .25, .75, 1`. Removing the temporary binding exposed the preceding
ordinary `prior` summary. Moving `rm(prior)` to its own top-level statement
produced the intended Package condition.

The reverse dependency produced a silently wrong public column:

```r
summarize_with_margins(
  d,
  value = sum(x),
  prior = share_of_total(value),
  derived = { prior <- 0; identity(rm(prior)); sum(prior) },
  .grouping = rollup(g)
)
```

The displayed `prior` shares were `.25, .75, 1`, while `derived` was
`100, 300, 400`, read from the same-named input column. The documented behavior
was a Package-condition refusal; evaluating a staged share was not supported.

Inspection localized the cause to the bound-name tracking used by
`expression_data_symbols()`. `statement_reads_and_bound()` reduced the bound
set for a removal only when that removal was the statement itself. A removal
inside `identity()`, an `if` branch, evaluation of visible quoted language, or
a loop body returned only its read set to the enclosing statement. A name
removed at runtime therefore remained in the static bound set, suppressing the
later genuine data-mask read. Removing a formal or a `for` index exposed the
same defect. No namespace instrumentation or implementation edits were used.

`scope-removal.R` established 38 missing refusals: eight removal wrappers ×
two helpers × two dependency directions, two removed-loop-index cases, and
four removed-formal cases. All 38 calls were accepted. The wrappers included
`rm`, `remove`, a namespaced removal, `list =` naming, conditional removal,
`eval(quote(...))`, `evalq()`, argument blocks and a loop body.

Nine controls passed: four straight-line-removal refusals; four successful
lexical-value cases, including a constant-false removal branch; and one
ordinary sequential-alias call with no share. The data-dependent condition
`sum(x) > 0` was true for every input group in the defect fixture. A condition
whose removal might run cannot prove that the lexical binding survives; the
conservative policy in #130 favors a diagnostic when that is undecidable. The
constant-false control executed no removal and returned its lexical value.

The full bodies and comments of [#130](https://github.com/sayuks/marginplyr/issues/130)
and [#162](https://github.com/sayuks/marginplyr/issues/162) were read.
Those closed issues established function and statement binding analysis; they
did not record propagation of binding removal through these enclosing evaluated
expressions. The new behavior differed from the repaired top-level-removal
control.

Recommended delivery: restore both dependency refusals when evaluated nested
removal can expose the mask, with public regressions for both share helpers,
removal aliases, conditionals, loops and function scopes. Preserve legitimate
lexical-only expressions and ordinary no-share evaluation. No blocking ticket
was needed.

## S2: an external environment ceased to be external while the expression ran

```r
e <- new.env(parent = baseenv())
summarize_with_margins(
  d,
  prior = sum(x),
  value = {
    parent.env(e) <- environment()
    get("prior", envir = e)
  },
  fraction = share_of_total(value),
  .grouping = rollup(g)
)
```

This was accepted with `value = 1, 3, 4`. The visibly written replacement
connected `e` to the active data mask before the reflective lookup. `value`
therefore depended on the earlier ordinary alias. In the reverse direction,
`derived = { parent.env(e) <- environment(); sum(get("prior", envir = e)) }`
was accepted after `prior = share_of_total(value)` and returned
`100, 300, 400` beside the displayed `.25, .75, 1` shares.

The source reader's `lookup_environment_is_external()` proved only that `e`
held an environment at planning time. `lookup_rebound_names()` invalidated that
proof for assignments to the name `e`, but a replacement of its parent did not
rebind that name. The reflective lookup was consequently exempted even though
its inherited lookup path reached the mask at execution. Reparenting in an
earlier visible ordinary summary also left this exemption in force.

`environment-reparent.R` established 48 missing refusals: two helpers × two
dependency directions × three mutation positions × four primitives. The
positions were the same expression, the preceding alias/source ordinary
summary, and a separate earlier ordinary summary. The primitives were `get`,
`get0`, inheriting `mget`, and `exists` converted to an integer. Later-summary
lookups were reduced with `sum()` so that unrelated summary cardinality did
not obscure the missing guard.

Eleven controls passed: local assignment `e <- environment()` and a lookup
whose explicit environment was `parent.env(e)` were conservatively refused
for both helpers; stable external lookup, reparenting to `baseenv()`, and a
non-inheriting lookup of a real external binding succeeded for both helpers;
an ordinary no-share call succeeded. A fresh environment was supplied for
each call, avoiding cross-case parent-state contamination.

The full bodies and comments of
[#173](https://github.com/sayuks/marginplyr/issues/173) and
[#872](https://github.com/sayuks/marginplyr/issues/872) were read. They required
reflective reads to follow the dependency policy and preserved genuinely
external lookup. #872 repaired explicit default/current-mask environment
arguments; its reader exempted an existing environment-valued binding. This
finding invalidated that proof through a statically visible replacement,
rather than using the already-covered current-mask argument spelling. The
public reference and relevant ADRs read for this investigation supplied no
exception for the visible replacement.

Recommended delivery: invalidate the external-environment exemption when
visibly evaluated parent replacement can connect that environment to the mask,
in either dependency direction and across visible summary positions. Preserve
stable genuinely external reads and conservative handling of unresolved
mask-capable environments. This evidence did not demand analysis of mutations
hidden in opaque caller functions. No blocking ticket was needed.

## Independent typed-key denominator oracle

`key-oracle.R` used seed 291043 to permute a finite numeric/missing measure
within 16 six-row key representations: plain numeric with signed zero, NA and
NaN; logical; integer; character with case/whitespace; factor; ordered factor;
factor with an explicit NA level; Date; POSIXct with fractional timestamps and
a timezone; difftime; integer64 values beyond exact double-integer identity;
complex; lists; raw-valued lists; matrix columns; and data-frame columns.

Each fixture was attempted locally and through immutable dtplyr, with ordinary,
composite and repeated-dimension rollups, with and without fixed keys. The
oracle matched each result row to its displayed Parent and Grand total source
values using `vctrs::vec_equal(..., na_equal = TRUE)` on the fixed and retained
keys. It checked both a numeric sum and an integer count, double share types,
zero/missing denominator rules and root shares. Internal share mapping and
ratio helpers were not used by the oracle.

Of 192 attempted result layouts, 162 completed and passed 8,828 scalar-ratio
assertions. Thirty were already refused by the ordinary no-share control:
dtplyr's complex/list keys and flattened matrix/data-frame input columns.
There were no additional share-only refusals and no ratio mismatches. These
results did not establish supported execution for the 30 excluded layouts.

## Replay and limits

Run from the investigated checkout root:

```sh
Rscript investigation/2026-10-29-summary-spec-share-exhaustive/share/scope-removal.R
Rscript investigation/2026-10-29-summary-spec-share-exhaustive/share/scope-removal.R --assert
Rscript investigation/2026-10-29-summary-spec-share-exhaustive/share/environment-reparent.R
Rscript investigation/2026-10-29-summary-spec-share-exhaustive/share/environment-reparent.R --assert
Rscript investigation/2026-10-29-summary-spec-share-exhaustive/share/key-oracle.R
```

Expected snapshot exits were 0, 1, 0, 1 and 0 respectively. Adjacent logs
retained the complete reports and deliberate assertion failures. The root
investigation note recorded the runtime versions and combined publication
checks.

These probes established finite local dependency cases and local/dtplyr typed
key denominator behavior. They did not establish arbitrary opaque environment
mutations, all R scoping constructs, live SQL execution of the R scope
expressions, other dependency versions, interruption safety or unbounded input
coverage. Arrow shares were outside the numeric oracle because the documented
adapter contract refused them. No absence-of-bugs claim followed from these
bounded loops.
