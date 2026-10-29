# Grouping specification selection and capture evidence

Investigated: 2026-10-29
Baseline: `ee43217` (`main`)

This track established no new independently actionable Grouping specification
finding on the pinned baseline. It audited the five constructor kinds,
preflight and nested-name reading, selection classification, expanded plan
normalization, inspection formatting, and the Grouping identifier boundary.
It read the earlier expansion and follow-up hunts for deduplication, including
#873, #946, #975, and the closed typed-dispatch finding #995. The latter's
implicit S3/vctrs dispatch boundary was not counted as a new issue.

The retained scripts added executable coverage rather than implementing a fix:

```sh
Rscript investigation/2026-10-29-expand-grouping-deep-hunt/spec/selection-context.R
Rscript investigation/2026-10-29-expand-grouping-deep-hunt/spec/capture-extended.R
```

Both scripts accepted an optional repository path as their first argument and
otherwise loaded the working tree from `.`. They required the ordinary package
development dependencies and dtplyr; version evidence was retained beside them.

`selection-context.R` used seed `10292026` and selected from an input carrying
integer, character, Date, list, and factor columns. It compared direct typed
tidyselect with `.by` and each of the five public constructors through
`inspect_grouping(.format = "list")`, on local and immutable-root dtplyr
inputs. Its 368 generated expressions included depth-three trees, qualified
and parenthesized operator heads, selection helpers inspecting typed data,
operator and subscript refusals, predicates, and injected quosures carrying
inner environments through three pairs of parentheses. Double-bang AST
spellings were excluded before interpreting results because direct tidyselect
and constructor capture did not necessarily share their injection phase.
One double-bang AST was excluded, leaving 367 expressions and 4,404
consumer comparisons. There were 2,276 matching successful selections, 2,052
matching External-condition classes, and 76 expected empty-dimension
refusals, with no mismatch. No renaming appeared in this admitted corpus, so
its rename-oracle branch added no renaming coverage. The oracle compared
exact selected columns and occurrence order, or the External-condition class
where direct tidyselect refused. An initial oracle wrapped already-captured
quosures in another quosure and thereby asked direct tidyselect a different
question; the retained oracle passed those captured quosures directly.

`capture-extended.R` made 585 comparisons across all five constructor
spellings, three parenthesis depths, and inspection/expansion/summary
consumers. A lexical function shaped like a constructor returned one of
thirteen ordinary selection values. These included expression and pairlist
objects, matrices and arrays, complex and raw values, Date and factor objects,
a custom class, nested selection quosures with different lexical environments,
and a predicate. The ordinary non-constructor function supplied the control.
There were 135 matching successful results and 450 matching condition classes.
Every caller function ran exactly once per public call. Source specifications
and package implementation files were not edited.

The hand audit found that the name-only helper-argument whitelist excluded
ordinary S3 generics after #995. Twelve supplemental package comparisons in `implicit-cast.R`
returned tidyselect's ordinary matrix/array refusals and did not invoke the
custom vctrs cast methods; unclassed scalar selectors agreed with direct
selection. These were excluded leads, not new contract failures.

The measured cases did not enumerate arbitrary R expressions, arbitrary
closure state, every custom S3 class, unbounded recursive/exponential plans,
or derived dtplyr schema transformations. In particular, this track's dtplyr
oracle used immutable roots; the separate expansion track's derived-step
metadata finding was outside that oracle. Passing measurements established
only the inputs actually exercised.
