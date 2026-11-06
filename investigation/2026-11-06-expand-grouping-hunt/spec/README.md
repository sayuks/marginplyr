# Grouping specification selector and pattern-proxy hunt

Investigated: 2026-11-06
Tested package SHA: `952a0d9b18fd3ff4631f153432f6024ad62fcac2`
Environment: R 4.6.1, aarch64-apple-darwin23, macOS Tahoe 26.6.2

The probes loaded the investigated checkout with `pkgload::load_all()` and
set `TESTTHAT_PARALLEL=false`. The recorded library included dplyr 1.2.1,
tidyselect 1.2.1, rlang 1.3.0, stringr 1.6.0, vctrs 0.7.3, dtplyr 1.3.3,
data.table 1.18.6.1, tibble 3.3.1 and pkgload 1.5.3. No implementation or
package regression test was edited. No suite, package build or render ran.

## Confirmed: pattern consumption reached unchecked vctrs proxy dispatch

The one-row, one-character-column witness in
[the minimized replay](pattern-proxy-minimal.R) used an ordinary
`vec_proxy.stringr_fixed()` S3 method on a standard `stringr::fixed()`
pattern. The method inspected only the selected column's type through
`tidyselect::peek_data()`. On character metadata it returned the pattern's
unclassed storage unchanged. On other metadata it raised a sentinel.

Direct typed tidyselect selected `a` and the method observed `character`.
`inspect_grouping()` with the same direct `matches(pattern)` selection
instead called the method on the names-only proxy, observed `integer`, and
falsely refused with the caller's sentinel. Wrapping the pattern in
`identity()` made inspection select `a` against character metadata. Thus the
typed path used a valid proxy of unchanged storage and size; this result did
not require making a valid typed proxy malformed.

[The matrix](pattern-proxy-matrix.R) provided a separate library-generated
symptom. Its two-column input had character `a` and integer `b`. The method
returned length one on character metadata and length three on the integer
placeholder, producing stringr's natural recycling error in the early pass:

```text
Can't recycle `string` (size 2) to match `pattern` (size 3).
```

The final matrix crossed fixed/regex/collation patterns, exact-class/inherited
`vec_proxy` methods, local/immutable dtplyr-root inputs, all five public
consumers, and fixed `.by` plus every Grouping constructor: 360 direct calls
falsely refused, and all 360 identity-wrapper controls succeeded. The recorded
actual method observations contained only `integer`; the wrapper controls
contained only `character`. Every inspection control asserted the exact
first included or fixed column `a`; the other consumer controls checked that
the returned public schema retained `a`. Six direct typed references also
selected `a`. After removing the custom methods, three ordinary patterns
selected `a` on a derived dtplyr input without demanding typed metadata.

The initial ranked explanations were malformed pattern storage, bad dtplyr
metadata, and incomplete consumption-dispatch classification. The unchanged
typed proxy and successful direct reference refuted the first; eager-input
failures refuted the second; the integer-only actual observations and typed
wrapper controls supported the third. Source inspection agreed:
`pattern_consumption_name_only()` checked `length.*` bindings, while installed
stringr's `str_detect()` called `check_lengths()` and then
`vctrs::vec_size_common()`, which reached `vec_proxy()` before matching.
The caller method was an ordinary S3 binding in the probe's global environment,
not a replacement of a package namespace function.

This was a follow-up to closed [#1344] and its repair in [PR #1352]. Their
recorded witnesses and regression coverage concerned pattern `length()`
dispatch; that repair did not account for this public vctrs generic. The
Grouping typed/name-only boundary in ADR 0008 and inspection correspondence
in ADR 0013 supplied the contract. The November 5 expansion, summary and
nesting Grouping lanes supplied prior boundaries; their counts were not
credited to this run.

### Duplicate disposition

All-state GitHub issue searches requested up to 100 results for
`"pattern" "dispatch"`, `"vec_proxy"`, `"stringr"`,
`"vec_proxy" "pattern"` and `"pattern" "proxy"`. All-state PR searches
used the first and final two queries. Each response had fewer than the
requested maximum. Full bodies/comments of #1344 and #1350 and
the body plus review/verification discussion on PR #1352 were retrieved.
The first proxy-specific searches returned no matching issue. A publication
refresh then found open [#1363], whose full body and empty comments described
the same baseline and vctrs boundary with an independent registered-method
72-case matrix. This lane recommended reusing that ticket and adding the
one-column and exact/inherited five-consumer evidence, rather than publishing
a second issue. No prerequisite ticket was needed for this repair.

## Negative differential oracle: 1,614 contract checks matched

[The selector oracle](selection-oracle.R), seed `202611061`, generated 300
selections from 48 base/qualified forms and 252 set-operator combinations.
Cases included helper aliases, lexical `.env` values, name introspection,
caller closures, qualified parentheses, ranges, complements, differences,
unions/intersections, and columns containing comma, pipe and numeric names.
Direct typed tidyselect determined each selected ordered column collection;
an independent enumerator determined rollup prefixes and cube subsets.

All 1,576 successful Grouping-family/fixed-key comparisons matched exact
included columns and sequential occurrence IDs. All 38 empty rollup/cube
cases raised the required Package condition with the empty-unit diagnostic.
The 31 selectors refused by direct tidyselect were excluded from the family
comparison. This finite run established no second lexical or algebra defect.

The oracle used names admitted by ordinary dplyr selection. It did not compare
raw injection syntax across different capture boundaries; constructor capture
interprets injection while a prebuilt tidyselect expression has already
crossed that boundary. It did not establish warning identity, evaluation
budgets for arbitrary caller effects, unbounded products, or arbitrary S3
dispatch safety. Those are separate contracts from its finite family oracle.

## Replay and execution evidence

Run from the investigated package root:

```sh
Rscript investigation/2026-11-06-expand-grouping-hunt/spec/pattern-proxy-minimal.R
MARGINPLYR_HUNT_OUTPUT=investigation/2026-11-06-expand-grouping-hunt/spec Rscript investigation/2026-11-06-expand-grouping-hunt/spec/pattern-proxy-matrix.R
Rscript investigation/2026-11-06-expand-grouping-hunt/spec/pattern-proxy-minimal.R --assert
Rscript investigation/2026-11-06-expand-grouping-hunt/spec/selection-oracle.R
```

Observation mode asserted the dated failure and passing controls. The minimal
desired-contract `--assert` execution ended at exit 1 with
`!inherits(actual, "error") is not TRUE`; its retained log records the exact
false refusal. The final matrix retained session `5475` through terminal exit
0. The final selector oracle retained session `70504` through terminal exit
0. Each process owned its temporary S3 bindings and started no child work,
connection or server. No live owned execution remained when this lane finished.
These were development reproductions, not Review-ready or package-check
evidence.

[#1344]: https://github.com/sayuks/marginplyr/issues/1344
[PR #1352]: https://github.com/sayuks/marginplyr/pull/1352
[#1363]: https://github.com/sayuks/marginplyr/issues/1363
