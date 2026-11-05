# Grouping specification composition and pattern-dispatch hunt

Investigated: 2026-11-05
Tested package SHA: `0403c9150394cbae21b59f34c75791d0873b8451`
Environment: R 4.6.1, aarch64-apple-darwin23, macOS Tahoe 26.6.2

This lane changed no implementation or package regression test. Standalone
R probes loaded the investigated checkout with `pkgload::load_all()` and set
`TESTTHAT_PARALLEL=false`. The environment included dplyr 1.2.1,
tidyselect 1.2.1, rlang 1.3.0, dtplyr 1.3.3, data.table 1.18.6.1,
stringr 1.6.0, stringi 1.8.9, tibble 3.3.1 and pkgload 1.5.3.
The parent investigator separately checked fetched main at the same SHA.

## Sources and hypotheses

The root `CLAUDE.md` and its `AGENTS.md` reference, `CONTEXT.md`, the local-check
policy, investigation-note guide, and ADRs 0007, 0008, 0013 and 0026 supplied
capture, grammar, evaluation budgets, inspection and ambiguity contracts.
The November 4 and November 5 summary/Grouping/share Grouping lanes and the
November 5 nesting/Grouping and expansion adversarial notes supplied prior
boundaries. Their previously measured cases were not counted again.

The initial hypotheses were that (1) a trusted name-only expression could still
reach caller dispatch that observes typed selection data; (2) cached nested
constructor returns or redundant parentheses could alter selection meaning;
and (3) overlapping composite dimensions and the revised product optimizer
could change family membership, first occurrence, dimension order or identifiers.
The pattern-dispatch probe confirmed the first. The retained composition oracle
refuted the second and third within its literal-selection bounds; it did not
claim arbitrary constructor-return or caller-function coverage.

## Confirmed: a standard stringr pattern still reached caller S3 dispatch

A two-row input held character `a` and integer `b`. A caller bound a standard
`stringr::fixed("a")` pattern and supplied an ordinary S3
`length.stringr_fixed()` method. The method reported length one when `a` in
`tidyselect::peek_data()` was character, and length two when it was integer.
It read types only and inspected no source values.

Direct typed tidyselect evaluated `matches(pattern)` successfully and selected
`a`. `inspect_grouping()` instead ran the method on its names-only integer
placeholders and raised the External condition
`stringr patterns must be length 1.` The error falsely rejected a valid
selection before canonical typed resolution. A method counter recorded only
`integer` on each failing operation; the direct reference and the
`matches(base::identity(pattern))` control recorded only `character`.

The same minimized witness reached inspection and all four Margin verbs:
`summarize_with_margins()`, `expand_with_margins()`, `nest_with_margins()` and
`nest_by_with_margins()`. Each typed identity-wrapper control succeeded.
The matrix then crossed:

- local input and an immutable dtplyr root;
- standard fixed, regex and collation pattern objects;
- prebound and inline pattern construction; and
- fixed `.by` selection and all five Grouping constructors.

All 72 calls falsely refused. All 72 direct typed selections and all 72 typed
identity-wrapper controls selected `a`; their first included/fixed key was
asserted, rather than success alone. The method was restored or removed after
the matrix. Each script ran in its own R process and started no child work,
connection or server.

The classifier accepted exactly recognized stringr pattern classes in a direct
`matches()` argument. That fact established the stored object shape but did
not establish which methods would execute when tidyselect consumed it.
The explicit type-dependent method survived that whitelist. The opaque
`identity()` wrapper deferred classification and supplied the canonical typed
metadata, isolating this boundary from pattern validity, regex options,
constructor grammar, a namespace override and the dtplyr metadata adapter.

Three causal predictions were considered after the minimized failure: if the
pattern value were malformed, direct typed tidyselect would also fail; if a
dtplyr adapter supplied wrong types, eager input would succeed; if name-only
classification admitted live dispatch, only the early call would see integer
placeholders and the typed wrapper would succeed. The controls refuted the
first two and matched the third. The source classifier inspection agreed.

`test-grouping-patterns.R` covered standard patterns, typed expressions that
construct patterns, computed callees, lexical inputs and malformed regex.
The targeted search found no method override for a standard pattern's length;
those existing assertions did not cover the confirmed consumption boundary.
No package tests were edited or run for this finding.

## Independent composition oracle: 2,697 comparisons matched

`composition-oracle.R` generated 900 trees with seed 20261105 and depth at
most three, using all five constructors and up to five variable dimensions.
The names included comma, pipe, equals and numbered-dot spellings. A separate
`fixed key` column was selected in every case. Constructor calls were randomly
wrapped in bare or genuine base-qualified parentheses.

The oracle represented the tree separately from its expression, expanded unions
and products with its own index grids, ordered cube combinations by independent
binary membership and subset size, and calculated bits and identifiers from
membership and powers of two. It used no package compiler or grouping helper
to obtain expected values. Families were bounded to 256 occurrences; one
oversized generated tree was excluded explicitly. All 899 retained trees were
checked under all three duplicate policies.

| Outcome | Count |
| --- | ---: |
| Exact family, fixed key, occurrence order, dimension order, omitted columns, bits and identifier match | 2,498 |
| Required duplicate-set Package refusal with the duplicate diagnostic | 199 |
| Total retained policy calls | 2,697 |

This was a new seed, deeper tree shape and fixed/non-syntactic-name frontier,
not a relabeling of the November 4 counts. It established no new composition
finding. It did not run summaries, database execution, resource-limit tests,
arbitrary external selections or every metadata environment.

## Duplicate search and proposed disposition

All-state GitHub issue searches used `specification args`, `grouping malformed`,
`grouping active binding`, `pattern dispatch`, `stringr length`, `stringr`,
`"stringr" "dispatch"` and `"pattern" "length"`; all-state PR searches used
`pattern dispatch`, `stringr` and `"stringr" "length"`. Each query requested
up to 100 results. The complete bodies and comments of [#1255] and [#995]
were retrieved. Both had no comments. The relevant [PR #1260] description and
review record identified the standard-pattern and computed-callee repairs.

[#1255] required standard stringr patterns to remain name-only when their
ordinary consumption was independent of types. Its opaque-expression typed
controls did not address an S3 method on the accepted standard object.
[#995] concerned implicit-class base generics and directly injected custom
selectors; it removed those generic calls from the whitelist. Neither described
this later explicit standard-pattern allowance. No matching open or closed
report was found for the new method-dispatch witness.

One independent ticket was recommended: resolve type-dependent consumption of
standard stringr patterns against typed Grouping metadata while preserving
ordinary name-only pattern behavior. No blocker was identified. The temporary
issue draft supplied to the parent investigator records the proposed vertical slice; root owns publication.

[#1255]: https://github.com/sayuks/marginplyr/issues/1255
[#995]: https://github.com/sayuks/marginplyr/issues/995
[PR #1260]: https://github.com/sayuks/marginplyr/pull/1260

## Replay and verification boundary

From the investigated worktree root:

```sh
Rscript investigation/2026-11-05-summary-grouping-share-greedy/spec/pattern-minimal.R
Rscript investigation/2026-11-05-summary-grouping-share-greedy/spec/pattern-dispatch.R
Rscript investigation/2026-11-05-summary-grouping-share-greedy/spec/composition-oracle.R
```

Observation mode asserts the dated failure and working controls. Add `--assert`
to either pattern script to assert the desired contract; the matrix's retained
red execution exited 1 with `!inherits(actual, "error") is not TRUE`.
The matrix observation log and both CSVs contain the completed finite results.
The minimized five-consumer execution exited zero. The composition execution
retained session `19312` through terminal exit zero; the matrix's red execution
was `d71618` with terminal exit one, and the minimized observation execution
was `e206e1` with terminal exit zero. No live owned process remained.

An artifact-targeted `jarl check` passed. These probes were development evidence,
not Review-ready, package build/check, complete-suite or snapshot evidence.
No result promised arbitrary S3 methods, other package versions, every pattern
class, hosted backends, warning identity or resource exhaustion behavior.
