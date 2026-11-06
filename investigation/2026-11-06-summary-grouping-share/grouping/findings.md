# Grouping specification selection and pattern-proxy hunt

Investigated: 2026-11-06
Tested package SHA: `952a0d9b18fd3ff4631f153432f6024ad62fcac2`

The lane used a disposable `git archive` of the tested main revision at
`/private/tmp/marginplyr-grouping-hunt-source-20261106`. Every R process set
`TESTTHAT_PARALLEL=false` and loaded that source through pkgload. No package
implementation or regression test was changed. The root investigator owned
the publication-time main assessment and issue publication.

## Confirmed: accepted patterns still dispatched a proxy against name placeholders

`pattern-proxy-minimal.R` used a standard `stringr::fixed("a")` pattern, a
character column `a` and an integer column `b`. A caller registered the
ordinary vctrs `vec_proxy.stringr_fixed` method. It required character
metadata for `a` in the active tidyselect context and returned `unclass(x)`
when that requirement held. Every returned proxy preserved the original
pattern's size and storage. It raised a specific condition otherwise; the
witness did not require an inconsistent proxy size, reading source values,
a masked namespace operator, or changing a package namespace binding.

Direct typed tidyselect selected `a`. The plain `matches(pattern)` argument
to `grouping_set()` instead called the method with integer metadata and
refused it with the condition attributed to `vec_proxy()`. Wrapping the same
pattern in `base::identity()` restored typed resolution and selected `a`.
The observation execution asserted that exact failure and successful control;
`--assert` required the desired contract and exited 1. The minimal output was:

```text
direct selected: a
actual proxy metadata: integer
In argument: matches(pattern).
Caused by error in vec_proxy():
pattern proxy requires character column metadata
typed wrapper selected: (a)
```

The separating hypotheses were (1) an invalid pattern or method would also
fail under direct typed tidyselect, (2) the dtplyr adapter would fail only
on a lazy input, and (3) the name-only pattern allowance would reach a
different generic from the repaired length path. Both eager references and
the identity control succeeded. The method saw integer metadata only on the
failing call, and the repaired length-method control succeeded with character
metadata. These observations supported the third hypothesis.

`pattern-proxy-dispatch.R` completed 72 comparisons: three standard pattern
modifiers (fixed, regex and collation), bound/inline construction, local/safe
dtplyr input, and fixed `.by`/all five constructor positions. All 72 plain
calls falsely refused after exactly one integer-context method entry. Every
direct typed reference and typed-wrapper control selected `a`, and every
control method entry saw character metadata. Five additional public consumer
checks reproduced the same earlier selection refusal through inspection,
summarization, expansion, and both Nest verbs. Their typed wrappers succeeded.
The script's final custom-length control selected `a`, establishing the
specific repaired #1344 route separately from this remaining proxy route.

The relevant implementation was the pattern allowance and
`pattern_consumption_name_only()` in the Grouping compiler. That reader
looked for custom `length` methods. Downstream `stringr::str_detect()` also
called `vctrs::vec_size_common()`, which invoked the custom `vec_proxy` method.
The active selection types were therefore observable beyond the method family
the guard inspected. ADR 0008's evaluation budgets and ADR 0013's inspection
correspondence supplied the contract; no new selection syntax or source-row
read was needed.

### Duplicate comparison and proposed slice

Live all-state issue queries included `grouping malformed`, `"pattern"
"proxy"` and `"pattern" "names"`. The parent also exported all 682 issue
and 674 pull-request records using `gh ... list --state all --limit 2000`.
This lane searched their titles and bodies locally for
`vec_proxy|stringr.*dispatch|pattern.*dispatch`, and separately the broader
`pattern|vec_proxy|stringr|proxy dispatch` terms. The narrow issue matches
were #1344 and its duplicate #1350; PR matches were #1352 and the two evidence
PRs #1347/#1351. No record described this standard-pattern proxy-method route.

The complete body and comments of #1344 and body/comments of #995 were read.
#995's vctrs witness concerned a directly injected custom selector, not a
standard pattern consumed by stringr. #1344's broad contract applied to this
same selection boundary, but its concrete witness and resulting repair in
PR #1352 covered the length generic. The retained successful length control
distinguished that repaired behavior. #1255 and PR #1260 concerned ordinary
pattern support and construction. This was a residual method family under
the same public contract, rather than several tickets for modifier, backend,
position or public consumer spellings.

One independently verifiable repair ticket was recommended, with no blockers:
keep accepted standard-pattern proxy dispatch in typed selection contexts,
preserving stock name-only patterns and the existing length-method repair.
The root investigator owned the final duplicate disposition and publication.

## Normal selection correspondence: 1,152 comparisons matched

`selection-correspondence.R` compared 48 ordinary tidyselect selections
against their separately computed direct typed selection. The fixture held
character, integer, double, factor, Date and logical columns, including comma
and numbered-dot names. The direct reference ran against a zero-row typed
frame, matching the metadata contract instead of asserting value-dependent
selection behavior.

Selections varied ranges, reversed ranges, negative and Boolean selection,
positions, empty/NULL selections, stock name helpers, lexical vectors and
`.env`, direct context-name reads, qualified parentheses, same-name aliases
and ordinary type predicates. Each selected vector was literalized with
`all_of()` and compared as a complete inspection tibble across six positions
(fixed keys and five constructors) and four eager/lazy input classes
(tibble, data.frame, data.table and immutable dtplyr).

All 1,128 successful comparisons matched their complete literal plan,
including occurrence order, fixed keys, included/omitted columns, bits and
identifiers. The 24 empty-dimension rollup/cube cases returned the required
Package refusal. No additional ordinary-selection defect was established.
This comparison tested selection correspondence; its expected expansion used
the package's literal plan and was not an independent family-algebra oracle.

## Replay, execution ownership and limits

From an archive of the tested package SHA, invoke the evidence by absolute
path. `MARGINPLYR_HUNT_OUTPUT` controls the two CSV destinations; it defaults
to R's temporary directory.

```sh
Rscript /path/to/grouping/pattern-proxy-minimal.R
Rscript /path/to/grouping/pattern-proxy-minimal.R --assert
MARGINPLYR_HUNT_OUTPUT=/path/to/output Rscript /path/to/grouping/pattern-proxy-dispatch.R
MARGINPLYR_HUNT_OUTPUT=/path/to/output Rscript /path/to/grouping/selection-correspondence.R
```

The final pattern matrix retained terminal session `98864` through exit 0.
The normal-selection matrix retained session `16850` through exit 0. The
minimal observation was execution `dcdfb0`, exit 0, and its desired-contract
assertion was execution `0c3571`, exit 1. All executions were collected before
another reproduction started. Each R process owned no child work, connection
or server; method-table bindings were process-local and restored on exit.

R was 4.6.1 on aarch64-apple-darwin23, with dplyr 1.2.1, tidyselect 1.2.1,
rlang 1.3.0, vctrs 0.7.3, stringr 1.6.0, stringi 1.8.9, dtplyr 1.3.3,
data.table 1.18.6.1 and pkgload 1.5.3. The pattern log retained sessionInfo.
The lane ran no complete suite, package build/check, snapshot suite or render.
Hosted backends, other versions, unbounded family growth and every possible
S3 dispatch path remained outside these finite results. No post-fix result
was claimed.
