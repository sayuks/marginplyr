# Public-qualified formula-shaped Grouping selections

Investigated: 2026-11-07
Tested package SHA: `0b5143133c023ccb4efb6fe27f1ef551e9f49186`

This lane used a disposable Git archive of the named main revision. No package
implementation, test or generated documentation was edited. All executions
disabled testthat parallel execution and ran sequentially; no owned process
remained running. The retained commands were development reproductions, not
Review-ready, package-suite or source-tarball results.

## Confirmed boundary

A call written with a public-qualified formula-shaped head, such as
`base::` followed by a backtick-quoted `~`, was classified as a name-only
selection even when the captured environment's namespace operation returned
an ordinary type-dependent selector. A genuine bare formula is refused by
tidyselect before evaluating its contents; a qualified call is ordinary code
and need not return a formula. The classifier's unconditional
`rlang::is_call(expr, "~")` branch also matched public namespace qualification.
It did not match private namespace qualification.

The witness input held two different character `a` keys, one constant integer
`b` key, and integer payloads. The captured namespace operation returned a
function choosing `a` when the selection metadata for `a` was character, and
`b` otherwise. Direct typed tidyselect selected `a`. A `.by` selection retained
`b` after observing integer name placeholders instead. Consequently a public
Margin summary returned one `b` row with count two rather than two `a` rows
with count one. Inspection reported the same incorrect fixed key. A
Grouping-set union containing this selection and an ordinary `b` selection
instead falsely reported duplicate grouping sets before the canonical typed
pass could run.

The [replay](formula-shape.R) had three modes. From a disposable source root:

```sh
Rscript /path/to/formula-shape.R observation
Rscript /path/to/formula-shape.R contract
Rscript /path/to/formula-shape.R controls
```

Observation returned zero and asserted the dated failure. Contract returned
one because it required agreement with the intended typed result. Controls
returned zero. Across four input classes (tibble, data.frame, data.table and
immutable dtplyr), fixed-key/union entrances, inspection/summarization and both
namespace operators, the 32-row observation matrix contained 16 public-qualified
failures and 16 correct private-qualified controls. Eight failures silently
changed fixed partitions; eight falsely rejected the union. Each failing
execution recorded integer metadata. Inputs were serialized before and after
every call and remained unchanged.

Only adding redundant parentheses around the qualified callee forced faithful
typed evaluation. All 32 parenthesized controls agreed with the intended result
and recorded character metadata. Three separate bare/genuine-qualified formula
controls continued to refuse invalid formula selections. Two direct typed
references, one per namespace operator, selected `a` in each mode. Counts from
different modes repeat the same finite cases and are not independent samples.

### Separating hypotheses and supported cause

1. If the caller selector was invalid, direct typed tidyselect and the
   parenthesized spelling would fail as well. Both succeeded.
2. If the namespace callback or capture environment was lost, private
   qualification and parenthesized public qualification would differ from
   direct typed evaluation. Both agreed with it.
3. If the formula shortcut treated a qualified ordinary call as a bare formula,
   public qualification would see integer placeholders while the equivalent
   private-qualified and parenthesized forms would see character metadata.
   The matrix established exactly this difference.

The third hypothesis survived. This is an existing selection-classification
boundary; it does not justify speculative caller evaluation, a general R
interpreter or a new lazy-source read.

### Contract, coverage gap and repair slice

ADR 0019's *isolate generated namespace operations* amendment preserves
caller-authored namespace expressions as ordinary code in their captured
environment. ADR 0008's evaluation and leading-selection amendments require
unproven caller work to use the faithful typed context and retain its canonical
evaluation budget. Its authored-namespace amendment specifically rejects
running unproven caller code against a names proxy. ADR 0013 requires inspection
and Margin operations to share Grouping semantics. The fixed key and duplicate
results violated those contracts.

The existing qualified-selection tests covered genuine leaf helper and `names`
shapes with caller namespace bindings. Formula-classifier tests covered bare
formula syntax, redundant whole-call parentheses and formula operands. They
did not cover an authored namespace operation on a public-qualified formula
shape. The repaired helper/parenthesis cases remained controls rather than
evidence that this early formula branch was covered.

Recommended independently verifiable slice, no blockers: **Preserve typed
Grouping selection for qualified formula-shaped calls.** Require fixed keys,
inspection and Margin summaries to agree with direct typed tidyselect for this
ordinary-call spelling; prevent the false duplicate refusal; preserve ordinary
formula refusals, legitimate leading name-only errors, captured environments,
callback evaluation budgets, input immutability and ADR 0020's query boundary.

## Duplicate comparison

All-state live issue searches `"formula" "qualified"`, `"formula"
"selector"` and `"namespace" "Grouping"`, and the all-state PR search
`"qualified" "formula"`, were completed. Full issue bodies and comments were
retrieved as structured JSON for #924, #1039, #1304 and #1396. The parent
coordinator owned the repository-wide archive and final live duplicate pass.

- Closed #924 covered a foreign-package export using a recognized leaf-helper
  name, rather than formula syntax.
- Closed #1039 covered the namespace-dispatch proof for familiar leaf helpers.
  This residual early formula branch executes before that repaired proof.
- Closed #1304 covered qualified parentheses nested inside helper operands,
  rather than a top-level formula-shaped selection.
- Closed #1396 covered retention of an authored namespace operation around
  a nested Grouping specification. This witness returned an ordinary selector
  and reached neither nested-specification interpretation nor its cache.
- The parent's #1421 comparison concerned ordinary `across()` output-name
  prediction, a separate consumer from Grouping selection classification.

## Other completed finite work

The seed-fixed [plan expansion oracle](plan-fuzz.R) compared 1,500 recursively
generated Grouping specifications against naive independent set-family
expansion with `.duplicates = "drop"`. It crossed all five constructors,
overlapping/composite dimensions, repeated sets and nested products/unions.
All final comparisons matched exact included-column lists and occurrence order.
Two initial oracle setup errors were corrected before this result: cube order
had incorrectly used descending bit masks rather than descending subset sizes
and combination order; an empty oracle dimension vector used NULL instead of
`character()`. Those runs were not package findings or part of the final 1,500.

Adjacent classifier inspection found the refused-operator branch already
restricted to a bare symbolic head, and the `.data` branch already restricted
to an unqualified accessor. No additional defect was established there.

## Environment and limitations

R was 4.6.1 on aarch64-apple-darwin23/macOS Tahoe 26.6.2, with dplyr 1.2.1,
tidyselect 1.2.1, rlang 1.3.0, vctrs 0.7.3, tibble 3.3.1 and pkgload 1.5.3.
The matrix loaded data.table and dtplyr from the provisioned local library.
The retained environment transcript records the base loaded session separately.
No guarantee was established for arbitrary R effects, all custom classes,
unbounded plans, SQL engines, other platforms/versions or concurrency. No
implementation repair or regression test was added.
