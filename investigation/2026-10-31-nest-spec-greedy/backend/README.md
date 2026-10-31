# dtplyr nesting display-helper investigation

Investigated: 2026-10-31
Baseline: `339315c3` (`main`)

## Finding

A function named `min` in the dtplyr root's environment silently replaced
included grouping keys with missing values in both nesting Margin verbs.
The source held `g = c("a", "b")` and `row_id = 1:2`. The function returned
`0L`; the caller supplied no summary expression and requested only the single
set containing `g`. Each result retained two one-row cells containing the
correct respective source rows, but displayed `NA` for both keys.

The retained reproducer built the lazy Margin result before installing the
binding in a separate root-local environment. Collecting that same result
then read the binding twice and returned the wrong keys. The rowwise verb
read it twice more and returned the same wrong keys. Rebinding `min` to
`base::min` restored the keys when collecting the exact same previously
constructed result. The caller's source snapshot stayed unchanged.

The public nesting reference promised one cell per group in each grouping
set. Grouping dimensions included in that set therefore had to identify their
source groups. The Grouping specification, source values, and source equality
relation were unchanged by these controls. No new public option, equality
policy, or source-reading permission was needed for the proposed repair.

### Hypotheses and controls

The ranked explanations were:

1. The package-generated display extraction used a helper resolved in the
   dtplyr root's environment. A binding installed after query construction
   would change the result, and restoring the base binding would recover it.
2. The mask changed source values or typed metadata. Ordinary source
   collection or the source serialization would then change as well.
3. Grouped data.table execution generally used the masked helper. Ordinary
   grouped dtplyr summary and upstream tidyr nesting would then also fail.

The first prediction held; the other controls did not show those failures.
Ordinary source collection, grouped dtplyr counting, `tidyr::nest()` with an
explicit payload selection, the Margin verb with `g` as a fixed `.by` key,
and local nesting of the explicitly collected source all retained `a`, `b`.
They read the masked `min` zero times. All source snapshots stayed identical.

`nest_expanded_margins()` generated a vector slice whose index used
unqualified `seq_len()` and `min()` calls. dtplyr deferred those expressions
into the root's evaluation environment. Returning an empty index made the
included display summary zero-length; data.table filled its output with
missing values. The identity keys still separated the cells. A separately
masked `seq_len` returning `integer()` reproduced the same boundary.
The qualified vector operations around these calls did not establish the
ownership of those two unqualified helper lookups.

### Completed matrix

`display-helper-matrix.R` performed 864 fresh-root comparisons. The dimensions
were zero/one/four source rows, character/integer/factor grouping keys,
single-set/rollup specifications, text/typed-missing Margin labels, both nesting
verbs, both keep policies, all three Margin orders, and each of the two helper
bindings. Every call retained a visible Grouping set identifier. Each unmasked
baseline first had to match local nesting of the explicitly collected source,
including exact outer values and nested cell projections.

| Masked helper | Incorrect populated results | Matching empty controls |
| --- | ---: | ---: |
| `min` | 288 | 144 |
| `seq_len` | 288 | 144 |

All 576 populated calls returned incorrect outer keys; all 288 empty controls
matched. No source snapshot changed, no warning was observed, and no unexpected
error occurred. `display-helper-results.csv` retained every completed case.
These counts were comparisons under this oracle, not package regression tests
or claims about untested environments.

### Candidate excluded

Masking `list` with a function that raised also broke the nesting Margin verbs,
but ordinary grouped dtplyr summary and upstream tidyr nesting raised that same
caller diagnostic. That observation did not distinguish a marginplyr defect
from general upstream generated-query lookup, and was excluded from the ticket.
The narrow display-extraction finding had the successful upstream controls
above. A duplicate search over all issue states for nesting with lexical/mask
terms, and for `min` with nesting or `seq_len`, found no existing report of this
boundary. Closed #996 concerned skipping cached row predicates, rather than
caller ownership of generated display extraction.

## Replay

Run from the investigated checkout root:

```sh
Rscript investigation/2026-10-31-nest-spec-greedy/backend/reproduce-display-helper.R
Rscript investigation/2026-10-31-nest-spec-greedy/backend/reproduce-display-helper.R --assert
Rscript investigation/2026-10-31-nest-spec-greedy/backend/display-helper-matrix.R
```

The observation reproducer and matrix returned terminal exit zero. The
`--assert` mode required the correct public keys and returned terminal exit one
on the baseline; its adjacent log preserved that intended failure. Its desired
behavior assertion did not require the dated wrong-key or helper-read counts,
so a repair could make that mode pass. `environment.log` recorded the R,
platform, and dependency versions. No implementation, permanent package test,
or dependency was changed.

The investigation consulted the October 30 backend track and the October 29
shared-cache/graph track, and read the nesting help, `CONTEXT.md`, and ADRs 0002,
0020, and 0029. Their historical measurements were not counted as this run.
These finite probes did not establish behavior of all dtplyr graphs, arbitrary
caller bindings, other dependency versions, other operating systems, or remote
nesting. The source inspection located the generated helpers; it did not claim
that every unqualified call in an upstream-generated query belonged to this
package.
