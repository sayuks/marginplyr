# dtplyr nesting and Margin-label helper investigation

Investigated: 2026-11-02
Baseline: `978d7bef4247bf1348af5b90bfab47924db25500` (`main`)

## Confirmed finding

The package-generated dtplyr Margin-label expressions resolved ordinary helper
names from the caller's root environment. That let unrelated caller bindings
corrupt displayed grouping keys and labels, raise a backend error, or bypass an
explicit observed-label collision check. One independently repairable ticket
was recommended for this shared Margin-label path; production code and package
tests were not changed.

The minimal `as.character` reproducer used one character key `g = "a"` and one
source row. It built a lazy nesting query before installing the root-local
binding. Collecting that same query with a function returning `"MASK"` produced
outer `g = "MASK"`, although the cell retained the original `g = "a"` and row
identifier. Restoring only the binding to `base::as.character` made the same
query return `g = "a"`. Ordinary source collection, grouped dtplyr counting, and
grouped tidyr nesting retained the correct values and read the callback zero
times. The caller's table serialization stayed identical.

Two additional minimal variants used a rollup over the same one-row source.
A root-local `rep` returning `"MASK"` replaced the omitted label `"Total"` with
`"MASK"`. A `length` returning zero caused the omitted branch to hold a list
where the included branch held character and made the union raise a native
`dt_join_type_mismatch_error`. Both variants recovered by restoring only the
corresponding base binding. `length` also reached the separately generated
omitted-identity sizing expression in nesting, so ownership of that expression
was part of the measured repair scope.

The public *Display labels and grouping identity* contract required an included
dimension's display conversion and an omitted dimension's requested Margin label.
The *Relationship to tidyr and dplyr* contract required the nested source rows.
The caller supplied no summary expression to either nesting verb; those
package-written transformations were independent of the caller's ordinary
lexical functions.

### Hypotheses and controls

After the initial wrong output, the explanations were ranked as follows:

1. Deferred package-written helper calls looked up root-local bindings. A late
   binding would change a previously built query; restoring it alone would
   recover the output.
2. The caller's function changed the source or its schema. Ordinary collection
   or a serialized source snapshot would then change too.
3. Grouped backend execution itself depended on the same helper. Ordinary
   grouped counting and grouped tidyr nesting would then fail as well.

The first prediction held, while the controls refuted the other two for the
minimal character-key cases. The emitted query retained unqualified
`as.character`, `rep`, and `length` calls. The prior #1089 repair had qualified
`min` and `seq_len` in representative extraction, a different generated
expression. Masking `copy` and `as.integer` did not change this character-key
probe; that did not establish their behavior for arbitrary inputs. Masking
`list` also broke the ordinary upstream controls and was excluded.

### Explicit collision checking

`collision-helper-controls.R` used an actual source value `g = "Total"` and
`.check_margin_label = TRUE`. Without a root mask, every verb raised the
expected Package condition saying the value was already present. With a
root-local `as.character` returning `"MASK"`, or a `sum` returning zero, all
four verbs accepted the collision: both nesting verbs, expansion, and summary.
All eight helper/verb cases therefore failed their desired-refusal assertion.
The native source and grouped-count controls invoked neither callback.

Explicit user-written `as.character(g)` and `sum(row_id)` expressions continued
to resolve the caller functions and produced their chosen values. That was a
positive control for the distinction between ordinary user expressions and the
expressions marginplyr itself wrote. This finding did not request changing the
lookup rules for the user's own expressions. The generated observed-collision
predicate used `sum(if_else(as.character(...)))`; the namespace-local `any`
over its collected counts was not a deferred root lookup.

## Completed finite measurements

All executions used `pkgload::load_all()` against the pinned working tree with
testthat parallel execution explicitly disabled. `environment.log` recorded
R 4.6.1 on Darwin 25.6.0 arm64, dplyr 1.2.1, dtplyr 1.3.3,
data.table 1.18.6.1, vctrs 0.7.3, tidyr 1.3.2 and tibble 3.3.1.
No dependency was installed or changed.

`label-helper-matrix.R` completed 432 comparisons: three helpers, character /
integer / factor keys, zero / one / three source rows, both `.keep` values,
text / typed-missing labels, included-only sets / rollups, and both nesting
verbs, with `.id = "set"` and `.sort = "last"`. Every unmasked lazy baseline
first matched local nesting. The per-comparison CSV retained callback reads,
warning counts, output differences and error messages. The complete source
serialization remained identical in every comparison.

| Root binding | Wrong result | Raised error | Matching result |
| --- | ---: | ---: | ---: |
| `as.character` | 32 | 0 | 112 |
| `rep` | 24 | 0 | 120 |
| `length` | 4 | 36 | 104 |

Matching output did not establish callback isolation: empty cases and some
factor / typed-missing paths either did not need the operation or retained the
same result. The recorded callbacks included 16 `as.character` reads, 12 `rep`
reads and 44 `length` reads among matching cases. Eight warnings occurred among
matching `length` cases, eight among wrong-result cases and eight among its
errors; no other matrix helper produced a warning. These were finite observed
comparisons, not package tests or a guarantee for untested inputs.

The separate type differential and native-limit controls are described below.

### Type differential and native limits

`type-differential.R` completed 864 cases: 24 payload/key types, zero / one /
four source rows, payload / fixed-key / variable-dimension roles, both `.keep`
values and both nesting verbs, using a rollup, typed-missing labels, visible
set identifiers and Margin order. Explicit ordinary source collection supplied
the local-input oracle. Outer rows were normalized by set identifier and cell
row identifiers, and cell classes were normalized to tibbles for an
`all.equal()` comparison including attributes. This matrix did not independently
assert Margin order or a uniform native cell class. The corrected completed
run had 612 matching projected results,
240 backend errors and 12 Package-condition refusals for unrepresentable raw
missing dimensions. It had no admitted output mismatch or warning among the
matching cases. The role totals were:

| Role | Matching projection | Native backend error | Package refusal |
| --- | ---: | ---: | ---: |
| Payload | 252 | 36 | 0 |
| Fixed key | 180 | 108 | 0 |
| Variable dimension | 180 | 96 | 12 |

An earlier harness wrote the fixed selection as `if (...) p else NULL`, whose
external evaluation could not resolve `p`; those 288 both-error observations
were not evidence for fixed-key safety. The retained CSV/log came from the
corrected `all_of()` selection and replaced those invalid measurements.

The native-limit control reproduced raw and complex payload failure in
ordinary dtplyr `union_all()`; each worked in one-set nesting and ordinary
grouped tidyr nesting. Native data.table also refused list grouping and
rejected the four-row custom record column as a malformed table because its
list-field length was not its row count. These were recorded as backend
composition / input-representation limits, not additional package findings.
No stronger arbitrary-payload support policy was inferred from them.

## Duplicate searches

All issue searches used `gh issue list --state all --limit 100`, including:
`dtplyr lexical`, `dtplyr "as.character"`, `dtplyr "rep"`,
`dtplyr "length"`, `dtplyr "sum" collision`, `dtplyr helper`,
`dtplyr raw`, `dtplyr complex`, and `nest union`.
Pull-request searches used `gh pr list --state all --limit 100` with
`dtplyr label` and `nest helper`. The retained `duplicate-*.json` files recorded
the focused conversion, replication, lexical and label-PR searches.

Full bodies and comments were read for
[#1089](https://github.com/sayuks/marginplyr/issues/1089),
[PR #1119](https://github.com/sayuks/marginplyr/pull/1119),
[#427](https://github.com/sayuks/marginplyr/issues/427),
[PR #433](https://github.com/sayuks/marginplyr/pull/433), and
[PR #1058](https://github.com/sayuks/marginplyr/pull/1058).
The first pair concerned only `min` / `seq_len` representative extraction;
the second concerned full-size replacement and coercion; the last concerned
`.SD` / `.ROW` generated column targets. None reported the measured Margin-label
helper ownership or explicit-collision-check bypass. No duplicate of this
boundary was found in those searches.

## Replay and terminal outcomes

From the investigated worktree root:

```sh
Rscript investigation/2026-11-02-nest-grouping-hunt/backend/reproduce-label-helper.R
Rscript investigation/2026-11-02-nest-grouping-hunt/backend/reproduce-label-helper.R --assert
Rscript investigation/2026-11-02-nest-grouping-hunt/backend/reproduce-label-helper.R rep
Rscript investigation/2026-11-02-nest-grouping-hunt/backend/reproduce-label-helper.R rep --assert
Rscript investigation/2026-11-02-nest-grouping-hunt/backend/reproduce-label-helper.R length
Rscript investigation/2026-11-02-nest-grouping-hunt/backend/reproduce-label-helper.R length --assert
Rscript investigation/2026-11-02-nest-grouping-hunt/backend/collision-helper-controls.R
Rscript investigation/2026-11-02-nest-grouping-hunt/backend/collision-helper-controls.R --assert
Rscript investigation/2026-11-02-nest-grouping-hunt/backend/label-helper-matrix.R
Rscript investigation/2026-11-02-nest-grouping-hunt/backend/type-differential.R
Rscript investigation/2026-11-02-nest-grouping-hunt/backend/type-limit-controls.R
```

Every observation / matrix / native-control invocation above returned terminal
exit zero. Each `--assert` command required correct public behavior and returned
terminal exit one, after the controls; adjacent logs retained those intended
failures. The desired assertions did not pin the wrong output, error, or
callback count, and could pass after a repair. The helper's `length` observation
caught and printed the native error so it could still run the rebinding control.
An exploratory helper-printing rerun lost its execution identity in an outer
wrapper and was excluded from terminal-result evidence; its artifacts were
removed. The retained minimal repros and matrix runs had collected terminal
outcomes.

The prior backend investigations consulted in full were the October 30 and
October 31 `nest-spec-greedy/backend/README.md` notes. Their historical runs did
not count as fresh measurements. The nesting help, `CONTEXT.md`, and ADRs 0001,
0002, 0003, 0005, 0016, 0020 and 0029 supplied the contracts.

## Limits

This finite track did not establish arbitrary dtplyr graph behavior, every
caller binding, other dependency versions or operating systems, nondeterministic
source expressions, performance at scale, remote nested-list support, or
unmeasured extension-class behavior. SQL/Arrow admission and reader consumption
were investigated separately by the coordinator. Native backend errors,
Package refusals and matching output with callback reads were not correctness
passes for helper isolation. No package build, full suite, permanent regression
test, or implementation repair was performed by this track.
