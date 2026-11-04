# Grouping specification dots and family-algebra boundaries

Investigated: 2026-11-04
Baseline: `78ebbc36200bdc5cef70838dd4e772c5c4a1401c` (`main`)

This track confirmed one independently actionable compiler defect: numbered
dots references forwarding a nested specification lost their argument reading
when surrounded by redundant parentheses. Without a colliding input column,
valid calls were refused. With an admitted leading-zero, signed or whitespace
dots-alias column, the wrapper silently selected that unrelated column instead.
No package implementation or permanent regression test was changed.

## Contract, hypothesis and source reading

`CONTEXT.md`'s *Nested specification position*, the Grouping specification
reference's *How an argument is read*, and the decision on
[#259](https://github.com/sayuks/marginplyr/issues/259) made redundant parentheses
transparent to a nested argument's reading. ADRs 0007, 0008, 0013 and 0026
established capture, preflight evaluation budgets, the public inspection seam
and ambiguity handling. `readings.csv` recorded full required-document coverage
and pinned Git blob identities. The prior
[November 3 selection frontier](../../2026-11-03-nest-spec-greedy/spec/README.md)
had tested numbered dots returning ordinary selectors inside helper arguments;
it had not tested parenthesized numbered references carrying nested
specifications. Its earlier counts were not counted again here.

The positive hypothesis was that capture forwarded an unparenthesized numbered
reference to its caller argument, whereas the preflight's names-only lookup
treated the same reference inside parentheses as an unbound column name. The
separating observation was identical admitted plans with no parentheses versus
one or two pairs, using the same dots payload and public consumer.

Inspection of `read_grouping_arg()` and `is_name_only_expr()` supported that
mechanism: stripping parentheses exposed the numbered symbol, but
`rlang::env_has()` did not report a normal binding for the dots reference.
The names-only branch returned the selection reading before evaluating the
forwarded value. With a leading-zero alias present in the input, that column
also satisfied the names-only branch. An ordinary named formal did not have
the defect. This was a compiler reading failure before nesting execution.

## Confirmed: parentheses changed the forwarded specification

A dependency-light witness was:

```r
input <- tibble::tibble(a = c("x", "y"), value = 1:2)
plain <- function(...) {
  inspect_grouping(input, .grouping = grouping_sets(..1), .format = "list")
}
wrapped <- function(...) {
  inspect_grouping(input, .grouping = grouping_sets((..1)), .format = "list")
}
plain(grouping_set(a))$included
# list("a")
wrapped(grouping_set(a))
# vctrs_error_subscript_oob: Column `..1` doesn't exist.
```

The same defect was silent when R's first-dots alias `..01` was also an input
column. With two source rows whose `a` values differed and whose `..01` values
were both `"wrong"`, the plain wrapper compiled the grouping set containing
`a`; its parenthesized counterpart compiled the grouping set containing
`..01`. A nesting call therefore changed from two one-row cells keyed by `a`
to one two-row cell keyed by the unrelated alias. R evaluated `..01`, `..001`,
`..+1`, `.. 1`, and `.. +1` as references to the first dots argument. Their
equivalent second-argument forms behaved likewise. Canonical `..1`/`..2` input
columns were not used for the silent-result claim: ordinary dplyr selection
rejected those source names before this boundary.

`numbered-dots.R` retained a 450-call differential across:

- all five parent constructors;
- inspection and all four Margin verbs;
- first and second numbered dots;
- no parentheses, one pair and two pairs;
- canonical and leading-zero aliases without a colliding column, and admitted
  leading-zero aliases with a colliding column.

The independent inspection expectation was a family containing `a`, plus the
empty set for rollup/cube; a grouping set itself had to refuse nesting.
Consumer comparisons used the equivalent literal public call as a control and
compared entire results or the expected condition class and message.

| Outcome | Calls |
| --- | ---: |
| Bare forwarding matched the literal call or required nesting refusal | 150 |
| Parenthesized forwarding falsely refused an admitted nested specification | 160 |
| Parenthesized forwarding replaced the required nesting refusal with a wrong diagnostic | 40 |
| Parenthesized forwarding returned a wrong successful result | 100 |

The successful controls comprised 120 valid plans/results and 30 required
nesting refusals. Among the 100 wrong successes, 80 were wrong admitted
plans/results and 20 incorrectly accepted a nested specification inside
`grouping_set()`, which admitted no nested kind.

The final observation run exited zero after asserting these dated observations.
Its `--assert` mode additionally required desired equality and exited one on
the unrepaired baseline. The observation mode was not a passing regression for
the desired contract. `numbered-dots.csv` identified every case.

`numbered-dots-boundaries.R` added 14 explicit boundary observations and 36
alias/parenthesis controls. Named default formals worked both bare and wrapped,
as did top-level `.grouping` forwarding. Forwarding the default through a
variadic wrapper then wrapping the numbered reference failed. Named dots
preserved written argument order in the bare second-reference control; the
wrapped form still failed. When an explicitly missing numbered dots payload
was the sole constructor argument, bare capture dropped that trailing missing
argument and the union constructor raised its required no-set refusal. The
wrapped form instead reported a missing column. With no first dots argument at
all, bare capture raised the ordinary `the ... list contains fewer than 1
elements` condition; wrapping again changed it to a missing-column diagnostic.
These were observations of the baseline, not a proposed new missing-argument
policy.

### Coverage and duplicate disposition

The existing `test-injected-nested-grouping.R` covered carried quosures,
embracing and parentheses. `test-grouping-selection-budget.R` covered retained
ordinary selections and recognized constructor calls. The earlier selection
frontier tested numbered dots inside ordinary selectors. Those cases did not
establish this numbered-reference-to-nested-specification boundary.

`duplicate-queries.csv` and `duplicate-results.csv` retained all-state issue and
PR searches for `dots in:title`, `parentheses in:title`, `parenthesized
in:title`, and `nested specification in:title`. Each result set was below its
100-item limit. Full bodies/comments were fetched and read for issues #259,
#897, #925, #1144 and #1182 and PRs #916, #938, #1166 and #1192. The unavailable
middle of the initially truncated #1192 rendering was retrieved from the saved
body/comment snapshot rather than assumed read.

- [#259](https://github.com/sayuks/marginplyr/issues/259) established the
  transparent reading for ordinary names. Its earlier cause was failure to
  strip parentheses before the name test; stripping alone did not resolve the
  numbered dots lookup measured here.
- [#897](https://github.com/sayuks/marginplyr/issues/897) and
  [#916](https://github.com/sayuks/marginplyr/pull/916) concerned injected
  quosures and their environments; this witness used a numbered reference that
  remained a symbol after parentheses were removed.
- [#925](https://github.com/sayuks/marginplyr/issues/925) and
  [#938](https://github.com/sayuks/marginplyr/pull/938) concerned an omitted
  argument carried by an injected quosure. The populated valid payload here
  failed before that missing-payload question arose.
- [#1144](https://github.com/sayuks/marginplyr/issues/1144) and
  [#1166](https://github.com/sayuks/marginplyr/pull/1166) concerned an empty
  `...` inside an ordinary helper argument. This failure needed no helper or
  empty dots.
- [#1182](https://github.com/sayuks/marginplyr/issues/1182) and
  [#1192](https://github.com/sayuks/marginplyr/pull/1192) concerned a
  summary-local `pick()` reader. Inspection alone reproduced the defect here,
  without a summary or local selection rewrite.

No matching report remained in these searches. The new repair was one
independent slice, with no blockers. The coordinator published it as
[#1214](https://github.com/sayuks/marginplyr/issues/1214); this track created no
issue.

## Refuted hypotheses and finite algebra coverage

An initial collision fixture used canonical `..1`/`..2` input names. Its
control failed the upstream dplyr name check, so it supplied no evidence about
the compiler and was excluded. The final collision fixture used admitted
leading-zero aliases; no passing result was inferred from that failed setup.

Permitted base operations in helper-argument classification were inspected for
implicit-class dispatch. Small probes of coercion, repetition, missingness,
length, names, subsetting, concatenation and unlisting over unclassed atomic,
matrix, array and list shapes did not invoke the installed implicit-class
methods. This did not establish safety for every possible custom dispatch.
A candidate in which a typed numbered dots payload inside `all_of()` might be
misclassified was also refuted: the classifier conservatively withheld that
selection, and the completed direct controls selected the typed character
column. No new helper-argument bug was claimed.

`family-algebra.R` independently expanded 180 deterministic trees of depth at
most three, with 36 roots of each constructor kind. Leaf selections varied
between names, `all_of()`, positions and `c()`. Empty identity selections,
composite dimensions, overlaps, repeated occurrences, unions, rollups, cubes
and Cartesian products were included. A cap of 64 family occurrences bounded
generated candidates. For each tree, all three duplicate policies were checked
with and without a fixed key.

All 1,080 complete inspection comparisons matched an independently constructed
six-column tibble or the required duplicate-set refusal. The oracle did not
call marginplyr's family expansion, mask, duplicate or formatting helpers. Its
exact comparisons covered included/omitted order, fixed keys, occurrence IDs,
named Grouping bits and Grouping identifiers. This was a separate finite
negative result, not additional examples of the positive numbered-dots defect.

## Replay, environment and limits

Run the R entry points from the investigated checkout root:

```sh
Rscript investigation/2026-11-04-nest-spec-greedy/spec/numbered-dots.R
Rscript investigation/2026-11-04-nest-spec-greedy/spec/numbered-dots.R --assert
Rscript investigation/2026-11-04-nest-spec-greedy/spec/numbered-dots-boundaries.R
Rscript investigation/2026-11-04-nest-spec-greedy/spec/family-algebra.R
```

Each script loaded its checkout with `pkgload::load_all()` and wrote fresh
outputs to a temporary directory by default. `MARGINPLYR_HUNT_SOURCE` and
`MARGINPLYR_HUNT_OUTPUT` selected an explicit source and output directory. The
retained executions used a disposable `git archive` of the baseline, with
`TESTTHAT_PARALLEL=false`. Every yielded execution was observed through its
original handle to a terminal result before another reproduction began.
`executions.csv` recorded the final retained runs; `.log` and `.csv` files
preserved their outputs. All measurements in this track used the baseline SHA
above. R 4.6.1 ran on aarch64 macOS Tahoe 26.6.2, with dplyr 1.2.1, tidyselect
1.2.1, rlang 1.3.0, tibble 3.3.1, vctrs 0.7.3 and pkgload 1.5.3. Full
`sessionInfo()` appeared in the execution logs.

This track ran no complete package suite, build, installation or site render.
It did not establish remote execution behavior, arbitrary custom specification
field methods, unbounded nesting/family size, complete External-condition
identity/backtraces, every possible missing-dot spelling, or behavior under
other R/dependency/OS versions. Ordinary closures and S3/S4 dispatch remained
unbounded input languages. No repair or post-fix result was claimed.

## Recommended ticket slice

**Title:** [P2] Preserve numbered dots forwarding through parenthesized nested
specifications

**What to build:** A reusable variadic wrapper can forward its first or later
numbered dots argument as a nested Grouping specification, with redundant
parentheses preserving the same plan and nested row membership. An admitted
input column whose name is also an R numbered-dots alias cannot silently
replace the forwarded specification. On the investigated main revision, the
450-call differential had 150 matching bare controls and 300 parenthesized
mismatches, including 100 wrong successful results. This extends the
transparent-parentheses decision in #259 to numbered dots; it is separate from
the injected-quosure, empty-helper-dots and summary-local selection repairs in
#897, #925, #1144 and #1182.

**Acceptance criteria:**

- [ ] First and later numbered dots carrying valid nested specifications
      resolve identically with no parentheses and with one or more redundant
      pairs, through inspection and all four Margin verbs.
- [ ] R-supported leading-zero, signed and whitespace numbered aliases retain
      their forwarded reading. Any ambiguity is decided on the caller's
      captured argument rather than by silently selecting an alias column.
- [ ] The resolved Grouping plan, outer keys and cell row membership match the
      equivalent literal call; constructor nesting restrictions remain intact.
- [ ] Named/default formals, written dots order, top-level specification
      forwarding, captured environments and evaluation budgets retain their
      behavior. Empty or absent dots retain the corresponding unparenthesized
      condition rather than becoming a fictitious missing column.
- [ ] Public regressions distinguish this numbered-reference boundary from
      ordinary named arguments, injected quosures and helper-argument dots,
      while preserving lazy-input early-validation guarantees.

**Blocked by:** None (can start immediately).
