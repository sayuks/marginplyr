# Ordinary dtplyr root dispatch and backend boundaries

Investigated: 2026-11-06
Tested source: `9ffb24614a71512170298a3827bc11c80354e742` (fetched main).

## Finding: a registered head method bypassed ordinary-root admission

[Issue #1397](https://github.com/sayuks/marginplyr/issues/1397) records one
independent repair, with no blockers. The input used the exact ordinary
`c("data.table", "data.frame")` class vector, not a root subclass. A temporary
S3 registration replaced `head.data.table` with a method that delegated to the
ordinary method but changed character `g` to integer on zero-row output.
Native dtplyr collection still returned `g = c("a", "b")` and invoked no head
method. Name-only inspection also selected `g` without invoking the method.

Typed inspection instead sent the populated two-row root to the registered
method, then called it again on zero rows. The inspected dimension or fixed
key disappeared. Each Nest verb returned one keyless cell containing both
source rows rather than two cells keyed by `g`. Restoring only the method
registration recovered the same input's plan and Nest partitions.

The [minimal reproduction](registered-head-minimal.R) needed one two-row
column, an ordinary immutable dtplyr root, the method registration and a typed
selection. Its observation run exited 0 with `Included: ()` and head input row
counts `2, 0`; its desired-behavior `--assert` run exited 1. The assertion allowed
either the correct plan or a metadata-safety Package condition, required no
populated-row method call, and retained source-serialization and registration-
restoration controls. It did not require accepting an unsafe graph.
[Observation output](registered-head-minimal.log) and
[desired-behavior output](registered-head-minimal-assert.log).

The [22-case probe](registered-head.R) separated retyping from reference
mutation. All 18 retyping cases failed: three graph shapes (direct root,
arithmetic mutation and filter), two typed selection positions (dimension and
fixed), and three consumers (inspection and both Nest verbs). Those cases
preserved serialized source bytes. Two separate inspection cases, one immutable
and one Mutable, used a registered method that wrote through the received
reference; each changed the caller's original `g` to `changed, changed`.
Both Mutable-step Nest controls refused before invoking the method and kept
the source unchanged. [Complete observations](registered-head.log).
Every temporary registration was restored on exit; no global registry change
persisted outside its short-lived probe process.

ADR 0020's amendment *a zero-row result does not permit full-source dtplyr
work* required faithful metadata without processing source rows, or a safe
Package refusal. ADR 0029's inspection amendments required protecting every
caller-owned root. The nesting reference promised one cell per source group.
All three contracts were violated in the respective observations.

### Ranked hypotheses and distinguishing controls

1. Ordinary-root admission verified the class vector but not the registered
   head method used while truncating the root. Prediction: changing only the
   registration would change typed metadata, while native collection and
   name-only inspection would be unaffected. That prediction held.
2. Native dtplyr collection already changed the source schema. Prediction:
   native collection would lose character `g` too. It retained both values and
   made no head call, refuting this explanation.
3. The character predicate selected incorrectly independently of dispatch.
   Prediction: the restored-registration control would still drop `g`.
   It selected `g`, refuting this explanation.

Source inspection after the red-capable probe located root admission in
`dtplyr_metadata_safe_step()`, which returned on the ordinary class vector.
`zero_row_dtplyr_proxy_input()` then called qualified `utils::head()` on the
populated root. Qualification fixed generic lookup but did not fix registered
S3 method lookup. `isolated_dtplyr_proxy()` also truncated the resulting proxy
through that generic. A plain GlobalEnv function named `head.data.table` did
not reproduce the case; registration was load-bearing.

### Existing reports and duplicate disposition

The complete issue bodies/comments for #1303, #1326, #1359 and #1382, and PR
bodies/comments for #1333, #1371 and #1392 were retrieved. Their JSON snapshots
are retained beside this note.

- [#1303](https://github.com/sayuks/marginplyr/issues/1303) and
  [#1333](https://github.com/sayuks/marginplyr/pull/1333) protected the caller-
  bound head generic. This witness kept the qualified ordinary generic and
  changed its registered method.
- [#1326](https://github.com/sayuks/marginplyr/issues/1326),
  [#1359](https://github.com/sayuks/marginplyr/issues/1359) and
  [#1371](https://github.com/sayuks/marginplyr/pull/1371) protected generated
  bracket dispatch on derived inputs, including the bracket registration.
  The new dispatch was head truncation of an admitted ordinary root.
- [#1382](https://github.com/sayuks/marginplyr/issues/1382) and
  [#1392](https://github.com/sayuks/marginplyr/pull/1392) isolated root-subclass
  behavior by refusing extra root classes. The new witness had no extra class;
  the new registration applied to the exact ordinary class still admitted by
  that repair. The implementation diff and its ordinary-root controls were
  read before treating this as a separate residual boundary.

All-state issue and PR searches used `dtplyr "head.data.table"`,
`dtplyr head registration` and `dtplyr registered metadata`, with limit 100.
The six `duplicate-{issue,pr}-{1,2,3}.json` snapshots retain the returned
numbers/titles/URLs. Additional broad `dtplyr head` PR search found the related
repairs above. No existing report describing the ordinary-root registered-head
witness was found. The coordinator independently replayed plan loss, both Nest
partitions, registration recovery and source mutation in another archive.

## Additional completed finite coverage

[The graph differential](graph-differential.R) completed **1,242 comparisons**:
23 ordinary dtplyr graph shapes, zero/one/four input rows, three type predicates,
two selection positions and three consumers. Explicit native collection
supplied the local-input comparison. There were **1,134 matching results** and
**108 metadata-safety Package refusals**; no admitted mismatch, native backend
error or local-only error was recorded. Graphs covered root/filter/projection/
rename/reordering/arrangement/limit/slice/distinct/copy/arithmetic/constants/
deletion, six join styles and four set operations. The single-set Nest
comparisons normalized cell classes and cell row order; they did not assert a
uniform native element class or independently establish all Grouping algebra.
[Per-case results](graph-results.csv) and [summary](graph-differential.log).
An initial unsupported native `cross_join()` graph stopped before the matrix;
it was removed, and none of that incomplete pilot counted toward the totals.

[The remote frontier replay](remote-frontier-controls.R), copied from the
earlier November 6 probe with its dated banner updated, completed **96 calls**:
36 SQLite, 36 DuckDB and 24 Arrow. SQL Nest refusals did not force supplied
Grouping specifications or send statements. Invalid local inspection arguments
sent none. SQLite sent none across these controls; DuckDB sent six `LIMIT 0`
statements for typed inspection, with none for name-only inspection. Arrow
inspection/refusal on six three-reader graphs left all 18 readers available
with their 36 original rows. [Replay output](remote-frontier-controls.log).
These refusal controls did not establish SQL/Arrow nesting support.

## Replay, environment and limits

The source was loaded with `pkgload::load_all()` from a Git archive of the
pinned SHA under `/tmp`; testthat parallel execution was explicitly disabled.
All retained executions reached terminal completion, with no owned children
left running. No package code, permanent test, dependency or installed file
was changed. The main-worktree source remained separate from probe execution.

From the investigated checkout, optionally set `MARGINPLYR_HUNT_SOURCE` to an
archive of its immutable source. Run commands one at a time:

```sh
p=investigation/2026-11-06-nest-grouping-hunt/backend
Rscript "$p/registered-head-minimal.R"
Rscript "$p/registered-head-minimal.R" --assert
Rscript "$p/registered-head.R"
Rscript "$p/registered-head.R" --assert
Rscript "$p/graph-differential.R"
Rscript "$p/remote-frontier-controls.R"
```

Graph CSV output defaults to a temporary path; set `MARGINPLYR_HUNT_OUTPUT` to
retain a new capture. Both registered-head scripts' `--assert` modes demanded
the desired safety contract and intentionally failed on the tested SHA.

Environment: R 4.6.1, aarch64-apple-darwin23, macOS Tahoe 26.6.2;
dplyr 1.2.1, dtplyr 1.3.3, data.table 1.18.6.1, tibble 3.3.1, rlang 1.3.0,
vctrs 0.7.3, tidyselect 1.2.1, DBI 1.3.0, dbplyr 2.6.0,
RSQLite 3.53.3, DuckDB 1.5.5 and Arrow 25.0.1.
The finite probes did not establish every custom method or graph, hosted
backends, other package/OS versions, interruption behavior or resource limits.
No implementation fix or post-fix pass was claimed.
