# Backend isolation and metadata investigation

Investigated: 2026-11-06
Tested source: `547d77095c2923573216d002f50bef21778247f9` (fetched main).

## Finding: root subclass dispatch ran before zero-row isolation

A data.table carrying the ordinary class vector
`c("marginplyr_hunt_root", "data.table", "data.frame")` entered
`dtplyr::lazy_dt(immutable = TRUE)` through its normal public interface.
A `head.marginplyr_hunt_root` method delegated through `NextMethod()` and
changed character `g` to integer only in its zero-row result. Native dtplyr
collection retained `g = c("a", "b")` and invoked no head method. Name-only
inspection also retained `g` and invoked no method.

Typed Grouping-dimension inspection called the method with the caller's two
real source rows before isolating the root. Its Grouping plan lost `g`.
Both Nest verbs consequently returned one keyless cell rather than two cells
identified by `g`; typed fixed-key selection failed at the same boundary.
An otherwise identical ordinary data.table retained the key.

The [36-case matrix](root-subclass-head.csv) crossed a root, an arithmetic
mutation and a filter; typed dimension/fixed-key selection; inspection and
both Nest verbs; and presence/absence of the custom root class. Eighteen
subclass cases returned the false partitioning; eighteen ordinary-class
controls matched. Every native collection retained the correct character
values, and every retyping case preserved serialized source bytes.
[Observed output](root-subclass-head.txt).

A separate mode of the same method used `data.table::set()` on the object it
received, rather than changing its empty result. Name-only inspection left
the source unchanged; typed inspection replaced the caller's original `g`
values with `c("changed", "changed")`. This was a write through the actual
source-table reference during inspection of an immutable input. A Mutable-step
Nest refusal ran no method.

The [minimal replay](root-subclass-minimal.R) needed only the classed
one-column two-row source, its subclass method and a typed selection.
It also substituted a `[.marginplyr_hunt_root` method, with no custom head
method. That method likewise received the populated root during truncation
and changed zero-row metadata; inspection again lost `g`.
[Head output](root-subclass-minimal.txt),
[bracket output](root-subclass-bracket.txt).
Neither witness changed the global S3 registry. The methods occupied fresh
GlobalEnv names and were removed on exit.

ADR 0020's amendment *a zero-row result does not permit full-source dtplyr
work* required faithful metadata or a safe refusal before source-row
processing. ADR 0029's inspection amendment required isolation of every
caller-owned root. Those guarantees were violated before the root was made
empty. The Nest reference's one-cell-per-group contract was violated in the
resulting output as well. The input was an admitted dtplyr step; its native
collection and ordinary-class controls established that its source types
were not inherently unavailable. Inspecting dtplyr's installed public
`lazy_dt()` and `step_first()` showed that they retained an existing
data.table subclass: they required `is.data.table()` and performed no
subclass normalization.

## Ranked hypotheses and discriminating controls

1. Root truncation dispatched a subclass method before isolation. Prediction:
   callbacks would see populated source rows, and removing only the extra
   class would restore the plan. Both predictions held, including the separate
   reference-mutation witness.
2. Native dtplyr collection already changed the source schema. Prediction:
   native collection would lose character `g`. Every native collection retained
   it, refuting this explanation.
3. Typed selection was independently wrong for the source. Prediction:
   the plain-class control would also lose `g`. It selected `g`, refuting that
   explanation. Name-only inspection and Mutable-step refusal supplied
   independent no-callback controls.

After establishing the feedback loop, source inspection located the boundary
in `zero_row_dtplyr_proxy_input()`: root isolation began by calling
`utils::head()` on the populated parent. The root safety branch admitted the
step before the bracket guard; derived bracket guards checked the ordinary
data.table method rather than these root subclass dispatches. Qualification
of `head()` protected generic lookup but did not establish its S3 method.
A plain GlobalEnv `head.data.table` replacement did not run in the control;
the added subclass method, rather than any head-named function, was the
load-bearing dispatch boundary.

The observation matrix exited 0 after checking its expected failing and
passing cases. Its desired-behavior `--assert` invocation exited 1. The
minimal bracket desired-behavior invocation also exited 1, with
`identical(result$included, "(g)") is not TRUE` after its native/name-only/
plain-class controls passed. The coordinator independently replayed typed
plan loss and source mutation from a Git archive of the same SHA.
No implementation fix or post-fix pass was claimed.

## Existing reports and duplicate disposition

All-state issue and PR searches used the four exact queries retained in
`duplicate-{issue,pr}-{1,2,3,4}.json`:

- `"dtplyr" "head" "subclass"`
- `"head.data.table"`
- `"head" "metadata"`
- `"dtplyr" "subclass"`

The full bodies and all comments of [#1303], [#1359], [#1333], [#1371] and
[#1296] were retrieved and read. The adjacent JSON/text records retained them.

- #1303/#1333 isolated immutable roots from a caller-bound **head generic**.
  Their regression fixtures used ordinary data.tables and a local function
  named head. The new witness kept the ordinary qualified generic and instead
  dispatched a new subclass method before isolation.
- #1359/#1371 checked generated **data.table bracket methods**, including
  ordinary/active/lazy registry behavior. Their fixture used an ordinary
  data.table root and replaced `[.data.table` in the query environment. The
  new witness left that method and registry unchanged, used an admitted root
  subclass, and reached its method while truncating the populated parent.
- #1296's local `head.data.table` control concerned a bare head added inside a
  derived join query. The new custom subclass dispatch occurred outside that
  query, at root truncation. The ordinary `head.data.table` control did not
  reproduce the new failure.
- The other subclass search matches concerned factor encoding/missingness,
  scalar `.key`, local sf, or class/attribute policy rather than dtplyr root
  isolation. They did not describe this witness.

One independent ticket, [#1382], was published for safe root truncation under both
head and bracket subclass dispatch. It required faithful inspection/Nest
partitioning or safe refusal, no construction-time populated-root callback,
source immutability, and the retained ordinary/name-only/Mutable controls.
No other ticket blocked it.

## Additional completed controls

The earlier [source-sharing replay] ran with `--assert` on the tested SHA and
exited 0. Both Nest verbs, with uncached and caller-cached volatile inputs,
evaluated that input once at execution and retained source row 1 in both
detail and total cells. Construction and query display ran the predicate
zero times. [Control output](source-reuse-control.txt).

The [remote frontier replay](remote-frontier-controls.R) completed 96
boundary calls: 36 SQLite, 36 DuckDB and 24 Arrow. SQL Nest calls refused
before forcing the supplied Grouping specification; invalid local arguments
sent no statement. SQLite sent no statement; DuckDB sent six `LIMIT 0`
statements, one for each typed inspection graph, and name-only inspection
sent none. Arrow inspection over six three-reader join/union graphs left
all 18 readers and their 36 rows available.
[Control output](remote-frontier-controls.txt).

The copied November 4 harness initially expected a DuckDB metadata statement
also for name-only inspection and stopped at that obsolete assertion. A
separate six-graph probe observed zero statements for name-only and one for
typed inspection, consistent with the schema-only planning contract. Only
that expectation and its stale printed SHA were updated in the copied
harness; the completed result above was a passing control, not a new defect.
No former investigation note was amended.

## Replay and limits

Run from the investigated package root; set `MARGINPLYR_HUNT_SOURCE` to an
independent archive to load that immutable source instead. `--assert` demands
the desired behavior and intentionally fails on the investigated SHA.

```sh
p=investigation/2026-11-06-nest-spec-exhaustive/backend
Rscript "$p/root-subclass-minimal.R"
Rscript "$p/root-subclass-minimal.R" --bracket --assert
Rscript "$p/root-subclass-head.R" --assert
Rscript "$p/remote-frontier-controls.R"
Rscript investigation/2026-11-04-nesting-grouping-boundaries/backend/dtplyr-source-reuse.R --assert
```

Environment: R 4.6.1, Darwin 25.6.0 arm64; dplyr 1.2.1, dtplyr 1.3.3,
data.table 1.18.6.1, tidyselect 1.2.1, vctrs 0.7.3, DBI 1.3.0, dbplyr
2.6.0, RSQLite 3.53.3, DuckDB 1.5.5, Arrow 25.0.1.
Testthat parallelism was explicitly disabled. Every execution reached a
terminal result; no child execution remained. No package code, permanent
test, dependency or global method registry was changed by this track.

The finite investigation did not establish every subclass method, every
query graph, hosted backends, other library/OS versions, resource exhaustion
or interruption behavior. SQL/Arrow refusal controls did not establish
support for nesting on those backends.

[#1303]: https://github.com/sayuks/marginplyr/issues/1303
[#1359]: https://github.com/sayuks/marginplyr/issues/1359
[#1333]: https://github.com/sayuks/marginplyr/pull/1333
[#1371]: https://github.com/sayuks/marginplyr/pull/1371
[#1296]: https://github.com/sayuks/marginplyr/pull/1296
[source-sharing replay]: ../../2026-11-04-nesting-grouping-boundaries/backend/dtplyr-source-reuse.R
[#1382]: https://github.com/sayuks/marginplyr/issues/1382
