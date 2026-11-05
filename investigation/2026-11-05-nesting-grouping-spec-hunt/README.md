# Nesting and Grouping specification boundary hunt

Investigated: 2026-11-05
Tested package SHA: `f158df41168bbf0b64bce01518fcff1bd88cb11f` (fetched `origin/main`)
Environment: R 4.6.1, aarch64-apple-darwin23

Four investigators used a dedicated worktree from fetched main to test
`nest_with_margins()`, `nest_by_with_margins()`, all five Grouping
specification constructors and `inspect_grouping()`. Expansion supplied a
second consumer for shared contracts. The November 4 [nesting boundary hunt],
[nesting follow-up], [expansion hunt] and [greedy nesting hunt] supplied
counterexamples and controls. The new probes varied public options, selector
dispatch, product composition, and the work needed to acquire typed metadata.

## Findings

Each issue received `bug` and `ready-for-agent`. Each can be implemented
and verified independently. The user delegated the breakdown and publication.

| Issue | Observed failure | Replay |
| --- | --- | --- |
| [#1301] | A classed `.id` bypassed a collision and replaced a source value with an occurrence identifier. | [Classed scalars](classed-scalars.R), `id` |
| [#1302] | A classed `.key` renamed the cell column and made it a row-wise identifier. | [Classed scalars](classed-scalars.R), `key` |
| [#1309] | A classed Option argument changed ordering, admitted unsupported choices, or changed the inspection format. | [Classed scalars](classed-scalars.R), `choice` |
| [#1303] | Typed inspection and nesting passed both rows of an immutable dtplyr root to a caller's `head()`. A separate probe changed the source by reference. | [Root head](dtplyr-root-head.R) |
| [#1326] | A caller's bracket function changed the type seen by derived dtplyr metadata and caused inspection and nesting to drop a real dimension. | [Derived bracket](dtplyr-derived-bracket.R) |
| [#1304] | Explicit qualification inside a selector argument let caller dispatch run against integer placeholders, changing keys or manufacturing duplicates. | [Qualified parentheses](qualified-parentheses.R) |
| [#1305] | Two leading duplicate occurrences in a nonfinal product factor reached a typed predicate or a metadata refusal. | [Partial product](partial-product.R) |
| [#1306] | A union-wrapped product of a cube exhausted a 96 MiB vector heap even though the outer product retained one set. | [Nested cube](nested-cube.R) |
| [#1307] | Typed metadata refusal displaced an already decidable nesting output-name conflict. | [Early Nest names](early-nest-names.R) |
| [#1308] | Named Margin label validation followed an avoidable DuckDB metadata statement or was displaced by dtplyr refusal. | [Early label names](early-label-names.R) |

### Stored scalars and display dispatch

The local probe held the stored character value constant and varied only its
S3 display method. Native `tidyr::nest()` and `dplyr::nest_by()` used the
stored list-column name `payload` on populated and empty input. Both
Margin verbs used the displayed `shown` on populated input, and the row-wise
verb used `shown` as its identifier. The empty keyless row-wise result used
`payload`. The populated path interpolated the classed string into a
dynamic name, and the row-wise identifier calculation compared the returned
column with the stored name.

For `.id`, both Nest verbs accepted a scalar storing `v` and displaying
`shown`. The outer `v` held 1 and the cell lost its sole payload column.
Expansion replaced the original integer 5 with 1. A scalar storing `data`
bypassed the list-column conflict. A scalar storing `set` but displaying
`v` was refused despite the absence of an actual collision. Plain strings
provided both positive and negative controls. The caller's original frame
still contained 5. Collision matching used the display conversion while
output assignment used the stored name.

The option probe distinguished argument interpretation from sorting and
expansion. A stored `first` displayed as `last` matched the plain `last`
result, the stored abbreviation `f` was accepted, and stored `keep`
displayed as `drop` dropped a duplicate in both Nest verbs. Stored inspection
format `list` displayed as `text` returned character columns. Each plain
string retained its documented meaning under the *Option argument* contract
in `CONTEXT.md`. The reports for R vector names and dimensions addressed
different attributes. [#830] [#831] [#997] [#1278]

### Typed metadata and dtplyr dispatch

The root probe changed only the effective `head()` binding, then compared
an immutable root with an isolated filter and a mutable root. The immutable
root delivered two real rows to the override in typed Grouping dimension and
fixed-key inspection and in both Nest preparations. The override supplied a
zero-row integer prototype for character column `g`. Inspection reported
no dimension and nesting produced one cell instead of two. A separate
override assigned into `g` by reference during inspection. The six controls
retained their expected keys and never invoked the override. The generic
metadata route built a dtplyr expression with a bare head call on the root.
The derived and mutable-root proxy routes bypassed that call. ADR 0020 governs
source processing at this boundary. [#1281] [#1296]

For a derived arithmetic mutation, a caller's bracket override changed the
prototype only when its input had zero rows. Native collection invoked it
with two rows and kept character `g`. Name-only inspection invoked it zero
times. Typed inspection and both Nest verbs invoked it on zero rows during
preparation and dropped `g`. The caller table remained unchanged. The
separating control showed that even an isolated zero-row input can give an
unfaithful answer when generated bracket dispatch still reaches caller code.

### Selection preflight and product composition

For #1304 the genuine outer helper was `all_of()`. Its argument called
explicitly qualified parentheses through a caller binding of the namespace
operator. The binding used the type of `a` in the active selection data.
Direct tidyselect and the typed pass selected the character column `a`.
Fixed-key resolution and names-only dtplyr inspection selected integer
placeholder `b`. Adding a real `b` branch gave six false duplicate refusals
through inspection and both Nest verbs, with and without a later typed
selection. A single set on local data returned `a` after both passes.
Unmasked qualification also selected `a`. The name-only classifier stripped
the qualified helper argument after checking the safety of syntactic
parentheses, so it skipped checking the written namespace lookup. ADR 0008
defines caller evaluation in a discarded pass. The nearest prior tickets
covered qualified helpers and constructor retention. Concurrent #1315 and
#1322 concern whole-expression qualification, where the lexical behavior
comes from the expression inside genuine qualified parentheses. [#1039]
[#1288] [#1315] [#1322]

For #1305 the first two branches of a nonfinal union factor were equal.
The other factor supplied one set, so positions 1 and 2 were already
duplicates. Inspection and both Nest verbs nevertheless evaluated the typed
predicate three times on the local frame. DuckDB inspection recorded a
`selection_proxy` statement with `LIMIT 0` in the Sent query view. Derived
dtplyr reported its metadata refusal, without running its source callback.
The direct union, reversed product and already repaired resolved-prefix
case each refused before the predicate or any recorded statement. The
[order controls](partial-product-controls.R) also found that valid plans and
duplicates that genuinely needed typed data resolved typed `.by` before the
dimension predicate under all three policies. The leading observer retained
completed occurrences from the final factor. The nonfinal union's completed
branches escaped that check. ADRs 0005 and 0008 supply the precedence.
[#1228] [#1246] [#1266] [#1286]

For #1306 the complete outer factor made all 262,144 intermediate
memberships equivalent. At width 8 every wrapper returned one complete
set. At width 18 the direct cube, direct union wrapper and direct product
wrapper passed under the same cap. The union of an inner product failed
through inspection and both Nest verbs. Inspection also failed with the
covering factor first and with an explicit empty set in the inner product.
Controls checked the included columns, occurrence identifier, Grouping
identifier and cell payload. The inner product enumerated its cube before
the outer product could apply its guaranteed columns. The result extends
the direct-cube and direct-union coverage in #1274 and #1286. Parallel
investigations independently filed #1314 and #1321 with the same composition
boundary. #1314 points to #1306 as its duplicate, and #1321 also verifies the
expansion consumer. [#1274] [#1286] [#1314] [#1321]

### Early admission

A locally decidable invalid output name and a valid output name both
reached the same unsafe dtplyr metadata refusal. Five invalid cases covered
`.key` matching a fixed or variable key and `.rows` as a fixed key,
dimension or requested row-wise identifier. Eager and safe derived controls
gave the intended Package conditions. The same unsafe derivation collected
successfully on explicit request and called the payload transform with two
rows. The invalid Nest calls did not call that transform. Their suggestion
to collect would perform work before a name conflict that was already known.
The checks ran after plan preparation. #857 and #879 established the role
boundary for `.rows`. [#857] [#879]

For the label probe, live DuckDB expansion sent one statement at the DBI
boundary, `SELECT * FROM label_names LIMIT 0`, before reporting that
`ghost` was an unknown dimension. The corresponding local diagnostic was
identical. Both Nest verbs on an unsafe dtplyr derivation substituted the
metadata refusal. A valid DuckDB control used the expected single zero-row
statement. Validation of label names only needed the names-only plan in
these calls. ADR 0005 governs both admission findings.

## Additional coverage

- The previous [independent algebra oracle] reran on the pinned SHA.
  Across 473 families, three duplicate policies and two fixed-key input
  routes, 1,974 comparisons matched the expected sets, order, fixed keys,
  Grouping bits and Grouping identifiers. The other 864 comparisons
  received the expected duplicate refusal. Widths 0, 1, 16, 29, 30, 31,
  32, 33 and 64 and a serialized lexical specification also matched.
- The [local membership oracle](local-membership-controls.R) compared
  both Nest verbs, both keep choices and all three sort options across ten
  structured key families. All 120 comparisons matched native detail
  membership, Grand total source rows and cell column projections, with
  unchanged serialized inputs. It compared membership independently of
  outer order.
- The previous [remote frontier] reran with 36 SQLite, 36 DuckDB and 24
  Arrow cases. SQLite sent zero DBI statements. DuckDB sent twelve, all
  `LIMIT 0`. The 18 original Arrow readers still supplied all 36 rows.
  These observations establish the tested inspection and refusal boundary.
  SQL and Arrow Nest calls were refusal controls.

All-state issue and PR searches included scalar class, `.id` and `.key`
collisions, Option argument display, qualified parentheses, dtplyr proxy
head and bracket dispatch, duplicate product prefixes, cube heap limits,
Nest naming before metadata, and named label metadata. The adjacent issues
and PRs above record the closest matches and the concurrent reports. Remote
main matched the tested SHA before issue publication.

## Replay

Run each command from the tested package root in a fresh R process. If using
a Git archive of the tested SHA, set `evidence` to the absolute path to this
note's directory. Observational replays record failures on the tested source.

```sh
evidence=${evidence:-investigation/2026-11-05-nesting-grouping-spec-hunt}
Rscript "$evidence/classed-scalars.R"
Rscript "$evidence/dtplyr-root-head.R"
Rscript "$evidence/dtplyr-derived-bracket.R"
Rscript "$evidence/qualified-parentheses.R"
Rscript "$evidence/partial-product.R"
Rscript "$evidence/partial-product-controls.R"
R_MAX_VSIZE=96M Rscript "$evidence/nested-cube.R"
Rscript "$evidence/early-nest-names.R"
Rscript "$evidence/early-label-names.R"
Rscript "$evidence/local-membership-controls.R"
```

`classed-scalars.R` accepts `--assert id`, `--assert key` and
`--assert choice`. The two dtplyr metadata replays, nested cube, and the two
early admission replays also accept `--assert` to require desired behavior.
The 96 MiB limit measures this fixture on this R and operating system.

The probes used dplyr 1.2.1, tidyr 1.3.2, tibble 3.3.1, rlang 1.3.0,
vctrs 0.7.3, tidyselect 1.2.1, dtplyr 1.3.3, data.table 1.18.6.1,
DBI 1.3.0, dbplyr 2.6.0, RSQLite 3.53.3, DuckDB 1.5.5 and Arrow 25.0.1.
The finite matrix did not cover hosted databases, other library versions,
arbitrary class methods, or all possible selection and query graphs.

Only dated evidence was submitted. Package sources, tests, examples,
vignettes, metadata, exclusions and check behavior retained their baseline
contents. The local verification and main compatibility record belong to
the submitted PR revision.

[nesting boundary hunt]: ../2026-11-04-nesting-grouping-boundaries/README.md
[nesting follow-up]: ../2026-11-04-nesting-grouping-followup/README.md
[expansion hunt]: ../2026-11-04-expand-grouping/README.md
[greedy nesting hunt]: ../2026-11-04-nest-spec-greedy/README.md
[independent algebra oracle]: ../2026-11-04-expand-grouping/grouping-algebra.R
[remote frontier]: ../2026-11-04-nest-grouping-bug-hunt/backend/remote-frontier.R
[#830]: https://github.com/sayuks/marginplyr/issues/830
[#831]: https://github.com/sayuks/marginplyr/issues/831
[#857]: https://github.com/sayuks/marginplyr/issues/857
[#997]: https://github.com/sayuks/marginplyr/issues/997
[#1039]: https://github.com/sayuks/marginplyr/issues/1039
[#1228]: https://github.com/sayuks/marginplyr/issues/1228
[#1246]: https://github.com/sayuks/marginplyr/issues/1246
[#1266]: https://github.com/sayuks/marginplyr/issues/1266
[#1274]: https://github.com/sayuks/marginplyr/issues/1274
[#1278]: https://github.com/sayuks/marginplyr/issues/1278
[#1281]: https://github.com/sayuks/marginplyr/issues/1281
[#1288]: https://github.com/sayuks/marginplyr/issues/1288
[#1301]: https://github.com/sayuks/marginplyr/issues/1301
[#1302]: https://github.com/sayuks/marginplyr/issues/1302
[#1303]: https://github.com/sayuks/marginplyr/issues/1303
[#1304]: https://github.com/sayuks/marginplyr/issues/1304
[#1305]: https://github.com/sayuks/marginplyr/issues/1305
[#1306]: https://github.com/sayuks/marginplyr/issues/1306
[#1307]: https://github.com/sayuks/marginplyr/issues/1307
[#1308]: https://github.com/sayuks/marginplyr/issues/1308
[#1309]: https://github.com/sayuks/marginplyr/issues/1309
[#1314]: https://github.com/sayuks/marginplyr/issues/1314
[#1315]: https://github.com/sayuks/marginplyr/issues/1315
[#1321]: https://github.com/sayuks/marginplyr/issues/1321
[#1322]: https://github.com/sayuks/marginplyr/issues/1322
[#1326]: https://github.com/sayuks/marginplyr/issues/1326
[#879]: https://github.com/sayuks/marginplyr/pull/879
[#1286]: https://github.com/sayuks/marginplyr/pull/1286
[#1296]: https://github.com/sayuks/marginplyr/pull/1296
