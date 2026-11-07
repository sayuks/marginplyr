# Nest cell membership and registered list converters

Investigated: 2026-11-08
Tested main: `ad7284118124f11a39ac29b10f1d101399d79524`

The investigation began from freshly fetched main in a new worktree on
`investigation/nest-contract-hunt-20261108`. Three fresh-context agents
independently examined implementation, public contracts and processing
consistency at that SHA. They shared no candidate before their initial
reports. The coordinator separately prepared a row-membership oracle and
small inputs. Each lane executed against its own disposable Git archive;
reproduction programs and logs stayed outside the submitting worktree.

One previously unreported defect was confirmed and published as
[#1490][ticket]. This submission recorded evidence only. It did not repair
the implementation, change package tests or establish another public contract.

## Finding: registered conversion replaced local cell values

An `as_tibble.list` method registered for tibble's generic replaced the `rid`
element of its list argument and delegated to the original method. Both local
Nest verbs passed their generated payload list to that replacement. The
smallest witness needed one source row, one integer payload and no grouping
argument: the source value `17L` became `-99L` inside each returned cell.

| Control | Cell value | Replacement calls |
| --- | --- | ---: |
| `tidyr::nest()` on the same source | `17L` | 0 |
| `dplyr::nest_by()` on the same source | `17L` | 0 |
| `nest_with_margins()` | `-99L` | 1 |
| `nest_by_with_margins()` | `-99L` | 1 |
| Either Margin verb after restoring the original registration | `17L` | 0 |

Every result still had one outer row and a one-row cell. Source serialization
was unchanged. A desired-contract reproduction exited 1 after the native,
restoration and ownership controls passed; its observed-result mode exited 0.
The Issue contains the standalone minimal reproduction.

The three-row coordinator witness used two source groups. Ordinary
caller-local and global unregistered method bindings produced no callbacks
and correct values. Registration alone changed the result. Native
`summarize(data = list(pick(rid)), .by = key)` also preserved the payload with
zero callbacks. Explicit conversion of a plain list invoked the replacement,
isolating the dispatch added by the generated cell expression.

An independent fresh-process replay used districts north/north/south,
payloads `41L/67L/89L`, and a Date payload. Both verbs, both `.keep` values,
and included-only versus rollup plans gave eight failing Margin paths. Every
cell's integer payload became `-311L`, with two or three callbacks per call.
Outer keys, Grouping set identifiers, cell row counts and Date values matched
the controls. Native nesting and `list(pick())` retained the source values.

The [Nest reference][nest-reference] promised data-frame cells holding the
input's non-key columns and original key values under `.keep = TRUE`.
[ADR 0016's converter amendment][converter-decision] identified the conversion
as marginplyr's choice. The receiver here was a newly generated plain list,
not a caller-defined input subclass. The delegated class/attribute boundary
therefore did not explain the replacement of payload values, and native
nesting did not exhibit it. This was silent value corruption within the
affected R process, without damage to the input object.

## Adjacent paths and ticket boundary

The generated dtplyr cell converter used the stock namespace list method.
Registering `as.data.table.list` did not change either dtplyr Nest result and
received zero populated-cell callbacks. The closed [#1466][dt-converter]
explicitly covered immutable dtplyr input and a caller-root list method;
[PR #1479][converter-fix] repaired that route. The local tibble-registry
witness remained broken on the tested main and was independently repairable.
One ticket covered both local verbs because they shared this cell expression.
It had no blockers or additional dependency prerequisite and was labelled
`bug` and `ready-for-agent` under `to-tickets`.

Observed tibble-list callbacks during dtplyr planning led to a further
hypothesis: changing a zero-row key's type might alter typed Grouping
selection. Stack observations instead identified dtplyr's `simulate_vars()`
name-only proxy, whose placeholders were logical vectors. A stronger
replacement changed the zero-row key placeholder to integer. Name-only and
typed inspections still selected the actual character key, both Nest verbs
retained the original source membership, and native collection remained
unchanged. These callbacks did not supply the faithful typed metadata.
That adjacent hypothesis was refuted rather than added to the ticket.

The existing `test-nest-namespace.R` caller-list-method regression exercised
dtplyr root lookup, not a registered local tibble-list method.
`test-nest-display-helpers.R` likewise exercised generated dtplyr helpers and
list bindings. A search of the test sources found no registration of the
local `as_tibble.list` replacement. Their ordinary payload assertions did not
execute this failing condition. The new Issue required a public regression
that checked cell values and failed on the tested main.

## Independent expectations and finite coverage

Input generation and expected-value generation were separate. The
coordinator manually enumerated grouping sets and partitioned source positions
by their active fixed/included keys, then extracted the original rows.
Neither specification expansion nor inspection output generated the expected
family. Explicit grouping selections kept the tracing `rid` out of the keys.
Assertions matched each outer key and `.id` to the expected cell, comparing
row-ID multisets, values, columns, original keys, promised types and grouping
state. Payload-free cases used actual key-only inputs without appending an
ID; independent kept-key controls supplied the corresponding source slices.

The order oracle independently constructed the missingness/value terms for
fixed keys, the bit/missingness/value terms for dimensions, and the identifier
tiebreaker from the manually enumerated sets. It compared exact outer key
vectors for both Margin orders. No physical order was required for
`.sort = "none"`, or for source rows within a cell.

Counts below described separate finite lanes and included overlapping
conditions. They were not summed into a claim of exhaustive coverage.

| Lane | Completed comparisons | Disposition |
| --- | --- | --- |
| Coordinator membership | 6,480 distinct calls: ten type fixtures, zero/one/six rows, five manually listed families, optional fixed key, both verbs, keep modes, local/dtplyr and all sort values | 6,120 exact matches after the structured-oracle correction below; 360 unsupported dtplyr list-key cases were separated with native controls. |
| Coordinator Margin order | 384 comparisons: character/integer/ordered/NA-level factor, fixed factor key or none, rollup/cube/reordered union, both verbs/backends/keep modes and first/last | Exact independent outer order matched. |
| A: implementation membership | 480 calls over NULL/set/sets/rollup/cube/composite/product/drop and typed source keys | All matched source membership, cells and guaranteed types. |
| A: structured payload | 960 calls across twenty type families, both backends/verbs/keep modes, empty/populated and three plan shapes | 860 matched; 100 were classified by native source/conversion/union controls or metadata refusal. |
| A: structured keys | 432 calls over fixed/dimension roles, empty/populated, keep and sort choices | 402 matched; 24 raw typed-missing refusals and six native matrix-column-name differences. |
| B: public boundaries | 1,131 comparisons: 456 cell/empty/factor cases, 576 name cases, twelve zero-column cases, sixteen implicit/explicit fixed-key cases, fifteen native name controls and 56 capture/grammar cases | Expected cells, row counts, names, metadata, one-read budgets and refusals matched. |
| C: derived inputs | Initial 640 scenarios, followed by an eighty-case targeted oracle correction and 64 factor-free join/set cases | Selected root/filter/select/rename/arrange/mutate/group/cache/join/set paths preserved membership and both caller tables. Factor-bearing graphs also exercised 160 safe-metadata refusals. |
| C: empty factor and errors | 96 cell scenarios plus native sentinel controls | NA level/missing code, payload-free duplicate rows, empty/fixed-key behavior and propagated External conditions matched. |

The coordinator fixtures included missing character values, NA/NaN and
infinities, unordered and ordered factors with unused/NA levels, list keys,
Date, POSIXct with timezone, difftime, matrix and packed-frame keys. Cells also
carried factors and list payloads. B covered `.key`/`.id` versus existing
payloads, `.rows`, dtplyr special names and private-name prefixes. A's
additional name sweep independently matched 1,140 cases; its numbered-dot
anomalies were adjudicated by B rather than counted as new failures.

## Short hypothesis ledger and excluded candidates

| Hypothesis and basis | Execution evidence | Verdict and next evidence |
| --- | --- | --- |
| `.keep` copies or omitted-dimension identities change row membership or original keys. | Independent source-position oracles exercised both verbs and local/dtplyr, including factor NA levels, missing codes and duplicate-dropped families. | Refuted in the selected corpus. A new case would need a different admitted type or input graph. |
| Empty keyless compensation leaks into empty fixed-key input, or zero-column cells lose duplicates. | B's keyless/fixed/implicit-group and zero-column controls, plus C's genuinely payload-free factor fixtures. | Only keyless empty Nest_by retained one cell; fixed-key empty results had zero rows. Promised payload-free row counts matched. |
| Special `.key`, `.id` or input names collide with internal names. | B's role-separated name comparisons and native `..1`/`...` versus `..01` controls. | Supported names matched. The two prohibited payload names were also rejected by native tidyr/dplyr cell conversion; no new ticket. |
| Structured payloads lose values through folding. | A compared collected-source slices and native conversions/unions. | AsIs matrix source construction, array source collection, raw/complex unions and record metadata admission explained the differences. Matrix column-name loss was also native union behavior, outside extra attribute restoration. |
| dtplyr's omitted identity uses `length()` of a first structured column with a different row size. | C compared included-only/rollup and first/moved matrix/array/packed/POSIXlt/record payloads: 32 ordinary controls and eighty retained-structure cases. | Ordinary input conversion expanded matrix columns; retained structures reached safe metadata refusal even in the included-only control. No admitted positive control reached the suspected failure. |
| List dimensions reveal another supported dtplyr result failure. | Native grouped summarize and grouped tidyr nesting refused list keys; a plain union alone succeeded. Margin multi-set translation also gave `Invalid input`. | No supported native list-key execution was established. The earlier plain-union hypothesis was refuted; these outcomes did not establish another promised successful Nest path. |
| Derived or cached steps mutate caller inputs or mix branch sources. | C's cache/rename/filter/select and two-input join/set controls compared row IDs and serialization of both source tables. | Selected admitted graphs matched and retained source bytes. Arbitrary unsafe metadata transformations remained intentionally refused. |
| Generated branching changes or repeats an upstream External condition. | C's safe-filter sentinel error preserved class, message and parent; a warning reached native/Nest/Nest_by once each. | Refuted for this upstream filter boundary; arbitrary callback and interruption graphs were not inferred covered. |
| A registered local list converter replaces cell values while native nesting stays correct. | Coordinator minimum and A's eight independent paths, with registration/restoration/native/source controls. | Confirmed as #1490. The dtplyr metadata placeholder hypothesis was separately refuted. |

Oracle errors were corrected before accepting results. The coordinator's
packed-column missingness assertion initially used column indexing instead of
row slicing; all 720 structured cases were rerun with `vec_slice()` and
matched. A initially compared against pre-dtplyr packed/matrix names; using
the collected source resolved eighty cases. C initially fixed payload order
despite an input `select()` changing it; the targeted eighty-case rerun
resolved all forty bad assertions. B initially compared native wrapper classes
instead of their prohibited-name cause. These were harness errors, not
marginplyr findings. A discarded stale native controls after failed source
creation and replaced them with successful final direct controls.

## History, duplicate search and freshness

The recent [systematic note][systematic], [cell-conversion note][cells],
[boundary note][boundaries], [deep note][deep] and [registry note][registry]
and the corresponding source history supplied prior candidates. Their earlier
execution counts were not reused. Fresh contract-mode replays of #1456's
omitted dtplyr identity and #1457's qualified constructor callee exited 0:
the former retained one Grand total cell with all source payloads and zero
intercepted helper lookups; the latter had zero violations over forty public
consumer calls.

All-state CLI exports contained 765 Issues and 721 PRs before publication.
Title/body searches used `as_tibble.list`, `as_tibble` with Nest/cell terms,
and the list-converter/namespace reports. A separate all-state Issue/PR search
for `as_tibble.list` returned no matches. #1466's full body and comments were
retrieved and distinguished as above; #1427's namespace operation and #1238's
generated `list()` lookup were different boundaries. #1490's published body
and `bug`/`ready-for-agent` labels were fetched and verified.

A further main fetch before Issue publication still resolved to the tested
SHA. The fixed archives therefore reproduced the defect on that fetched main;
tracker closure alone was not used to claim a repair. Submission integration
and Review-ready evidence belonged to the separately committed note revision
and were recorded on the PR.

## Environment, artifacts and limits

Executions used arm64 macOS Tahoe 26.6.2, R 4.6.1, dplyr 1.2.1, tidyr 1.3.2,
tibble 3.3.1, dtplyr 1.3.3, data.table 1.18.6.1, rlang 1.3.0,
tidyselect 1.2.1, vctrs 0.7.3, testthat 3.3.2 and pkgload 1.5.3. Each lane
kept testthat parallel execution disabled and at most one reproduction active
in its own isolated copy. All execution handles were collected to terminal
completion. No live database was needed or accessed; every input was synthetic.

Temporary evidence stayed under these roots:

- `/private/tmp/marginplyr-nest-parent-20261108/`: manual membership/order
  oracles, corrected CSVs, minimal observed/contract reproductions, native
  controls, historical replays and all-state tracker snapshots.
- `/private/tmp/marginplyr-nest-A-20261108/`: implementation/structured probes,
  independent converter replay, metadata-placeholder controls, final native
  controls and document-reading ledger.
- `/private/tmp/marginplyr-nest-B-20261108/`: public boundary/name/capture probes
  and the short hypothesis report.
- `/private/tmp/marginplyr-nest-C-20261108/`: derived/ownership/error probes and
  artifact manifest. The original 640-case CSV was overwritten by its targeted
  eighty-case rerun; the lane ledger and terminal transcript retained the
  original outcome counts, and the surviving CSV described only the rerun.

The investigation exhausted its concrete, executable candidate list after
integrating all three reports and rechecking both verbs and backend paths.
It did not prove arbitrary R programs, S3/S4 registrations or structured
classes correct. Other R/dependency versions, operating systems, interrupts,
resource-exhaustion behavior and external database systems remained untested.
No result depended on a shared element subclass across backends or on restoring
arbitrary attributes that ADR 0016 delegated. Those limits supplied possible
future evidence rather than unresolved reproduced bugs.

The committed change added only this dated evidence note. Its consumers were
investigation readers; no package runtime, test, generated documentation,
dependency or build behavior changed. Those effects justified Review-ready
`source`, with package build/check, complete suite and snapshots omitted.
No NEWS entry was required for the evidence-only change.

[ticket]: https://github.com/sayuks/marginplyr/issues/1490
[nest-reference]: ../man/nest_with_margins.Rd
[converter-decision]: ../design/adr/0016-delegate-result-class-and-attributes-to-dplyr.md#amendment-the-cell-expression-names-the-converter-and-it-names-the-one-each-backends-own-nesting-gives
[dt-converter]: https://github.com/sayuks/marginplyr/issues/1466
[converter-fix]: https://github.com/sayuks/marginplyr/pull/1479
[systematic]: nest-spec-systematic-validation-2026-11-07.md
[cells]: nest-grouping-contract-sweep-2026-11-07.md
[boundaries]: nest-grouping-boundaries-2026-11-07.md
[deep]: nest-spec-deep-bughunt-2026-11-07.md
[registry]: nest-spec-bughunt-2026-11-07.md
