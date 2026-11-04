# Nesting and Grouping specification boundary hunt

Investigated: 2026-11-04
Tested package SHA: `7a05a1e42d53af62c057a30ad1892ffb35d4eef8`
Environment: R 4.6.1 on arm64 macOS. The track notes and replay output
record dependency versions.

The investigation covered both Nest verbs and the five Grouping specification
constructors through public calls. Inspection and, where useful, other Margin
verbs supplied cross-checks on the shared Grouping plan or branch machinery.
The source SHA included the fixes from
[#1250](https://github.com/sayuks/marginplyr/pull/1250),
[#1259](https://github.com/sayuks/marginplyr/pull/1259),
[#1260](https://github.com/sayuks/marginplyr/pull/1260), and
[#1262](https://github.com/sayuks/marginplyr/pull/1262).

The starting cases came from the
[recent nesting follow-up](../2026-11-04-nesting-grouping-followup/README.md),
[expansion and Grouping specification hunt](../2026-11-04-expand-grouping/README.md),
[public tidyselect audit](../2026-11-04-tidyselect/README.md), and
[earlier greedy nesting hunt](../2026-11-04-nest-spec-greedy/README.md).
The tests here changed one boundary at a time: shape inside a packed column,
attributes on the supplied label, number of geometry columns in the payload,
evaluation per Grouping set, lexical evaluation of a parenthesized callee,
direct selection introspection, and compaction across constructor kinds.

## Confirmed findings and tracker disposition

| Witness | Effect | GitHub issue |
| --- | --- | --- |
| [Cube inside a Cartesian product](#composition-and-inspection) | The one-set duplicate-drop plan exceeded a 96 MiB heap because its cube factor was fully enumerated. | [#1274](https://github.com/sayuks/marginplyr/issues/1274), new |
| [Packed AsIs POSIXlt](local/README.md) | A typed-missing branch could not combine with the included branch. | [#1275](https://github.com/sayuks/marginplyr/issues/1275), new |
| [Two sf geometries](backend/README.md) | The multi-set Nest failed before returning cells with the two source geometries. | [#1276](https://github.com/sayuks/marginplyr/issues/1276), new |
| [Named-list label scalar](local/README.md) | A shaped scalar caused collision checking to fail or changed the display column's shape. | [#1278](https://github.com/sayuks/marginplyr/issues/1278), new |
| [Direct name introspection](spec/README.md) | Derived dtplyr inspection refused locally available names. A locally detectable DuckDB error followed a metadata query. | [#1277](https://github.com/sayuks/marginplyr/issues/1277), new |
| [Repeated dtplyr evaluation](backend/README.md) | The detail cell and Grand total cell came from different evaluations of one immutable source. | [#1271](https://github.com/sayuks/marginplyr/issues/1271), existing shared expansion defect with [Nest evidence](https://github.com/sayuks/marginplyr/issues/1271#issuecomment-6629474580) |
| [Parenthesized constructor callee](spec/README.md) | A second head evaluation changed the selected key or delivered a later condition. | [#1273](https://github.com/sayuks/marginplyr/issues/1273), existing with [Nest evidence](https://github.com/sayuks/marginplyr/issues/1273#issuecomment-6629475031) |

The new tickets can be worked independently. The main SHA was also checked
against the remote and used to extract a separate source tree for replay.
The track notes distinguish controls, nearby issues and remaining limits.

## Composition and inspection

[The composition oracle](plan/composition-oracle.R) enumerated 603
specifications using seed 94371. It used Boolean choices for cube subsets,
grid coordinates for Cartesian products, and explicit prefixes for rollups.
It compared 1,809 inspections across all three duplicate policies, covering
2,067 pre-deduplication occurrences and 147 specifications with duplicates.
The public plans matched exact membership and order, fixed keys, Grouping
set identifiers, Grouping bits and Grouping identifiers. The exact-name
format was exercised with commas, a newline, a backtick and a non-ASCII
name. Identifier boundary controls agreed at widths 1, 30, 31, 32 and 65.

[The capped-heap witness](plan/composed-cube-memory.R) used a one-row input
and a cube with 18 different dimensions, crossed with one grouping set
containing all of them. Every one of the 262,144 memberships in the cube
becomes the same complete set in the product. Under duplicate drop and a
96 MiB R vector heap cap, both factor orders failed in inspection.
`nest_with_margins()` failed with the cube first, and
`nest_by_with_margins()` failed with the cube last. A direct set of the same
18 dimensions returned one occurrence and retained the payload in both
nests. The composed plans at width 10 succeeded in both factor orders.
The cube deduplicates its own memberships before the enclosing product
can apply the columns required by its other factor.
[Issue #1245](https://github.com/sayuks/marginplyr/issues/1245) and
[PR #1250](https://github.com/sayuks/marginplyr/pull/1250) covered repeated
units and product prefixes. In this witness every intermediate cube
membership differs. The measured cap establishes this case at these
settings. The probe does not estimate a universal maximum cube width.

[The inspection controls](plan/inspection-boundaries.R) obtained the same
typed plan from a local tibble, a direct Arrow reader, a derived dtplyr filter
and a DuckDB query. The Arrow reader still supplied its original first batch.
A dtplyr filter failed under explicit collection as designed and was untouched
by inspection. The DuckDB source sequence was unadvanced after inspection.
Those controls cover source evaluation. The spec track measures the separate
zero-row metadata request for a locally decidable error.

The cube disposition used all-state Issue and PR searches for
`cube "duplicate-drop"`, `"Cartesian product" "memory"`,
`"distinct sets" "suffix"`, `"cube" "required columns"`, and
`"intermediate" "grouping" "exponential"`. The nearest matching records
were #1245 and #1250. The open-issue list was checked again before publication.

## Replay

Run each replay in a fresh R process from the tested package root. If using
a Git archive of the tested SHA, set `evidence` to the absolute path of this
note's directory in the investigation checkout.

```sh
evidence="${evidence:-investigation/2026-11-04-nesting-grouping-boundaries}"
Rscript "$evidence/plan/composition-oracle.R"
Rscript "$evidence/plan/inspection-boundaries.R"
R_MAX_VSIZE=96M Rscript "$evidence/plan/composed-cube-memory.R" --expect-defect
```

The capped replay without `--expect-defect` asserts the desired one-set
result. See the [local](local/README.md), [backend](backend/README.md),
and [specification](spec/README.md) notes for their observation and
desired-behavior modes. These finite probes cover their listed shapes and
installed backends. The study used no hosted database or live PostgreSQL
execution.
