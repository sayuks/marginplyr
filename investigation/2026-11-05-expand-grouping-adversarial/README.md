# Expansion and Grouping specification adversarial bug hunt

Investigated: 2026-11-05
Baseline: `f158df41168bbf0b64bce01518fcff1bd88cb11f` (fetched `main`)

A fresh worktree from main split the public surface across local and dtplyr
expansion, SQL and Arrow expansion, Grouping family algebra and constructor
capture, and tidyselect evaluation. The [November 4 expansion boundary
hunt](../2026-11-04-expand-grouping/README.md), [invariants
hunt](../2026-11-04-expand-grouping-invariants/README.md), [nesting
hunt](../2026-11-04-nesting-grouping-boundaries/README.md), and [Margin
contracts hunt](../margin-contracts-bug-hunt-2026-11-04.md) supplied prior
witnesses and exclusions. Earlier observations did not count as new runs.

## Findings

| Ticket | Observed behavior |
| --- | --- |
| [#1320](https://github.com/sayuks/marginplyr/issues/1320) | A named constructor argument ran its value while the constructor was capturing arguments for rejection. |
| [#1321](https://github.com/sayuks/marginplyr/issues/1321) | A duplicate-drop Cartesian product needed exponential intermediate space for one retained set when a grouping-set union enclosed a nested product. |
| [#1322](https://github.com/sayuks/marginplyr/issues/1322) | Explicitly qualified parentheses around a complete selection caused inspection and expansion to use the wrong column. |
| [#1323](https://github.com/sayuks/marginplyr/issues/1323) | An embraced column inside a name-only selection caused a metadata refusal or an avoidable query. |
| [#1324](https://github.com/sayuks/marginplyr/issues/1324) | Standalone environment-pronoun selections had the same metadata symptoms, through a different classification boundary. |
| [#1325](https://github.com/sayuks/marginplyr/issues/1325) | SQLite direct CTE collection lost source collation and changed the first row of a sorted expansion. |

The issue split follows observable behavior. Each slice has an independent
public witness and can land without a blocker or a prefactoring issue. Each
received `bug` and `ready-for-agent`. No parent issue supplied the work.

### Named constructor tags ran capture controls

[The capture replay](named-capture-options.R) supplied one positional column
and a named argument whose value counted its own invocation and threw an
error. All 60 comparisons across five constructors, six capture-control tags
and top-level versus nested positions evaluated the value and propagated its
error. Ten otherwise identical calls using an unknown tag reported the
Package condition with zero invocations. Direct `.named = TRUE` changed the
diagnostic to include a synthetic name, and `.ignore_empty = "all"` removed
the empty position before reporting the tag. A dynamic tag or a spliced list
with the same name reached the ordinary rejection without running its value.

The constructor documentation requires positional arguments. The capture
call forwarded dots into a function with named control formals, so the tag
influenced capture before the constructor checked it. The distinction between
the direct and injected tag isolated this from the handling of tagged missing
arguments in #1227 and #1234.

### A product inside a union enumerated a cube before applying outer keys

[The compaction replay](product-scope-repro.R) used one row with 18 distinct
dimensions. A Cartesian product combined a grouping-set union containing an
inner product of a cube with a grouping set requiring all 18 dimensions.
Every cube occurrence therefore produced the same final set. At
`R_MAX_VSIZE=96M`, inspection and expansion both reached the vector memory
limit in both outer factor orders. Removing only the inner product wrapper
made both consumers return the one expected set at the same cap. The
ten-dimension version of the original shape also completed. The expected
Grouping identifier was zero and the Grouping set identifier was one.

The outer product could give guaranteed columns to a directly nested cube,
including a cube in a union branch. A product inside that branch resolved its
own factor family before it could use those columns. #1274 and PR #1286
addressed the direct cube and union-of-cube paths.

### Qualified parentheses gave a lexical selection the name proxy

In [the selection replay](grouping-selection-boundaries.R), a lexical promise
read the type of column `a` from tidyselect and returned the name `b` when
`a` was character. With explicit `base::` qualification of the parenthesis
operator around `c(a)`, ordinary tidyselect chose `b`, as did a
`base::identity()` wrapper and a qualified inner `base::c()`. The Margin
fixed-key and Grouping inspectors chose `a`, and expansion labelled `a`.
Syntactic parentheses around `c(a)` correctly selected the column `a`
through tidyselect's selection walk.

The name-only classifier unwrapped the explicitly qualified outer call and
classified the bare inner combining call as a walk, so the lexical promise
saw the name proxy. This exercised the typed-context requirement from #889
and PR #904. #1288 and PR #1297 covered constructor-shaped calls in nested
specification positions.

### Embracing and direct lexical pronouns required unnecessary metadata

The same selection replay examined both fixed keys and Grouping dimensions.
On a derived dtplyr step, a wrapper combining `{{ col }}` with a literal
column inside `c()` refused the call for lack of faithful typed metadata.
A local frame and the inlined selection on the derived step selected both
columns. The name-only classifier treated the child quosure as typed.
Literal variadic forwarding in #1280 and forwarding a nested specification in
#897 reached different capture boundaries.

A standalone `.env$cols` or `.env[["cols"]]` likewise refused the derived
step, though ordinary tidyselect on a local frame and
`all_of(.env$cols)` on the same derived step returned the supplied column
order. The classifier understood lexical lookup inside the helper argument
but did not recognize the direct pronoun as name-only. #1265 covered the
helper form and #1277 covered direct selection-context name introspection.

For each of the four fixed-key or dimension witnesses, supplying an absent
name on live DuckDB produced one `LIMIT 0` DBI send before the missing-column
condition. The explicit helper control produced the missing-column condition
with zero sends. A genuine type-predicate control still required typed
metadata. ADR 0005 sets the local-error boundary and ADR 0020 sets the
faithful-metadata boundary.

### SQLite CTE collection dropped the source value order

[The SQLite replay](sqlite-cte-collation.R) used a SQLite column declared
`TEXT COLLATE NOCASE` with values `B` and `a`. Included-only expansion with
last Margin order returned `a, B` by default, then `B, a` from the same
result with CTE rendering. A one-row fetch changed from `a` to `B`.
Ordinary arranged collection returned `a, B` with both renderings. Running
the expanded CTE SQL directly reproduced the reversal. Explicitly specifying
the source collation in the final value-order term restored `a, B`.

The typed companion's leading zero-row arm was reshaped by CTE rendering,
and the outer sort lost the collation supplied by the source. The
[192-case matrix](sqlite-cte-collation-matrix.R) crossed two collations,
source shapes, key roles, families, labels, sort directions and three
identifier forms. All default direct and ordinary CTE order controls passed.
The two direct renderings agreed on the public schema and rows in all 192
cases. Relative non-Margin key order differed in 64 cases, all using NOCASE
with an ordinary identifier or no identifier. Boolean-word identifiers used
the separate rendering path added for #1180 and passed all 32 NOCASE cases.
ADR 0031 applies to direct collection.

## Additional coverage and exclusions

| Probe | Result on the baseline |
| --- | --- |
| [Mixed Grouping families](mixed-family-oracle.R) | Seed 41248 generated 400 shapes. The independent set-family oracle checked 326 specifications under all three duplicate policies, including 109 required duplicate refusals whose reported positions referred to equal memberships. It excluded 73 top-level selectors and one fixture exceeding 300 occurrences. |
| [dtplyr public names](dt-names.R) | 600 comparisons crossed 25 spellings, four column roles, two labels and three sort options. 552 matched the eager comparison. The other 48 hit dot-dot name validation, 36 in ordinary source collection and 12 in tibble collection of the generated identifier. The dtplyr package tests use data.table materialization to observe those identifiers. |
| [dtplyr typed values](dt-types.R) | Of 270 admitted comparisons, 240 matched eager expansion and 30 matched the ordinary lazy union control. The comparisons covered empty, single and three-row inputs, fixed, dimension and payload roles, and both branch orders. Unsupported source construction was outside that count. |
| [Observed label checks](collision.R) | On eager and dtplyr inputs, the only differences were the source names `...` and `..1`. Ordinary tidyselect input selection refused those names before label checking, matching the earlier October 26 exclusion. |
| [Duplicate refusal resource lead](duplicate-refusal-memory.R) | A cube repeating one dimension twenty times reached the vector heap cap under the default error policy. Drop returned two sets and the small error case diagnosed repeated positions. The expected diagnostic budget for a large error-policy family was left open. |

A name-only `all_of()` callback returning successive columns chose the
second column after two invocations. The fixed-key, direct tidyselect and
typed-sibling controls ran it once. ADR 0008 explicitly allows two evaluations
for general name-only selections, so this observation stayed out of the
tracker. The once-only rule in that ADR applies to recognized nested
arguments.

SQLite CTE rendering failed with a circular reference for a source table
named `q01`. Ordinary dbplyr `union_all()` produced the same error with
the same source. PostgreSQL 17.9 shortened a 64-byte output identifier and
could make it ambiguous with a 63-byte source column. Ordinary dbplyr
mutation also hit the identifier-length limit. These experiments did not
establish an expansion-specific defect. Arrow probes of joins and projection
did not establish a new ticket. These were focused controls and exploratory
observations, with the finite successful comparisons bounded by their
fixtures.

## Replay and publication

The baseline was checked again against fetched main before publication.
Duplicate checking combined all-state issue and PR searches, the recent
issue and PR lists, open issues, and adjacent reports named above. Search
terms included capture options and `ignore_empty`, nested product and
cube, qualified parentheses and `peek_data`, nested quosures and embracing,
direct environment pronouns and name-only Grouping, and SQLite CTE collation
and NOCASE order. Some overlapping search requests were rate-limited. The
recent all-state lists and specific prior reports supplied coverage for those
queries.

From the repository root on the investigated SHA, these desired-contract
assertions intentionally exited one. Without the assertion arguments, the
scripts printed observations after checking their controls.

```sh
export TESTTHAT_PARALLEL=false
evidence=investigation/2026-11-05-expand-grouping-adversarial
Rscript "$evidence/named-capture-options.R" --assert
R_MAX_VSIZE=96M Rscript "$evidence/product-scope-repro.R" 18 nested inspect first --assert
for case in qualified embraced pronoun; do
  Rscript "$evidence/grouping-selection-boundaries.R" --assert="$case"
done
Rscript "$evidence/sqlite-cte-collation.R" --assert
```

The environment was arm64 macOS with R 4.6.1, dplyr 1.2.1, rlang 1.3.0,
tidyselect 1.2.1, vctrs 0.7.3, dbplyr 2.6.0, dtplyr 1.3.3, data.table
1.18.6.1, RSQLite 3.53.3, DuckDB 1.5.5 and Arrow 25.0.1. The SQL witnesses
used private in-memory databases. The PostgreSQL exploratory control used a
disposable local server. Polars, live SQL Server and hosted databases were
outside the measured scope.

This submission contains dated evidence and standalone replays under the
existing investigation build exclusion. It uses Review-ready `source` scope
because the package, tests, installed files, and check tooling are untouched.
