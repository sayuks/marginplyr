# Hunt over the nesting verbs and Grouping specifications

Investigated: 2026-11-12
Repository snapshot: `11471772e4a8a8518a689c538c2bc2929bd5d53d`

The investigation ran against freshly fetched main in one worktree. No package
implementation, package test, dependency, public specification or generated
documentation was changed, and no repair was implemented.

Three fresh-context investigators worked read-only in the same checkout from
separate starting points and reported before any candidate was shared: A
compared dtplyr with local data frames and both with an oracle of its own, B
took the Grouping specification grammar against its own plan expander, and C
took the Option arguments, Margin order, Margin labels, result classes and
attributes, and grouped and row-wise inputs. The coordinator separately probed
the neighbourhood of the repairs merged between 2026-11-09 and 2026-11-12, and
replayed every reported finding in a fresh R process from its own script.

Versions were R 4.6.1 on arm64 macOS, dplyr 1.2.1, tidyr 1.3.2, dtplyr 1.3.3,
data.table 1.18.6.1, tibble 3.3.1, rlang 1.3.0 and vctrs 0.7.3. Inputs were
synthetic local data frames, tibbles, data.tables and dtplyr steps; no database
was used.

## Method and independent expectations

Expected values never came from marginplyr internals or from agreement between
two marginplyr paths. A's oracle grouped the source with
`vctrs::vec_group_loc()` for each Grouping set in plan order, sliced the
original rows into cells, replaced omitted dimensions with the Margin label
(a rebuilt factor for a factor dimension, keeping an NA level apart from a
missing code) and bound the branches with `vctrs::vec_rbind()`. A and C each
wrote the Margin order key of ADR 0018 by hand. B wrote a plan expander from
the `?grouping_set` reading rules and compared plan membership, dimension
order, Grouping bits, `grouping_id` values and duplicate partitions with it.
Native references were `tidyr::nest(.by =)`, `dplyr::nest_by()` and grouped
`summarise()`.

Each candidate was checked against the issue tracker (open and closed, through
`gh issue list --search`), `investigation/`, `tests/testthat/`, `NEWS.md` and
the ADR governing it.

## Confirmed findings

Each script under `investigation/2026-11-12-nest-spec-hunt/` is run from the
repository root, prints the observed behaviour and exits zero.

| Boundary | Established effect | Script | Issue |
| --- | --- | --- | --- |
| Duplicate grouping sets proven before typed metadata, when a later argument is a typed selection | The refusal is worded as the complete partition of duplicate positions but omits groups, or splits one set's occurrences into several groups. | `duplicate-positions-typed-suffix.R` | pending |
| A grouped input whose grouping column also appears in `.grouping` or is named by `.margin_label` | Both refusals name `.by`, which the caller did not write. | `grouped-input-names-by.R` | pending |

### Duplicate positions with a typed selection after them

```r
d <- data.frame(a = "x", n = 1L)
inspect_grouping(d, .grouping = grouping_sets(a, n, a, where(is.integer)))
```

`where(is.integer)` selects `n`, so the plan is `a`, `n`, `a`, `n`, whose
duplicate occurrences are `1, 3` and `2, 4`. The same call spelled with `n`
reported "position groups: 2, 4; 1, 3". With the typed spelling it reported
"Duplicate grouping sets were produced at positions: 1, 3." and nothing about
`2, 4`. dtplyr reproduced.

The product `grouping_spec(cube(a, n), cube(a), where(is.integer))` expands to
eight sets whose duplicates are `1, 2, 3, 4, 5, 7` (all `{a, n}`) and `6, 8`
(both `{n}`); the name spelling reported exactly that. The typed spelling
reported "position groups: 3, 4, 7; 1, 2, 5": one set shown as two groups and
the second set missing. `nest_with_margins()`, `nest_by_with_margins()` and
`summarize_with_margins()` agreed with `inspect_grouping()`. In B's random
specification corpus, 5 of 291 duplicate diagnostics that used the
complete-partition wording disagreed with the independent partition, and all 5
contained `where()`.

Every listed group did hold equal sets, so the refusal itself was correct.
`R/grouping-plan.R` words partial evidence differently: `abort_duplicate_witness()`
says "For example, these positions ... have the same set", while
`validate_unique_grouping_sets()` presents its groups as all of them. By
reading, the pre-typed path that validates leading selections passes the
proof it holds — completed sets, and the first and last choices of each
pending typed suffix, kept under separate keys because the suffix is unknown —
to `validate_unique_grouping_sets()`, which formats it as the complete
grouping. Keys kept apart for an unknown suffix are what later turn out equal.
#1342 added that path and asked it to "report the duplicate occurrences
consistently with the resolved Grouping plan"; its tests in
`test-grouping-leading-errors.R` match single pairs only, and the duplicate
position audit in `2026-11-06-nest-grouping-hunt/` checked that each reported
group held equal sets, not that the list was complete.

### A grouped input's refusals name `.by`

```r
gd <- dplyr::group_by(
  tibble::tibble(year = c(1L, 1L, 2L), region = c("a", "b", "a"), v = 1:3),
  year
)
nest_with_margins(gd, .grouping = rollup(year, region))
nest_with_margins(gd, .grouping = rollup(region),
                  .margin_label = c(region = "All", year = "All"))
```

The first refused with "Columns cannot appear in both `.by` and `.grouping`",
the second with "`.margin_label` must not name fixed `.by` column". The
caller wrote no `.by`. Both Margin verbs that nest, `summarize_with_margins()`,
`expand_with_margins()` and, for the first, `inspect_grouping()` gave the same
text. *Grouped and row-wise inputs* says the grouping columns "become implicit
fixed keys" and that "A grouping column also cannot appear in `.grouping`", so
the refusals are correct; their wording sends the reader to an argument that
is not in the call. `CONTEXT.md` asks a Package condition to name what the
caller supplied in the caller's own terms.

The tests pin the "cannot appear in both" fragment and the `.margin_label`
message for an explicit `.by` only; the grouped-input test in
`test-grouping-interface.R` pins a different refusal, a grouped input that
also supplies `.by`. #515 concerned the remediation text of other
grouped-input refusals.

## Rejected candidates

| Candidate | Reason |
| --- | --- |
| A dtplyr cell's `.internal.selfref` is stale, so `:=` warns about a shallow copy | Native `tidyr::nest()` of the same `lazy_dt()` and of a direct data.table produced the same stale cells. |
| `.keep = TRUE` leads each cell with the grouping columns rather than input order | Deliberate in both backend paths; commit `e031e92a` and the comment in `nest_expanded_margins()`. |
| A labelled integer dimension sorts as text under `.sort` | ADR 0018 keys on the result's own grouping columns. |
| Doubles that render alike tie in Margin order, so backends order them differently | Same key; cell membership stayed correct. |
| A factor dimension with a non-missing label drops outer element names | The native `union_all()` reference drops them too. |
| dtplyr flattens packed payloads, drops element names, refuses complex or raw dimensions | Upstream, before marginplyr reads the input. |
| A name bound to an empty `grouping_set()` colliding with a column is refused in `rollup()` or `cube()` | ADR 0026. |
| `nest_by_with_margins()` of a zero-row input with grouping keys returns zero rows | Documented: the one-row result is for an input with no grouping keys. |
| Cell classes or frame labels for sf, custom tibble subclasses or data.table cells | Element class is not promised; extra frame classes are excluded. |

## Covered without a finding

Counts are the investigators' reports.

- A, about 16,000 calls: twelve key types (including factors with an NA level
  beside missing codes, POSIXct with a time zone and fractional seconds,
  difftime and integer64 above 2^53) crossed with labels `"Total"`, `NULL`,
  `NA_character_` and a custom label, both `.keep` values and both verbs, on
  local and dtplyr; Margin order under both `.sort` values and label
  positions; payload types at 0, 1 and 6 rows; columns spelled with fifteen
  internal prefixes; data.table-special and dplyr-reserved names; random nested
  specifications; display collisions without `.id`; grouped and direct
  data.table inputs; repeated collection of one lazy result.
- B, about 12,000 calls: random specifications up to depth 3 over all five
  constructors, composites, repeated dimensions and selection helpers, under
  both duplicate policies, with no fixed key, `.by` or `group_by()`; plans over
  256 sets; about 90 targeted probes of injection, forwarding, shadowed and
  qualified constructors and refusals.
- C, about 1,000 calls: Option argument validation; Margin order against a
  hand-written key; agreement between the summary, nest and row-wise nest
  verbs; cell membership; ADR 0012's factor table; variable labels, dataset
  attributes and element names against tidyr and dplyr; result classes; the
  `.rows` refusal and the keyless empty cell; what dtplyr reads under ADR 0020.
