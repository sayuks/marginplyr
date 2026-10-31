# Local nesting source-equality and structural boundary audit

Investigated: 2026-10-31
Baseline: `339315c3` (`main`)

## Finding: NA-level factor encoding split one source equality group

A factor subclass defined a valid vctrs equality proxy under which `A` and
`a` were one group. Its two source rows used those levels, and the factor
also declared one unused NA level. Both nesting Margin verbs with a single
`grouping_set(g)` returned two one-row cells instead of one two-row cell.
Neither a rollup, an omitted dimension, an actual missing source value, a
non-missing Margin label, `.keep`, `.sort`, nor `.id` was necessary.

`factor-equality.R` retained the minimal public fixture. It registered
same-class vctrs prototype and cast methods as well as the equality proxy.
Direct `vctrs::vec_group_loc()`, ordinary `dplyr::nest_by()`, and an ordinary
`dplyr::union_all()` control retained one source group. The union also retained
the factor subclass. This separated the membership change from a subclass
that dplyr/vctrs itself discarded. Removing just the unused NA level or
selecting the source column as a fixed `.by` key restored one cell in both
nesting verbs.

The hypotheses and their separating predictions were:

1. The package's factor identity encoder replaced the source equality proxy
   with character equality. Removing the declared NA level would avoid that
   route; a typed-missing label would still fail because encoding guarded the
   identity copy independently of whether a display label was inserted.
2. Ordinary union or slicing discarded the subclass. The direct dplyr union
   control would then also lose the proxy and split the source group.
3. The source equality definition was not honored by ordinary nesting. Direct
   `vec_group_loc()` or `nest_by()` would then also split the two source rows.

The controls supported the first explanation. `execute_margin_nest()` set
`identity_encoded` for the NA-level factor dimension and copied its identity
through `encode_factor_for_margin()`. That helper called `as.character()`;
the fold consequently grouped by `A` and `a`, rather than by the original
proxy's one equality group. The unused NA level was enough to select that
route. The recent #1073 display fix prevented a displayed value from refining
an intact typed identity, but this fixture lost the identity's equality before
that fold. The full closed #1073 body and its empty comment list were read;
an all-state search for factor equality and source equality supplied no
separate open duplicate at the time of this track's search. Tracker publication
was left to the coordinating investigator.

The public nesting reference promised one nested data frame per source group.
The finding concerned the source-row membership marginplyr constructed. It
did not require restoration of the factor subclass on output columns, or any
other arbitrary attribute guarantee forbidden by ADR 0016.

The ordinary minimal invocation exited 0 after asserting the dated incorrect
membership. The `--assert` invocation required the correct one-cell result
and exited 1, with the exact failed assertion retained in its adjacent log:

```sh
Rscript investigation/2026-10-31-nest-spec-greedy/local/factor-equality.R
Rscript investigation/2026-10-31-nest-spec-greedy/local/factor-equality.R --assert
```

No repair was attempted. These were observation and red-capable reproduction
modes, not regression tests claiming that the defect had been fixed.

## Completed boundary probes

The track read `CLAUDE.md`, `AGENTS.md`, `CONTEXT.md`, the public nesting and
shared grouping references, ADRs 0016 and 0018, and the October 29 exhaustive
and October 30 local nesting investigations. Prior runs guided the new cases
and were not included in these completed observation counts.

| Evidence | Completed observations |
| --- | --- |
| `equality-cross-product.R` | 576 calls crossing the factor's NA-level presence, 0/4/8 source rows, fixed/dimension roles, both nesting verbs, both keep modes, all sort choices, string/typed-missing labels, and single-set/rollup plans. A package-version second dimension and packed/matrix payload ran in the same inputs. There were 480 faithful calls and 96 wrong memberships, all in populated NA-level factor dimension cases; 1,536 cell projections retained the source packed values and original kept key values. Every serialized source stayed unchanged. |
| `unicode-names.R` | Seed 31103172; 480 constructed calls with simultaneous fixed/dimension/payload/output names from 32 spellings, including combining/precomposed accented text, a zero-width character, emoji, braces, backticks, backslashes, embedded newline/carriage-return/tab, data.table names, dot-dot names, and private-name prefixes. Of these, 372 were accepted with matching independent source membership and 1,298 exact cell projections. Eighteen matched the documented rowwise `.rows` refusal; 90 source dot-dot-name refusals also occurred in an independent ordinary `dplyr::select()` control. Serialized sources stayed unchanged. |
| `reference-list-keys.R` | 432 calls over list keys containing shared/distinct environments, closures, formulas, pairlists, expressions and recursive reference-bearing objects; 0/4/8 source rows, fixed/dimension roles, both verbs, both keep modes and all sort choices, with typed-missing labels. Independent source memberships matched and 936 kept-list-key projections remained exact. Serialized sources stayed unchanged. |

The source-group oracles called `vctrs::vec_group_loc()` on the original source
keys for each explicitly enumerated set. They normalized only the unspecified
outer group order; source-row order inside each cell and the selected source
projections were compared exactly. The names probe used `all_of()` for
selection, because a bare `.data` is tidyselect's pronoun rather than a column
selection. Source dot-dot names were recorded as upstream refusals, separately
from accepted results and the package's rowwise restriction.

Each additional script exited 0 at terminal completion and retained its
adjacent log. Replay from the repository root:

```sh
Rscript investigation/2026-10-31-nest-spec-greedy/local/equality-cross-product.R
Rscript investigation/2026-10-31-nest-spec-greedy/local/unicode-names.R
Rscript investigation/2026-10-31-nest-spec-greedy/local/reference-list-keys.R
```

These finite probes established neither all possible S3/S4 behavior nor a
new arbitrary-class restoration promise. They did not cover remote nesting,
other dependency versions, other operating systems, unbounded inputs or
resource exhaustion. The track changed only dated investigation evidence,
ran no full test suite, and published no tracker mutation.
