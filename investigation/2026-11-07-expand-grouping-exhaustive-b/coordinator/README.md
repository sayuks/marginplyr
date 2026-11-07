# Independent witnesses and public-name controls

Investigated: 2026-11-07
Baseline: `0b5143133c023ccb4efb6fe27f1ef551e9f49186`

The coordinator loaded a separate `git archive` export of the freshly fetched
main SHA. It changed neither the package nor the lane sources during these
reproductions. `session-info.txt` retained the names-matrix environment; the
two witnesses used the same R 4.6.1 provisioned library and pinned package
source. Every execution was serial, with `TESTTHAT_PARALLEL=false`.

## Independent finding witnesses

[independent-label.R](independent-label.R) used an immutable dtplyr source with
two keys, `east, west`, and payload `17, 43`. It constructed an included-only
expansion, then installed a selective `:::` dispatcher in the root environment.
Native source, mutation and union controls matched exactly and invoked zero
selective callbacks. The same expansion changed its keys to
`changed:east, changed:west`, invoked one callback, retained its payload and
left serialized source bytes unchanged. Removing the dispatcher restored the
exact expected frame. The [log](independent-label.log) recorded these controls
before the explicit desired-contract assertion failed, exit one. This was an
independent witness for [#1433](https://github.com/sayuks/marginplyr/issues/1433).

[independent-grouping.R](independent-grouping.R) used a different subclass,
two rows, numeric fixed values `5, 8` and character dimensions `east, west`.
Its names method returned literal names outside selection and reversed them
when the active metadata's second column was character. Direct typed
tidyselect selected `dimension`; ordinary grouping on `fixed` succeeded. The
opaque typed control inspected fixed `fixed` and included `dimension`.
Inspection and expansion with the direct spelling instead falsely refused an
overlap on `fixed`, without changing the source. The
[log](independent-grouping.log) recorded successful controls before the
desired-contract assertion failed, exit one. This independently confirmed
[#1434](https://github.com/sayuks/marginplyr/issues/1434)'s false-refusal witness;
the Grouping lane separately retained its silent fixed-key consequence.

The root minimal witnesses used execution chunks `f25135` and `1046c6`;
both terminal exits were one for the intended desired-contract assertions.
These were unfixed defects, not package test failures or Review-ready results.

## Special public names

[names.R](names.R) crossed 26 names, dimension/fixed/payload/identifier roles,
local/dtplyr/live SQLite/live DuckDB inputs and all three Margin orders. Names
included pronouns, data.table special variables, canonical and noncanonical
dot-number spellings, backticks, spaces, tab/newline and a non-ASCII name.
Expected values came from independently repeated source rows, omitted labels
and occurrence numbers. Exact canonicalized full-frame identity checked
names/order, values/types and multiplicities, not physical Margin order.

The final [CSV](names.csv) and [log](names.log) recorded 1,248 classifications:

| Backend | Exact matches | Ordinary-comparator exclusions |
| --- | ---: | ---: |
| local | 243 | 69 |
| dtplyr | 288 | 24 |
| SQLite | 180 | 132 |
| DuckDB | 192 | 120 |

There were 903 exact matches, 345 ordinary-comparator exclusions and zero
remaining mismatches. Exclusion meant the ordinary constructor, selection,
mutation/union/relocation or collection control could not supply the same
comparison; it did not mean every excluded public call was unsupported by
marginplyr. Some ordinary operations repaired names that the package preserved.

Preliminary runs used an `as_tibble()` comparison that itself rejected canonical
dot names, and initially omitted ordinary selection or relocation from the
controls. They were excluded from the final counts. The separating
[dot-payload.R](dot-payload.R) and [log](dot-payload.log) confirmed that source
payloads `...`/`..1` were refused by the ordinary `select(everything())` the
metadata path called. This External condition supplied no new finding.
The noncanonical `..01` and ordinary-name controls succeeded.

The final names execution was session `20397`, terminal exit zero. Earlier
serial handles `93353`, `4785`, `95847`, `99522`, `72853` and `64122` were
collected before later runs; none supplied the final counts.

## Spatial columns

[sf.R](sf.R) crossed zero/one/three rows, two active geometry choices,
geometry in payload/fixed/dimension roles, ordinary/noncanonical dot names,
all Margin orders and explicit/persistent fixed keys. The final
[CSV](sf.csv) and [log](sf.log) recorded 216 classifications:

- 72 exact column-value/type and promised public-order matches;
- 108 exclusions where ordinary sf composition repaired a public name; and
- 36 specifically checked typed-missing label refusals where the geometry
  prototype was not missing under vctrs row semantics.

Outer sf class and arbitrary frame attributes were not compared as a package
promise under ADR 0016. Its multiple-geometry branch conversion was not a
class-restoration finding. Preliminary comparisons incorrectly treated outer
frame metadata as promised and evaluated a lexical dimension name inside a
data mask. Those harness errors were corrected before the final classifications.
A short intermediate edit failed to parse and contributed no observations.
The final execution, session `62592`, exited zero; earlier sessions were
collected and excluded. No spatial implementation ticket followed.

## Replay

Export the baseline into a disposable directory, then set its path through
`MARGINPLYR_HUNT_SOURCE`. For matrix outputs use a separate existing directory
through `MARGINPLYR_HUNT_OUTPUT` so a replay does not replace retained evidence.
The scripts' startup paths were parameterized after measurement; their original
default source/output paths and probe expressions were preserved.

```sh
MARGINPLYR_HUNT_SOURCE=/absolute/baseline-source Rscript independent-label.R --assert
MARGINPLYR_HUNT_SOURCE=/absolute/baseline-source Rscript independent-grouping.R --assert
MARGINPLYR_HUNT_SOURCE=/absolute/baseline-source MARGINPLYR_HUNT_OUTPUT=/private/tmp/replay Rscript names.R
MARGINPLYR_HUNT_SOURCE=/absolute/baseline-source MARGINPLYR_HUNT_OUTPUT=/private/tmp/replay Rscript sf.R
MARGINPLYR_HUNT_SOURCE=/absolute/baseline-source Rscript dot-payload.R
```

The first two intentionally exit one on the baseline after the symptom/control
checks; omitting `--assert` retains the observed-symptom checks and exits zero.
The other scripts finish at zero with their classifications. All original
processes were collected; none owned asynchronous child work. No full suite,
build or rendering ran. Other engines/platforms/versions, arbitrary name or
spatial classes, warning identity and unbounded schemas remained unmeasured.
