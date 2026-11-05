# Local nesting structured-key and extension-vector controls

Investigated: 2026-11-05
Tested package SHA: `0403c9150394cbae21b59f34c75791d0873b8451`

This independent local track investigated both nesting Margin verbs without
changing package implementation or permanent tests. The November 5 nesting
and Grouping specification boundary hunt supplied the previously investigated
frontier. The November 4 local factor and packed-key records supplied controls.
`CONTEXT.md`, the nesting references and ADRs 0012, 0016 and 0018 supplied
the source-membership, type-restoration and result-property boundaries.

## Confirmed controls

`local-structured.R` made 816 calls across seventeen key families, empty and
populated inputs, absent/present fixed keys, both keep choices, both verbs and
all three sort choices. It independently enumerated the included set and the
Grand total set, grouped original source keys with `vctrs::vec_group_loc()`,
and compared every returned cell's source-row identifiers against that oracle.
It also compared source serialization before and after every call.

All 768 admitted calls matched. These covered complex values, distinct NA and
NaN, infinities, signed zero, nested lists, list elements containing frames,
matrices, language objects and named vectors, zero-column packed keys,
two-level packed keys, one- and three-dimensional arrays, matrices, factor NA
levels and separate missing factor codes. The other 48 calls used raw keys
and received the documented typed-missing representability refusal; raw has
no distinct missing value. No source changed.

`local-extension.R` made another 504 calls across haven labelled vectors,
SPSS-labelled vectors with user-defined missing values, distinct tagged NAs,
blob vectors, a two-field record vector, typed list-of vectors and units
vectors. It crossed empty/populated data, dimension/fixed-key/payload roles,
both keep choices, both verbs and all three sort choices. All 504 calls
matched source-cell memberships, and every corresponding cell-column value
matched a vctrs slice of the original source. No source changed. Equality
used vctrs vector semantics, leaving arbitrary attribute restoration outside
ADR 0016's promise boundary.

The corrected final scripts asserted these outcomes and completed with exit
0. The first structured-key harness compared NULL with character(0) for an
empty expected result; that harness-only mismatch was corrected before the
final recorded run. No package failure depended on that comparison.

These controls exercised sorting for interference with membership. They did
not independently assert the complete Margin order or every outer display
value. The extension track did assert original values carried in the cells.

## sf display-conversion boundary: not promoted to a finding

`sf-disposition.R` used an sf frame with an active point geometry. Eight
included-only calls selecting that active geometry as a dimension, crossing
empty/populated input, both verbs and both keep choices, failed with the
active-geometry metadata error. Eight fixed-geometry controls and eight
geometry-payload controls succeeded. A plain tibble carrying the same geometry
vector supported a display-labelled rollup with four expected cells. The
source serialization remained unchanged.

The corresponding ordinary `dplyr::mutate()` replacing the active geometry
with its character rendering also left an sf object whose `st_geometry()`
raised that same error. That failed native-composition control did not
establish a package-created invariant defect independently of sf's behavior.
ADR 0016 delegates the input container's arbitrary class and attributes.
Accordingly this observation was not published as a new bug; a stronger
contract or a successful equivalent native composition would be needed.

All-state GitHub Issue and PR searches used `sf geometry`. The complete Issue
bodies read for #949, #951, #993, #1000 and #1276 concerned earlier temporary
renaming, key restoration, column ordering and multiple geometry boundaries.
None was reopened or modified. Search result snapshots accompany this note.

## Replay and limits

Run one command at a time from the tested checkout root. An optional first
argument selects another source checkout, and an optional second argument
selects an existing output directory; without arguments, source is the
working directory and CSV output goes to R's temporary directory.

```sh
evidence=investigation/2026-11-05-nest-grouping-adversarial-followup/local
Rscript "$evidence/local-structured.R"
Rscript "$evidence/local-extension.R"
Rscript "$evidence/sf-disposition.R"
```

The final structured-key/extension invocation was execution session 55907,
terminal exit 0. The sf disposition invocation was session 57729, terminal
exit 0. Both were direct R processes without owned child work. No full suite,
render, build or Review-ready run was started in this track.

The probes used R 4.6.1 on aarch64 macOS, dplyr 1.2.1, tidyr 1.3.2, vctrs
0.7.3, tibble 3.3.1, haven 2.5.5, blob 1.3.0, units 1.0.1 and sf 1.1.2.
These finite controls established neither all S3/S4 vector methods, arbitrary
subclasses, hosted backend behavior nor other dependency or OS versions.
