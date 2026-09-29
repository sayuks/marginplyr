---
status: accepted
---

# Require a loadable dbplyr dependency floor

## Decision

While marginplyr requires dbplyr 2.6.0, it requires dplyr 1.2.0 or later.
This is the first dplyr release that exports `filter_out()`, which dbplyr
2.6.0 imports when its namespace loads. The direct bound keeps source-package
installation and namespace loading possible for every graph marginplyr
advertises.

## Considered options

Waiting for an upstream dbplyr metadata correction would leave the released
2.6.0 graph admitted by marginplyr and still unusable. A later dbplyr release
cannot change the metadata of that version. Requiring a newer dbplyr release
would also discard 2.6.0, which works with dplyr 1.2.0. Changing marginplyr's
dplyr bound is the smaller correction. If dbplyr changes its minimum or import
in a later release, reassess both bounds together.

## Evidence

The versioned upstream metadata and exports, isolated-library reproduction,
and public-call comparison are recorded in
`investigation/2026-09-29-minimum-dependency-compatibility.md` and
`investigation/2026-09-29-dependency-floor-correction.md`.
