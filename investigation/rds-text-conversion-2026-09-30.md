# RDS evidence converted to text

Investigated: 2026-09-30
Source snapshot: `056bae562385e627d6eb4ca2410183d594c0960f`
Environment: R 4.6.1 on macOS arm64

## Inventory and result

All six tracked RDS files were inspected. None required RDS for the objects
they contained. The conversion preserved the recorded observations rather
than repeating the PostgreSQL or SIGINT experiments.

| Original | Contents | Text replacement |
| --- | --- | --- |
| PostgreSQL `near/values.rds` | Seven typed result objects | `near/values.dput` |
| PostgreSQL `current-r45/values.rds` | Seven typed result objects | `current-r45/values.dput` |
| PostgreSQL `current-r46/values.rds` | Seven typed result objects | `current-r46/values.dput` |
| Signaling `evidence.rds` | Seven UTF-8 files, nested environment RDS, date and baseline | Original text files, `environment.dput`, `archive.dput` |
| SQLite `acceptance.rds` | 1,184 UTF-8 files and integer format field | `acceptance/`, including `archive.dput` |
| SQLite `harness-development.rds` | 1,792 UTF-8 files and integer format field | `harness-development/`, including `archive.dput` |

The [source hash record](rds-text-conversion-2026-09-30/source-rds-sha256.json)
identifies every original RDS. The replacements are under
[PostgreSQL evidence](2026-09-30-postgres-near-floor/),
[signaling evidence](condition-resignaling-2026-09-30/README.md), and
[SQLite evidence](sqlite-interruption-recovery-2026-09-30/README.md).

## Typed values

The PostgreSQL objects included tibbles, list columns, named integer grouping
bits, integer source columns, missing strings, double shares, and
`bit64::integer64` aggregates. CSV alone would not preserve that contract.
`dput()` with `keepNA`, `keepInteger`, `niceNames`, `showAttributes` and
`digits17`, followed by `dget()`, preserved every object under
`identical(original, restored, num.eq = FALSE)`.

Default `dput()` precision changed some double shares. `hexNumeric` parsing
underflowed the small double storage values carrying the integer64 bits on
this R build. The decimal `digits17` representation passed the strict
round-trip checks for all three snapshots. Its tiny aggregate storage numbers
are intentional; the existing CSVs display the human-readable integer totals.
This measurement does not establish a universal text encoder for arbitrary R
objects or integer64 bit patterns.

The nested signaling environment contained `R.version`, rlang's package
description, `sessionInfo()` and selected `Sys.info()` fields. They were lists
and character vectors with class/name attributes, with no live connection,
environment or external pointer. Their `.dput` round trip passed the same
strict comparison. The archive metadata also passed.

## Original file bytes

The remaining 2,983 archived entries were UTF-8 text without NUL bytes. Each
nonempty file was compared as raw bytes against its archived entry. The 291
zero-byte handshake paths were preserved in `empty-files.txt` lists, checked
against the archives. All
matched, including 961,666 acceptance bytes and 1,455,232 development bytes.
SHA-256 manifests accompany the extracted directories. Original script hashes,
source snapshots, condition records, logs and failed development runs remained
unchanged. Historical whitespace was retained: 23 lines across three development
logs had trailing whitespace in the original archives. Formatting whitespace
in newly generated `.dput` syntax was trimmed before its round-trip check.

## Reproduction of the conversion

The [converter](rds-text-conversion-2026-09-30/convert.R) accepts the original
checkout and a new output directory. It asserts strict object round trips and
raw-byte equality before returning; it never runs the archived experiments.
From the repository root:

```sh
scratch=$(mktemp -d /private/tmp/rds-text-conversion-XXXXXX)
mkdir "$scratch/source"
git archive 056bae562385e627d6eb4ca2410183d594c0960f | tar -x -C "$scratch/source"
Rscript --vanilla investigation/rds-text-conversion-2026-09-30/convert.R \
  "$scratch/source" "$scratch/text"
```

The PostgreSQL probe writes `values.dput` and asserts its round trip; the
comparison script reads it with `dget()`. Their public-call assertions and
comparison columns were unchanged. The archived measurements were not rerun.
