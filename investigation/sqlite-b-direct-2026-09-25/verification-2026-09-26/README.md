# Archival reproducibility verification

Investigated: 2026-09-26
Base: `6a5f611d41bd462f3c933edea1dd0b9861d6e892`

These checks were performed while preserving the 2026-09-25 investigation. They
supplement its evidence and do not overwrite the original results.

- The archived review matched all 154 original files byte for byte.
- Every final R source matched its original variant directory (25 files for
  each of A, B, B-direct, C, and A661-stop).
- Every non-R baseline file matched the recorded base commit (240 files per
  variant); the saved R files plus that commit reproduced the package source.
- The original SHA256 lists and source-hash JSON matched the saved final source.
- Applying the original B-direct patch to the recorded base commit reproduced
  all 25 final B-direct R source files byte for byte.
- The portable helper reconstructed the packages in a new scratch directory.
  The shared B-direct matrix passed 98 of 98 cases, with zero failing cases.

The exact archival checks are recorded in [archive-integrity.txt](archive-integrity.txt).
The new matrix [log](cross-check.log), [CSV](cross-check.csv),
[environment](session-info.txt), and [path adaptations](replay-adaptations.json)
are independent of the original review's files.

The reconstruction and run used:

```sh
python3 investigation/sqlite-b-direct-2026-09-25/prepare-replay.py \
  --scratch /private/tmp/marginplyr-b-direct-replay-20260926 --run B-direct
```

The scratch pathname in that command identifies the historical run, not a
required path for reproduction. A different new scratch directory works through
the helper's `--scratch` argument. This run did not repeat the existing 990
assertions or the full package checks; those original results remain dated
2026-09-25.
