# SQLite interruption evidence

`acceptance/` holds the 1,184 text-file entries from `acceptance.rds`.
`harness-development/` holds the 1,792 entries from the earlier incomplete runs;
those runs are not acceptance evidence. Each directory preserves the archived
relative paths and bytes. Nonempty files are stored directly; `empty-files.txt`
lists the zero-byte checkpoint handshake files (114 acceptance, 177 development).
Its `archive.dput` retains the R archive's integer format field, readable with
`dget()`; `SHA256SUMS` covers the extracted files, empty-path list and metadata.

The top-level `archive-hashes.json` is the historical hash record of the
original archives and logs. The [conversion investigation](../rds-text-conversion-2026-09-30.md)
identifies the Git snapshot containing those archives and the preservation
checks. The existing top-level summaries, manifest and check logs are unchanged.

To check the extracted evidence:

```sh
(cd acceptance && shasum -a 256 -c SHA256SUMS)
(cd harness-development && shasum -a 256 -c SHA256SUMS)
```

The [investigation note](../sqlite-interruption-recovery-2026-09-30.md) holds
the original snapshots, outcomes, reproduction command and limits. Inspecting
the files does not execute the supervisor or deliver SIGINT.
