# PostgreSQL summary and share boundaries

Investigated: 2026-11-03
Snapshot: `4b34fdecb2d8a41034b3d9b5d5af8e51592555e1`

## Confirmed: private denominator names collided after server truncation

Published as [#1175](https://github.com/sayuks/marginplyr/issues/1175).

The fixture started a disposable PostgreSQL 17.11 cluster, connected through a
private Unix socket, and stopped it and deleted its files on exit. RPostgres
was 1.4.10; the shared environment inventory recorded the remaining versions.
No caller database or external service was used. The execution environment
needed shared-memory permission beyond the filesystem sandbox; the initial
sandboxed `initdb` attempt failed before any server started.

Two rows, g = a/b and x = 1/3, were sufficient. Two ordinary summaries held
sum(x) and twice sum(x), named with 35 repetitions of s followed by a/b. Both
public identifiers were 36 bytes, below the server's measured
`max_identifier_length` of 63. Ordinary PostgreSQL summarisation returned the
two original names and values four/eight. A Margin rollup requesting both
Total shares constructed successfully but collection failed with:

```text
ERROR: column reference "..marginplyr_denominator_of_sssssssssssssssssssssssssssssssssss" is ambiguous
```

The expected fractions for each source were one-quarter, three-quarters and
one. Keeping the names but requesting one share succeeded; shortening the names
and requesting both succeeded. Twelve repetitions of 計 followed by a/b
reproduced the same failure with 37-byte public names, distinguishing a byte
boundary from a character-count rule.

After discovery, the ranked hypotheses were (1) server truncation of generated
denominator aliases, (2) a general multiple-share window defect, and (3) an
unsupported public identifier. The source-length threshold, measured server
setting, ordinary-summary control, short-name two-share control and one-share
control separated the first from the others. Server notices reported both
generated aliases truncating to the same identifier.

The owning source sites were `share_denominator_names()` and
`apply_windowed_shares()` in [the tested share module](../../../R/share.R),
and `new_margin_internal_names()` / `margin_identifier_key()` in
[the tested name allocator](../../../R/internal-names.R). The denominator
prefix alone was 28 bytes. At the failing common prefix, the suffix that made
the requested aliases distinct lay beyond the server boundary. The allocator
compared the untruncated strings. ADRs [0010](../../../design/adr/0010-compute-parent-shares-as-a-contextual-summary.md)
and [0017](../../../design/adr/0017-calculate-total-shares-against-the-grand-total-set.md)
and the share reference owned the supported-source and displayed-denominator
contracts; none excluded these valid public identifiers. No repair was made.

## Matrices and rejected hypotheses

| Loop | Completed scope | Observation |
| --- | --- | --- |
| Initial name threshold | Seven lengths × one/two shares: 14 cases | Five two-source failures beginning at 36-byte public names; all ordinary controls passed |
| Minimized name matrix | ASCII/UTF-8 × long/short × Parent/Total × one/two shares × native/portable: 32 cases | Eight long-name two-share failures; 24 controls passed |
| PostgreSQL grouping keys | 14 types × variable/fixed × native/portable × sorted/unsorted: 112 cases | Both displayed-row share-value oracles passed in every case |
| Finite NUMERIC extremes | Seven decimal exponents × Parent/Total: 14 cases | Fractions agreed with a database oracle dividing NUMERIC before conversion |

The key matrix included text, padded text, numeric, very large/tiny numeric,
integer64 values above double's consecutive-integer range, date, zoned timestamp,
interval, array, jsonb, bytea, range and Boolean keys, including missing keys.
Its checks compared share values under both sort requests; they did not assert
the resulting row order. Each distinct fixed partition held one source fact;
these were bounded partition-identity probes rather than all key-equivalence
relations.

The numeric hypothesis was that conversion before division might overflow or
underflow for finite NUMERIC sources with exponents -400/-310/-300/0/300/310/400.
All 14 cases returned .25/.75/one. The observed PostgreSQL translation preserved
the ratios; this was rejected as a finding. It established no guarantee for
other numeric expressions, nonfinite values or another dialect.

The PostgreSQL backend advertised native grouping sets but not native duplicate
retention. The `error`/`keep` duplicate policies selected native/portable paths
for these calls, as decided by `supports_grouping_sets()` in the tested backend
module. There were no duplicate occurrences in these particular fixtures.

## Replay and duplicate checks

Run from the checkout root:

```sh
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/backend/postgres-identifier-reproduction.R
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/backend/postgres-identifier-reproduction.R --assert
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/backend/postgres-key-matrix.R
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/backend/postgres-numeric-range.R
```

The name reproducer's report mode asserted the investigated failure boundary
and successful controls and exited zero. Its `--assert` mode required all
supported calls to succeed, exited one, and still cleaned up its server. The
retained terminal handles were 11470 (report, exit zero), 53903 (assert, exit
one), 86505 (final key matrix, exit zero), and 43884 (numeric matrix, exit zero).
They were research execution handles, not Review-ready evidence. Adjacent logs
and CSVs retained the final outputs. The initial threshold script was retained
as discovery evidence. No package tests or implementation files were changed.

The coordinator searched open/closed issues with `PostgreSQL long identifier`,
`denominator long`, `truncated`, `truncation`, `long names`, `identifier length`,
`numeric double overflow`, and `underflow`; PR searches included
`PostgreSQL identifier` and `long source`. Closed issues
[#430](https://github.com/sayuks/marginplyr/issues/430),
[#439](https://github.com/sayuks/marginplyr/issues/439), and
[#411](https://github.com/sayuks/marginplyr/issues/411) were read with their full
bodies and comments. They concerned Assigned summary naming or condition
labels, rather than PostgreSQL private alias truncation. No matching report was
found. A fresh fetch before publication still resolved main to the snapshot
above.
