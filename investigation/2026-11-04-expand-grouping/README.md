# Expansion and Grouping specification boundary hunt

Investigated: 2026-11-04
Baseline: `ca50a281e0707897ff5768b4c20f533cf7194c4c` (`main`)

Four investigators split the public surface across selection capture, Grouping
family algebra, eager and dtplyr expansion, and live SQL and Arrow expansion.
The dedicated worktree branched from main. A fresh fetch and a separate remote
ref check confirmed the baseline before publication. The
[November 3 expansion hunt](../expand-grouping-greedy-hunt-2026-11-03.md),
[November 2 expansion hunt](../expand-grouping-greedy-hunt-2026-11-02.md),
[November 4 nesting hunt](../2026-11-04-nest-spec-greedy/README.md), and
[November 4 summary hunt](../summary-grouping-share-bughunt-2026-11-04.md)
supplied earlier witnesses, fixes and exclusions. Their counts were separate
from this investigation.

## Findings and ticket disposition

| Finding | Repair issue | Observed effect |
| --- | --- | --- |
| G1 | [#1227](https://github.com/sayuks/marginplyr/issues/1227), existing | Supported dot-like columns were treated as numbered dots outside a dots frame |
| G2 | [#1240](https://github.com/sayuks/marginplyr/issues/1240) | A masked parenthesis operator bypassed a required ambiguity refusal |
| D1 | [#1241](https://github.com/sayuks/marginplyr/issues/1241) | A cache named copy prevented collection or caused an unrelated caller callback |
| F1 | [#1242](https://github.com/sayuks/marginplyr/issues/1242) | A factor subclass's displayed missingness erased a stored NA-level code |
| S1 | [#1243](https://github.com/sayuks/marginplyr/issues/1243) | Re-evaluation changed SQL payloads and row membership across copies |
| G3 | [#1244](https://github.com/sayuks/marginplyr/issues/1244) | Reference wording contradicted intentional acceptance of identity aliases |
| G4 | [#1245](https://github.com/sayuks/marginplyr/issues/1245) | Duplicate-drop plans materialized exponentially many occurrences for two sets |
| G5 | [#1228](https://github.com/sayuks/marginplyr/issues/1228), existing | Resolved duplicate sets reached a typed metadata read or refusal first |

Each open ticket describes an independently verifiable behavior and has no
blocking edge. The user delegated the breakdown, so `to-tickets` proceeded
directly to publication. Six new issues retained `ready-for-agent`. G1 and
G5 were independently reported during the concurrent investigations.
Post-publication comparison found #1227 and #1228, so the two issues opened
here for those findings, #1239 and #1246, were closed as duplicates. #1234
also reproduced G1 and separately covered tagged missing dots. Existing
issues were left as the other investigations published them.

### G1 and G2. Nested specification lookup

In [the numbered-column differential](numbered-column.R), ordinary tidyselect,
a Margin `.by` selection and explicit `all_of()` agreed on six positive
dot-like names such as `..01` and two zero/negative controls. An injected
selection quosure or one or three parenthesis pairs triggered an
incorrect-context dots error in 90 of 120 constructor comparisons. The
remaining 30 matched. The `..01` witness also failed in row expansion. A
wrapper forwarding a supplied Grouping set through a parenthesized numbered
dot selected the forwarded column. With the exact predecessor
`f5a8d7d1b18c981f044102fd56d92606679c7c52` loaded, the injected and
parenthesized `..01` witnesses both selected the input column. Bare direct
capture of a positive dot-like spelling could fail upstream in rlang on both
revisions. The regression was isolated to already captured references
entering the new forwarding reader from #1214/#1223.

In [the parenthesis collision probe](parenthesized-collision.R), the input
had column `s`, and lexical `s` held a Grouping set for `key`. With a local
parenthesis operator returning the character name `s`, the bare spelling
refused the collision. All 16 parenthesized calls across four admitting
constructors and two consumers accepted the column and invoked the masked
operator once per call. A noncolliding reference and both explicit
disambiguations worked. ADRs 0019 and 0026 specify transparency in this
reading. The ambiguity guard used an unwrapped expression to recognize the
name, then evaluated the still-wrapped quosure to read its binding. The
masking witness reached a boundary absent from #259/#308's ordinary-operator
reproductions.

### D1 and F1. Deferred copy and factor reconstruction

[The cache witness](dtplyr-cache-copy.R) used one immutable row, a filtered
source, an explicit intermediate name and a single default Grouping set.
Native collection and native union preserved the row. With cache name
`copy`, Margin collection raised `could not find function "copy"`, or ran a
caller's unrelated `copy` once when one was available. The construction
returned a lazy step in both cases. The ordinary cache name worked. The
[cache matrix](dtplyr-cache-matrix.R) recorded 216 collection errors and
216 matches across cache graphs, sizes, families, label forms and sorting.
Native source controls passed and the caller tables were unchanged. The
generated unqualified copy in the isolation path was the differentiator
under ADR 0029. #1003/#1009 addressed reuse after a destructive rename.
#1024/#1030 addressed metadata cache sharing.

[The factor witness](factor-na-code.R) used a factor subclass with levels
`a` and `NA`, and source codes `1, 2, NA`. The subclass's `is.na()`
reported displayed missingness. Ordinary grouping distinguished all three
rows, and union and collection preserved the codes. An included-only set
under the default label returned `1, NA, NA` locally and on dtplyr. A NULL
label preserved the eager codes and still lost the lazy code. The
[factor matrix](factor-na-matrix.R) established 96 losses and 480 matches
across the four Margin verbs, two input backends, three fixture kinds, three
row counts, label forms/positions and two families. Plain factors with a
declared NA level and subclasses without that level matched. The output
retained the level domain even where it lost a source code. The repaired
character method in #1215/#1221 preserved the source level string. The
encoder still asked the subclass's missingness method when assigning its
missing-code sentinel. ADR 0012 distinguishes the stored NA-level value
from a truly missing code.

### S1. A single evaluated source across SQL branches

[The SQL witness](sql-volatile-source.R) used two deterministic fresh DuckDB
sequences. A source query returning one row with a sequence payload yielded
tokens 1 and 2 in detail and total. A source predicate admitting only the
first sequence evaluation yielded the detail row alone. Explicit
single-statement `MATERIALIZED` CTE controls returned tokens 1 and 1 and
both row copies, respectively. Each probe sequence advanced twice and each
control once. At construction, DuckDB performed a `LIMIT 0` metadata
query and the payload probe sequence was unadvanced. SQLite corroborated
the payload difference with `hex(randomblob(16))`. The deterministic
DuckDB probes supplied the primary evidence. Ordinary independent union
operands could legitimately re-evaluate a volatile relation. The public
expansion contract describes copies of the input rows, and the shared-source
controls implement that comparison within one statement. ADR 0020 governs
construction-time reads. PR #749's read-count fixture returned stable values
and established construction behavior.

### G3, G4 and G5. Grouping family contracts

[The identity-alias probe](grouping-identity-alias.R) showed that
`grouping_set(c(a = a))` and an identity alias supplied through `all_of()`
selected column `a`, matching the unaliased form. A true rename and a
named constructor argument were refused. Existing regression assertions
and the selection resolver deliberately accepted the identity alias. The
reference's *How an argument is read* wording described any name inside a
selection as refused. #103 and #134 addressed actual renaming.

[The duplicate-drop probe](grouping-drop-growth.R) used one input row and
one dimension. A cube of repeated units and a Cartesian product of repeated
two-set families both had exactly two retained sets, the key and the empty
set. At a 64 MiB vector heap cap, the 12-unit/factor controls returned both
sets. A 20-unit cube and a 21-factor product raised vector memory limit
errors. The cube also failed under a 96 MiB cap in a separate probe. The
small controls covered drop, keep, error and actual row copies. The
expansion path enumerated occurrences before applying drop. #980 dealt with
SQL composition after a wide family was built. The measured limit concerns
compact duplicate-drop output and does not estimate maximum feasible
general cube width.

[The mixed duplicate probe](grouping-mixed-duplicate.R) compared an
all-name plan, a union with two identical leading sets followed by
`where(is.character)`, and a mixed unique control. Each of the two mixed
duplicate consumers issued one DuckDB `LIMIT 0` before reporting the
duplicate, measured at DBI's send boundary. The derived dtplyr input
reported a metadata refusal in place of the duplicate. Both all-name
consumers refused the same leading duplicates without a send. The
mixed unique control succeeded on local and DuckDB inputs and reached the
expected dtplyr metadata boundary. ADR 0005 requires known plan errors
before metadata. #1090 covered leading selection errors and #1139 fixed-key
overlap. This witness concerned duplicate identity between two complete
sets.

## Additional bounded observations

| Evidence | Outcome on the investigated revision |
| --- | --- |
| [Local row copies](local-row-copies.R) | 720 exact comparisons across eight column types, empty/single/shuffled inputs, fixed keys, five families, and three sort choices |
| [Grouping algebra](grouping-algebra.R) | 2,838 comparisons, including 1,974 exact set/order/bit/ID matches and 864 required duplicate refusals. Nine plan widths and a serialized nested closure also matched. |
| [Arrow row copies](arrow-row-copies.R) | 1,980 comparisons across eleven column families and ten source shapes. 1,587 exact matches and 198 required grouped-input plus explicit-`.by` refusals. |
| [Arrow empty controls](arrow-empty-controls.R) | The remaining 195 Arrow comparisons differed only in empty R representations. Ordinary Arrow reproduced int64 downcasts and ordered or unordered dictionary level losses after arranging. Physical schema types remained intact. |

The local and Arrow oracles used explicit row copies, typed initialization
and source identifiers. The algebra oracle generated independent set
families before comparing the public plan. These are finite observations,
including separate controls and intentionally refused cases. They cover
neither all extension classes and backends nor general resource bounds.
The database executions used private in-memory DuckDB and SQLite connections.
No live PostgreSQL, hosted database or Polars execution supplied evidence.

All-state GitHub Issue and PR searches used the following terms along with
the cited neighboring reports. A fresh list of open issues after publication
caught the concurrent overlaps that keyword search had missed.

| Boundary | Search terms |
| --- | --- |
| Numbered names and parenthesis collision | numbered-dot + column, noncanonical + column, parentheses + ambiguous, shadowed + parentheses |
| dtplyr cache | dtplyr copy cache, dtplyr copy, copy cache name |
| Factor reconstruction | is.na factor subclass, NA level factor |
| SQL repeated evaluation | expand_with_margins + volatile or random, expansion + volatile or sequence, nextval, branch + volatile |
| Aliases and compact plans | identity + rename, same name + rename, cube + memory, Cartesian product + memory, duplicate grouping + exponential |
| Duplicate preflight | duplicate grouping + metadata, duplicate grouping sets + before |

## Replay

Run the commands from the evidence checkout root, or invoke the scripts by
absolute path with the archived baseline as the working directory. The replay
used R 4.6.1 on
macOS arm64. Core versions were dplyr 1.2.1, rlang 1.3.0, tidyselect 1.2.1,
vctrs 0.7.3, dbplyr 2.6.0, dtplyr 1.3.3, data.table 1.18.6.1,
DuckDB 1.5.5, RSQLite 3.53.3 and Arrow 25.0.1. Run each script in a fresh
process to isolate its registrations, DBI instrumentation and heap cap.

```sh
evidence=investigation/2026-11-04-expand-grouping
Rscript "$evidence/numbered-column.R"
Rscript "$evidence/parenthesized-collision.R"
Rscript "$evidence/dtplyr-cache-copy.R"
Rscript "$evidence/factor-na-code.R"
Rscript "$evidence/sql-volatile-source.R"
Rscript "$evidence/grouping-mixed-duplicate.R"
Rscript "$evidence/grouping-drop-growth.R" --memory
Rscript "$evidence/grouping-identity-alias.R"
```

The defect scripts support `--assert` for desired behavior and intentionally
fail against the investigated source. The identity-alias script records
the documented behavior mismatch in observation mode. Review-ready
verification belongs to the committed evidence SHA and is recorded on the PR.
