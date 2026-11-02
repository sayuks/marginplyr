# SQL expansion physical types, names and binary graphs

Investigated: 2026-11-02
Baseline: `978d7bef4247bf1348af5b90bfab47924db25500` (`origin/main`)

This track added a direct SQL typed-NULL oracle, public name boundaries, and an
independent witness for [#1136](https://github.com/sayuks/marginplyr/issues/1136).
Prior October 29–31 source-composition measurements were not counted again.
No implementation or package regression test was edited. SQL fixtures used
private in-memory SQLite/DuckDB databases.

## Physical-type and value differential

Hypothesis: omitted DuckDB dimensions or carried physical types might be
coerced, rejected, or lose values during branch combination.
`physical-types.R` compared direct collection with independently written SQL
branches, using a typed NULL for omitted values and ordinary VARCHAR conversion
for nonmissing labels. It aligned by occurrence and compared R values and
column attributes; ENUM expectations included restored factor levels.

Twenty requested families included ENUM, UUID, BIT, signed/unsigned/huge
integers, DECIMAL, TIME/TIMETZ, nanosecond/time-zone timestamps, INTERVAL,
BLOB, JSON, lists, arrays, STRUCT, MAP, UNION and an all-NULL VARCHAR control.
Dimension/fixed/payload roles crossed NULL/text labels and three sorts.
The final 292 classified observations contained 285 matches, three documented
STRUCT text-label refusals, and four unadmitted source populations. BIT and
UNION failed ordinary driver field acquisition; fixed arrays required a
nondefault driver option; JSON needed an unavailable extension. A shared
nanosecond-resolution warning was retained. No new defect was established.

The initial harness stopped at BIT field acquisition; the corrected run isolated
source failures. The text-label oracle initially omitted the included branch's
VARCHAR conversion and ENUM expectations omitted factor restoration. Both
were corrected before a complete final rerun. This compared collected
representations, not every physical SQL type or warning identity.

## Forty public names

Hypothesis: generated aliases, reserved/pronoun-like words or quoting might
corrupt the public schema at direct, head, identity-filter or compute boundaries.
`name-boundaries.R` constructed row copies independently, placed public columns
in fixed/dimension/identifier/payload order, and compared scalar row bags.
Forty spellings crossed four roles, SQLite/DuckDB, NULL/text labels and
none/last sorts. They included NA/NaN/Inf/NULL words, `.data`/`.env`, dot-dot
forms, private-prefix collisions, Boolean words, rowid aliases, control
whitespace, quotes/backslashes, Unicode and SQL tokens.

The final 4,928 observations contained 4,848 matches and 80 errors. Sixty-four
were required construction refusals for prohibited `...`/`..1` names. Sixteen
DuckDB compute failures for `.data` were independently reproduced on an ordinary
source by `excluded-name-controls.R`. No additional name-specific package
finding emerged. Head compared with direct collection's head because none-sort
had no order promise; this was not a complete independent Margin-order proof.

The initial comparator tried ordinary `arrange(across())` on `..0`, which
itself failed, and initially placed expected identifiers after payload. Both
harness errors were corrected and the complete matrix rerun. Only the final
CSV/log were retained. CSV readers must preserve embedded CR/LF names when
counting the forty spellings.

## Independent binary-graph witness

Primary minimized and cause-isolating evidence was in the adjacent
[local track](../local/README.md#sqlite-boolean-aliases-across-binary-relation-graphs).
`independent-alias-graph.R` used FALSE payload 19 and g equal to b. Direct
collection returned both copies. Original-source and explicitly materialized
controls succeeded for both left-join positions, both union-all positions and
right-input intersection. The lazy expansion failed for both joins,
right-input union-all and intersection with `no such column: FALSE`;
left-input union-all succeeded.

The left join retained the special result class, while right-input union-all
did not. The primary explicit-repair probes separated untraversed join/set
graphs from missing final-render dispatch. The independent assertion required
all compositions to execute after asserting controls and direct values; it
intentionally exited 1 on the baseline. No repair was attempted.

## Replay and limits

Run from the repository root:

```sh
Rscript investigation/2026-11-02-expand-grouping-greedy/sql/physical-types.R
Rscript investigation/2026-11-02-expand-grouping-greedy/sql/name-boundaries.R
Rscript investigation/2026-11-02-expand-grouping-greedy/sql/excluded-name-controls.R
Rscript investigation/2026-11-02-expand-grouping-greedy/sql/independent-alias-graph.R --assert
```

The first three commands exited zero; the last intentionally exited one.
Exploratory runner success meant classifications completed. The independent
witness's first draft had an invalid unquoted FALSE argument name; it was
corrected before reaching any package call and was not counted.

R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0, RSQLite 3.53.3, DuckDB 1.5.5,
vctrs 0.7.3 and rlang 1.3.0 ran on macOS arm64. Complete DuckDB runs supplied
`shared_home = FALSE` to the driver; an initial attempt supplied it at the
connection boundary and emitted the shared-home message. No credentials,
external database, live PostgreSQL/SQL Server, other OS, all-version matrix,
or complete physical-type enumeration was exercised.
