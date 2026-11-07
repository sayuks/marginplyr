# Expansion and Grouping specification typed boundary hunt

Investigated: 2026-11-07
Baseline: `db3a7e4f5215574a71a9af60698d0c945af0cb83` (freshly fetched main)
Scope: dated observations and independent repair tickets

Four investigation tracks covered local expansion, Grouping specification
capture and resolution, deferred expansion, and live SQL. The November 6
[boundary hunt](expand-grouping-bughunt-2026-11-06.md) and
[deep hunt](expand-grouping-deep-bughunt-2026-11-06.md), plus the November 7
[deferred-boundary hunt](expand-grouping-bughunt-2026-11-07.md) and
[receiver-dispatch hunt](expand-grouping-receiver-dispatch-bughunt-2026-11-07.md),
supplied prior coverage. The tested revision included the fixes from PR
[#1438](https://github.com/sayuks/marginplyr/pull/1438). Earlier measurements
were excluded from this hunt's counts.

## Findings and repair slices

| Finding | Ticket | Independently verifiable outcome |
| --- | --- | --- |
| G1 | [#1449](https://github.com/sayuks/marginplyr/issues/1449) | Resolve the names of a multi-column Grouping selection as ordinary tidyselect does. |
| L1 | [#1450](https://github.com/sayuks/marginplyr/issues/1450) | Compare the actual emitted detail display in observed Margin label checks. |
| L2 | [#1451](https://github.com/sayuks/marginplyr/issues/1451) | Check declared collisions against stored factor level strings. |
| D1 | [#1452](https://github.com/sayuks/marginplyr/issues/1452) | Bind generated dtplyr collision-check and omitted-label operations to their intended helpers. |
| D2 | [#1453](https://github.com/sayuks/marginplyr/issues/1453) | Keep a zero-row dtplyr expansion empty when adding its identifier. |
| S1 | [#1454](https://github.com/sayuks/marginplyr/issues/1454) | Count observed collisions on executable derived SQLite sources with Boolean-word columns. |
| S2 | [#1455](https://github.com/sayuks/marginplyr/issues/1455) | Retain text displays in direct unsorted SQLite expansion of Date dimensions. |

Each ticket was published with `bug` and `ready-for-agent`. The user
delegated the breakdown. Each slice could start immediately, with no blocking
edge or prerequisite prefactoring identified. L1 concerns the values seen by
observed checking. L2 concerns the separate declaration check even when
observed checking is disabled. D1 groups two label operations sharing deferred
namespace lookup. D2 can be reproduced on an input with no dimensions to
label.

### G1. A valid multi-column selection triggered a renaming refusal

For columns named `a1` and `a2`, `c(a = c(a1, a2))` returned positions
`1L, 2L` with names `a1, a2` under direct typed tidyselect. Ordinary
`dplyr::select()` returned those columns. The Grouping specification
preflight used a list as its names proxy. Tidyselect on that proxy returned
`a, a`, and inspection and expansion raised a false dimension-renaming
condition. Literal and opaque typed selections produced the expected plans
and rows. ADR 0013 and the public tidy-select argument contract supplied the
oracle.

The [local matrix](2026-11-07-expand-grouping-typed-boundaries/grouping-name-matrix.R)
observed 720 refusals across three ordinary carriers, four selector spellings,
name-only and mixed typed selections, five constructor positions, both
consumers, and three duplicate policies. Literal, typed and fixed-key controls
agreed and the source bytes were unchanged. The
[lazy replay](2026-11-07-expand-grouping-typed-boundaries/grouping-name-lazy.R)
added 16 refusals across immutable dtplyr, Arrow Table, SQLite and DuckDB,
where both fixed-key and dimension positions failed for both consumers.
Ordinary selection and opaque typed controls passed. Closed #103 and #134
covered true renames. #1434 and #1439 covered context-sensitive receiver
methods. The G1 fixture used stock data frames and no such method.

### L1. Observed checking compared a different display from the emitted one

A source column's `as.character()` method returned a character subclass.
Its comparison methods made validation miss identical plain strings or
reject distinct strings. A vctrs character subclass also caused a type
diagnostic during the check. The unchecked expansions emitted ordinary
character strings. The opening expansion contract and the observed
Margin-label collision guarantee concern the displayed detail and Margin
values, so a label identical to an emitted detail is the relevant witness.

The [collision matrix](2026-11-07-expand-grouping-typed-boundaries/local-collision-matrix.R)
recorded 220 exact results, 16 correct refusals, 8 missed collisions,
8 false collisions and 36 vctrs type diagnostics in 288 observations across
local and immutable dtplyr inputs. The
[presentation replay](2026-11-07-expand-grouping-typed-boundaries/local-display-source.R)
used a local frame subclass whose `as_tibble()` exposed a different
presentation value. Ordinary mutation and union retained the stored value.
The checked expansion compared the presentation value, giving one missed
collision and one false refusal. The plain and unchecked controls agreed
with ordinary execution. This was a synthetic subclass case. #1178 repaired
the shape of the logical result after comparison, and #1143 covered deferred
lookup of generated helpers.

### L2. A factor level accessor's result class changed declared equality

An ordered factor stored levels `south`, `north` and `unused`. Its
`levels()` method returned the same strings with a character subclass whose
`as.character()` method uppercased them. The declared check coerced that
vector while comparing. It missed the existing levels `south` and `unused`,
and reconstruction could duplicate a stored level. It also falsely refused
the distinct label `SOUTH`. The factor level and declared-collision
contracts include unused levels and apply for both observed-check settings.

The [collision matrix](2026-11-07-expand-grouping-typed-boundaries/local-collision-matrix.R)
recorded 240 exact results, 96 correct refusals, 96 misses with duplicate
stored levels, and 48 false refusals in 480 local/dtplyr observations.
Ordinary factor, unrelated label and typed-missing controls separated the
stored level strings from the accessor result's class. #1215 and PR #1221
covered conversion of source factor values. L2 affected the returned levels
vector with ordinary source factor-value conversion. The probes checked
source bytes. L1 result comparisons checked public names, values and
multiplicities after row alignment. L2 exact cases compared names, values,
factor codes and stored levels after aligning rows and canonicalizing the
factor subclass. L2 collision misses checked ordered-factor status and
duplicate stored levels.

### D1. Two generated label operations inherited caller namespace lookup

On an immutable dtplyr row with a character dimension equal to `Total`, a
selective namespace dispatcher on the lazy root replaced the generated
character conversion during the explicit collision check. It ran once and
validation allowed two indistinguishable displays. Removing the dispatcher
restored the documented Package condition.

In a second witness a dispatcher installed after construction supplied the
omitted row's text. The same expansion returned `a, MASK` while the
dispatcher was installed and `a, Total` after its removal. Ordinary source
collection, mutation and union were exact controls and the caller table
stayed byte-identical. The [deferred replay](2026-11-07-expand-grouping-typed-boundaries/dtplyr-deferred.R)
separated these paths from #1433's included-display, factor encoding and
factor reconstruction lookups and #1143/PR #1168's bare helper bindings.

### D2. A generated identifier invented a row on a zero-column source

A zero-column, zero-row immutable dtplyr source with one empty grouping set
and a requested identifier should have collected to an empty integer column.
Installing a selective root namespace dispatcher after construction instead
made the same query return one row with identifier `999L`. Native source
collection stayed empty, removal of the binding restored the expected result,
and caller bytes were unchanged. The
[deferred replay](2026-11-07-expand-grouping-typed-boundaries/dtplyr-deferred.R)
isolated the generated repetition helper. #1171 and PR #1173 protected the
earlier bare spelling of the helper. This fixture used the qualified
spelling with caller-resolved namespace lookup.

### S1. A derived SQLite Boolean-word column disappeared during checking

A collapsed SQLite source with one text column named `TRUE` collected
normally and expanded correctly with the default unchecked path. Enabling
the observed check failed at construction with `no such column: TRUE`.
The [replay](2026-11-07-expand-grouping-typed-boundaries/sqlite-boolean-check.R)
confirmed the failure for both `TRUE` and `FaLsE`, with a noncolliding
detail and with a genuine `Total` collision. The same check on the root or on a source aliased before
collapse gave the expected decision. The SQL record identified the
`observed_label_collision` statement and its nested star projection.
#1398 repaired the direct expansion of a derived source, and
#930 covered the label-check path for the `.env` pronoun.

### S2. A SQLite source declaration overrode the text result type

With RSQLite extended types enabled, a Date dimension storing 2020-01-01
had a `DATE` declaration. An ordinary SQLite cast-and-union reference
returned `"18262"` and `"Total"` for an included row and a Margin row.
Direct unsorted expansion returned two missing character values and emitted
an RSQLite warning about an unknown string format. Occurrence identifiers
and the integer payload survived. A zero-row source anchor preserved the
`DATE` declaration in the direct query even though its later arms emitted
text. ADR 0031's B-direct amendment covers direct collection of text-label
expansions and requires their character declarations. This path needed the
display semantics of the ordinary branch union.

The [replay](2026-11-07-expand-grouping-typed-boundaries/sqlite-date-display.R)
compared both observed-check settings. The requested Margin order and direct
materialization controls returned the expected displays. With extended types
disabled, direct unsorted collection matched the ordinary backend union.
A `NULL` label preserved the Date and a typed missing value on the
extended-types connection. Closed #527 addressed an all-missing result
losing its source type under a typed-missing label.

## Additional finite checks

| Probe | Result |
| --- | --- |
| [Grouping algebra](2026-11-07-expand-grouping-typed-boundaries/grouping-algebra.R) | 1,798 bounded seeded expressions yielded 3,596 exact keep/drop comparisons of included columns, occurrence order and identifiers. The error policy matched 1,310 unique plans, and every displayed group in 488 duplicate diagnostics referred to equal sets. Two generated plans exceeded the 128-occurrence bound. |
| [Malformed internal fields](2026-11-07-expand-grouping-typed-boundaries/grouping-malformed.R) | 80 manually modified specifications exposed a bare quosure diagnostic. Public captured integer selections and trailing commas succeeded. Leading missing arguments and a non-list argument field had the existing Package refusals. No public constructor path produced a non-quosure argument entry. ADR 0015 limits the condition promise for forged objects, so this remained an excluded observation. |
| [Live PostgreSQL 17.11](2026-11-07-expand-grouping-typed-boundaries/postgres-expansion-matrix.R) | 495 exact full-frame comparisons across 19 native column declarations, three source graphs, three plan shapes and three sort choices. Eighteen sorted point cases failed in both the package and ordinary PostgreSQL ordering. The sorted comparisons aligned only missing-value ties, while unsorted comparisons aligned by occurrence and payload. |
| Arrow names | Tables with `..1`, `..2` or `...` could be collected directly. Ordinary Arrow `select()` and Grouping selection refused those names with the same tibble name-policy diagnostic. A nonnumeric dot-dot name passed both. |

The PostgreSQL matrix used explicit SQL source projections and a separate
handwritten SQL branch union. A preliminary oracle used the wrong output
column order. A second used ordinary dbplyr projection of a column named
`.env`, which also failed without a Margin verb. The final raw SQL
projection handled the literal `.env` name, and the final run replaced both
preliminary counts. The matrix used exact frame equality after row alignment.
Its comparisons covered collection, and do not establish PostgreSQL
materialization of special names or general backend compatibility.

## Freshness, duplicate checking and replay

Two fetches of main before publication agreed on the tested SHA. The
duplicate search combined recent all-state issue listings, all-state
Issue/PR search, and relevant full report discussions. Search topics included
Grouping name repair, classed displays, declared classed levels, generated
namespace lookup for labels and zero-column inputs, SQLite Boolean-word
collision SQL, and extended-types Date labels. A publication-time refresh
included #1439 and #1440–#1447. PR #1448's Grouping report covered a dtplyr
context-sensitive receiver method. Its other reports covered summary and
share behavior.

The scripts in the [evidence bundle](2026-11-07-expand-grouping-typed-boundaries)
load the package from the working directory. Run them in fresh R sessions
from an archive of the tested source, passing the absolute script path.
The finding replays in default mode check the observed failures and their
controls. The supplemental matrices report their classifications. The local
matrix accepts `--assert-observed` and
`--assert-declared`, the dtplyr script accepts `--assert-label` and
`--assert-id`, and the local presentation and both SQLite scripts accept
`--assert` to assert the desired contracts. Those modes intentionally fail
on the measured SHA. The PostgreSQL script accepts the socket directory
of a disposable PostgreSQL instance on port 55378. It creates two domains,
an enum and a temporary table in that instance. Run it on a fresh instance
and stop the server after collection.

Environment was arm64 macOS, R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
tidyselect 1.2.1, rlang 1.3.0, vctrs 0.7.3, dtplyr 1.3.3,
data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5, RSQLite 3.53.3,
DBI 1.3.0 and RPostgres 1.4.10. The observations were finite. SQL Server,
hosted stores, other R and dependency versions, other platforms and
unbounded Grouping plans remained outside this run.

## Submission effects

The submission added this dated note and standalone evidence under the
existing investigation exclusion. Its consumers were readers and explicit
replay commands. It had no effect on the source tarball, package runtime,
tests, examples, vignettes, generated documentation or check machinery.
The cumulative comparison used the tree-identical full-checked ancestor
`2e96704f83479f7c2a16b3899b40c094f9b77146`, recorded on
[PR #1438](https://github.com/sayuks/marginplyr/pull/1438#issuecomment-6702138535).
These effects justified Review-ready `source`. The PR records the submitted
revision, verification and review. The investigation supplied evidence for
the linked repairs and introduced no package behavior change requiring a
NEWS entry.
