# DuckDB typed keys and numeric share controls

Investigated: 2026-11-06
Repository snapshot: `547d77095c2923573216d002f50bef21778247f9`

The coordinator loaded a disposable archive of that SHA at
`/private/tmp/marginplyr-deep-sql`. No additional defect was established by
this lane. ADRs 0010, 0014 and 0017 supplied the typed-key, displayed-denominator,
numeric-source and partition rules. ADR 0020 supplied the construction-time
query boundary. The November 6 SQL evidence and November 3 typed-key
comparisons supplied preceding finite controls; this lane increased key
cardinality and checked multiple measures together.

## Typed keys and visible denominators

[The comparison script](typed-keys-and-shares.R) constructed ten DuckDB key
types: BIGINT values above exact double-integer precision, DECIMAL, TIME,
DATE, TIMESTAMP, INTERVAL, BLOB, LIST, STRUCT and MAP. Each fixture contained
four distinct typed keys, repeated values, two child values, a missing fixed
key and a negative measure. Sources were ordinary doubles, DECIMAL, large
BIGINT and small DECIMAL. Cases crossed fixed/unfixed partitions, auto/portable
aggregation and all three sort requests, for 480 collections.

For each collection, two Total shares were recomputed from their displayed
Grand total set rows. Parent shares were independently matched to the next
less detailed displayed occurrence using typed values through vctrs rather
than display labels. All 480 collections matched all three ratio columns at
tolerance 1e-12. The comparison did not assert Margin order or exact numeric
storage classes. It did not introduce zero-denominator fixtures, whose rules
were not retested here.

The first execution, retained handle `51934`, checked the two Total-share
columns and ended with exit zero. The second, handle `73499`, added the Parent
oracle and ended with exit zero. The retained script added a final zero-failure
assertion before the coordinator replay. [The lane transcript](typed-keys-and-shares.log)
records the second run; the coordinator transcript records the final script.
The ordinary SQL missing-value warning was emitted once by dbplyr and was not
a package condition or a failed comparison.

## Semantic source types

[The exploratory source script](source-types.R) paired ordinary summaries
with Grand-total-only and rollup Total-share requests. Ordinary UUID, ENUM,
BOOLEAN, DATE, TIMESTAMP, INTERVAL and BLOB summaries succeeded; both share
requests for each were refused at database execution. The numeric DECIMAL
control succeeded in both shapes with shares of one. This refuted the tested
type-admission hypothesis without establishing general semantic-type coverage.

Two JSON fixtures could not acquire a schema because the disposable DuckDB
home lacked its JSON extension. A BIT literal was also unsupported at fixture
setup. Those three cases were not share results. An initial script stopped at
the first setup error; the retained version isolated setup failures before
continuing. Its final execution `56157` ended with exit zero. The
[transcript](source-types.log) preserves ordinary successes, share diagnostics
and setup limitations. This exploratory script reports outcomes; the typed-key
script is the assertion-bearing numeric oracle.

## Environment and limits

The environment was R 4.6.1 on aarch64 macOS, DuckDB 1.5.5, DBI 1.3.0,
dplyr 1.2.1, dbplyr 2.6.0, vctrs 0.7.3 and pkgload 1.5.3. The
[session record](environment.txt) supplements the versions recorded by the
coordinator. Each connection used a disposable DuckDB home and was disconnected
by on.exit. All execution handles were collected to their terminal results.

These finite probes did not cover live PostgreSQL or SQLite, hosted services,
database collations, non-finite numeric portability, arbitrary SQL extensions,
later materialization/composition, concurrency, interruption or other versions.
No full package suite, build, rendering or implementation repair was run in
this lane. Replay by changing into an archive of the named SHA and invoking
Rscript with the retained script's absolute path.
