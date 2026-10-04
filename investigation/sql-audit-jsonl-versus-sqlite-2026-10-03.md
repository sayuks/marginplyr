# Rechecking JSON Lines and SQLite for SQL history

Investigated: 2026-10-03

## Scope and assessment

This source-only follow-up rechecked the earlier storage findings against the
requirements of persistent events, limited retained memory, and few dependencies.
No packages were installed or performance/crash experiments run. The October 2
notes remained unchanged. Most findings were reconfirmations; the explicit
jsonlite streaming-handler behavior below added implementation-relevant detail.

SQLite remained the stronger first candidate for transactional local history and
selective retrieval. JSON Lines remained the second candidate when straightforward
structured text append mattered more than transactional guarantees. This was a
suitability assessment, not an adopted design or a measured performance ranking.
CSV's age did not establish its suitability; the relevant differences were event
structure, recovery, and storage guarantees.

## Compared interfaces

| Topic | Ordinary JSON array | JSON Lines | SQLite |
| --- | --- | --- | --- |
| Record structure | RFC 8259 defined a single JSON value; array elements were comma-separated inside brackets. | The community documentation required UTF-8 and one valid JSON value per line. Nested objects/arrays were available. | Tables could hold event fields and IDs; parameter payload encoding remained a schema choice. |
| Append | Maintaining a valid array needed comma/closing-bracket management; appending an independent object after the closing bracket was not one valid JSON text. | A complete compact object followed by a newline could be appended as an event. | INSERTs could be committed independently; updates or event rows could represent an attempt's progress. |
| Bounded retrieval | A streaming parser was possible; the grammar did not mandate loading the whole file. | Line/page processing could retain only requested records, but arbitrary ID/date lookup could still require scanning or an added index. | SELECT conditions and LIMIT could return a requested subset without returning all history to R. |
| Interrupted write | A missing closing bracket could leave an invalid document. | A partial final event could be isolated from earlier valid lines by a purpose-built reader. That was recovery logic, not a commit or durability guarantee. | Transaction recovery and configured disk synchronization were engine features. |

Sources: [RFC 8259, JSON grammar and arrays](https://www.rfc-editor.org/rfc/rfc8259.html),
[JSON Lines documentation](https://jsonlines.org/),
[SQLite transactions](https://www.sqlite.org/lang_transaction.html),
and [SQLite SELECT/LIMIT](https://www.sqlite.org/lang_select.html).

The JSON Lines site described `application/jsonl` as not yet standardized. Its
framing documentation was community-maintained, distinct from JSON's RFC. This
did not prevent its use as a log convention, but a file containing multiple JSON
lines was not itself a single JSON text. A compact encoder would escape SQL
line breaks inside each object rather than introduce multiline event records.
[JSON Lines](https://jsonlines.org/),
[RFC 8259, strings](https://www.rfc-editor.org/rfc/rfc8259.html)

jsonlite's documented `stream_in()` read pages, but its default handler retained
all pages and returned their combined data frame. A custom handler caused it not
to retain intermediate pages; bounded memory therefore depended on the handler
also avoiding accumulation. Its `stream_out()` supported data frames, rather
than providing an arbitrary event-list sink automatically. Exact R type/missing
value restoration still required explicit mapping conventions.
[jsonlite official reference, streaming and conversion](https://cran.r-project.org/web/packages/jsonlite/refman/jsonlite.html)

## SQLite operation and dependencies

SQLite documented running inside the application's process and accessing local
files directly, without a separate database server. An RSQLite sink therefore
did not require running a database application or using the sqlite3 command-line
program. The inspected `DESCRIPTION` already listed RSQLite in Suggests, but
that did not prove it was installed for every user. A selected sink could require
that optional package. jsonlite was not a declared direct dependency; using its
encoder would require declaring it appropriately, even if another dependency
installed it transitively. No installation or metadata change was made here.
[SQLite serverless](https://www.sqlite.org/serverless.html),
[RSQLite connection](https://rsqlite.r-dbi.org/reference/SQLite.html)

The inferred audit design still used a separate local database and private
connection: commit a start event, invoke the target, then commit a return/error
event. Target rollback would not include those independent audit transactions.
The global DBI observer would exclude its own sink and guard against re-entry.
This did not make target execution and audit writes one atomic transaction, or
prove a server received a query from the start record alone.
[SQLite transactions](https://www.sqlite.org/lang_transaction.html)

RSQLite again documented `synchronous = "off"` by default. SQLite documented WAL
with FULL as durable and WAL with NORMAL as potentially losing committed
transactions after power loss/system crash. An explicit persistent path and
durability configuration were therefore necessary for the stronger guarantee.
WAL still allowed one writer at a time and required same-host participation.
These guarantees relied on SQLite's storage assumptions; no particular hardware
was tested. JSONL tail recovery could not substitute for disk-sync durability,
and neither format automatically resolved a failed audit sink without an
explicit stop/gap/fallback policy.
[RSQLite defaults](https://rsqlite.r-dbi.org/reference/SQLite.html),
[SQLite synchronous](https://www.sqlite.org/pragma.html#pragma_synchronous),
[SQLite WAL](https://www.sqlite.org/wal.html)
