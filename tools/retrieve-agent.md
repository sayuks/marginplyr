# Bounded agent retrieval

Use the existing navigation and retrieval policy to choose material. This local
R command snapshots the supplied files into a new disposable bundle; it keeps
no maintained document inventory. It needs `jsonlite` (already a review tool).
Store bundles outside the submitting worktree and remove them after use; log
projections may contain sensitive selected fields.

```sh
Rscript tools/retrieve-agent.R docs --output /tmp/agent-reading --budget 6000 -- CLAUDE.md AGENTS.md
Rscript tools/retrieve-agent.R page /tmp/agent-reading 1
```

Preparation prints the page count. Each `page` call prints one JSON envelope
followed by `END_PAGE N`. Its total stdout, including escaped content, metadata
and receipt, fits `--budget` bytes (default 6000, minimum 1200). This is a byte
budget, not a host token allowance: leave room for host wrappers and other
outputs. Files must be UTF-8 text; a budget unable to hold their identity fails.
Preparation fails if the output directory exists, preserving earlier bundles.
The command works from any directory when invoked by its absolute script path.

The envelope names the absolute source, captured-byte MD5, document number,
total document count, zero-based half-open `[start_byte, end_byte)` range,
total source bytes, current/total page numbers and `next_page`. `content` is
JSON-escaped text; parse it before reading or reconstructing the original.
An empty file has one explicit zero-byte page. Original-file edits do not change
the captured pages. MD5 identifies captured content, not authenticity.

At the host-tool boundary, accept a page only when its JSON parses, the decoded
content byte count equals `end_byte - start_byte`, and its matching final
`END_PAGE N` is present. Track the union of accepted ranges separately for each
source and MD5. Continue with `next_page`, or the first missing page after a
partial batch. Required-document reading finishes only when every supplied
document covers `[0, total_bytes)` with no gaps (including its zero-byte page
when empty). A final page alone establishes no coverage of earlier pages.

If a host aggregates several independent calls and cuts off the batch, accept
only intact pages and retrieve the missing ones. Reads never advance stored
state, so a lost response cannot silently mark bytes read. For a receipt that
cannot fit the host limit, prepare a new bundle with a smaller budget. Host
adapters must apply this receipt rule where they receive actual tool output;
the R process cannot observe truncation performed after it exits.

## Session logs

```sh
Rscript tools/retrieve-agent.R logs --output /tmp/agent-events --budget 6000 --type response_item --tool exec_command --after 2026-11-03T00:00:00Z --before 2026-11-04T00:00:00Z --fields timestamp,payload.arguments -- session.jsonl
Rscript tools/retrieve-agent.R page /tmp/agent-events 1
```

`--type` and `--tool` are exact scalar matches, applied in that order, followed
by the inclusive time interval. Their default field paths are `type`,
`payload.name` and `timestamp`; override with `--type-field`, `--tool-field` and
`--time-field` for another event schema (for example `--type-field payload.type`).
Time bounds and filtered timestamps use UTC RFC3339 with optional fractional
seconds. Select comma-separated dotted object paths with the required `--fields`.
Only requested values appear; object-valued fields include their descendants.
Array indexing and raw-record output are not supported.

Each input log is a separately identified projected document, paginated through
the same `page` command. Its first JSONL record summarizes the query, original
input MD5, scanned/matched counts, malformed-record count, unavailable-filter
count, missing-requested-field count and `selection_complete`. Following records
contain the original one-based input line number and either selected fields or
an explicit diagnostic. A malformed/non-object record is reported even when
its selectors cannot be evaluated. A missing, null or invalid scalar filter on
an otherwise matching candidate is reported as `missing_filter`; an explicit
null requested value is present, while an absent requested key is listed in
`missing_fields`. These defects make `selection_complete` false even when every
projection page has been read. A well-formed query with no matches reports zero.

Large records are parsed locally, and even a single projected value spans as
many bounded pages as necessary. This bounds tool output, not process memory;
preparation holds source text and the projection in memory. It reads local
files only and performs no log discovery, remote fetching or host-tool mutation.

Run the command-boundary regression fixtures with:

```sh
Rscript tools/test-retrieve-agent.R
```
