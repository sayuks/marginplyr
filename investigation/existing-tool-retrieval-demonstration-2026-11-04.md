# Existing-tool retrieval demonstration

Investigated: 2026-11-04

## Scope

The user superseded #1200's custom-command implementation with instructions
using existing tools. This demonstration exercised the replacement procedure
in `design/agents/retrieval.md`, prepared after local commit
`2cd2bf7ddedc52fa090fede79418b47c982e693c`. It used R 4.6.1, jsonlite 2.0.0
and this session's ordinary shell tool with `max_output_tokens = 350` per
response. It added no repository launcher, parser, test harness or CI job.

Temporary fixtures, one-off query and response receipts were held outside the
worktree at `/private/tmp/marginplyr-1200-existing-10cea39fbd906`. Their presence
was temporary evidence, not a prerequisite for later use of the procedure.

## Required document beyond one response budget

Base R generated 80 lines with
`sprintf("A%03d Required paragraph %03d: retain this ordinary source text.", 1:80, 1:80)`.
The UTF-8 file held 5,040 bytes and MD5
`4e3c269bd952dfd8a085f843cda4828d`.

Eight separate shell responses used `readLines()`, ordinary vector selection
and `cat()` to display ranges 1-10, 11-20, 21-30, 31-40, 41-50, 51-60, 61-70
and 71-80. Each response displayed the source name, digest, range, total extent
and the next unread range. The digest stayed the same. Returned record IDs
were exactly A001 through A080, in order, with no repeated or missing range.
The shell reported 181 tokens for each of the first seven responses and 180
for the last; none was truncated. The final unread range was empty.

## Independent documents beyond a combined response budget

Base R generated two 20-line documents using
`sprintf("%s-%02d Independent required document content for this range.", name, 1:20)`
with names `first` and `second`. They held 1,260 and 1,280 bytes respectively.
Their digests were `32caf3a90941c48d88be88909a1afa9e` and
`6059cc9d12db6a1be5298db948e9802c`.

Each document was read once in its own response. The shell reported 342 and
347 tokens, a combined 689 exceeding the 350-token per-response budget. Both
responses were intact. After the first, working progress recorded first as
complete and second as unread 1-20; after the second, both were complete.
No document or range was reread. This avoided an aggregate cutoff by keeping
the responses separate; it did not demonstrate recovery after a real cutoff.

## A single oversized JSONL record

A seven-record fixture included a selected event carrying the value
`strrep("large payload ", 10000)`, three events excluded by tool/type/time,
malformed JSON at line 5, a missing requested value at line 6 and an explicit
null value at line 7. The large scalar alone held 140,000 bytes.

A one-off R query read one line at a time, parsed with
`jsonlite::fromJSON(..., simplifyVector = FALSE)`, selected type
`response_item`, tool `exec_command` and the UTC day 2026-11-04. It projected
line number, type, timestamp, tool, value byte count and presence/null state.
It printed none of the large value's text. Its 138-token response reported:

```text
scanned: 7; matched: 3; malformed_lines: [5]; missing_value_lines: [6]
scan_finished: true; selection_complete: false
line 1: value_bytes 140000, state present
line 6: value_bytes null, state missing
line 7: value_bytes null, state null
```

## Limits

These observations established that the existing tools supported the three
concrete retrieval tasks in this session. They did not establish automatic
coverage enforcement, reliable behavior across all agents or hosts, recovery
from a host that silently discarded output, or an end-to-end accuracy gain.
The operator selected ranges, inspected delivery and tracked unread content.
The one-off log query targeted the fixture's known schema; it was not a new
general-purpose log reader. Neither a source byte size nor a line-count cap
was treated as an exact host token count.
