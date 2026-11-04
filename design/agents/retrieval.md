# Retrieval with existing tools

Use the agent's available file reader, search, shell and structured-data parser.
Keep scratch files outside the submitting worktree. Follow existing navigation
to choose documents; this procedure keeps only task-local reading progress.

## Required documents

1. Record each document's path, version (Git blob or content digest), extent and
   coordinate unit in working notes. Keep accepted and unread ranges separately
   for each document. A known empty document has no unread content.
2. Request one explicit range per response. Start small, such as 40 lines, and
   adjust to the host's output budget. A line count alone does not bound output:
   use smaller character or byte ranges when one line is too large. A native
   range reader or a one-off expression with ordinary text functions suffices.
3. Accept only the range actually delivered in full. Check range information
   and truncation diagnostics before advancing. After a cut, record the last
   verified coordinate and retrieve only the missing suffix; use a smaller
   range when no part can be verified. Requested or saved content is not yet
   accepted content. When the host cannot establish delivery, keep it pending.
4. Give independent documents separate responses to avoid a shared output cap.
   If responses were batched, account for each delivered range independently;
   an intact first document says nothing about a cut-off second one.
5. Continue until every required document has no gaps. Check its version again
   before combining ranges from separate reads; if it changed, read the required
   material at one consistent version. A search hit or summary does not finish
   a required full read. Search additional material for relevant sections first.

## Structured session logs

1. Inspect the event schema using small scalar fields, such as type, timestamp
   and tool name. Parse JSON locally before displaying values; a raw JSONL line
   or a text-search match can contain the entire oversized payload.
2. In a one-off query, select event type, tool name and time interval, then
   project only the requested fields. Use an available structured parser (for
   example `jsonlite` in this repository). Keep large results in an ordinary
   scratch file for range reading with the procedure above.
3. Report scanned and selected counts, malformed-record locations, unavailable
   selector fields and missing requested fields. Preserve source line numbers
   or record IDs. State any scan limit or unread input; an interrupted scan is
   incomplete. Distinguish explicit null values from missing keys.
4. Preview only a small selection whose field sizes fit the output budget.
   For an oversized selected value, report its size and source location and
   read the requested value in parts. A record-count cap alone is insufficient.

Use a host-provided saved-output reference when available. Its preview is only
the displayed range, and query completion, result-page exhaustion and complete
host delivery are separate observations. Record unresolved retrieval rather
than claiming full coverage. These steps depend on agent execution and host
diagnostics; they do not enforce reading or prove understanding automatically.
