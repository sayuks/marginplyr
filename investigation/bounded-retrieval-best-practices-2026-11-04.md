# Bounded retrieval and structured session-log queries

Investigated: 2026-11-04

## Scope and evidence

This investigation compared general software and agent practices for reading
large required documents and querying session logs. It was not limited to R
package development. It reviewed primary specifications and first-party
documentation; it did not benchmark agent consumption or implement a reader.
The application to #1200 below was an inference, not an adopted repository
decision or evidence that any candidate worked better in this environment.

**Specification** denotes a protocol requirement within its stated scope.
**Vendor guidance/behavior** denotes a provider's recommendations or documented
product contract. **Inference** denotes this investigation's design judgment.

## Agent-facing retrieval practices

**Vendor guidance.** Anthropic's September 2025 tool-design article favored
tools organized around useful workflows and distinguished searching logs from
reading entire logs. It recommended pagination, filtering, bounded ranges and
truncation with actionable continuation instructions for large responses.
Its evaluation advice included realistic tasks, correctness, runtime, tool
calls, token use and tool errors. It did not establish a universally superior
JSON, XML or Markdown response format. These were recommendations to evaluate
against the actual agent and tasks, rather than a protocol requiring a
particular custom reader. [Writing effective tools for agents](https://www.anthropic.com/research/writing-tools-for-agents)

**Vendor guidance.** Anthropic's context-engineering article described keeping
lightweight references, including file paths, and retrieving relevant content
just in time with tools such as search. It also described a hybrid approach
that supplied essential context upfront and explored other content on demand.
**Inference:** this supported distinguishing documents the task required in full from
additional material whose relevant sections could be located first. It did not
show that precomputing every document into pages was always preferable to
on-demand exploration. [Effective context engineering for AI agents](https://www.anthropic.com/research/effective-context-engineering-for-ai-agents)

**Vendor guidance.** Anthropic's MCP code-execution article described filtering,
transforming and aggregating tool results in an execution environment before
returning selected information to the model. Large intermediate values could
remain outside model context. **Inference:** a local JSON log query could
therefore parse, select and project records before emitting its bounded result;
printing the whole log and asking the model to ignore most of it would miss
that benefit. This concerned the model boundary; it did not imply the source
backend scanned fewer records. [Code execution with MCP](https://www.anthropic.com/research/code-execution-with-mcp)

## Pagination and completion

**API design guidance with normative language.** Google AIP-158 allowed a page
to contain fewer items than requested, including zero, before the collection
ended. Its end-of-collection signal was an empty `next_page_token`; otherwise a
token had to be returned. Tokens were opaque, and subsequent requests kept
other arguments unchanged, except that page size could change. Consequently,
counting a short page as completion would violate this API contract.
[AIP-158, Guidance and Opacity](https://google.aip.dev/158)

**Specification.** MCP's pinned 2025-11-25 pagination specification used opaque
cursors for supported list operations. Clients could not assume a fixed page
size; absence of `nextCursor` signaled the end. Clients had to avoid parsing,
modifying or persisting cursors across sessions. These rules applied to the
listed resource, template, prompt and tool discovery operations. They did not
automatically paginate the content of an arbitrary tool result.
[MCP pagination](https://modelcontextprotocol.io/specification/2025-11-25/server/utilities/pagination)

**Specification.** MCP tools could return resource links for separately
retrievable content, and could describe structured results with an output
schema. This supplied a standard way to expose a retrieval reference without
embedding the entire artifact in the initial result. Resource links and
structured schemas did not themselves guarantee that a host would fetch every
resource or that every byte would enter model context.
[MCP tools, Resource Links and Structured Content](https://modelcontextprotocol.io/specification/2025-11-25/server/tools)

**Inference:** distinguish source exhaustion, query completion, successful
transport, host exposure, and task-level understanding. A terminal cursor
established only the source's pagination condition. A successful shell exit
did not prove an untruncated host display; complete delivery did not prove
that an agent processed or understood every required section. Record observed
per-document coverage and evaluate downstream answers separately. A textual
`END_PAGE` marker could help diagnose a particular interface, but no reviewed
specification made that marker a universal requirement.

## Stable range retrieval

**Specification.** RFC 9110 permitted servers to ignore `Range`. `Content-Range`
described the returned interval, and combining partial responses required the
same strong validator. `If-Range` could use a strong ETag; a weak ETag was
forbidden. A validator mismatch caused the recipient to ignore `Range` and
return the new representation rather than the requested fragment. Thus a range
request was not by itself a strict response-size guarantee.
[RFC 9110, sections 13.1.5, 14.2, 14.4 and 15.3.7.3](https://www.rfc-editor.org/rfc/rfc9110.html#section-13.1.5)

**Inference:** a local range reader should name its source version, validate
actual returned coverage and distinguish changed-source, truncated-output and
end-of-source outcomes. Immutable Git blobs, snapshot copies or validated
content identities were candidate mechanisms. HTTP ETags and a particular local
hash or serialization format were not interchangeable standards requirements.
Snapshot identity and output bounds were separate obligations: detecting a
changed source did not make an unexpectedly large fallback response safe for a
small display budget.

## Querying logs before returning them

**Vendor API behavior.** CloudWatch `FilterLogEvents` accepted filter patterns,
time bounds and stream selection. Empty or partly filled pages did not imply
completion: the absence of `nextToken` did. Its `limit` bounded events on one
page rather than promising that many matches or a complete query result.
[FilterLogEvents](https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_FilterLogEvents.html)

**Vendor API behavior.** `GetQueryResults` returned selected fields plus a
record pointer for later detail retrieval. `Running` results were partial;
`Scheduled` and `Running` required later polling. The documentation separately
described pagination for Logs Insights QL, so query execution completing did
not imply that all result pages had been retrieved. Failure, cancellation and
timeout were distinct states. [GetQueryResults](https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_GetQueryResults.html)

**Vendor guidance.** Logs Insights recommended necessary log groups and narrow
time ranges. Its `filter` selected records; `fields` and `display` selected
returned fields. These were separate controls. A small projection should not
be claimed to reduce scanned work without backend-specific evidence.
[Logs Insights query syntax](https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html)

**Vendor guidance.** Elasticsearch documented that refreshes could change
`search_after` ordering between pages, and that a point in time preserved the
index state. A unique tiebreaker prevented missed or duplicated hits when sort
values tied. **Inference:** log-query continuation needed a stated ordering and
consistency model; offset alone did not establish a stable snapshot.
[Paginate search results](https://www.elastic.co/docs/reference/elasticsearch/rest-apis/paginate-search-results)

**Tool behavior.** The jq manual described a stream of JSON values, `select()`
for record selection, object construction for projection, and compact output.
`--slurp` collected the whole input, whereas `--stream` supported incremental
processing of very large JSON values. **Inference:** structured parsing could
separate metadata from payloads and emit selected records with source offsets
or IDs. A record count cap still needed an output-size bound because one record
could be enormous. [jq 1.8 manual](https://jqlang.org/manual/)

## Where an output bound can live

**Vendor behavior observed in documentation on the investigation date.**
Claude Code documented spilling successful text-only MCP outputs above its
default character threshold into a file and returning a reference. Managed
Agents documented a truncated preview plus sandbox file path for oversized
results. These illustrated harness-owned storage and retrieval. Their different
thresholds and error/transport paths were product-specific behavior, not an MCP
standard or a guarantee about other hosts. [Claude Code MCP output limits](https://code.claude.com/docs/en/mcp#mcp-output-limits-and-warnings),
[Managed Agents tools](https://platform.claude.com/docs/en/managed-agents/tools#available-tools)

## Candidate comparison and application to #1200

The following comparison was **inference from the evidence**, not a performance
ranking. No option removed the need to measure whether the agent actually
continued and consumed required material.

| Candidate | Appropriate circumstances | Costs and limits to establish |
| --- | --- | --- |
| Existing tools plus operating practice | Available search/range tools already returned usable bounded output | Track required coverage, retrieve only missing ranges after truncation, preserve source identity; depends on agent compliance and host behavior |
| Stateless range reader plus structured log projection | Demonstrated gaps in existing source/version metadata, continuation or payload selection | A small maintained interface, explicit ranges/cursors and query states; avoid assuming a record cap also bounds bytes |
| Harness spill with preview and retrieval reference | The tool host could retain full output before presentation limits applied | Retention, permissions, reference lifetime and a bounded follow-up reader; a preview alone remained incomplete |
| Pre-generated immutable page bundles | Archival replay, reproducibility or mutable-source constraints justified freezing material | Upfront work, storage, invalidation and artifact lifecycle; generating unused pages added cost |

A reasonable next step for #1200 was to test the first option, retain a thin
reader only for observed gaps, and prefer host-level spill when the host was
under project control. Immutable bundles remained an option where their
reproducibility benefit justified their lifecycle. This was not evidence for
deleting or adopting any particular implementation.

Evaluation should compare realistic full-document and targeted-log tasks:
correct answers, observed coverage, continuation completion, calls, tokens,
latency and recovery. Include oversized single lines/records, multibyte text,
empty intermediate pages, source changes, interrupted retrieval, malformed
records, no matches, query timeout and host truncation. Report source bytes,
emitted bytes and host-visible bytes separately where observable. No empirical
results for these alternatives were established by this investigation.
