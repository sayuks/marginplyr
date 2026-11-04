# Exercises retrieval through the command boundary and complete page receipts.
local({
  workspace <- tempfile("retrieve-agent-test-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  entry <- normalizePath("tools/retrieve-agent.R", mustWork = FALSE)
  run <- function(...) {
    processx::run("Rscript", c(entry, ...), error_on_status = FALSE)
  }
  expect <- function(value, label) if (!isTRUE(value)) stop(label, call. = FALSE)
  document <- file.path(workspace, "required.md")
  content <- paste0(strrep("必要な文書\n", 300), "last byte")
  writeChar(content, document, eos = NULL, useBytes = TRUE)
  bundle <- file.path(workspace, "documents")
  prepared <- run("docs", "--output", bundle, "--budget", "1800", "--", document)
  expect(prepared$status == 0L, paste("prepare document:", prepared$stderr))
  info <- jsonlite::fromJSON(prepared$stdout)
  expect(info$pages > 1L, "required document exceeds one output budget")
  chunks <- character()
  previous <- 0
  for (page in seq_len(info$pages)) {
    result <- run("page", bundle, as.character(page))
    expect(result$status == 0L && nchar(result$stdout, type = "bytes") <= 1800L,
      "each response respects its aggregate byte budget")
    lines <- strsplit(result$stdout, "\n", fixed = TRUE)[[1L]]
    expect(tail(lines, 1L) == paste("END_PAGE", page), "page receipt is last")
    value <- jsonlite::fromJSON(lines[[1L]])
    expect(value$start_byte == previous, "next page starts at first unread byte")
    previous <- value$end_byte
    chunks <- c(chunks, value$content)
  }
  expect(identical(paste0(chunks, collapse = ""), content), "all required bytes retrieved once")
  expect(previous == file.size(document), "coverage reaches document end")

  second <- file.path(workspace, "second.md")
  writeLines(strrep("second document ", 300), second)
  pair <- file.path(workspace, "pair")
  paired <- run("docs", "--output", pair, "--budget", "1800", "--", document, second)
  expect(paired$status == 0L, "prepare independent documents together")
  pair_info <- jsonlite::fromJSON(paired$stdout)
  covered <- c(0, 0)
  responses <- character()
  for (page in seq_len(pair_info$pages)) {
    result <- run("page", pair, as.character(page))
    responses <- c(responses, result$stdout)
    value <- jsonlite::fromJSON(strsplit(result$stdout, "\n", fixed = TRUE)[[1L]][[1L]])
    expect(value$start_byte == covered[[value$document]], "independent document cursor")
    expect(value$source == normalizePath(c(document, second)[[value$document]]),
      "document identity survives batching")
    covered[[value$document]] <- value$end_byte
    expect(nchar(result$stdout, type = "bytes") <= 1800L, "batch shares one output budget")
  }
  expect(identical(covered, unname(file.size(c(document, second)))), "both documents covered")

  # A host batch loses its tail: only intact JSON plus its final receipt is accepted.
  batch <- rawToChar(head(charToRaw(paste0(responses, collapse = "")), 1800L))
  accepted <- which(vapply(seq_along(responses), function(page) {
    grepl(paste0("\nEND_PAGE ", page, "\n"), batch, fixed = TRUE, useBytes = TRUE)
  }, logical(1L)))
  expect(identical(accepted, 1L), "outer aggregate truncation accepts only the first complete page")
  requested <- setdiff(seq_along(responses), accepted)
  expect(!any(requested %in% accepted), "no already accepted range is requested again")
  for (page in requested) {
    expect(identical(run("page", pair, as.character(page))$stdout, responses[[page]]),
      "missing ranges remain replayable after outer truncation")
  }
  writeLines("changed after snapshot", document)
  expect(identical(run("page", pair, "1")$stdout, responses[[1L]]), "snapshot identity stays stable")

  log <- file.path(workspace, "session.jsonl")
  event <- function(type, tool, time, text) {
    as.character(jsonlite::toJSON(list(type = type, timestamp = time,
      payload = list(name = tool, text = text, secret = "UNREQUESTED_SECRET")), auto_unbox = TRUE))
  }
  oversized <- strrep("projected 日本語\n", 400)
  writeLines(c(
    event("response_item", "exec", "2026-11-03T10:00:00Z", oversized),
    event("response_item", "other", "2026-11-03T10:00:00Z", "wrong tool"),
    event("event_msg", "exec", "2026-11-03T10:00:00Z", "wrong type"),
    event("response_item", "exec", "2026-11-03T12:00:00Z", "outside time"),
    "{malformed",
    '{"type":"response_item","payload":{"name":"exec"}}',
    '{"type":"response_item","timestamp":"2026-11-03T10:00:00Z","payload":{"name":"exec"}}'
  ), log)
  projected <- file.path(workspace, "projected")
  selection <- run("logs", "--output", projected, "--budget", "1800",
    "--type", "response_item", "--tool", "exec", "--after", "2026-11-03T09:00:00Z",
    "--before", "2026-11-03T11:00:00Z", "--fields", "payload.text", "--", log)
  expect(selection$status == 0L, paste("query logs:", selection$stderr))
  log_info <- jsonlite::fromJSON(selection$stdout)
  projection <- character()
  for (page in seq_len(log_info$pages)) {
    result <- run("page", projected, as.character(page))
    expect(nchar(result$stdout, type = "bytes") <= 1800L, "oversized JSONL stays bounded")
    value <- jsonlite::fromJSON(strsplit(result$stdout, "\n", fixed = TRUE)[[1L]][[1L]])
    projection <- c(projection, value$content)
  }
  projection <- paste0(projection, collapse = "")
  expect(!grepl("UNREQUESTED_SECRET|wrong tool|wrong type|outside time", projection),
    "only selected events and requested fields are emitted")
  records <- lapply(strsplit(projection, "\n", fixed = TRUE)[[1L]], jsonlite::fromJSON)
  expect(identical(records[[2L]]$fields$payload.text, oversized), "oversized field is retrieved completely")
  expect(records[[1L]]$scanned == 7L && records[[1L]]$matched == 2L &&
    records[[1L]]$malformed == 1L && records[[1L]]$missing_filter == 1L &&
    records[[1L]]$missing_fields == 1L && !records[[1L]]$selection_complete,
    "malformed records and incomplete selections remain visible")
  expect(any(vapply(records, function(record) {
    identical(record$status, "malformed") && identical(record$line, 5L)
  }, logical(1L))),
    "malformed record identifies its input line")
  expect(run("page", bundle, "0")$status != 0L, "invalid page is refused")
  expect(run("docs", "--output", bundle, "--", document)$status != 0L,
    "existing bundle cannot be overwritten")
  empty <- file.path(workspace, "empty.md")
  file.create(empty)
  empty_bundle <- file.path(workspace, "empty")
  expect(run("docs", "--output", empty_bundle, "--", empty)$status == 0L, "empty document has a page")
  empty_page <- run("page", empty_bundle, "1")
  value <- jsonlite::fromJSON(strsplit(empty_page$stdout, "\n", fixed = TRUE)[[1L]][[1L]])
  expect(value$total_bytes == 0L && value$end_byte == 0L && is.null(value$next_page),
    "empty document completion is explicit")
  cat("Agent retrieval CLI fixtures passed.\n")
})
