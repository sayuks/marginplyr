# Read-only issue and PR duplicate searches, investigated 2026-11-03.
artifact <- "investigation/2026-11-03-summary-grouping-share-bug-hunt/summary"
queries <- list(
  pick = c('"pick" "forward"', '"pick" "forwarded"', '"pick" "..1"', '"populated" "selector"'),
  names = c('"across" "glue"', '"glue" "vector"', '".col" "names"', '"seq_along" "across"', '"template" "evaluation"')
)
records <- list()
for (lane in names(queries)) for (query in queries[[lane]]) for (kind in c("issue", "pr")) {
  output <- system2("gh", c(kind, "list", "--state", "all", "--limit", "100", "--search", shQuote(query), "--json", "number,title,state,url"), stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  stopifnot(status == 0L)
  records[[length(records) + 1L]] <- list(lane = lane, query = query, kind = kind, output = jsonlite::fromJSON(paste(output, collapse = "\n")))
}
jsonlite::write_json(records, file.path(artifact, "duplicate-searches.json"), pretty = TRUE, auto_unbox = TRUE)
cat(length(records), "all-state searches completed\n")
