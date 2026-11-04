out_dir <- "investigation/2026-11-04-summary-grouping-share-bug-hunt/summary"
queries <- c('"noncanonical"', '"..01"', '"dots" "signed"',
             '"dots" "leading"', '"dots" "whitespace"',
             '"numbered" "forward"', '"unpack" "factory"')
results <- list()
for (kind in c("issue", "pr")) {
  for (query in queries) {
    output <- system2("gh", c(kind, "list", "--state", "all", "--search",
      shQuote(query), "--limit", "100", "--json", "number,title,state,url"),
      stdout = TRUE)
    stopifnot(is.null(attr(output, "status")))
    value <- jsonlite::fromJSON(paste(output, collapse = "\n"), simplifyVector = FALSE)
    stopifnot(length(value) < 100L)
    results[[length(results) + 1L]] <- list(kind = kind, query = query, results = value)
  }
}
jsonlite::write_json(results, file.path(out_dir, "duplicate-searches.json"),
                     auto_unbox = TRUE, pretty = TRUE)
for (number in c(1138L, 1182L, 1170L, 1002L)) {
  output <- system2("gh", c("issue", "view", number, "--comments", "--json",
    "number,title,state,body,comments"), stdout = TRUE)
  stopifnot(is.null(attr(output, "status")))
  writeLines(output, file.path(out_dir, paste0("issue-", number, ".json")))
}
for (number in c(1174L, 1192L)) {
  output <- system2("gh", c("pr", "view", number, "--json", "number,title,state,body"),
                    stdout = TRUE)
  stopifnot(is.null(attr(output, "status")))
  writeLines(output, file.path(out_dir, paste0("pr-", number, ".json")))
}
cat("Completed", length(results), "all-state issue/PR queries.\n")
