queries <- list(
 list(kind = "issue", query = "logLik"),
 list(kind = "issue", query = "logLik dtplyr"),
 list(kind = "issue", query = "dist dtplyr prototype"),
 list(kind = "issue", query = "typed-missing dtplyr"),
 list(kind = "pr", query = "logLik"),
 list(kind = "pr", query = "scalar dtplyr")
)
for (i in seq_along(queries)) {
 item <- queries[[i]]
 output <- system2("gh", c(item$kind, "list", "--state", "all", "--search", shQuote(item$query), "--limit", "100", "--json", "number,title,state,url"), stdout = TRUE)
 queries[[i]]$matches <- jsonlite::fromJSON(paste(output, collapse = "\n"), simplifyVector = FALSE)
}
jsonlite::write_json(queries, "investigation/2026-11-03-expand-grouping-greedy/lazy/duplicate-search.json", auto_unbox = TRUE, pretty = TRUE)
