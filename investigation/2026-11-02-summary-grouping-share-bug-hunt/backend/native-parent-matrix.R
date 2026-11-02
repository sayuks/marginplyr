Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
out <- "investigation/2026-11-02-summary-grouping-share-bug-hunt/backend"
run <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- copy_to(con, tibble(k = c("east", NA_character_, "east", NA_character_),
    g = c("a", "a", "b", "b"), h = c("x", "y", "x", "y"), x = 1:4), "matrix_source")
  records <- list()
  calls <- 0L
  for (dims in 1:2) for (fixed in c(FALSE, TRUE)) for (kind in c("parent", "total", "both")) for (adapter in c("native", "portable")) for (aggregate in c(FALSE, TRUE)) for (label in c("text", "missing")) {
    DBI::dbExecute(con, "CREATE OR REPLACE SEQUENCE parent_stamp")
    measure <- dbplyr::sql("nextval('parent_stamp')")
    dots <- list(value = if (aggregate) rlang::expr(sum(x, na.rm = TRUE) + !!measure) else measure)
    if (kind %in% c("parent", "both")) dots$parent <- quote(share_of_parent(value))
    if (kind %in% c("total", "both")) dots$total <- quote(share_of_total(value))
    query <- summarize_with_margins(source, !!!dots,
      .by = if (fixed) "k" else NULL,
      .grouping = if (dims == 1L) rollup(g) else rollup(g, h),
      .margin_label = if (label == "text") "Total" else NULL,
      .duplicates = if (adapter == "native") "error" else "keep", .id = "set",
      .check_share_source = FALSE)
    sql <- as.character(dbplyr::sql_render(query))
    stopifnot(identical(grepl("GROUPING SETS", sql, fixed = TRUE), adapter == "native"))
    before <- tryCatch(DBI::dbGetQuery(con, "SELECT currval('parent_stamp')"), error = identity)
    stopifnot(inherits(before, "error"))
    result <- collect(query)
    calls <- calls + 1L
    for (share in intersect(c("parent", "total"), names(result))) {
      expected <- vapply(seq_len(nrow(result)), function(i) {
        if (result$set[[i]] == dims + 1L) return(1)
        parent_id <- if (share == "total") dims + 1L else result$set[[i]] + 1L
        matches <- result$set == parent_id
        if (fixed) matches <- matches & vctrs::vec_equal(result$k, result$k[[i]], na_equal = TRUE)
        if (parent_id != dims + 1L) matches <- matches & vctrs::vec_equal(result$g, result$g[[i]], na_equal = TRUE)
        j <- which(matches)
        stopifnot(length(j) == 1L)
        result$value[[i]] / result$value[[j]]
      }, numeric(1))
      match <- isTRUE(all.equal(result[[share]], expected))
      records[[length(records) + 1L]] <- data.frame(dims = dims, fixed = fixed, kind = kind,
        adapter = adapter, aggregate = aggregate, label = label, share = share, matched = match)
    }
  }
  records <- bind_rows(records)
  write.csv(records, file.path(out, "native-parent-matrix.csv"), row.names = FALSE)
  print(with(records, table(dims, adapter, share, matched)))
  print(records[!records$matched, ])
  stopifnot(!all(records$matched))
  cat("Calls:", calls, "share-column comparisons:", nrow(records),
      "mismatches:", sum(!records$matched), "\n")
}
run()
