Sys.setenv(TESTTHAT_PARALLEL = "false")
args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args)) args[[1L]] else "observation"
backend <- if (length(args) > 1L) args[[2L]] else "duckdb"
stopifnot(mode %in% c("observation", "control", "contract"))
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))

verify <- function(mode, backend) {
  con <- switch(backend,
    duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE),
                            config = list(threads = "1")),
    postgres = DBI::dbConnect(RPostgres::Postgres())
  )
  stopifnot(!is.null(con))
  on.exit(DBI::dbDisconnect(con))
  data <- data.frame(g = c("a", "b"), x = c(1L, 3L))
  source <- dplyr::copy_to(con, data, "share_cardinality_facts")
  counts <- if (mode == "control") 1L else 0:2
  rows <- list()
  for (count in counts) for (duplicates in c("error", "keep")) {
    members <- switch(as.character(count), "0" = "", "1" = "10", "2" = "10,20")
    array <- if (backend == "duckdb") {
      paste0("CAST([", members, "] AS INTEGER[])")
    } else {
      paste0("ARRAY[", members, "]::INTEGER[]")
    }
    value <- dbplyr::sql(paste0("SUM(x) + unnest(", array, ")"))
    query <- summarize_with_margins(
      source, value = !!value,
      parent = share_of_parent(value), total = share_of_total(value),
      .grouping = rollup(g), .id = "set", .margin_label = NULL, .sort = "last",
      .duplicates = duplicates
    )
    stopifnot(inherits(query, "tbl_lazy"))
    sql <- as.character(dbplyr::sql_render(query))
    native <- grepl("GROUPING SETS", sql, fixed = TRUE)
    stopifnot(identical(native, duplicates == "error"))
    result <- tryCatch(dplyr::collect(query), error = identity)
    if (count == 1L) {
      stopifnot(identical(as.double(result$value), c(11, 13, 14)))
      stopifnot(isTRUE(all.equal(result$parent, c(11, 13, 14) / 14)))
      stopifnot(identical(result$parent, result$total))
    }
    if (mode == "observation" && count == 0L) stopifnot(nrow(result) == 0L)
    if (mode == "observation" && count == 2L) {
      stopifnot(nrow(result) == 6L)
      result <- dplyr::arrange(result, set, g, value)
      stopifnot(identical(as.double(result$value), c(11, 21, 13, 23, 14, 24)))
      stopifnot(isTRUE(all.equal(result$parent, c(11, 21, 13, 23, 24, 24) / 24)))
      stopifnot(identical(result$parent, result$total))
    }
    rows[[length(rows) + 1L]] <- data.frame(
      backend = backend, count = count, native = native, duplicates = duplicates,
      nrow = if (inherits(result, "error")) NA_integer_ else nrow(result),
      refused = inherits(result, "error")
    )
  }
  outcomes <- do.call(rbind, rows)
  print(outcomes, row.names = FALSE)
  if (mode == "contract") stopifnot(all(outcomes$refused[outcomes$count != 1L]))
}

verify(mode, backend)
