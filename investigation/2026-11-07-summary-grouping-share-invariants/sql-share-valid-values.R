Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
connections <- list(
  sqlite = DBI::dbConnect(RSQLite::SQLite()),
  duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE),
                           config = list(threads = "1")),
  postgres = DBI::dbConnect(RPostgres::Postgres())
)
data <- data.frame(fixed = rep(c("east", "west", NA), each = 4L),
  g = rep(c("a", NA, "a", "b"), 3L),
  h = rep(c("u", "x", "y", NA), 3L), x = c(0, -2, 2, 0, -1, 2, -3, 4, NA, 3, NA, 4))
agree <- function(result) {
  for (i in seq_len(nrow(result))) {
    row <- result[i, ]
    f <- vctrs::vec_equal(result$fixed, row$fixed, na_equal = TRUE)
    g <- vctrs::vec_equal(result$g, row$g, na_equal = TRUE)
    for (k in c("parent", "total")) {
      den <- if (row$set == 3L) 1 else {
        target <- if (k == "parent") row$set + 1L else 3L
        as.double(result$value[result$set == target & f & (target == 3L | g)])
      }
      stopifnot(length(den) == 1L, typeof(result[[k]]) == "double")
      expected <- if (row$set == 3L) 1 else {
        if (is.na(den) || den == 0 || is.na(row$value)) NA_real_ else as.double(row$value) / den
      }
      stopifnot(isTRUE(all.equal(result[[k]][[i]], expected, tolerance = 1e-12)))
    }
  }
}
counts <- c(ok = 0, fail = 0)
for (kind in names(connections)) {
  con <- connections[[kind]]
  cat(kind, DBI::dbGetInfo(con)$db.version, "\n")
  lazy <- dplyr::copy_to(con, data, "share_facts", overwrite = TRUE)
  if (kind != "sqlite") DBI::dbExecute(con, "CREATE TEMP SEQUENCE share_probe_seq")
  extra <- switch(kind,
    sqlite = list(volatile = quote(sum(x, na.rm = TRUE) + dbplyr::sql("1.0+ABS(RANDOM()%999)"))),
    duckdb = list(volatile = quote(sum(x, na.rm = TRUE) + dbplyr::sql("nextval('share_probe_seq')")),
                  highscale = quote(sum(dbplyr::sql("CAST(x / 10 AS DECIMAL(38,37))"), na.rm = TRUE))),
    postgres = list(volatile = quote(sum(x, na.rm = TRUE) + dbplyr::sql("nextval('share_probe_seq')")),
                    highscale = quote(sum(dbplyr::sql("CAST(x / 3 AS NUMERIC(100,80))"), na.rm = TRUE)))
  )
  values <- c(list(sum = quote(sum(x, na.rm = TRUE)),
                   average = quote(mean(x, na.rm = TRUE)),
                   count = quote(dplyr::n()),
                   small = quote(sum(x * 1e-300, na.rm = TRUE)),
                   large = quote(sum(x * 1e300, na.rm = TRUE)),
                   missing = quote(sum(x, na.rm = TRUE) * NA_real_)), extra)
  for (expr in names(values)) for (policy in c("error", "keep"))
    for (order in c("none", "first", "last")) for (fetch in c("collect", "compute")) {
      outcome <- tryCatch({
        query <- summarize_with_margins(
          lazy, value = !!values[[expr]], parent = share_of_parent(value),
          total = share_of_total(value), .by = fixed, .grouping = rollup(g, h),
          .duplicates = policy, .id = "set", .sort = order, .margin_label = NULL,
          .check_share_source = kind != "sqlite"
        )
        sql <- as.character(dbplyr::sql_render(query))
        if (kind == "duckdb") stopifnot(grepl("GROUPING SETS", sql, fixed = TRUE) ==
          (policy == "error"))
        if (fetch == "compute") {
          result <- dplyr::collect(dplyr::compute(query, name = "frozen"))
          DBI::dbExecute(con, "DROP TABLE frozen")
        } else result <- dplyr::collect(query)
        agree(result)
        TRUE
      }, error = identity)
      if (isTRUE(outcome)) counts["ok"] <- counts["ok"] + 1 else {
        counts["fail"] <- counts["fail"] + 1
        cat(paste(kind, expr, policy, order, fetch), "FAIL", conditionMessage(outcome), "\n")
        try(DBI::dbExecute(con, "DROP TABLE IF EXISTS frozen"), silent = TRUE)
      }
    }
}
print(counts)
for (con in connections) DBI::dbDisconnect(con)
stopifnot(counts[["ok"]] == 276L, counts[["fail"]] == 0L)
