# Run from the source root. --assert requires the documented numeric contract.
pkgload::load_all(quiet = TRUE)
run <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  sql_types <- c(
    decimal38 = "DECIMAL(38,0)",
    decimal37 = "DECIMAL(37,0)",
    decimal_scale1 = "DECIMAL(38,1)",
    hugeint = "HUGEINT",
    uhugeint = "UHUGEINT",
    double = "DOUBLE"
  )
  sql_values <- c(
    "99999999999999999999999999999999999999",
    "9999999999999999999999999999999999999",
    "9999999999999999999999999999999999999.9",
    "170141183460469231731687303715884105727",
    "340282366920938463463374607431768211455",
    "1e38"
  )
  names(sql_values) <- names(sql_types)
  result <- character()
  for (type in names(sql_types)) {
    row <- paste0("CAST('", sql_values[[type]], "' AS ", sql_types[[type]], ")")
    small <- paste0("CAST(1 AS ", sql_types[[type]], ")")
    table_sql <- paste("SELECT * FROM (VALUES ('a',", row, "), ('b',", small,
                       ")) AS t(g, x)")
    source <- dplyr::tbl(con, dbplyr::sql(table_sql), vars = c("g", "x"))
    for (duplicates in c("error", "keep")) {
      base <- summarize_with_margins(
        source, value = max(x, na.rm = TRUE), .grouping = rollup(g),
        .duplicates = duplicates, .margin_label = NULL, .id = "set"
      ) |>
        dplyr::collect() |>
        dplyr::arrange(set, g)
      stopifnot(identical(base$set, c(1L, 1L, 2L)))
      expected <- c(1, 1 / as.double(sql_values[[type]]), 1)
      for (kind in c("parent", "total")) {
        helper <- rlang::call2(paste0("share_of_", kind), quote(value))
        query <- summarize_with_margins(
          source, value = max(x, na.rm = TRUE), ratio = !!helper,
          .grouping = rollup(g), .duplicates = duplicates,
          .margin_label = NULL, .id = "set"
        )
        sql <- as.character(dbplyr::sql_render(query))
        stopifnot(grepl("GROUPING SETS", sql, fixed = TRUE) ==
                    (duplicates == "error"))
        got <- tryCatch(dplyr::collect(query), error = identity)
        if (inherits(got, "error")) {
          stopifnot(grepl("DECIMAL(38,1)", conditionMessage(got), fixed = TRUE))
          stopifnot(grepl("out of range|Conversion Error", conditionMessage(got)))
          stopifnot(grepl("denominator_of_value", conditionMessage(got), fixed = TRUE))
          verdict <- "overflow"
        } else {
          got <- dplyr::arrange(got, set, g)
          stopifnot(identical(got[names(base)], base))
          stopifnot(is.double(got$ratio), all(got$ratio > 0))
          stopifnot(isTRUE(all.equal(got$ratio / expected, rep(1, 3))))
          verdict <- "matches"
        }
        result <- c(result, verdict)
        cat(type, duplicates, kind, verdict, "\n")
      }
    }
    root <- summarize_with_margins(
      source, value = max(x, na.rm = TRUE),
      ratio = share_of_total(value), .grouping = grouping_set()
    ) |>
      dplyr::collect()
    stopifnot(identical(root$ratio, 1))
  }
  # Isolate the denominator conversion from the rest of marginplyr's query.
  comparison <- DBI::dbGetQuery(con, paste0(
    "SELECT CAST('", sql_values[["decimal38"]], "' AS DECIMAL(38,0)) = 0 AS zero"
  ))
  stopifnot(identical(comparison$zero, FALSE))
  problematic <- tryCatch(DBI::dbGetQuery(con, paste0(
    "SELECT x = 0.0 AS zero FROM (VALUES (CAST('", sql_values[["decimal38"]],
    "' AS DECIMAL(38,0)))) AS t(x)"
  )), error = identity)
  stopifnot(inherits(problematic, "error"),
            grepl("DECIMAL(38,1)", conditionMessage(problematic), fixed = TRUE))
  print(table(result))
  if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
    stopifnot(all(result == "matches"))
  } else {
    stopifnot(sum(result == "overflow") == 12L,
              sum(result == "matches") == 12L)
  }
}
run()
