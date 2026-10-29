# Scalar translation and ordinary-output controls for the dated SQL finding.
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
connections <- list(
  sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
  duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
)
for (backend in names(connections)) {
  con <- connections[[backend]]
  if (backend == "duckdb") invisible(DBI::dbExecute(con, "SET threads = 1"))
  input <- copy_to(con, tibble(g = c("a", "b"), v = c(1, 3)), "signed_reference")
  for (name in c("..+0", "..+1", ".. 1", "..-1", ".env")) {
    cat("NAME", backend, encodeString(name), "predicate",
        utils::getFromNamespace("sql_name_needs_quoting", "marginplyr")(name), "\n")
    for (kind in c("bare", "identifier")) {
      expression <- if (kind == "bare") rlang::sym(name) else dbplyr::ident(name)
      result <- tryCatch(dbplyr::translate_sql(!!expression, con = con), error = identity)
      cat("TRANSLATION", kind,
          if (inherits(result, "error")) conditionMessage(result) else as.character(result), "\n")
    }
    dots <- setNames(list(rlang::quo(sum(v, na.rm = TRUE))), name)
    ordinary <- summarise(group_by(input, g), !!!dots, .groups = "drop")
    result <- collect(arrange(ordinary, g))
    stopifnot(identical(names(result), c("g", name)),
              isTRUE(all.equal(result[[name]], c(1, 3), check.attributes = FALSE)))
    cat("ORDINARY_COLLECT", backend, encodeString(name), "OK: 1,3\n")
    if (name != ".env") {
      materialized <- collect(arrange(compute(ordinary), g))
      stopifnot(identical(names(materialized), c("g", name)),
                isTRUE(all.equal(materialized[[name]], c(1, 3), check.attributes = FALSE)))
      cat("ORDINARY_COMPUTE", backend, encodeString(name), "OK: 1,3\n")
    }
  }
  modes <- if (backend == "duckdb") c("drop", "keep") else "drop"
  for (duplicates in modes) {
    safe <- summarize_with_margins(input, amount = sum(v, na.rm = TRUE),
                                   .grouping = rollup(g), .duplicates = duplicates, .id = "sid")
    sql <- as.character(dbplyr::sql_render(safe))
    expected_adapter <- if (backend == "duckdb" && duplicates == "drop") {
      "GROUPING SETS"
    } else {
      "UNION ALL"
    }
    stopifnot(grepl(expected_adapter, sql, fixed = TRUE))
    cat("ADAPTER", backend, duplicates, expected_adapter, "OK\n")
    for (name in c("..+0", "..+1", ".. 1", "..-1")) {
      dots <- setNames(list(rlang::quo(sum(v, na.rm = TRUE))), name)
      dots[["fraction"]] <- rlang::new_quosure(
        rlang::call2("share_of_parent", rlang::sym(name)), globalenv()
      )
      result <- tryCatch(collect(summarize_with_margins(
        input, !!!dots, .grouping = rollup(g), .duplicates = duplicates, .sort = "last", .id = "sid",
        .check_share_source = FALSE
      )), error = identity)
      cat("PARENT_SHARE", backend, duplicates, encodeString(name),
          if (inherits(result, "error")) conditionMessage(result) else
            paste(result$fraction, collapse = ","), "\n")
      if (name == "..-1") stopifnot(identical(result$fraction, c(0.25, 0.75, 1)))
    }
  }
  DBI::dbDisconnect(con)
}
