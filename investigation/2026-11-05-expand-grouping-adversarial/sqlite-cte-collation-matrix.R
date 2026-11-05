#!/usr/bin/env Rscript
# Run from the investigated worktree root.
pkgload::load_all(quiet = TRUE)

main <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite())
  on.exit(DBI::dbDisconnect(con))
  for (collation in c("NOCASE", "BINARY")) {
    table <- paste0("source_", tolower(collation))
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", table, " (g TEXT COLLATE ", collation, ", payload INT)"
    ))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", table, " VALUES ('B', 2), ('a', 1)"
    ))
  }
  cases <- expand.grid(
    collation = c("NOCASE", "BINARY"), source = c("root", "filter"),
    role = c("dimension", "fixed"), family = c("included", "rollup"),
    label = c("null", "text"), identifier = c("absent", "ordinary", "boolean"),
    sort = c("last", "first"), stringsAsFactors = FALSE
  )
  opts <- dbplyr::sql_options(cte = TRUE)
  cases$default <- cases$cte <- cases$ordinary_cte <- cases$schema <- FALSE
  for (i in seq_len(nrow(cases))) {
    row <- cases[i, ]
    source <- dplyr::tbl(con, paste0("source_", tolower(row$collation)))
    if (row$source == "filter") {
      source <- dplyr::filter(source, payload > 0L)
    }
    group <- switch(row$role,
      dimension = if (row$family == "rollup") rollup(g) else grouping_set(g),
      fixed = if (row$family == "rollup") rollup(payload) else grouping_set(payload)
    )
    by <- if (row$role == "fixed") rlang::sym("g") else NULL
    label <- if (row$label == "text") "Total" else NULL
    id <- switch(row$identifier, absent = NULL, ordinary = "occurrence",
                 boolean = "TRUE")
    result <- expand_with_margins(
      source, .by = !!by, .grouping = group, .margin_label = label,
      .id = id, .sort = row$sort
    )
    # Only compare relative order of non-Margin g values. NOCASE and BINARY
    # have distinct orderings; each source g has one payload row.
    expected <- if (row$collation == "NOCASE") c("a", "B") else c("B", "a")
    levels_in <- function(x) unique(x$g[x$g %in% expected])
    ordinary <- dplyr::arrange(dplyr::filter(source, !is.na(g)), g)
    default <- dplyr::collect(result)
    cte <- dplyr::collect(result, sql_options = opts)
    cases$default[i] <- identical(levels_in(default), expected)
    cases$cte[i] <- identical(levels_in(cte), expected)
    cases$ordinary_cte[i] <- identical(dplyr::collect(ordinary, sql_options = opts)$g,
                                       expected)
    canonical <- function(out) {
      out <- as.data.frame(out)
      out <- out[do.call(order, out), , drop = FALSE]
      row.names(out) <- NULL
      out
    }
    cases$schema[i] <- identical(canonical(default), canonical(cte))
  }
  stopifnot(all(cases$default), all(cases$ordinary_cte), all(cases$schema))
  print(with(cases, table(collation, identifier, cte)))
  cat("cases", nrow(cases), "direct CTE order mismatches", sum(!cases$cte), "\n")
  print(with(cases[!cases$cte, ], table(role, family, sort)))
}
main()
