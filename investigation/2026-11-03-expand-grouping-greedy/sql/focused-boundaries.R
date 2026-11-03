Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
outdir <- "investigation/2026-11-03-expand-grouping-greedy/sql"
attempt <- function(code) tryCatch(list(ok = TRUE, value = force(code)),
  error = function(e) list(ok = FALSE, error = conditionMessage(e)))
opts <- dbplyr::sql_options(cte = TRUE)
local({
  con <- DBI::dbConnect(RSQLite::SQLite())
  on.exit(DBI::dbDisconnect(con))
  rows <- list()
  for (n in c(0L, 1L, 3L)) for (nm in c("TRUE", "fAlSe", "ordinary")) {
    dat <- tibble(g = rep(c("b", "a", NA_character_), length.out = n),
                  k = rep(c("x", "y", NA_character_), length.out = n), rid = seq_len(n))
    for (role in c("dimension", "fixed", "payload", "id")) {
      source <- dat
      if (role != "id") names(source)[match(switch(role, dimension = "g", fixed = "k", payload = "rid"), names(source))] <- nm
      DBI::dbWriteTable(con, "src", as.data.frame(source), overwrite = TRUE)
      src <- tbl(con, "src")
      g <- if (role == "dimension") nm else "g"
      k <- if (role == "fixed") nm else "k"
      control <- attempt(collect(filter(src, TRUE), sql_options = opts))
      stopifnot(control$ok, identical(names(control$value), names(source)))
      for (sort in c("none", "first", "last")) for (label in list(NULL, "Total")) {
        for (kind in c("single", "rollup")) {
          spec <- if (kind == "single") grouping_set(all_of(g)) else rollup(all_of(g))
          x <- expand_with_margins(src, .by = all_of(k), .grouping = spec,
                                   .margin_label = label, .sort = sort,
                                   .id = if (role == "id") nm else "sid")
          direct <- collect(x)
          cte <- attempt(collect(x, sql_options = opts))
          raw <- attempt(DBI::dbGetQuery(con, dbplyr::sql_render(x, sql_options = opts)))
          folded <- attempt(collect(collapse(x)))
          statuses <- c(cte = if (!cte$ok) "error" else if (!identical(names(cte$value), names(direct))) "schema_difference" else "match",
            collapse = if (!folded$ok) "error" else if (!identical(names(folded$value), names(direct))) "schema_difference" else "match")
          for (boundary in names(statuses)) rows[[length(rows) + 1L]] <- data.frame(n, name = nm, role, sort,
            label = if (is.null(label)) "null" else "text", kind, boundary, status = statuses[[boundary]],
            raw_names = if (raw$ok) paste(names(raw$value), collapse = "|") else raw$error,
            detail = if (boundary == "cte" && !cte$ok) cte$error else if (boundary == "collapse" && !folded$ok) folded$error else "")
        }
      }
    }
  }
  res <- do.call(rbind, rows)
  write.csv(res, file.path(outdir, "focused-boundaries.csv"), row.names = FALSE)
  print(with(res, table(boundary, status)))
  print(with(res[res$boundary == "cte", ], table(name, sort, status)))
  print(with(res[res$boundary == "collapse", ], table(sort, status)))
})
