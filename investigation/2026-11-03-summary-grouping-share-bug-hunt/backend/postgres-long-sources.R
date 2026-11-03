pkgload::load_all(quiet = TRUE)
source("investigation/2026-11-03-summary-grouping-share-bug-hunt/backend/postgres-fixture.R")
options(dplyr.summarise.inform = FALSE)
with_postgres(function(con) {
  cat(DBI::dbGetQuery(con, "SELECT version()")[[1L]], "\n")
  source <- dplyr::copy_to(con, data.frame(g = c("a", "b"), x = c(1, 3)), "facts")
  rows <- list()
  for (length in c(1L, 30L, 35L, 36L, 37L, 40L, 50L)) {
    for (count in 1:2) {
      ordinary_names <- paste0(strrep("s", length), seq_len(count))
      ordinary <- setNames(rep(list(rlang::expr(sum(x))), count), ordinary_names)
      shares <- setNames(lapply(ordinary_names, function(nm) rlang::expr(share_of_total(!!rlang::sym(nm)))), paste0("share", seq_len(count)))
      ordinary_control <- tryCatch(dplyr::collect(dplyr::summarise(source, !!!ordinary)), error = identity)
      query <- tryCatch(summarize_with_margins(source, !!!ordinary, !!!shares,
        .grouping = rollup(g), .id = "set"), error = identity)
      value <- if (inherits(query, "error")) query else tryCatch(dplyr::collect(query), error = identity)
      if (!inherits(value, "error")) {
        stopifnot(nrow(value) == 3L)
        for (i in seq_len(count)) stopifnot(isTRUE(all.equal(sort(value[[paste0("share", i)]]), c(0.25, 0.75, 1))))
      }
      rows[[length(rows) + 1L]] <- data.frame(source_bytes = length + 1L, sources = count,
        ordinary = if (inherits(ordinary_control, "error")) conditionMessage(ordinary_control) else "pass",
        margin = if (inherits(value, "error")) conditionMessage(value) else "pass")
    }
  }
  results <- do.call(rbind, rows)
  print(results, row.names = FALSE)
  utils::write.csv(results, "investigation/2026-11-03-summary-grouping-share-bug-hunt/backend/postgres-long-sources.csv", row.names = FALSE)
  if ("--assert" %in% commandArgs(TRUE)) stopifnot(all(results$margin == "pass"))
})
