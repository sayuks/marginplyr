pkgload::load_all(quiet = TRUE)
source("investigation/2026-11-03-summary-grouping-share-bug-hunt/backend/postgres-fixture.R")
with_postgres(function(con) {
  cat(DBI::dbGetQuery(con, "SELECT version()")[[1L]], "\n")
  cat("max_identifier_length:", DBI::dbGetQuery(con, "SHOW max_identifier_length")[[1L]], "\n")
  source <- dplyr::copy_to(con, data.frame(g = c("a", "b"), x = c(1, 3)), "facts")
  rows <- list()
  for (alphabet in c("ascii", "utf8")) {
    stem <- if (alphabet == "ascii") strrep("s", 35L) else strrep("計", 12L)
    long <- paste0(stem, c("a", "b"))
    stopifnot(all(nchar(long, type = "bytes") < 63L))
    for (short in c(FALSE, TRUE)) {
      nms <- if (short) c("first", "second") else long
      ordinary <- setNames(list(rlang::expr(sum(x, na.rm = TRUE)),
        rlang::expr(2L * sum(x, na.rm = TRUE))), nms)
      ordinary_control <- dplyr::collect(dplyr::summarise(source, !!!ordinary))
      stopifnot(identical(names(ordinary_control), nms),
        isTRUE(all.equal(as.numeric(ordinary_control[1, ]), c(4, 8))))
      for (kind in c("total", "parent")) {
        for (count in 1:2) {
          for (portable in c(FALSE, TRUE)) {
            shares <- setNames(lapply(nms[seq_len(count)], function(nm) {
              if (kind == "total") rlang::expr(share_of_total(!!rlang::sym(nm)))
              else rlang::expr(share_of_parent(!!rlang::sym(nm)))
            }), paste0("fraction", seq_len(count)))
            query <- summarize_with_margins(source, !!!ordinary, !!!shares,
              .grouping = rollup(g), .id = "set", .margin_label = NULL,
              .duplicates = if (portable) "keep" else "error")
            value <- tryCatch(dplyr::collect(query), error = identity)
            pass <- !inherits(value, "error")
            if (pass) {
              for (i in seq_len(count)) stopifnot(isTRUE(all.equal(
                sort(value[[paste0("fraction", i)]]), c(.25, .75, 1))))
            }
            rows[[length(rows) + 1L]] <- data.frame(alphabet, short, kind, count,
              portable, pass, condition = if (pass) "pass" else conditionMessage(value))
          }
        }
      }
    }
  }
  results <- do.call(rbind, rows)
  # The report itself detects drift in the investigated boundary and controls.
  expected <- results$short | results$count == 1L
  stopifnot(identical(results$pass, expected))
  cat("cases:", nrow(results), "failures:", sum(!results$pass),
    "successful controls:", sum(results$pass), "\n")
  print(results[, setdiff(names(results), "condition")], row.names = FALSE)
  cat("First failure:\n", results$condition[which(!results$pass)[[1L]]], "\n")
  utils::write.csv(results, "investigation/2026-11-03-summary-grouping-share-bug-hunt/backend/postgres-identifier-reproduction.csv", row.names = FALSE)
  if ("--assert" %in% commandArgs(TRUE)) stopifnot(all(results$pass))
})
