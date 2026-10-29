# Dated differential audit, not a package regression test.
pkgload::load_all(".", quiet = TRUE)
candidates <- unique(c(
  ".N", ".I", ".SD", ".GRP", ".NGRP", ".BY", ".EACHI", ".NATURAL", ".ROW",
  ".SDall", ".xSD", ".iSD", ".Last.updated", ".SDcols", ".internal.selfref",
  "..FUN1", "..FUN2", "..value", "i.value", "x.value", "key", "keep.rownames",
  "check.names", "stringsAsFactors", ".named", "x", "i", "j", "by", "keyby",
  "with", "nomatch", "mult", "roll", "rollends", "which", "verbose",
  "allow.cartesian", "drop", "on", "env", "showProgress", "T", "F", "get",
  "mget", "eval", "list", ".", "_nqgrp_", ".ll.tech.split", ".nm.tech.split",
  ".ll.tech.split.names", ".data.table.locked", "sorted", "unique", "print",
  "-.POSIXt", "Cfastmean", "order", ".iGRP", ".xGRP", ".rows", "..1", "..."
))
args <- commandArgs(trailingOnly = TRUE)
phase <- if (length(args)) args[[1L]] else "baseline"
typed <- phase %in% c("typed", "typed-control")
if (typed) candidates <- if (phase == "typed-control") "ordinary" else candidates[seq_len(12L)]
attempt <- function(expr) {
  warnings <- character()
  answer <- tryCatch(withCallingHandlers(list(value = force(expr)),
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }), error = function(cnd) list(error = conditionMessage(cnd), classes = class(cnd)))
  answer$warnings <- warnings
  answer
}
canonical <- function(result, key = NULL) {
  result <- dplyr::ungroup(result)
  outer <- setdiff(names(result), key)
  ordering <- if (length(outer)) vctrs::vec_order(result[outer]) else seq_len(nrow(result))
  result <- result[ordering, , drop = FALSE]
  list(names = names(result), outer = as.list(result[outer]),
    cells = if (!is.null(key)) lapply(result[[key]], function(cell) {
      cell <- tibble::as_tibble(cell, .name_repair = "minimal")
      if (ncol(cell)) cell <- cell[vctrs::vec_order(cell), , drop = FALSE]
      list(names = names(cell), rows = nrow(cell), values = as.list(cell))
    }))
}
results <- list()
for (column in candidates) {
  types <- if (typed) c("factor", "ordered", "integer", "Date", "list") else "character"
  sizes <- if (typed) c(0L, 4L) else c(0L, 1L, 4L)
  sorts <- if (typed) "last" else c("none", "last")
  labels <- if (typed) c("missing", "named") else "named"
  for (type in types) for (size in sizes) for (label in labels) {
    if (typed && label == "named" && !(type %in% c("factor", "ordered"))) next
    g <- switch(type, character = c("b", "a", NA_character_, "a"),
      factor = factor(c("b", "a", NA_character_, "a"), levels = c("b", "a", "unused")),
      ordered = ordered(c("b", "a", NA_character_, "a"), levels = c("b", "a", "unused")),
      integer = c(2L, 1L, NA_integer_, 1L),
      Date = as.Date("2026-10-29") + c(2L, 1L, NA_integer_, 1L),
      list = list(2L, 1L, NULL, 1L))
    g <- g[seq_len(size)]
    for (role in c("payload", "fixed", "dimension", "id", "key")) {
      input <- tibble::tibble(g = g, value = rep(c(2L, 1L, 4L, 1L), length.out = size),
        h = rep(c("x", "y", "x", "y"), length.out = size),
        ..marginplyr_dtplyr_column_1 = rep(4L, size),
        ..marginplyr_dtplyr_column_1_ = rep(5L, size))
      if (role == "payload") names(input)[2L] <- column
      if (role %in% c("fixed", "dimension")) names(input)[1L] <- column
      safe_input <- input
      names(safe_input) <- paste0("column", seq_along(input))
      table <- data.table::as.data.table(safe_input)
      data.table::setnames(table, names(input))
      original <- data.table::copy(table)
      source <- dtplyr::lazy_dt(table, immutable = TRUE)
      source_control <- attempt(dplyr::collect(source))
      source_equal <- is.null(source_control$error) && identical(source_control$value, input)
      grouping <- if (role == "dimension") rollup(dplyr::all_of(column), h) else if (role == "fixed") rollup(h) else rollup(g, h)
      key <- if (role == "key") column else "data"
      for (sort in sorts) for (verb_name in c("expand_with_margins", "nest_with_margins", "nest_by_with_margins")) {
        if (verb_name == "expand_with_margins" && role == "key") next
        keeps <- if (verb_name == "expand_with_margins") FALSE else c(FALSE, TRUE)
        for (keep in keeps) {
          verb <- get(verb_name)
          run <- function(data) {
            common <- list(.data = data, .grouping = grouping,
              .margin_label = if (label == "missing" || !(type %in% c("character", "factor", "ordered"))) NULL else "Total",
              .id = if (role == "id") column else "set", .sort = sort)
            common$.by <- if (role == "fixed") rlang::expr(dplyr::all_of(!!column)) else NULL
            if (verb_name != "expand_with_margins") {
              common$.key <- key
              common$.keep <- keep
            }
            result <- rlang::eval_tidy(rlang::expr((!!verb)(!!!common)))
            lazy <- inherits(data, "dtplyr_step")
            if (lazy && verb_name != "nest_by_with_margins") stopifnot(inherits(result, "dtplyr_step"))
            if (verb_name == "nest_by_with_margins") stopifnot(inherits(result, "rowwise_df"))
            if (inherits(result, "dtplyr_step")) {
              result <- if (any(margin_dot_dot_names(margin_col_names(result)))) {
                tibble::as_tibble(result, .name_repair = "minimal")
              } else dplyr::collect(result)
            }
            canonical(result, if (verb_name != "expand_with_margins") key)
          }
          expected <- attempt(run(input))
          actual <- attempt(run(source))
          status <- if (!is.null(expected$error)) "local-control-error" else if (!is.null(actual$error)) "dtplyr-error" else if (!identical(actual$value, expected$value)) "different-values" else if (length(actual$warnings)) "dtplyr-warning" else "pass"
          if (!identical(table, original)) status <- "source-changed"
          results[[length(results) + 1L]] <- data.frame(phase, column, type, size, label, role, keep, sort, verb = verb_name,
            status, source_equal,
            source_error = if (is.null(source_control$error)) "" else source_control$error,
            local_error = if (is.null(expected$error)) "" else expected$error,
            dtplyr_error = if (is.null(actual$error)) "" else actual$error,
            warnings = paste(actual$warnings, collapse = " | "))
        }
      }
    }
  }
  utils::write.csv(do.call(rbind, results), file.path("investigation/2026-10-29-datatable-reserved-bug-hunt/rows", paste0(phase, "-results.csv")), row.names = FALSE)
  cat("Completed", column, "\n")
  flush.console()
}
results <- do.call(rbind, results)
utils::write.csv(results, file.path("investigation/2026-10-29-datatable-reserved-bug-hunt/rows", paste0(phase, "-results.csv")), row.names = FALSE)
print(table(results$status))
print(with(results[results$status != "pass", ], table(column, status)))
cat("Candidates:", length(candidates), "; calls:", nrow(results), "\n")
for (p in c("data.table", "dtplyr", "dplyr", "tidyr")) cat(p, as.character(utils::packageVersion(p)), "\n")
