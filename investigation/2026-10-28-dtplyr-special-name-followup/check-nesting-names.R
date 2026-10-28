# Dated public-verb audit of column spellings at the PR #961 snapshot.
pkgload::load_all(".", quiet = TRUE)

candidates <- unique(c(
  ".N", ".I", ".SD", ".GRP", ".NGRP", ".BY", ".EACHI", ".NATURAL", ".ROW",
  ".SDall", ".xSD", ".iSD", ".Last.updated", ".SDcols", ".internal.selfref",
  ".iGRP", ".xGRP", "..FUN1", "..FUN2", "..value", "..1", "...",
  "i.value", "x.value", "key", "keep.rownames", "check.names", "stringsAsFactors",
  ".named", "x", "i", "j", "by", "keyby", "with", "nomatch", "mult", "roll",
  "rollends", "which", "verbose", "allow.cartesian", "drop", "on", "env",
  "showProgress", "T", "F", "get", "mget", "eval", "list", ".",
  "_nqgrp_", ".ll.tech.split", ".nm.tech.split", ".ll.tech.split.names",
  ".data.table.locked", "sorted", "unique"
))
requested <- commandArgs(trailingOnly = TRUE)
if (length(requested) > 0L) candidates <- requested
output_name <- if (length(requested) > 0L) {
  "supplemental-results.csv"
} else {
  "nesting-results.csv"
}

# Compare typed outer columns and full cell row multisets across backends.
canonical <- function(result, key) {
  result <- dplyr::ungroup(result)
  outer <- setdiff(names(result), key)
  list(
    names = names(result),
    outer = as.list(result[outer]),
    cells = lapply(result[[key]], function(cell) {
      cell <- tibble::as_tibble(cell, .name_repair = "minimal")
      if (ncol(cell) > 0L) cell <- cell[vctrs::vec_order(cell), , drop = FALSE]
      list(names = names(cell), rows = nrow(cell), values = as.list(cell))
    })
  )
}

attempt <- function(expr) {
  warnings <- character()
  answer <- tryCatch(withCallingHandlers(
    list(value = force(expr)),
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  ), error = function(cnd) list(error = conditionMessage(cnd)))
  answer$warnings <- warnings
  answer
}

results <- list()
for (column in candidates) {
  for (size in c(0L, 1L, 3L)) {
    for (role in c("payload", "fixed", "dimension", "id", "key")) {
      input <- tibble::tibble(
        g = rep(c("a", NA_character_, "a"), length.out = size),
        value = rep(c(1L, 2L, 1L), length.out = size),
        stamp = as.Date("2026-10-28") + rep(c(0L, 1L, 0L), length.out = size),
        ..marginplyr_dtplyr_column_1 = rep(4L, size),
        ..marginplyr_dtplyr_column_1_ = rep(5L, size)
      )
      if (role %in% c("payload", "key")) names(input)[2L] <- column
      if (role %in% c("fixed", "dimension")) names(input)[1L] <- column
      # Construct before assigning the audited name, including `.SD`.
      safe_input <- input
      names(safe_input) <- paste0("column", seq_along(input))
      table <- data.table::as.data.table(safe_input)
      data.table::setnames(table, names(input))
      original <- data.table::copy(table)
      source <- dtplyr::lazy_dt(table, immutable = TRUE)
      source_control <- attempt(dplyr::collect(source))
      source_equal <- is.null(source_control$error) &&
        identical(source_control$value, input)
      grouping <- if (role == "dimension") {
        rollup(dplyr::all_of(column))
      } else if (role == "fixed") {
        grouping_set()
      } else {
        rollup(g)
      }
      key <- if (role == "key") column else "data"
      for (keep in c(FALSE, TRUE)) {
        for (verb_name in c("nest_with_margins", "nest_by_with_margins")) {
          verb <- get(verb_name)
          run <- function(data) {
            result <- verb(
              data, .by = if (role == "fixed") dplyr::all_of(column),
              .grouping = grouping, .keep = keep, .key = key,
              .id = if (role == "id") column else "set", .sort = "last"
            )
            if (identical(verb_name, "nest_with_margins") &&
                inherits(data, "dtplyr_step")) {
              stopifnot(inherits(result, "dtplyr_step"))
              result <- dplyr::collect(result)
            }
            if (identical(verb_name, "nest_by_with_margins")) {
              stopifnot(inherits(result, "rowwise_df"))
            }
            canonical(result, key)
          }
          expected <- attempt(run(input))
          actual <- attempt(run(source))
          status <- if (!is.null(expected$error)) {
            "local-control-error"
          } else if (!is.null(actual$error)) {
            "dtplyr-error"
          } else if (!identical(actual$value, expected$value)) {
            "different-values"
          } else if (length(actual$warnings) > 0L) {
            "dtplyr-warning"
          } else {
            "pass"
          }
          if (!identical(table, original)) status <- "source-changed"
          results[[length(results) + 1L]] <- data.frame(
            column, size, role, keep, verb = verb_name, status, source_equal,
            source_error = if (is.null(source_control$error)) "" else source_control$error,
            local_error = if (is.null(expected$error)) "" else expected$error,
            dtplyr_error = if (is.null(actual$error)) "" else actual$error,
            warnings = paste(actual$warnings, collapse = " | ")
          )
        }
      }
    }
  }
  cat("Completed", column, "\n")
  flush.console()
}
results <- do.call(rbind, results)
utils::write.csv(results,
  file.path("investigation/2026-10-28-dtplyr-special-name-followup", output_name),
  row.names = FALSE)
print(table(results$status))
print(with(results[results$status != "pass", ], table(column, status)))
cat("Candidates:", length(candidates), "; nesting calls:", nrow(results), "\n")
cat("R:", R.version.string, "\n")
cat("data.table:", as.character(utils::packageVersion("data.table")), "\n")
cat("dtplyr:", as.character(utils::packageVersion("dtplyr")), "\n")
