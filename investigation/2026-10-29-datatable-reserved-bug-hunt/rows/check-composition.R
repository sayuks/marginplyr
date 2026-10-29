# Dated interactions among caller spellings, generated aliases, and Margin results.
pkgload::load_all(".", quiet = TRUE)
helper_source <- parse("investigation/2026-10-29-datatable-reserved-bug-hunt/rows/check-row-verbs.R")
for (expression in helper_source) {
  if (is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
      is.symbol(expression[[2L]]) && as.character(expression[[2L]]) %in% c("attempt", "canonical")) eval(expression)
}

special <- c(".N", ".I", ".SD", ".GRP", ".NGRP", ".BY", ".EACHI", ".NATURAL", ".ROW", ".SDall", ".xSD", ".iSD")
results <- list()
record <- function(kind, spelling, detail, expected, actual, intact) {
  status <- if (!is.null(expected$error)) "local-control-error" else if (!is.null(actual$error)) "dtplyr-error" else if (!identical(actual$value, expected$value)) "different-values" else if (length(actual$warnings)) "dtplyr-warning" else "pass"
  if (!intact) status <- "source-changed"
  results[[length(results) + 1L]] <<- data.frame(kind, spelling, detail, status,
    local_error = if (is.null(expected$error)) "" else expected$error,
    dtplyr_error = if (is.null(actual$error)) "" else actual$error,
    warnings = paste(actual$warnings, collapse = " | "))
}

# Twelve spellings simultaneously occupy every user-visible name role.
for (offset in seq_along(special)) {
  rotated <- special[(seq_along(special) + offset - 2L) %% length(special) + 1L]
  fixed <- rotated[[1L]]
  dimensions <- rotated[2:3]
  id <- rotated[[4L]]
  key <- rotated[[5L]]
  columns <- setdiff(rotated, id)
  input <- tibble::as_tibble(stats::setNames(lapply(seq_along(columns), function(index) {
    if (columns[[index]] %in% c(fixed, dimensions)) c("b", "a", NA_character_, "a") else c(1L, 3L, 2L, 3L)
  }), columns), .name_repair = "minimal")
  for (alias in c("..marginplyr_dtplyr_column_1", "..marginplyr_dtplyr_column_1_", "..marginplyr_cell_1", "..marginplyr_nest_1")) input[[alias]] <- seq_len(nrow(input))
  safe <- input
  names(safe) <- paste0("column", seq_along(safe))
  table <- data.table::as.data.table(safe)
  data.table::setnames(table, names(input))
  before <- data.table::copy(table)
  source <- dtplyr::lazy_dt(table, immutable = TRUE)
  for (verb_name in c("expand_with_margins", "nest_with_margins", "nest_by_with_margins")) {
    for (sort in c("none", "first", "last")) {
      keeps <- if (verb_name == "expand_with_margins") FALSE else c(FALSE, TRUE)
      for (keep in keeps) {
        run <- function(data) {
          verb <- get(verb_name)
          common <- list(.data = data, .by = rlang::expr(dplyr::all_of(!!fixed)),
            .grouping = rollup(dplyr::all_of(dimensions)), .id = id, .sort = sort)
          if (verb_name != "expand_with_margins") {
            common$.key <- key
            common$.keep <- keep
          }
          answer <- rlang::eval_tidy(rlang::expr((!!verb)(!!!common)))
          if (inherits(answer, "dtplyr_step")) answer <- dplyr::collect(answer)
          canonical(answer, if (verb_name != "expand_with_margins") key)
        }
        record("simultaneous", fixed, paste(verb_name, sort, keep), attempt(run(input)), attempt(run(source)), identical(table, before))
      }
    }
  }
}

# Each Margin result is collected explicitly only for the local comparison.
# Both inputs contain the same backend-produced nested-cell classes.
for (name in c(special, "ordinary")) {
  input <- tibble::tibble(g = c("b", "a", "a"), value = c(1L, 2L, 2L))
  names(input)[1L] <- name
  source <- dtplyr::lazy_dt(input, immutable = TRUE)
  for (first_name in c("expand_with_margins", "nest_with_margins")) {
    first <- get(first_name)(source, .grouping = rollup(dplyr::all_of(name)), .sort = "last", .id = "set1")
    collected <- dplyr::collect(first)
    for (second_name in c("expand_with_margins", "nest_with_margins", "nest_by_with_margins")) {
      for (keep in if (second_name == "expand_with_margins") FALSE else c(FALSE, TRUE)) {
        run <- function(data) {
          verb <- get(second_name)
          common <- list(.data = data, .grouping = rollup(dplyr::all_of(name)),
            .margin_label = NULL, .id = "set2", .sort = "last", .check_margin_label = FALSE)
          if (second_name != "expand_with_margins") {
            common$.key <- "payload"
            common$.keep <- keep
          }
          answer <- do.call(verb, common)
          if (inherits(answer, "dtplyr_step")) answer <- dplyr::collect(answer)
          canonical(answer, if (second_name != "expand_with_margins") "payload")
        }
        record("composition-dimension", name, paste(first_name, second_name, keep), attempt(run(collected)), attempt(run(first)), identical(dplyr::collect(source), input))
      }
    }
  }
  first <- nest_with_margins(source, .grouping = rollup(dplyr::all_of(name)), .key = "nested", .sort = "last")
  collected <- dplyr::collect(first)
  for (selection in c("list", "character")) {
    run <- function(data) inspect_grouping(data, .by = if (selection == "list") dplyr::where(is.list) else dplyr::where(is.character))
    record("composition-inspection", name, selection, attempt(run(collected)), attempt(run(first)), identical(dplyr::collect(source), input))
  }
}

# A single special key leaves zero-column cells whose row multiplicities matter.
for (name in special) for (size in c(0L, 1L, 4L)) {
  input <- stats::setNames(tibble::tibble(value = rep(c("a", "a", NA_character_, "b"), length.out = size)), name)
  safe <- stats::setNames(input, "value")
  table <- data.table::as.data.table(safe)
  data.table::setnames(table, name)
  before <- data.table::copy(table)
  source <- dtplyr::lazy_dt(table)
  for (verb_name in c("nest_with_margins", "nest_by_with_margins")) {
    run <- function(data) {
      answer <- get(verb_name)(data, .grouping = rollup(dplyr::all_of(name)), .sort = "first", .keep = FALSE)
      if (inherits(answer, "dtplyr_step")) answer <- dplyr::collect(answer)
      canonical(answer, "data")
    }
    record("zero-column-cells", name, paste(size, verb_name), attempt(run(input)), attempt(run(source)), identical(table, before))
  }
}
results <- do.call(rbind, results)
utils::write.csv(results, "investigation/2026-10-29-datatable-reserved-bug-hunt/rows/composition-results.csv", row.names = FALSE)
print(table(results$kind, results$status))
print(unique(results[results$status != "pass", c("kind", "detail", "status", "dtplyr_error")]))
cat("Total calls:", nrow(results), "\n")
