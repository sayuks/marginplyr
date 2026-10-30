# Bounded observational sweep; the CSV distinguishes recursion and false refusal.
suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
output <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", unset = tempdir())
dir.create(output, recursive = TRUE, showWarnings = FALSE)

capture <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    tryCatch(force(expr), error = identity),
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warnings)
}

families <- c("Date", "POSIXct", "difftime", "Surv", "factor", "integer",
              "units", "integer64", "table")
results <- list()
index <- 0L
for (family in families) {
  for (size in c(0L, 1L, 3L)) {
    for (tail in list(1L, 2L, c(1L, 2L))) {
      if (family == "Surv" && !identical(tail, 2L)) next
      shape <- c(size, tail)
      values <- rep(c(2, NA, 1), length.out = prod(shape))
      key <- switch(
        family,
        Date = as.Date(values, origin = "2026-01-01"),
        POSIXct = as.POSIXct(values, origin = "2026-01-01", tz = "UTC"),
        difftime = as.difftime(values, units = "hours"),
        Surv = survival::Surv(rep(1, size), rep(TRUE, size)),
        factor = factor(values, levels = c(1, 2, 3)),
        integer = as.integer(values),
        units = units::set_units(values, "m"),
        integer64 = bit64::as.integer64(values),
        table = as.table(array(as.integer(values), dim = shape))
      )
      if (!(family %in% c("Surv", "table"))) dim(key) <- shape
      for (packed in c(FALSE, TRUE)) {
        dimension <- if (packed) tibble::tibble(inner = key) else key
        for (label_name in c("NULL", "NA_character_")) {
          label <- if (label_name == "NULL") NULL else NA_character_
          input <- tibble::tibble(key = dimension, rid = seq_len(size))
          control <- capture({
            result <- dplyr::union_all(
              dplyr::mutate(input, sid = 1L),
              dplyr::mutate(
                input, key = vctrs::vec_init(dimension, size), sid = 2L
              )
            )
            dplyr::relocate(result, sid, .after = key)
          })
          actual <- capture(expand_with_margins(
            input, .grouping = rollup(key), .margin_label = label,
            .id = "sid", .sort = "none"
          ))
          included <- capture(expand_with_margins(
            input, .grouping = grouping_set(key), .margin_label = label
          ))
          condition <- actual$value
          verdict <- if (inherits(control$value, "error")) {
            "ordinary_error"
          } else if (inherits(condition, "expressionStackOverflowError")) {
            "recursion"
          } else if (inherits(condition, "marginplyr_error")) {
            "package_refusal"
          } else if (inherits(condition, "error")) {
            "external_error"
          } else if (identical(condition, control$value)) {
            "match"
          } else {
            "difference"
          }
          index <- index + 1L
          results[[index]] <- data.frame(
            family = family, size = size, tail = paste(tail, collapse = "x"),
            packed = packed, label = label_name, verdict = verdict,
            included = if (identical(included$value, input)) "match" else "other",
            control = if (inherits(control$value, "error")) "error" else "success",
            warning_count = length(actual$warnings),
            detail = if (inherits(condition, "error")) {
              paste(class(condition)[[1L]], conditionMessage(condition))
            } else {
              ""
            }
          )
        }
      }
    }
  }
}
results <- do.call(rbind, results)
print(with(results, table(family, verdict)))
print(with(results, table(included, control)))
write.csv(results, file.path(output, "classed-structured-matrix.csv"),
          row.names = FALSE)
stopifnot(nrow(results) == 300L, all(results$control == "success"),
          all(results$included == "match"))
