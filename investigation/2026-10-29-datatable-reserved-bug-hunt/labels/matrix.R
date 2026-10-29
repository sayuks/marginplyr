# Dated collision-check audit; no package implementation is changed.
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
path <- "investigation/2026-10-29-datatable-reserved-bug-hunt/labels"
candidates <- c(
  ".SD", ".BY", ".N", ".I", ".GRP", ".NGRP", ".EACHI", ".NATURAL", ".ROW",
  ".SDall", ".xSD", ".iSD", ".Last.updated", ".SDcols", "..value", "..FUN1",
  "..FUN2", "i.value", "x.value", "_nqgrp_", ".ll.tech.split", ".nm.tech.split",
  ".ll.tech.split.names", ".internal.selfref", ".data.table.locked", ".",
  "key", "keep.rownames", "check.names", "stringsAsFactors", ".named", "sorted",
  "unique", "x", "i", "j", "by", "keyby", "with", "nomatch", "mult", "roll",
  "rollends", "which", "verbose", "allow.cartesian", "drop", "on", "env",
  "showProgress", "T", "F", "get", "mget", "eval", "list", "print", "order",
  "Cfastmean", "-.POSIXt", "plain"
)
attempt <- function(code) {
  warnings <- character()
  result <- tryCatch(withCallingHandlers({force(code); list(status = "ok", message = "")},
    warning = function(cnd) {warnings <<- c(warnings, conditionMessage(cnd)); invokeRestart("muffleWarning")}),
    error = function(cnd) list(status = if (inherits(cnd, "marginplyr_error")) "package-error" else "external-error", message = conditionMessage(cnd)))
  result$warnings <- paste(warnings, collapse = " | ")
  result
}
rows <- list()
for (name in candidates) {
  for (kind in c("character", "factor")) for (collision in c(FALSE, TRUE)) {
    input <- tibble(dimension = c("a", if (collision) "Total" else NA_character_, "a"), value = c(1L, 2L, 4L))
    if (kind == "factor") input$dimension <- factor(input$dimension, levels = c("a", if (collision) "Total" else "unused"))
    names(input)[1L] <- name
    for (verb in c("summarize_with_margins", "expand_with_margins", "nest_with_margins", "nest_by_with_margins")) {
      for (check in c(FALSE, TRUE)) {
        run <- function(data) {
          fun <- get(verb)
          grouping <- rollup(all_of(name))
          args <- list(.data = data, .grouping = grouping, .margin_label = "Total", .check_margin_label = check)
          if (grepl("summarize", verb)) args$total <- rlang::expr(sum(value))
          # Inject summary syntax only in summary calls; other arguments are values.
          result <- if (grepl("summarize", verb)) {
            rlang::inject(fun(.data = data, total = sum(value), .grouping = grouping,
                             .margin_label = "Total", .check_margin_label = check))
          } else do.call(fun, args)
          if (inherits(result, "dtplyr_step")) result <- collect(result)
          result
        }
        local <- attempt(run(input))
        table <- data.table::as.data.table(setNames(unname(as.list(input)), c("safe", "value")))
        data.table::setnames(table, names(input))
        before <- data.table::copy(table)
        actual <- attempt(run(dtplyr::lazy_dt(table, immutable = TRUE)))
        # Only the collision decision is compared here, not unrelated row ordering.
        status <- if (!identical(before, table)) "source-changed" else if (!identical(local$status, actual$status)) "different-decision" else if (actual$status == "external-error") "shared-or-external-error" else if (nzchar(actual$warnings)) "warning" else "pass"
        rows[[length(rows) + 1L]] <- data.frame(name, kind, collision, verb, check, status, local = local$status, actual = actual$status, local_message = local$message, actual_message = actual$message, warnings = actual$warnings)
      }
    }
  }
  cat("Completed", name, "\n"); flush.console()
}
results <- do.call(rbind, rows)
write.csv(results, file.path(path, "results.csv"), row.names = FALSE)
print(table(results$status))
print(results[results$status != "pass", c("name", "kind", "collision", "verb", "check", "status", "actual_message")], row.names = FALSE)
cat("CASES", nrow(results), "NAMES", length(candidates), "\n")
stopifnot(all(results$status == "pass"))
