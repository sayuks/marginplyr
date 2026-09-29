args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
control <- readRDS(args[[1L]])
candidate <- readRDS(args[[2L]])
stopifnot(identical(names(control), names(candidate)))
rows <- lapply(names(control), function(name) {
  a <- control[[name]]
  b <- candidate[[name]]
  both_ok <- isTRUE(a$ok) && isTRUE(b$ok)
  both_error <- identical(a$ok, FALSE) && identical(b$ok, FALSE)
  av <- a$value
  bv <- b$value
  data.frame(
    case = name,
    control_status = if (a$ok) "ok" else "error",
    candidate_status = if (b$ok) "ok" else "error",
    values_identical = if (both_ok) identical(av, bv) else NA,
    columns_identical = if (both_ok && is.data.frame(av) && is.data.frame(bv)) identical(names(av), names(bv)) else NA,
    row_counts_identical = if (both_ok && is.data.frame(av) && is.data.frame(bv)) identical(nrow(av), nrow(bv)) else NA,
    types_identical = if (both_ok && is.data.frame(av) && is.data.frame(bv)) identical(vapply(av, typeof, character(1)), vapply(bv, typeof, character(1))) else NA,
    error_class_identical = if (both_error) identical(a$class, b$class) else NA,
    diagnostic_identical = if (both_error) identical(a$message, b$message) else NA,
    warnings_identical = identical(a$warnings, b$warnings)
  )
})
result <- do.call(rbind, rows)
write.csv(result, args[[3L]], row.names = FALSE, na = "")
print(result, row.names = FALSE)
stopifnot(all(result$control_status == result$candidate_status))
for (column in c("values_identical", "columns_identical", "row_counts_identical", "types_identical", "error_class_identical", "diagnostic_identical", "warnings_identical")) {
  stopifnot(all(result[[column]][!is.na(result[[column]])]))
}
