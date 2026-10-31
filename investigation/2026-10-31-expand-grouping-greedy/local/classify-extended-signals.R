# The exploratory sweep's unsorted value oracle did not test ordinary sorting.
# Check that boundary separately before treating its sorted errors as findings.
devtools::load_all(quiet = TRUE)
output <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempfile("extended-classify-"))
dir.create(output, recursive = TRUE, showWarnings = FALSE)
signals <- read.csv("investigation/2026-10-31-expand-grouping-greedy/local/extended-types-results.csv")
signals <- signals[signals$status == "package_error", ]
results <- lapply(seq_len(nrow(signals)), function(i) {
  row <- signals[i, ]
  key <- if (row$family == "matrix_date") {
    structure(c(1, NA, 2, 3, 4, NA), dim = c(3L, 2L), class = "Date")
  } else {
    structure(c(1L, NA_integer_, 2L, 2L, 1L, NA_integer_),
              dim = c(3L, 2L), levels = c("a", "b", NA), class = "factor")
  }
  key <- vctrs::vec_slice(key, seq_len(row$n))
  if (row$packed) key <- tibble::tibble(inner = key)
  input <- tibble::tibble(g = key, row = seq_len(row$n))
  ordinary <- tryCatch(dplyr::arrange(input, .data$g), error = identity)
  data.frame(row, ordinary_error = inherits(ordinary, "error"),
             ordinary_class = class(ordinary)[[1L]],
             ordinary_detail = if (inherits(ordinary, "error")) conditionMessage(ordinary) else "")
})
classified <- do.call(rbind, results)
write.csv(classified, file.path(output, "extended-sorting-controls.csv"), row.names = FALSE)
print(with(classified, table(family, ordinary_error)))
stopifnot(nrow(classified) == 48L, all(classified$ordinary_error))
cat("All 48 sorted-operation signals also failed ordinary class-key arrange.\n")
