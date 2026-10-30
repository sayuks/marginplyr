suppressPackageStartupMessages({
  pkgload::load_all(".", quiet = TRUE)
  library(dplyr)
})

path <- tempfile("arrow-virtual-fields-")
dir.create(path)
arrow::write_parquet(data.frame(g = 1:2, row_id = 1:2),
                     file.path(path, "facts.parquet"))
source <- arrow::open_dataset(path)
for (name in c("__filename", "__fragment_index", "__batch_index",
               "__last_in_fragment")) {
  result <- dplyr::collect(expand_with_margins(
    source, .grouping = rollup(g), .margin_label = NULL, .id = name
  ))
  result <- result[order(result[[name]], result$row_id), ]
  stopifnot(identical(result[[name]], c(1L, 1L, 2L, 2L)),
            identical(result$g, c(1L, 2L, NA_integer_, NA_integer_)),
            identical(result$row_id, c(1L, 2L, 1L, 2L)))
  cat("Arrow virtual scanner name:", name, "passed\n")
}
unlink(path, recursive = TRUE)

seen <- 0L
copy <- function(x) {
  seen <<- seen + 1L
  x[0L]
}
source <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), value = 1:2))
ordinary <- dplyr::collect(dplyr::mutate(source, value = value))
query <- expand_with_margins(source, .grouping = rollup(g), .id = "sid")
stopifnot(identical(seen, 0L))
actual <- dplyr::collect(query)
stopifnot(nrow(actual) == 4L, identical(seen, 0L), nrow(ordinary) == 2L)
cat("dtplyr generated copy callee ignores caller's copy binding: passed\n")

for (name in c("..x", ".env", ".data")) {
  data <- data.frame(x = c("a", "b"), value = 1:2, check.names = FALSE)
  names(data)[[1L]] <- name
  source <- dtplyr::lazy_dt(data)
  step <- dplyr::mutate(source, g = !!rlang::sym(name))
  actual <- dplyr::collect(step)
  stopifnot(identical(actual$g, data[[name]]))
  query <- expand_with_margins(
    step, .grouping = rollup(g), .margin_label = NULL, .id = "sid"
  )
  result <- dplyr::collect(query)
  result <- result[order(result$sid, result$value), ]
  stopifnot(identical(result$g, c("a", "b", NA_character_, NA_character_)),
            identical(result[[name]], c("a", "b", "a", "b")))
  cat("dtplyr special alias source:", name, "passed\n")
}
