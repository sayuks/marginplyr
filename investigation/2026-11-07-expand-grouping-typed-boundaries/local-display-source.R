# Run with Rscript from the package source root.
pkgload::load_all(".", quiet = TRUE)

as_tibble.presentation_probe <- function(x, ...) {
  converted <- tibble::as_tibble(unclass(x), ...)
  dplyr::mutate(converted, k = rep("presentation", dplyr::n()))
}

ordinary <- tibble::tibble(k = "stored", payload = 17L)
source <- structure(
  as.data.frame(ordinary), class = c("presentation_probe", "data.frame")
)
source_bytes <- serialize(source, NULL)
converted <- dplyr::as_tibble(source)
native <- dplyr::union_all(
  dplyr::mutate(source, k = as.character(k)),
  dplyr::mutate(source, k = "presentation")
)

expand <- function(input, label, checked = TRUE) {
  tryCatch(
    expand_with_margins(
      input, .grouping = rollup(k), .margin_label = label,
      .check_margin_label = checked, .id = "set"
    ),
    error = identity
  )
}

collision <- function(x) {
  inherits(x, "marginplyr_error") &&
    grepl("is already present in grouping", conditionMessage(x), fixed = TRUE)
}

keys <- function(x) x$k[order(x$set)]

stopifnot(
  identical(converted$k, "presentation"),
  identical(native$k, c("stored", "presentation")),
  identical(native$payload, rep(17L, 2L)),
  collision(expand(ordinary, "stored")),
  identical(keys(expand(ordinary, "presentation")),
            c("stored", "presentation")),
  identical(keys(expand(source, "presentation", checked = FALSE)),
            c("stored", "presentation")),
  identical(keys(expand(source, "unrelated")), c("stored", "unrelated"))
)
cat("Native mutation/union, plain checked and subclass unchecked controls passed.\n")

actual_collision <- expand(source, "stored")
false_collision <- expand(source, "presentation")
stopifnot(identical(serialize(source, NULL), source_bytes))

if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(
    collision(actual_collision),
    identical(keys(false_collision), c("stored", "presentation"))
  )
} else {
  stopifnot(
    identical(keys(actual_collision), c("stored", "stored")),
    identical(sort(actual_collision$set), c(1L, 2L)),
    identical(actual_collision$payload, rep(17L, 2L)),
    collision(false_collision)
  )
  cat("Subclass conversion hid one real collision and added one false collision.\n")
}
