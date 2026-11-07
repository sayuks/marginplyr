pkgload::load_all(".", quiet = TRUE)
mode <- commandArgs(trailingOnly = TRUE)
stopifnot(length(mode) == 1L, mode %in% c("observe", "contract"))

source_data <- tibble::tibble(g = c("x", "y"), x = c(1, 3))
stored <- "out"
reported <- "out"
frame <- function(x) {
  columns <- stats::setNames(list(sum(x)), stored)
  vctrs::new_data_frame(columns, n = 1L, class = "probe_summary_frame")
}
frame_names <- function(x) {
  if (nrow(x) == 0L) attr(x, "names", exact = TRUE) else reported
}
registerS3method("names", "probe_summary_frame", frame_names, baseenv())

summarize <- function(role) {
  switch(role,
    fixed = marginplyr::summarize_with_margins(source_data, frame(x), .by = g),
    dimension = marginplyr::summarize_with_margins(
      source_data, frame(x), .grouping = marginplyr::grouping_set(g)
    ),
    identifier = marginplyr::summarize_with_margins(
      source_data, frame(x), .grouping = marginplyr::grouping_set(),
      .id = "set"
    )
  )
}
capture <- function(role) tryCatch(summarize(role), error = identity)
collision <- function(result) {
  inherits(result, "marginplyr_error") &&
    grepl("overwrite|conflicts with a summary output", conditionMessage(result))
}

matches <- logical()
for (role in c("fixed", "dimension", "identifier")) {
  stored <- reported <- "out"
  baseline <- capture(role)
  reserved <- if (role == "identifier") "set" else "g"
  stopifnot(is.data.frame(baseline),
            identical(names(baseline), c(reserved, "out")))
  reported <- reserved
  reference <- dplyr::summarise(source_data, frame(x), .by = g)
  stopifnot(identical(reference, tibble::tibble(g = c("x", "y"), out = c(1, 3))))
  actual <- capture(role)
  if (inherits(actual, "error")) cat(role, conditionMessage(actual), "\n")
  matches <- c(matches, identical(actual, baseline))
  if (mode == "observe") stopifnot(collision(actual))

  stored <- reserved
  genuine <- capture(role)
  stopifnot(collision(genuine))
  reported <- "out"
  hidden <- capture(role)
  cat(role, "hidden collision", if (inherits(hidden, "error")) {
    conditionMessage(hidden)
  } else {
    paste(names(hidden), collapse = ",")
  }, "\n")
  matches <- c(matches, collision(hidden))
}
print(matches)
if (mode == "contract") stopifnot(all(matches))
