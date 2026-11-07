# Intentional desired-contract assertion: exits 1 on the investigated SHA.
args <- commandArgs(trailingOnly = TRUE)
pkgload::load_all(if (length(args)) args[[1L]] else ".", quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
x <- data.frame(a = "A", b = 1L)
class(x) <- c("selector_frame", "data.frame")
busy <- FALSE
names.selector_frame <- function(x) {
  ordinary <- attr(x, "names")
  # Formatting an absent selection-context error can itself ask for names.
  if (busy) return(ordinary)
  busy <<- TRUE
  on.exit({ busy <<- FALSE })
  data <- tryCatch(tidyselect::peek_data(), error = function(e) NULL)
  if (!is.null(data) && is.character(data[[1L]])) rev(ordinary) else ordinary
}
stopifnot(identical(names(x), c("a", "b")))
stopifnot(identical(dplyr::group_vars(dplyr::group_by(x, a)), "a"))
stopifnot(identical(tidyselect::eval_select(
  quote(names(tidyselect::peek_data())[1L]), x
), c(b = 2L)))
control <- inspect_grouping(
  x, .by = a,
  .grouping = grouping_set(identity(names(tidyselect::peek_data())[1L])),
  .format = "list"
)
stopifnot(identical(control$fixed, list("a")),
          identical(control$included, list("b")))
cat("Ordinary names/grouping and direct typed selection controls passed.\n")
observed <- tryCatch(inspect_grouping(
  x, .by = a,
  .grouping = grouping_set(names(tidyselect::peek_data())[1L]),
  .format = "list"
), error = identity)
if (inherits(observed, "condition")) cat(conditionMessage(observed), "\n")
stopifnot(identical(observed, control))
