Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "/private/tmp/marginplyr-exhaustive-b-root/source"), quiet = TRUE)
x <- structure(data.frame(fixed = c(5L, 8L), dimension = c("east", "west")), class = c("selection_view", "data.frame"))
busy <- FALSE
names.selection_view <- function(x) {
  original <- attr(x, "names")
  if (busy) return(original)
  busy <<- TRUE
  on.exit({ busy <<- FALSE })
  proxy <- tryCatch(tidyselect::peek_data(), error = function(cnd) NULL)
  if (!is.null(proxy) && is.character(proxy[[2L]])) rev(original) else original
}
before <- serialize(x, NULL)
stopifnot(identical(names(x), c("fixed", "dimension")))
selection <- quote(names(tidyselect::peek_data())[1L])
direct <- tidyselect::eval_select(selection, x)
stopifnot(identical(direct, c(dimension = 2L)))
ordinary <- dplyr::group_by(x, fixed)
stopifnot(identical(dplyr::group_vars(ordinary), "fixed"))
inspect <- tryCatch(inspect_grouping(x, .by = fixed, .grouping = grouping_set(names(tidyselect::peek_data())[1L])), error = identity)
expand <- tryCatch(expand_with_margins(x, .by = fixed, .grouping = grouping_set(names(tidyselect::peek_data())[1L])), error = identity)
control <- inspect_grouping(x, .by = fixed, .grouping = grouping_set(identity(names(tidyselect::peek_data())[1L])), .format = "list")
stopifnot(identical(control$fixed, list("fixed")), identical(control$included, list("dimension")), inherits(inspect, "marginplyr_error"), inherits(expand, "marginplyr_error"), identical(serialize(x, NULL), before))
cat("Direct selection:", names(direct), "; ordinary fixed group:", dplyr::group_vars(ordinary), "\n")
cat("Typed control included:", control$included[[1L]], "\n")
cat("Inspection error:", conditionMessage(inspect), "\n")
cat("Expansion error:", conditionMessage(expand), "\n")
cat("Caller source bytes unchanged\n")
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) stopifnot(!inherits(inspect, "error"), !inherits(expand, "error"))
