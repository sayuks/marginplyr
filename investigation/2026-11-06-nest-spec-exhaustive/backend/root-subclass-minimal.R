main <- function() {
  pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
  Sys.setenv(TESTTHAT_PARALLEL = "false")
  args <- commandArgs(trailingOnly = TRUE)
  dispatch <- if ("--bracket" %in% args) "[" else "head"
  name <- paste0(dispatch, ".marginplyr_hunt_root")
  stopifnot(!exists(name, envir = .GlobalEnv, inherits = FALSE))
  on.exit(rm(list = name, envir = .GlobalEnv), add = TRUE)
  seen <- integer()
  method <- if (dispatch == "head") function(x, n = 6L, ...) {
    seen <<- c(seen, nrow(x))
    out <- NextMethod("head")
    if (nrow(out) == 0L) data.table::set(out, j = "g", value = integer())
    out
  } else function(x, ...) {
    seen <<- c(seen, nrow(x))
    out <- NextMethod("[")
    if (nrow(out) == 0L) data.table::set(out, j = "g", value = integer())
    out
  }
  assign(name, method, envir = .GlobalEnv)
  source <- data.table::data.table(g = c("a", "b"))
  class(source) <- c("marginplyr_hunt_root", class(source))
  before <- serialize(source, NULL)
  input <- dtplyr::lazy_dt(source, immutable = TRUE)
  native <- dplyr::collect(input)
  stopifnot(identical(native$g, c("a", "b")), length(seen) == 0L)
  named <- inspect_grouping(input, .grouping = grouping_set(g))
  stopifnot(identical(named$included, "(g)"), length(seen) == 0L)
  result <- inspect_grouping(input, .grouping = grouping_set(where(is.character)))
  cat("Dispatch: ", dispatch, "; source row counts received: ",
      paste(seen, collapse = ","), "\n", sep = "")
  cat("Native g: ", paste(native$g, collapse = ","),
      "; inspected included: ", result$included, "\n", sep = "")
  stopifnot(identical(result$included, "()"), any(seen == 2L),
            identical(serialize(source, NULL), before))
  class(source) <- c("data.table", "data.frame")
  seen <- integer()
  control <- inspect_grouping(dtplyr::lazy_dt(source),
                             .grouping = grouping_set(where(is.character)))
  stopifnot(identical(control$included, "(g)"), length(seen) == 0L)
  cat("Plain data.table control: (g), no custom callback\n")
  if ("--assert" %in% args) stopifnot(identical(result$included, "(g)"))
}
main()
