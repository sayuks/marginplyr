# Independent public-seam replay against an immutable Git archive.
main <- function() {
  pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
  method <- "head.marginplyr_independent_root"
  stopifnot(!exists(method, envir = .GlobalEnv, inherits = FALSE))
  on.exit(rm(list = method, envir = .GlobalEnv), add = TRUE)
  observed <- integer()
  mode <- "retype"
  assign(method, function(x, n = 6L, ...) {
    observed <<- c(observed, nrow(x))
    if (mode == "mutate") data.table::set(x, j = "g", value = rep("changed", nrow(x)))
    out <- NextMethod()
    if (mode == "retype") data.table::set(out, j = "g", value = integer())
    out
  }, .GlobalEnv)
  source <- data.table::data.table(g = c("a", "b"), v = 1:2)
  class(source) <- c("marginplyr_independent_root", class(source))
  lazy <- dtplyr::lazy_dt(source, immutable = TRUE)
  stopifnot(identical(dplyr::collect(lazy)$g, c("a", "b")), length(observed) == 0L)
  named <- inspect_grouping(lazy, .grouping = grouping_set(g), .format = "list")
  stopifnot(identical(named$included, list("g")), length(observed) == 0L)
  typed <- inspect_grouping(lazy, .grouping = grouping_set(where(is.character)), .format = "list")
  stopifnot(identical(typed$included, list(character())), identical(observed, c(2L, 0L)))
  stopifnot(identical(source$g, c("a", "b")))
  cat("Native collection and name-only inspection: g retained, callback count 0.\n")
  cat("Typed inspection: g silently omitted, callback row counts 2,0.\n")
  observed <- integer()
  mode <- "mutate"
  invisible(inspect_grouping(lazy, .grouping = grouping_set(where(is.character))))
  stopifnot(identical(source$g, c("changed", "changed")), identical(observed, c(2L, 0L)))
  cat("Typed inspection changed caller-owned immutable source to changed,changed.\n")
  cat("R:", R.version.string, "\n")
  if ("--assert" %in% commandArgs(TRUE)) {
    stopifnot(identical(typed$included, list("g")), identical(source$g, c("a", "b")))
  }
}
main()
