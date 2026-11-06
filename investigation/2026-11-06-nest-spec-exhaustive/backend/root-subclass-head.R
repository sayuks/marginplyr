# Dated reproduction: a root subclass method runs before dtplyr isolation.
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
  Sys.setenv(TESTTHAT_PARALLEL = "false")
  method_name <- "head.marginplyr_hunt_root"
  stopifnot(!exists(method_name, envir = .GlobalEnv, inherits = FALSE))
  on.exit(rm(list = method_name, envir = .GlobalEnv), add = TRUE)
  cases <- expand.grid(graph = c("root", "arithmetic", "filter"),
                       role = c("dimension", "fixed"),
                       consumer = c("inspect", "nest", "nest_by"),
                       masked = c(FALSE, TRUE), stringsAsFactors = FALSE)
  cases$expected <- FALSE
  cases$source_rows_seen <- 0L
  cases$callbacks <- 0L
  cases$source_unchanged <- FALSE
  row_counts <- integer()
  mode <- "faithful"
  assign(method_name, function(x, n = 6L, ...) {
    row_counts <<- c(row_counts, nrow(x))
    if (identical(mode, "mutate") && nrow(x) > 0L) {
      data.table::set(x, j = "g", value = rep("changed", nrow(x)))
    }
    out <- NextMethod("head")
    if (identical(mode, "retype") && nrow(out) == 0L) {
      data.table::set(out, j = "g", value = integer())
    }
    out
  }, envir = .GlobalEnv)
  fixture <- function(graph, subclass = TRUE, immutable = TRUE) {
    src <- data.table::data.table(g = c("a", "b"), v = 1:2)
    if (subclass) class(src) <- c("marginplyr_hunt_root", class(src))
    lazy <- dtplyr::lazy_dt(src, immutable = immutable)
    lazy <- switch(graph, root = lazy,
                   arithmetic = dplyr::mutate(lazy, v = v + 1L),
                   filter = dplyr::filter(lazy, v > 0L))
    list(source = src, lazy = lazy)
  }
  execute <- function(x, role, consumer) {
    fixed <- identical(role, "fixed")
    if (consumer == "inspect") {
      if (fixed) return(inspect_grouping(x, .by = where(is.character)))
      return(inspect_grouping(x, .grouping = grouping_set(where(is.character))))
    }
    verb <- if (consumer == "nest") nest_with_margins else nest_by_with_margins
    out <- if (fixed) verb(x, .by = where(is.character)) else
      verb(x, .grouping = grouping_set(where(is.character)))
    if (inherits(out, "dtplyr_step")) out <- dplyr::collect(out)
    out
  }
  has_expected <- function(out, consumer, role) {
    if (consumer == "inspect") {
      field <- if (role == "fixed") "fixed" else "included"
      return(identical(out[[field]], "(g)"))
    }
    "g" %in% names(out) && identical(sort(out$g), c("a", "b")) &&
      nrow(out) == 2L && identical(lapply(out$data, nrow), list(1L, 1L))
  }
  for (i in seq_len(nrow(cases))) {
    f <- fixture(cases$graph[[i]], subclass = cases$masked[[i]])
    before <- serialize(f$source, NULL)
    row_counts <- integer()
    mode <- "retype"
    native <- dplyr::collect(f$lazy)
    stopifnot(identical(native$g, c("a", "b")), length(row_counts) == 0L)
    out <- execute(f$lazy, cases$role[[i]], cases$consumer[[i]])
    cases$expected[[i]] <- has_expected(out, cases$consumer[[i]], cases$role[[i]])
    cases$source_rows_seen[[i]] <- sum(row_counts[row_counts > 0L])
    cases$callbacks[[i]] <- length(row_counts)
    cases$source_unchanged[[i]] <- identical(serialize(f$source, NULL), before)
  }
  stopifnot(all(cases$source_unchanged), all(cases$expected[!cases$masked]),
            !any(cases$expected[cases$masked]),
            all(cases$source_rows_seen[cases$masked] == 2L))
  print(cases, row.names = FALSE)
  cat("retype matrix: ", nrow(cases), " calls; ", sum(!cases$expected),
      " false partitions, ", sum(cases$expected), " controls\n", sep = "")
  f <- fixture("root")
  before <- serialize(f$source, NULL)
  row_counts <- integer()
  mode <- "mutate"
  named <- inspect_grouping(f$lazy, .grouping = grouping_set(g))
  stopifnot(identical(named$included, "(g)"), length(row_counts) == 0L,
            identical(serialize(f$source, NULL), before))
  inspected <- inspect_grouping(f$lazy, .grouping = grouping_set(where(is.character)))
  stopifnot(identical(inspected$included, "(g)"), any(row_counts == 2L),
            identical(f$source$g, c("changed", "changed")))
  cat("mutation witness: name-only inspection retained a,b; typed inspection changed source g to changed,changed\n")
  mutable <- fixture("root", immutable = FALSE)
  row_counts <- integer()
  mode <- "faithful"
  rejected <- tryCatch(nest_with_margins(mutable$lazy, .by = g), error = identity)
  stopifnot(inherits(rejected, "marginplyr_error"), length(row_counts) == 0L)
  cat("Mutable-step Nest refusal ran no subclass callback\n")
  cat("R: ", R.version.string, "\n", sep = "")
  for (p in c("dplyr", "dtplyr", "data.table", "tidyselect", "vctrs")) {
    cat(p, ": ", as.character(packageVersion(p)), "\n", sep = "")
  }
  output <- Sys.getenv("MARGINPLYR_MATRIX_OUTPUT", "")
  if (nzchar(output)) write.csv(cases, output, row.names = FALSE)
  if ("--assert" %in% args) stopifnot(all(cases$expected), all(cases$source_rows_seen == 0L))
}
main()
