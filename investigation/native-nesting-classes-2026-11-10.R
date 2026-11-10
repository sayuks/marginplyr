args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Supply one marginplyr source directory.")
}
repo <- normalizePath(args[[1L]], mustWork = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false", TESTTHAT_CPUS = "1")
options(testthat.parallel = FALSE)
cat("Investigated: 2026-11-10\n")
cat("SHA:", system2("git", c("-C", shQuote(repo), "rev-parse", "HEAD"),
                    stdout = TRUE), "\n", sep = "")
cat(R.version.string, "\n", sep = "")
packages <- c("dplyr", "tidyr", "dtplyr", "data.table", "tibble", "vctrs",
              "rlang", "pkgload")
for (package in packages) {
  cat(package, " ", as.character(utils::packageVersion(package)), "\n", sep = "")
}
pkgload::load_all(repo, quiet = TRUE, export_all = FALSE)
cat("marginplyr ", as.character(utils::packageVersion("marginplyr")), "\n", sep = "")

cases <- list(
  payload = list(data = data.frame(k = c("a", "a", "b"), value = 1:3),
                 keys = "k"),
  zero_payload_with_rows = list(data = data.frame(k = c("a", "a", "b")),
                                keys = "k"),
  keyless_empty = list(data = data.frame(value = integer()),
                      keys = character()),
  keyless_empty_zero_columns = list(data = data.frame(), keys = character())
)
constructors <- list(
  data.frame = identity,
  tibble = tibble::as_tibble,
  data.table = data.table::as.data.table,
  dtplyr = function(x) dtplyr::lazy_dt(x, immutable = TRUE)
)
class_string <- function(x) paste(class(x), collapse = "/")
run_case <- function(case, input_kind, operation, api) {
  scenario <- cases[[case]]
  input <- constructors[[input_kind]](scenario$data)
  keys <- scenario$keys
  warnings <- character()
  result <- withCallingHandlers({
    if (api == "native") {
      if (operation == "nest") {
        tidyr::nest(input, data = -dplyr::all_of(keys))
      } else {
        reference <- if (input_kind == "dtplyr") dplyr::collect(input) else input
        dplyr::nest_by(reference, !!!rlang::syms(keys))
      }
    } else if (operation == "nest") {
      marginplyr::nest_with_margins(input, .by = dplyr::all_of(keys))
    } else {
      marginplyr::nest_by_with_margins(input, .by = dplyr::all_of(keys))
    }
  }, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  observed <- if (inherits(result, "dtplyr_step")) {
    dplyr::collect(result)
  } else {
    result
  }
  if (!"data" %in% names(observed)) {
    stop("The selected nesting spelling did not produce its data column.")
  }
  cells <- observed[["data"]]
  ptype <- attr(cells, "ptype", exact = TRUE)
  data.frame(
    case, input = input_kind, operation, api,
    returned_outer = class_string(result),
    observed_outer = class_string(observed), outer_nrow = nrow(observed),
    list_class = class_string(cells),
    list_ptype_class = if (is.null(ptype)) "<NULL>" else class_string(ptype),
    inner_classes = if (length(cells)) {
      paste(vapply(cells, class_string, character(1)), collapse = ";")
    } else "<no elements>",
    inner_dims = if (length(cells)) {
      paste(vapply(cells, function(x) paste(dim(x), collapse = "x"),
                   character(1)), collapse = ";")
    } else "<no elements>",
    warnings = paste(warnings, collapse = " | "), stringsAsFactors = FALSE
  )
}
records <- list()
for (case in names(cases)) {
  for (input_kind in names(constructors)) {
    for (operation in c("nest", "nest_by")) {
      for (api in c("native", "margin")) {
        records[[length(records) + 1L]] <- run_case(
          case, input_kind, operation, api
        )
      }
    }
  }
}
cat("RESULTS\n")
write.table(do.call(rbind, records), stdout(), sep = "\t", row.names = FALSE,
            quote = TRUE)
cat("Completed", length(records), "observations; no test suite executed.\n")
