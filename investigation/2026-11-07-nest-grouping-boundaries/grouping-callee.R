args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L, args[[2L]] %in% c("observed", "contract"))
pkgload::load_all(args[[1L]], quiet = TRUE)
mode <- args[[2L]]
source <- data.frame(region = c("east", "west"), units = c(1L, 1L), value = 7:8)
before <- serialize(source, NULL)
consumers <- list(
  inspect_grouping = function(s) inspect_grouping(source, .grouping = s),
  nest_with_margins = function(s) nest_with_margins(source, .grouping = s),
  nest_by_with_margins = function(s) nest_by_with_margins(source, .grouping = s),
  expand_with_margins = function(s) expand_with_margins(source, .grouping = s),
  summarize_with_margins = function(s) {
    summarize_with_margins(source, n = dplyr::n(), .grouping = s)
  }
)
choose <- function() {
  calls <<- calls + 1L
  "units"
}
make_namespace <- function(strict) {
  function(package, name) {
    package <- as.character(substitute(package))
    name <- as.character(substitute(name))
    lookups <<- c(lookups, paste(package, name, sep = "::"))
    if (identical(package, "marginplyr") && identical(name, "rollup")) {
      return(choose)
    }
    stopifnot(identical(package, "base"), identical(name, "("))
    active <- if (strict) {
      tidyselect::peek_data()
    } else {
      tryCatch(tidyselect::peek_data(), error = function(cnd) NULL)
    }
    if (is.character(active$region)) {
      return(base::`(`)
    }
    function(...) {
      function() {
        calls <<- calls + 1L
        "region"
      }
    }
  }
}
callees <- list(
  explicit = quote(base::`(`(rollup)()),
  syntactic_head = quote((base::`(`(rollup))()),
  syntactic_whole = quote((base::`(`(rollup)())),
  namespaced_inner = quote(base::`(`(marginplyr::rollup)())
)
violations <- character()
for (strict in c(FALSE, TRUE)) {
  for (spelling in names(callees)) {
    context <- rlang::env(globalenv(), rollup = choose, `::` = make_namespace(strict))
    selection <- rlang::new_quosure(callees[[spelling]], context)
    lookups <- character()
    calls <- 0L
    native <- tidyselect::eval_select(selection, source)
    lookups_expected <- if (spelling == "namespaced_inner") {
      c("base::(", "marginplyr::rollup")
    } else {
      "base::("
    }
    stopifnot(identical(native, c(units = 2L)),
              identical(lookups, lookups_expected), identical(calls, 1L))
    spec <- grouping_sets(!!selection)
    for (consumer in names(consumers)) {
      run <- consumers[[consumer]]
      expected <- run(grouping_sets(units))
      wrong <- run(grouping_sets(region))
      stopifnot(!identical(expected, wrong))
      lookups <- character()
      calls <- 0L
      actual <- tryCatch(run(spec), error = identity)
      stopifnot(identical(lookups, lookups_expected))
      good <- identical(actual, expected) && identical(calls, 1L)
      if (spelling == "namespaced_inner") {
        stopifnot(good)
      } else if (mode == "observed") {
        if (strict) {
          stopifnot(inherits(actual, "rlang_error"),
                    grepl("must be used within a", conditionMessage(actual), fixed = TRUE),
                    identical(calls, 0L))
        } else {
          stopifnot(identical(actual, wrong), identical(calls, 1L))
        }
      }
      if (!good) {
        effect <- if (inherits(actual, "error")) "missing selection context" else "region instead of units"
        violations <- c(violations, paste(strict, spelling, consumer, effect, sep = " | "))
      }
      stopifnot(identical(serialize(source, NULL), before))
    }
  }
}
cat("Public consumer calls:", length(callees) * length(consumers) * 2L, "\n")
cat("Violations:", length(violations), "\n")
writeLines(violations)
stopifnot(length(violations) == if (mode == "observed") 30L else 0L)
