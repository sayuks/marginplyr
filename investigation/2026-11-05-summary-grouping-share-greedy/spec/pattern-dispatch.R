args <- commandArgs(trailingOnly = TRUE)
assert_contract <- "--assert" %in% args
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
root <- "investigation/2026-11-05-summary-grouping-share-greedy/spec"
data <- tibble(a = c("x", "y"), b = c(1L, 2L))
records <- list()
run <- function() {
  constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
  methods <- paste0("length.stringr_", c("fixed", "regex", "coll"))
  old <- mget(methods, globalenv(), ifnotfound = rep(list(NULL), length(methods)), inherits = FALSE)
  existed <- vapply(methods, exists, logical(1), envir = globalenv(), inherits = FALSE)
  on.exit({
    for (i in seq_along(methods)) {
      if (existed[[i]]) assign(methods[[i]], old[[i]], globalenv()) else rm(list = methods[[i]], envir = globalenv())
    }
  })
  observed_types <- character()
  method <- function(x) {
    actual <- typeof(tidyselect::peek_data()$a)
    observed_types <<- c(observed_types, actual)
    if (identical(actual, "character")) 1L else 2L
  }
  for (name in methods) assign(name, method, globalenv())
  for (backend in c("local", "dtplyr")) {
    input <- if (backend == "local") data else dtplyr::lazy_dt(data)
    for (modifier in c("fixed", "regex", "coll")) {
      for (shape in c("bound", "inline")) {
        env <- rlang::env(globalenv(), p = getExportedValue("stringr", modifier)("a"))
        pattern <- if (shape == "bound") quote(p) else rlang::call2(modifier, "a", .ns = "stringr")
        selector <- rlang::call2("matches", pattern)
        control <- rlang::call2("matches", rlang::call2("identity", pattern, .ns = "base"))
        for (position in c(".by", constructors)) {
          observed_types <- character()
          direct <- tidyselect::eval_select(rlang::new_quosure(selector, env), data)
          stopifnot(identical(unname(direct), 1L), identical(names(direct), "a"),
                    all(observed_types == "character"))
          inspect <- function(expression) {
            q <- rlang::new_quosure(expression, env)
            if (position == ".by") {
              return(rlang::inject(inspect_grouping(input, .by = !!q, .format = "list")))
            }
            spec <- eval(rlang::call2(position, expression), env)
            inspect_grouping(input, .grouping = spec, .format = "list")
          }
          observed_types <- character()
          actual <- tryCatch(inspect(selector), error = identity)
          actual_types <- observed_types
          observed_types <- character()
          safe <- inspect(control)
          expected_names <- if (position == ".by") safe$fixed[[1L]] else safe$included[[1L]]
          stopifnot(identical(expected_names, "a"), all(observed_types == "character"))
          bad <- inherits(actual, "error") && grepl("stringr patterns must be length 1", conditionMessage(actual), fixed = TRUE)
          records[[length(records) + 1L]] <<- data.frame(
            backend, modifier, shape, position,
            verdict = if (bad) "false-pattern-length-refusal" else "contract-match",
            selected_count = length(direct),
            proxy_types = paste(actual_types, collapse = ","),
            control = "typed-identity-wrapper-match"
          )
          if (assert_contract) stopifnot(!inherits(actual, "error")) else stopifnot(bad, identical(actual_types, "integer"))
        }
      }
    }
  }
}
run()
records <- bind_rows(records)
write.csv(records, file.path(root, "pattern-dispatch-results.csv"), row.names = FALSE)
print(table(records$verdict))
print(table(records$control))
cat("Direct typed selections and typed identity-wrapper controls all selected a.\n")
cat("Tested SHA:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE), "\n")
print(sessionInfo())
