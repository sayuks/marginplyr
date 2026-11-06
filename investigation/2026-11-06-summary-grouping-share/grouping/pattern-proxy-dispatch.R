Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))

args <- commandArgs(trailingOnly = TRUE)
assert_contract <- "--assert" %in% args
output <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", unset = tempdir())
dir.create(output, recursive = TRUE, showWarnings = FALSE)
data <- data.frame(a = c("x", "y"), b = 1:2)
table <- asNamespace("vctrs")$.__S3MethodsTable__.
records <- list()

capture <- function(expr) tryCatch(force(expr), error = identity)
run <- function(input, selection, position, verb = "inspect_grouping") {
  dots <- list(.data = input)
  if (position == ".by") {
    dots$.by <- selection
  } else {
    dots$.grouping <- rlang::eval_tidy(rlang::call2(position, selection))
  }
  if (verb == "summarize_with_margins") dots$n <- rlang::quo(dplyr::n())
  if (verb == "inspect_grouping") dots$.format <- "list"
  rlang::eval_tidy(rlang::call2(verb, !!!dots))
}

for (modifier in c("fixed", "regex", "coll")) {
  local({
    reads <- character()
    method <- function(x) {
      type <- typeof(tidyselect::peek_data()$a)
      reads <<- c(reads, type)
      if (type != "character") {
        stop(structure(
          list(message = "pattern proxy requires character column metadata", call = NULL),
          class = c("hunt_pattern_metadata_error", "error", "condition")
        ))
      }
      # Every returned proxy has exactly the pattern's size and storage.
      unclass(x)
    }
    method_name <- paste0("vec_proxy.stringr_", modifier)
    rlang::local_bindings(!!!setNames(list(method), method_name), .env = table)
    pattern <- getExportedValue("stringr", modifier)("a")
    for (form in c("bound", "inline")) {
      expression <- if (form == "bound") quote(pattern) else {
        rlang::call2(modifier, "a", .ns = "stringr")
      }
      selection <- rlang::new_quosure(rlang::call2("matches", expression), environment())
      wrapped <- rlang::new_quosure(
        rlang::call2("matches", rlang::call2("identity", expression, .ns = "base")),
        environment()
      )
      reads <- character()
      direct <- tidyselect::eval_select(selection, data)
      stopifnot(identical(names(direct), "a"), length(reads) > 0L,
                all(reads == "character"))
      for (backend in c("local", "dtplyr")) {
        input <- if (backend == "local") data else dtplyr::lazy_dt(data)
        for (position in c(".by", "grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")) {
          reads <- character()
          actual <- capture(run(input, selection, position))
          observed <- reads
          reads <- character()
          control <- run(input, wrapped, position)
          selected <- if (position == ".by") control$fixed[[1L]] else control$included[[1L]]
          stopifnot(identical(selected, "a"), length(reads) > 0L,
                    all(reads == "character"))
          records[[length(records) + 1L]] <<- data.frame(
            modifier, form, backend, position,
            actual = if (inherits(actual, "error")) conditionMessage(actual) else "success",
            method_types = paste(observed, collapse = ","), control = "selected a"
          )
          if (assert_contract) {
            stopifnot(!inherits(actual, "error"), identical(actual, control))
          } else {
            stopifnot(inherits(actual, "error"),
                      grepl("pattern proxy requires character column metadata", conditionMessage(actual), fixed = TRUE),
                      identical(observed, "integer"))
          }
        }
      }
    }
  })
}
results <- do.call(rbind, records)
write.csv(results, file.path(output, "pattern-proxy-results.csv"), row.names = FALSE)
cat(nrow(results), if (assert_contract) {
  "typed-context contract comparisons passed\n"
} else {
  "false refusals; every typed reference and wrapper selected a\n"
})

# All public consumers encounter the same earlier selection failure.
local({
  reads <- character()
  pattern <- stringr::fixed("a")
  method <- function(x) {
    type <- typeof(tidyselect::peek_data()$a)
    reads <<- c(reads, type)
    if (type != "character") stop("pattern proxy requires character column metadata")
    unclass(x)
  }
  rlang::local_bindings(vec_proxy.stringr_fixed = method, .env = table)
  for (verb in c("inspect_grouping", "summarize_with_margins", "expand_with_margins", "nest_with_margins", "nest_by_with_margins")) {
    selection <- rlang::quo(matches(pattern))
    wrapped <- rlang::quo(matches(identity(pattern)))
    actual <- capture(run(data, selection, "grouping_set", verb))
    control <- run(data, wrapped, "grouping_set", verb)
    stopifnot(!inherits(control, "error"))
    if (assert_contract) {
      stopifnot(!inherits(actual, "error"), identical(actual, control))
      cat(verb, ": typed-context contract passed\n", sep = "")
    } else {
      stopifnot(inherits(actual, "error"))
      cat(verb, ": early proxy dispatch refused; typed wrapper succeeded\n", sep = "")
    }
  }
})

# The repaired length-method path remains a separating successful control.
local({
  pattern <- stringr::fixed("a")
  reads <- character()
  method <- function(x) {
    type <- typeof(tidyselect::peek_data()$a)
    reads <<- c(reads, type)
    if (type == "character") 1L else 2L
  }
  rlang::local_bindings(length.stringr_fixed = method, .env = baseenv()$.__S3MethodsTable__.)
  actual <- inspect_grouping(data, .grouping = grouping_set(matches(pattern)), .format = "list")
  stopifnot(identical(actual$included[[1L]], "a"), all(reads == "character"))
  cat("closed #1344 length-method control: typed selection succeeded\n")
})

print(sessionInfo())
