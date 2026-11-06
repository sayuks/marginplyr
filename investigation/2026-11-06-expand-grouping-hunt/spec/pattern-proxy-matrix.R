# Finite pattern-consumption boundary matrix; run from the package root.
# The process owns all caller S3 bindings and starts no child work.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
assert_desired <- "--assert" %in% commandArgs(TRUE)
input <- data.frame(a = c("x", "y"), b = 1:2)
constructors <- c("grouping_set", "grouping_sets", "rollup", "cube",
                  "grouping_spec")
consumers <- c("inspect_grouping", "summarize_with_margins",
               "expand_with_margins", "nest_with_margins",
               "nest_by_with_margins")
patterns <- list(fixed = stringr::fixed("a"), regex = stringr::regex("^a$"),
                 coll = stringr::coll("a"))
reads <- character()
method <- function(x, ...) {
  type <- typeof(tidyselect::peek_data()$a)
  reads <<- c(reads, type)
  rep(unclass(x), if (type == "character") 1L else 3L)
}
run <- function(backend, consumer, position, wrapped) {
  data <- if (backend == "local") input else dtplyr::lazy_dt(input)
  selection <- if (wrapped) quote(matches(identity(pattern))) else
    quote(matches(pattern))
  args <- list(.data = data)
  if (position == ".by") {
    args$.by <- selection
    args$.grouping <- quote(grouping_set(b))
  } else {
    args$.grouping <- rlang::call2(position, selection)
  }
  if (consumer == "inspect_grouping") args$.format <- "list"
  if (consumer == "summarize_with_margins") args$count <- quote(dplyr::n())
  reads <<- character()
  value <- tryCatch(rlang::eval_bare(rlang::call2(consumer, !!!args),
                                    environment()), error = identity)
  if (!inherits(value, "error")) {
    if (consumer == "inspect_grouping") {
      selected <- if (position == ".by") value$fixed[[1L]] else
        value$included[[1L]]
      stopifnot(identical(selected, "a"))
    } else {
      stopifnot("a" %in% dplyr::tbl_vars(value))
    }
  }
  list(error = inherits(value, "error"), reads = reads,
       message = if (inherits(value, "error")) conditionMessage(value) else "")
}
results <- list()
index <- 0L
for (kind in names(patterns)) {
  pattern <- patterns[[kind]]
  for (scope in c("exact", "inherited")) {
    class_name <- if (scope == "exact") paste0("stringr_", kind) else
      "stringr_pattern"
    method_name <- paste0("vec_proxy.", class_name)
    assign(method_name, method, envir = globalenv())
    reads <- character()
    direct <- tidyselect::eval_select(rlang::quo(matches(pattern)), input)
    stopifnot(identical(names(direct), "a"), all(reads == "character"),
              length(reads) > 0L)
    for (backend in c("local", "dtplyr")) {
      for (consumer in consumers) {
        for (position in c(constructors, ".by")) {
          actual <- run(backend, consumer, position, FALSE)
          control <- run(backend, consumer, position, TRUE)
          stopifnot(actual$error, length(actual$reads) > 0L,
                    all(actual$reads == "integer"), !control$error,
                    length(control$reads) > 0L,
                    all(control$reads == "character"),
                    grepl("Can't recycle", actual$message, fixed = TRUE))
          index <- index + 1L
          results[[index]] <- data.frame(pattern = kind, scope = scope,
            backend = backend, consumer = consumer, position = position,
            false_refusal = actual$error,
            actual_reads = paste(actual$reads, collapse = ","),
            control_reads = paste(control$reads, collapse = ","))
        }
      }
    }
    rm(list = method_name, envir = globalenv())
  }
}
results <- do.call(rbind, results)
output <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", unset = tempdir())
dir.create(output, recursive = TRUE, showWarnings = FALSE)
write.csv(results, file.path(output, "pattern-proxy-matrix.csv"),
          row.names = FALSE)
cat("False refusals:", nrow(results), "\nTyped-wrapper controls:",
    nrow(results), "\nDirect typed controls: 6\n")
# Ordinary method-free patterns remain usable without typed derived metadata.
ordinary <- 0L
for (pattern in patterns) {
  derived <- dplyr::filter(dtplyr::lazy_dt(input), b > 0)
  value <- inspect_grouping(derived,
    .grouping = grouping_set(matches(pattern)), .format = "list")
  stopifnot(identical(value$included[[1L]], "a"))
  ordinary <- ordinary + 1L
}
cat("Ordinary method-free derived controls:", ordinary, "\n")
print(sessionInfo())
if (assert_desired) stopifnot(!any(results$false_refusal))
