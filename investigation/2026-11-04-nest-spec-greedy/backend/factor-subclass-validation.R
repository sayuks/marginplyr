Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", getwd()), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
as.character.validation_factor <- function(x, ...) {
  out <- as.character.factor(x)
  present <- !is.na(out)
  out[present] <- paste0("[", out[present], "]")
  out
}
vctrs::s3_register("vctrs::vec_ptype2", "validation_factor.validation_factor",
                  function(x, y, ...) vctrs::vec_slice(x, integer()))
vctrs::s3_register("vctrs::vec_cast", "validation_factor.validation_factor",
                  function(x, to, ...) x)
new_factor <- function(case) {
  x <- switch(case,
    one = factor("a"),
    ordinary = factor(c("a", "b", "a")),
    unused_na = factor(c("a", "b", "a"), levels = c("a", "b", NA), exclude = NULL),
    used_na = factor(c("a", NA, "b"), levels = c("a", "b", NA), exclude = NULL),
    actual_missing = structure(c(1L, NA_integer_, 2L), levels = c("a", "b", NA), class = "factor"))
  class(x) <- c("validation_factor", class(x))
  x
}
codes <- function(x) as.integer(unclass(x))
observe <- function(expr) tryCatch(force(expr), error = identity)
run <- function(x, role, verb, keep, missing_label) {
  group <- if (role == "variable") grouping_set(g) else grouping_set(d)
  if (role == "fixed") group <- grouping_set()
  label <- if (missing_label) NULL else "Total"
  if (role == "fixed") {
    ans <- if (verb == "summarize_with_margins") {
      summarize_with_margins(x, .by = g, .grouping = group, .margin_label = label,
                             .id = "set", count = n())
    } else rlang::inject((!!get(verb))(
      x, .by = g, .grouping = group, .margin_label = label, .id = "set",
      !!!if (startsWith(verb, "nest")) list(.keep = keep) else list()))
  } else {
    ans <- if (verb == "summarize_with_margins") {
      summarize_with_margins(x, .grouping = group, .margin_label = label,
                             .id = "set", count = n())
    } else rlang::inject((!!get(verb))(
      x, .grouping = group, .margin_label = label, .id = "set",
      !!!if (startsWith(verb, "nest")) list(.keep = keep) else list()))
  }
  if (inherits(ans, "dtplyr_step")) ans <- as_tibble(ans, .name_repair = "minimal")
  ans
}
results <- list()
for (case in c("one", "ordinary", "unused_na", "used_na", "actual_missing")) {
  x <- tibble(g = new_factor(case), d = rep("key", length(new_factor(case))),
              row = seq_along(new_factor(case)))
  expected_codes <- codes(x$g)
  before <- serialize(x, NULL)
  for (backend in c("local", "dtplyr")) {
    source <- if (backend == "local") x else dtplyr::lazy_dt(x)
    source_before <- if (backend == "local") before else serialize(source[["parent"]], NULL)
    native <- if (backend == "local") source else collect(source)
    stopifnot(identical(codes(native$g), expected_codes),
              identical(levels(native$g), levels(x$g)))
    native_nested <- if (backend == "local") tidyr::nest(native, .by = g) else
      as_tibble(tidyr::nest(group_by(source, g), data = c(d, row)))
    stopifnot(identical(sort(codes(native_nested$g), na.last = TRUE),
                        sort(unique(expected_codes), na.last = TRUE)))
    for (role in c("variable", "fixed", "payload")) {
      for (verb in c("nest_with_margins", "nest_by_with_margins", "expand_with_margins", "summarize_with_margins")) {
        if (role == "payload" && verb == "summarize_with_margins") next
        for (keep in if (startsWith(verb, "nest")) c(FALSE, TRUE) else FALSE) {
          for (missing_label in c(FALSE, TRUE)) {
            ans <- observe(run(source, role, verb, keep, missing_label))
            error <- inherits(ans, "error")
            outer <- if (!error && "g" %in% names(ans)) codes(ans$g) else integer()
            inner <- if (!error && "data" %in% names(ans)) lapply(ans$data, function(cell) {
              if ("g" %in% names(cell)) codes(cell$g) else integer()
            }) else list()
            source_same <- if (backend == "local") identical(source_before, serialize(source, NULL)) else
              identical(source_before, serialize(source[["parent"]], NULL))
            stopifnot(source_same)
            same_codes <- function(a, b) identical(sort(a, na.last = TRUE), sort(b, na.last = TRUE))
            outer_ok <- if (role == "payload" && startsWith(verb, "nest")) NA else
              !error && same_codes(outer, if (verb == "expand_with_margins") expected_codes else unique(expected_codes))
            inner_ok <- if (length(inner) && (role == "payload" || keep)) {
              !error && same_codes(unlist(inner), expected_codes)
            } else NA
            results[[length(results) + 1L]] <- data.frame(case, backend, role, verb,
              keep, missing_label, error, source_same, outer_ok, inner_ok,
              outer = paste(outer, collapse = ","),
              inner = paste(vapply(inner, paste, "", collapse = ","), collapse = ";"),
              detail = if (error) conditionMessage(ans) else "")
          }
        }
      }
    }
  }
}
results <- do.call(rbind, results)
args <- commandArgs(trailingOnly = TRUE)
write.csv(results, if (length(args)) args[[1L]] else tempfile(fileext = ".csv"), row.names = FALSE)
print(results[results$case == "one" & results$role == "variable" & !results$keep, ], row.names = FALSE)
print(table(results$error))
print(sessionInfo())
