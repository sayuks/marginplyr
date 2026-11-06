args <- commandArgs(trailingOnly = TRUE)
source_root <- if (length(args) && !startsWith(args[[1L]], "--")) {
  args[[1L]]
} else {
  getwd()
}
assert_contract <- "--assert" %in% args
controls_only <- "--controls-only" %in% args
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(source_root, quiet = TRUE)
source_sha <- if (file.exists(file.path(source_root, ".git"))) {
  system2("git", c("-C", shQuote(source_root), "rev-parse", "HEAD"), stdout = TRUE)
} else {
  "archive: see the coordinator's execution record"
}
cat("Source SHA:", source_sha, "\n")
cat(R.version.string, "\n")
for (package in c("dplyr", "rlang", "tidyselect", "vctrs", "tibble", "data.table")) {
  cat(package, as.character(packageVersion(package)), "\n")
}
inputs <- list(data.frame = identity, tibble = tibble::as_tibble,
               data.table = data.table::as.data.table)
rows <- list()
for (input_kind in names(inputs)) {
  for (operator in c("::", ":::")) {
    for (role in c("id", "fixed", "rollup")) {
      for (count in c(1L, 2L)) {
       for (shape in c("namespace_value", "namespace_list_call")) {
        protected <- if (shape == "namespace_value") "answer_1" else "answer_fake"
        payload <- stats::setNames(data.frame(key = c("a", "b"), x = c(1L, 3L)), c(protected, "x"))
        input <- inputs[[input_kind]](payload)
        input_x <- dplyr::select(input, "x")
        original <- serialize(input, NULL)
        selected <- if (count == 1L) list(s = sum) else list(s = sum, m = max)
        hits <- 0L
        caller <- rlang::env(selected = selected)
        caller[[operator]] <- function(pkg, name) {
          pkg_name <- as.character(substitute(pkg))
          member <- as.character(substitute(name))
          target <- if (shape == "namespace_value") "sum" else "list"
          if (identical(pkg_name, "base") && identical(member, target)) {
            hits <<- hits + 1L
            return(if (shape == "namespace_value") selected else function(...) selected)
          }
          get(member, envir = asNamespace(pkg_name))
        }
        namespace_expr <- if (shape == "namespace_value") {
          rlang::call2(operator, quote(base), quote(sum))
        } else {
          rlang::call2(rlang::call2(operator, quote(base), quote(list)), fake = quote(mean))
        }
        expression <- rlang::expr(dplyr::across(
          x, !!namespace_expr, .names = "answer_{.fn}"
        ))
        summary <- rlang::new_quosure(expression, caller)
        literal <- rlang::new_quosure(rlang::expr(dplyr::across(
          x, !!selected, .names = "answer_{.fn}"
        )), caller)
        ordinary <- if (role == "id") {
          dplyr::summarise(input_x, !!summary)
        } else {
          dplyr::summarise(input, !!summary, .by = dplyr::all_of(protected))
        }
        expected <- ordinary
        if (role == "id") {
          expected <- dplyr::mutate(expected, !!protected := 1L, .before = 1L)
        }
        if (role == "rollup") {
          total <- dplyr::summarise(input, !!summary)
          total <- dplyr::mutate(total, !!protected := NA_character_, .before = 1L)
          expected <- dplyr::bind_rows(ordinary, total)
        }
        call <- function(dot) {
          switch(role,
            id = summarize_with_margins(input_x, !!dot, .id = protected),
            fixed = summarize_with_margins(input, !!dot, .by = dplyr::all_of(protected)),
            rollup = summarize_with_margins(
              input, !!dot, .grouping = rollup(dplyr::all_of(protected)), .margin_label = NULL
            )
          )
        }
        expected_hits <- hits
        hits <- 0L
        control <- call(literal)
        stopifnot(identical(control, expected), identical(hits, 0L))
        actual <- tryCatch(call(summary), error = identity)
        observation <- inherits(actual, "marginplyr_error") &&
          grepl(if (role == "id") "conflicts with a summary output" else "overwrite grouping column",
                conditionMessage(actual), fixed = TRUE)
        if (assert_contract || controls_only) {
          checked <- if (controls_only) control else actual
          stopifnot(identical(checked, expected))
        } else {
          stopifnot(observation, identical(hits, 0L))
        }
        stopifnot(identical(serialize(input, NULL), original))
        rows[[length(rows) + 1L]] <- data.frame(
          input_kind, operator, role, shape, functions = count,
          expected_names = paste(names(expected), collapse = ";"),
          expected_values = paste(vapply(expected, paste, "", collapse = ","), collapse = ";"),
          observed = if (inherits(actual, "error")) conditionMessage(actual) else "accepted",
          namespace_hits = hits, ordinary_hits = expected_hits,
          literal_control = identical(control, expected)
        )
       }
      }
    }
  }
}
result <- do.call(rbind, rows)
if (!assert_contract && !controls_only) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  write.csv(result, file.path(dirname(script), "across-namespace-shape.csv"), row.names = FALSE)
}
cat("Comparisons:", nrow(result), "\n")
cat("Literal controls:", sum(result$literal_control), "\n")
cat("Mode:", if (assert_contract) "desired contract" else if (controls_only) "controls only" else "dated observation", "\n")
