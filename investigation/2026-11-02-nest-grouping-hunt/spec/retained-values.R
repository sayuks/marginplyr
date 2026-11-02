# Compare retained constructor-shaped answers with one ordinary tidyselect call.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
d <- tibble::tibble(a = 1:2, b = c("x", "y"), z = 3:4)
constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
values <- list("a", c("a", "z"), 1L, -2L, integer(), NULL, TRUE, NA_integer_,
  stats::setNames("a", "z"), structure("a", class = "custom_selection"),
  function(x) is.numeric(x), quote(a), quote(c(a,z)), rlang::quo(a), expression(a), pairlist(a = 1), new.env())
tidyselect_direct <- function(q) tidyselect::eval_select(q, d, allow_rename = FALSE)
rows <- list()
conditions <- function(expr, env) {
  warn <- list()
  result <- tryCatch(withCallingHandlers(eval(expr, env), warning = function(cnd) {
    warn[[length(warn) + 1L]] <<- cnd
    invokeRestart("muffleWarning")
  }), error = identity)
  list(result = result, warnings = warn)
}
for (value_index in seq_along(values)) {
  for (constructor in constructors) {
    for (shape in c("call", "qualified", "binding")) {
      for (parens in 0:2) {
        env <- new.env(parent = environment())
        env$n <- 0L
        env$value <- values[[value_index]]
        env$f <- function(...) {env$n <- env$n + 1L; env$value}
        env$grouping_set <- env$f
        env$`::` <- function(...) env$f
        makeActiveBinding("selected", function(value) {if (!missing(value)) stop("read only"); env$f()}, env)
        sel <- switch(shape, call = quote(grouping_set()), qualified = quote(marginplyr::grouping_set()), binding = quote(selected))
        for (unused in seq_len(parens)) sel <- rlang::call2("(", sel)
        direct <- conditions(rlang::call2("tidyselect_direct", rlang::new_quosure(sel, env)), env)
        spec <- eval(rlang::call2(get(constructor, asNamespace("marginplyr")), sel), env)
        env$n <- 0L
        actual <- conditions(rlang::call2("inspect_grouping", d, .grouping = spec, .format = "list"), env)
        stopifnot(env$n == 1L)
        expected <- if (value_index == 9L) "rename-refusal" else if (inherits(direct$result, "error")) "selection-error" else if (length(direct$result) == 0L && constructor %in% c("rollup", "cube")) "empty-unit-error" else "success"
        observed <- if (inherits(actual$result, "error")) {
          if (inherits(actual$result, "marginplyr_error")) "package-error" else "selection-error"
        } else "success"
        match <- (expected == observed) || (expected %in% c("empty-unit-error", "rename-refusal") && observed == "package-error")
        if (expected == "selection-error" && observed == "selection-error") stopifnot(identical(class(direct$result), class(actual$result)), identical(conditionMessage(direct$result), conditionMessage(actual$result)))
        if (expected == "success" && observed == "success") {
          selected <- unname(names(d)[unname(direct$result)])
          answer <- actual$result$included[[1L]]
          stopifnot(setequal(selected, answer))
        }
        rows[[length(rows) + 1L]] <- data.frame(value_index, constructor, shape, parens, expected, observed, match)
      }
    }
  }
}
write.csv(do.call(rbind, rows), "investigation/2026-11-02-nest-grouping-hunt/spec/retained-values.csv", row.names = FALSE)
stopifnot(all(vapply(rows, function(x) x$match, logical(1))))
cat("Completed", length(rows), "retained-value comparisons\n")
print(sessionInfo())
