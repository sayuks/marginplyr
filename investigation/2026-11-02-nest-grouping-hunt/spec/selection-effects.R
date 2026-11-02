# Finite observational probe; no package implementation is changed.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
d <- tibble::tibble(a = 1:2, b = c("x", "y"), z = 3:4)
constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
rows <- list()
record <- function(id, expr, env) {
  warnings <- list()
  value <- tryCatch(withCallingHandlers(eval(expr, env), warning = function(cnd) {
    warnings[[length(warnings) + 1L]] <<- list(class = class(cnd), message = conditionMessage(cnd))
    invokeRestart("muffleWarning")
  }), error = identity)
  stopifnot(!inherits(value, "error"),
    sum(vapply(warnings, function(x) "probe_warning" %in% x$class, logical(1))) == 1L)
  rows[[length(rows) + 1L]] <<- data.frame(id, events = paste(env$events, collapse = ","),
    warnings = paste(vapply(warnings, function(x) x$message, character(1)), collapse = "|"),
    outcome = if (inherits(value, "error")) paste(class(value)[[1L]], conditionMessage(value)) else paste(value$included, collapse = "|"))
  value
}
for (constructor in constructors) {
  for (typed in c(FALSE, TRUE)) {
    for (shape in c("closure", "binding", "constructor", "qualified_constructor", "injected_closure")) {
      for (parens in 0:3) {
        env <- new.env(parent = environment())
        env$events <- character()
        env$f <- function() {env$events <- c(env$events, "f"); warning(structure(list(message = "PROBE", call = NULL), class = c("probe_warning", "warning", "condition"))); "a"}
        env$grouping_set <- env$f
        if (shape == "binding") makeActiveBinding("selected", function(value) {if (!missing(value)) stop("read only"); env$f()}, env)
        env$`::` <- function(pkg, name) {env$events <- c(env$events, "ns"); env$f}
        sel <- switch(shape, closure = quote(all_of(f())), binding = quote(selected), constructor = quote(grouping_set()), qualified_constructor = quote(marginplyr::grouping_set()), injected_closure = rlang::new_quosure(quote(all_of(f())), env))
        for (unused in seq_len(parens)) sel <- rlang::call2("(", sel)
        env$spec <- eval(rlang::call2(get(constructor, asNamespace("marginplyr")), sel), env)
        by <- if (typed) quote(where(function(x) {env$events <- c(env$events, "by"); is.character(x)})) else NULL
        call <- rlang::call2("inspect_grouping", quote(d), .by = by, .grouping = quote(spec), .format = "list")
        result <- record(paste(constructor, typed, shape, parens, sep = "/"), call, env)
        stopifnot(identical(result$included[[1L]], "a"))
        n_f <- sum(env$events == "f")
        if (n_f != 1L) { print(tail(rows, 1L)); stop("unexpected f count: ", n_f) }
      }
    }
  }
}
write.csv(do.call(rbind, rows), "investigation/2026-11-02-nest-grouping-hunt/spec/selection-effects.csv", row.names = FALSE)
cat("Completed", length(rows), "condition/effect combinations\n")
print(sessionInfo())
