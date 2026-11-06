Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
dat <- data.frame(a = "x", b = 1L, c = TRUE)
forms <- list(
  public = function(x) rlang::call2(quote(base::`(`), x),
  private = function(x) rlang::call2(rlang::parse_expr("base:::`(`"), x),
  syntactic = function(x) rlang::call2("(", x),
  public_head = function(x) rlang::call2(quote((base::`(`)), x),
  private_head = function(x) rlang::call2(rlang::parse_expr("(base:::`(`)"), x),
  double_head = function(x) rlang::call2(rlang::parse_expr("((base::`(`))"), x),
  syntactic_head = function(x) rlang::call2(quote((`(`)), x)
)
records <- list()
excluded <- 0L
observe <- function(expr) {
  tryCatch(expr, error = function(cnd) {
    paste("ERROR", class(cnd)[1], conditionMessage(cnd))
  })
}
for (outer in names(forms)) for (inner in c("none", names(forms))) {
  for (payload in c("bound", "constructor", "injected")) {
    state <- new.env(parent = emptyenv())
    state$n <- 0L
    caller <- rlang::env(s = grouping_set(a))
    caller[["::"]] <- caller[[":::"]] <- function(...) {
      state$n <- state$n + 1L
      function(x) { if (is.character(tidyselect::peek_data()$a)) "b" else "a" }
    }
    leaf <- switch(payload, bound = quote(s),
                   constructor = quote(grouping_set(a)),
                   injected = rlang::new_quosure(quote(s), caller))
    expr <- if (inner == "none") leaf else forms[[inner]](leaf)
    expr <- forms[[outer]](expr)
    q <- rlang::new_quosure(expr, caller)
    expected <- observe(names(tidyselect::eval_select(q, dat)))
    reference_n <- state$n
    if (length(expected) && startsWith(expected[[1L]], "ERROR")) {
      excluded <- excluded + 5L
      next
    }
    constructors <- c("grouping_set", "grouping_sets", "rollup", "cube",
                      "grouping_spec")
    for (constructor in constructors) {
      spec <- eval(rlang::call2(constructor, q), caller)
      state$n <- 0L
      actual <- observe(inspect_grouping(
        dat, .grouping = spec, .format = "list"
      )$included[[1]])
      inspection_n <- state$n
      state$n <- 0L
      expanded <- expand_with_margins(dat, .grouping = spec,
                                      .id = "set", .margin_label = NULL)
      expected_spec <- eval(rlang::call2(constructor, expected))
      reference <- expand_with_margins(dat, .grouping = expected_spec,
                                       .id = "set", .margin_label = NULL)
      stopifnot(identical(expanded, reference),
                identical(state$n, reference_n))
      records[[length(records) + 1L]] <- data.frame(
        outer, inner, payload, constructor,
        expected = paste(expected, collapse = ","),
        actual = paste(actual, collapse = ","),
        reference_n, inspection_n, expansion_n = state$n,
        same = identical(expected, actual) &&
          identical(reference_n, inspection_n) &&
          identical(reference_n, state$n)
      )
    }
  }
}
results <- do.call(rbind, records)
write.csv(results, file.path(Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempdir()),
                           "namespace-grid.csv"), row.names = FALSE)
cat("Excluded syntactic grammar comparisons:", excluded, "\n")
print(table(results$same))
print(results[!results$same, ], row.names = FALSE)
stopifnot(all(results$same))
