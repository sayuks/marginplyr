args <- commandArgs(trailingOnly = TRUE)
source_root <- if (length(args)) args[[1L]] else getwd()
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(source_root, quiet = TRUE)
for (operator in c("::", ":::")) {
  for (grouped in c(FALSE, TRUE)) {
    data <- if (grouped) data.frame(g = c("a", "b"), x = c(1L, 3L)) else data.frame(x = c(1L, 3L))
    caller <- rlang::env()
    caller[[operator]] <- function(pkg, name) {
      package <- as.character(substitute(pkg))
      member <- as.character(substitute(name))
      if (identical(package, "base") && identical(member, "sum")) {
        return(list(s = sum, m = max))
      }
      get(member, envir = asNamespace(package))
    }
    qualified <- rlang::call2(operator, quote(base), quote(sum))
    ordinary <- rlang::new_quosure(rlang::expr(dplyr::across(x, !!qualified)), caller)
    literal <- rlang::quo(dplyr::across(x, list(s = sum, m = max)))
    run <- function(dot) {
      summarize_with_margins(
        data, !!dot,
        dplyr::across(dplyr::starts_with("x"), share_of_total, .names = "{.col}_p"),
        .grouping = if (grouped) rollup("g") else NULL
      )
    }
    control <- run(literal)
    stopifnot(all(c("x_s", "x_m", "x_s_p", "x_m_p") %in% names(control)))
    actual <- tryCatch(run(ordinary), error = identity)
    stopifnot(inherits(actual, "rlang_error"), !inherits(actual, "marginplyr_error"),
              grepl("names|name", conditionMessage(actual)))
    cat(operator, if (grouped) "rollup" else "dimensionless", "\n")
    cat(conditionMessage(actual), "\n")
  }
}
cat("Four related shape-mismatch errors observed; literal-list controls accepted.\n")
