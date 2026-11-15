expect_share_dependencies <- function(
  data, reads, reverse_message = "depends on earlier summary alias") {
  env <- parent.frame()
  for (kind in c("parent", "total")) {
    for (read in reads) {
      read <- rlang::as_quosure(read, env)
      share <- rlang::call2(paste0("share_of_", kind), quote(source))
      expect_error(
        summarize_with_margins(
          data, source = sum(.data$x), prior = !!share, observed = !!read,
          .grouping = rollup("g")
        ),
        "Ordinary summaries cannot use an earlier", class = "marginplyr_error"
      )
      share <- rlang::call2(paste0("share_of_", kind), quote(observed))
      expect_error(
        summarize_with_margins(
          data, prior = sum(.data$x), observed = !!read, ratio = !!share,
          .grouping = rollup("g")
        ),
        reverse_message, class = "marginplyr_error"
      )
    }
  }
}

expect_share_dependency_values <- function(data, reads, expected) {
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (read in reads) {
      forward <- summarize_with_margins(
        data, value = sum(.data$x), prior = !!helper, observed = !!read,
        .grouping = rollup("g")
      )
      reverse <- summarize_with_margins(
        data, prior = sum(.data$x), value = !!read, ratio = !!helper,
        .grouping = rollup("g")
      )
      expect_equal(forward$observed, rep(expected, 3L))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      expect_equal(reverse$value, rep(expected, 3L))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
}

# The same read with each `rm()` call replaced by `invisible(NULL)`, so a
# closure reads the local binding instead of what a removal would expose. A
# quosure keeps its environment.
without_removal <- function(read) {
  if (rlang::is_quosure(read)) {
    expr <- without_removal(rlang::quo_get_expr(read))
    return(rlang::quo_set_expr(read, expr))
  }
  if (!is.call(read)) {
    return(read)
  }
  if (rlang::is_call(read, "rm")) {
    return(quote(invisible(NULL)))
  }
  parts <- as.list(read)
  for (index in seq_along(parts)) {
    if (!rlang::is_missing(parts[[index]])) {
      parts[index] <- list(without_removal(parts[[index]]))
    }
  }
  as.call(parts)
}
