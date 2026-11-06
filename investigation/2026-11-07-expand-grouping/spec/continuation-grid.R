Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
set.seed(61107)
data <- data.frame(a = "x", b = 1L, c = TRUE, d = "y")
state <- new.env(parent = emptyenv())
state$events <- character()
build <- function(depth = 0L) {
  if (depth == 3L || runif(1) < 0.5) {
    column <- sample(c("a", "b", "c", "d"), 1)
    nested <- sample(c(TRUE, FALSE), 1)
    tag <- paste0("leaf-", length(state$expected) + 1L)
    state$expected <- c(state$expected, tag)
    caller <- rlang::env(s = grouping_set(a))
    caller[["::"]] <- function(...) {
      state$events <- c(state$events, tag)
      stopifnot(is.character(tidyselect::peek_data()$a))
      function(x) {
        if (nested) grouping_set(!!rlang::sym(column)) else column
      }
    }
    q <- rlang::new_quosure(quote(base::`(`(s)), caller)
    return(list(actual = q, expected = grouping_set(!!rlang::sym(column))))
  }
  count <- sample(1:3, 1)
  constructor <- sample(c("grouping_sets", "grouping_spec"), 1)
  children <- lapply(seq_len(count), function(i) build(depth + 1L))
  list(
    actual = eval(rlang::call2(constructor,
                            !!!lapply(children, function(x) x$actual))),
    expected = eval(rlang::call2(constructor,
                              !!!lapply(children, function(x) x$expected)))
  )
}
results <- list()
for (i in seq_len(200L)) {
  state$expected <- character()
  tree <- build()
  if (rlang::is_quosure(tree$actual)) {
    tree$actual <- grouping_sets(!!tree$actual)
  }
  for (policy in c("keep", "drop")) {
    state$events <- character()
    actual <- inspect_grouping(data, .grouping = tree$actual,
                               .duplicates = policy, .format = "list")
    expected <- inspect_grouping(data, .grouping = tree$expected,
                                 .duplicates = policy, .format = "list")
    same <- identical(actual, expected)
    order_same <- identical(state$events, state$expected)
    results[[length(results) + 1L]] <- data.frame(
      shape = i, policy, leaves = length(state$expected),
      occurrences = nrow(actual), same, order_same
    )
  }
}
results <- do.call(rbind, results)
write.csv(results, file.path(Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempdir()),
                           "continuation-grid.csv"), row.names = FALSE)
print(table(results$same, results$order_same))
cat("Leaf callbacks:", sum(results$leaves), "\n")
stopifnot(all(results$same), all(results$order_same))
