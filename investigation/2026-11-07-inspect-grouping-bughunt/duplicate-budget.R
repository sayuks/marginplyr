# Run with R_MAX_VSIZE=96M. The largest product takes tens of seconds on R 4.6.1.
pkgload::load_all(".", quiet = TRUE)
capture <- function(expr) tryCatch(expr, error = identity)
cube_of <- function(n) rlang::inject(cube(!!!rep(list(quote(k)), n)))
product_of <- function(n) rlang::inject(grouping_spec(!!!rep(
  list(grouping_sets(k, grouping_set())), n
)))
main <- function(mode) {
  stopifnot(mem.maxVSize() == 96)
  input <- data.frame(k = 1L)
  inspect <- function(spec, policy) capture(inspect_grouping(
    input, .grouping = spec, .duplicates = policy, .format = "list"
  ))
  for (family in list(cube_of, product_of)) {
    small <- inspect(family(3L), "error")
    kept <- inspect(family(3L), "keep")
    dropped <- inspect(family(20L), "drop")
    stopifnot(
      inherits(small, "marginplyr_error"),
      grepl("1, 2, 3, 4, 5, 6, 7", conditionMessage(small), fixed = TRUE),
      identical(kept$set_id, 1:8),
      identical(dropped$included, list("k", character())),
      identical(dropped$set_id, 1:2),
      identical(dropped$grouping_id, 0:1)
    )
  }
  started <- proc.time()[["elapsed"]]
  cube_error <- inspect(cube_of(20L), "error")
  print(list(family = "cube", seconds = proc.time()[["elapsed"]] - started,
             class = class(cube_error)[[1L]],
             diagnostic = substr(conditionMessage(cube_error), 1L, 100L)))
  started <- proc.time()[["elapsed"]]
  product_error <- inspect(product_of(20L), "error")
  print(list(family = "product", seconds = proc.time()[["elapsed"]] - started,
             class = class(product_error)[[1L]],
             diagnostic = substr(conditionMessage(product_error), 1L, 100L)))
  errors <- list(cube_error, product_error)
  passed <- if (mode == "observed") {
    all(vapply(errors, function(e) {
      inherits(e, "simpleError") &&
        grepl("vector memory limit", conditionMessage(e), fixed = TRUE)
    }, logical(1)))
  } else {
    all(vapply(errors, function(e) {
      inherits(e, "marginplyr_error") &&
        grepl("Duplicate grouping sets", conditionMessage(e), fixed = TRUE)
    }, logical(1)))
  }
  if (!passed) quit(status = 1L)
}

mode <- commandArgs(trailingOnly = TRUE)
stopifnot(length(mode) == 1L, mode %in% c("observed", "contract"))
main(mode)
