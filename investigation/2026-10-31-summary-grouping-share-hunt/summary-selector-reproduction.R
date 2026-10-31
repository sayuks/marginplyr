# Dated investigation evidence. Run from the checkout root.
pkgload::load_all(".", quiet = TRUE)

# Bare external vectors are deprecated, but still accepted by tidyselect.
# This reproduction compares their accepted behavior, with that warning quiet.
options(lifecycle_verbosity = "quiet")
data <- tibble::tibble(g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7))

run <- function(margin = FALSE, selection = quote(cols), prior = FALSE,
                throw_on_second = FALSE) {
  state <- new.env(parent = emptyenv())
  state$reads <- 0L
  state$stacks <- list()
  env <- new.env(parent = globalenv())
  makeActiveBinding("cols", function(value) {
    state$reads <- state$reads + 1L
    state$stacks[[state$reads]] <- vapply(
      sys.calls(), deparse1, character(1)
    )
    if (throw_on_second && state$reads > 1L) {
      stop("selector was read twice")
    }
    if (state$reads == 1L) "x" else "y"
  }, env)
  expr <- rlang::call2("across", selection, quote(sum), .ns = "dplyr")
  dots <- list(rlang::new_quosure(expr, env))
  if (prior) dots <- c(list(first = rlang::quo(1L)), dots)
  value <- tryCatch({
    if (margin) {
      summarize_with_margins(data, !!!dots, .by = g)
    } else {
      dplyr::summarise(data, !!!dots, .by = g)
    }
  }, error = identity)
  list(value = value, reads = state$reads, stacks = state$stacks)
}

expected <- run()
actual <- run(TRUE)
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(
    identical(actual$value, expected$value),
    identical(actual$reads, expected$reads)
  )
  cat("Desired selector results and evaluation counts agreed.\n")
  quit(status = 0L)
}
stopifnot(
  identical(expected$reads, 1L), identical(actual$reads, 2L),
  identical(names(expected$value), c("g", "x")),
  identical(expected$value$x, c(3L, 3L)),
  identical(names(actual$value), c("g", "y")),
  identical(actual$value$y, c(11, 7)),
  any(grepl("preflight_local_selection", actual$stacks[[1L]], fixed = TRUE)),
  !any(grepl("preflight_local_selection", actual$stacks[[2L]], fixed = TRUE))
)
cat("Ordinary: selector read once; x = 3, 3.\n")
cat("Margin: selector read twice; y = 11, 7.\n")
cat("Only the first Margin read came from preflight_local_selection().\n")

for (selection in list(
  quote(tidyselect::all_of(cols)), quote((cols)),
  quote(.env$cols), quote(.env[["cols"]])
)) {
  a <- run(selection = selection)
  b <- run(TRUE, selection = selection)
  stopifnot(identical(a$value, b$value), a$reads == 1L, b$reads == 1L)
}
a <- run(prior = TRUE)
b <- run(TRUE, prior = TRUE)
stopifnot(identical(a$value, b$value), a$reads == 1L, b$reads == 1L)
cat("Four selector controls and a preceding-summary control agreed.\n")

a <- run(throw_on_second = TRUE)
b <- run(TRUE, throw_on_second = TRUE)
stopifnot(
  !inherits(a$value, "error"), inherits(b$value, "error"),
  grepl("selector was read twice", conditionMessage(b$value), fixed = TRUE)
)
cat("A read-once binding succeeded ordinarily and failed in the Margin call.\n")
