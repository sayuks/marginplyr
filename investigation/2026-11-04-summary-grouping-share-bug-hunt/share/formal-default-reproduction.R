Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
input <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
expression <- quote((function(prior = {base::rm(prior); 1}) {prior; sum(prior)})())
body_control <- quote((function(prior = 1) {prior; base::rm(prior); sum(prior)})())
results <- list()
for (kind in c("parent", "total")) {
  helper <- rlang::call2(paste0("share_of_", kind), quote(value))
  for (direction in c("after_share", "source")) {
    run <- function(expr) {
      tryCatch(if (direction == "after_share") {
        summarize_with_margins(input, value = sum(x), prior = !!helper,
          derived = !!expr, .grouping = rollup(g))
      } else {
        summarize_with_margins(input, prior = sum(x), value = !!expr,
          fraction = !!helper, .grouping = rollup(g))
      }, error = identity)
    }
    observed <- run(expression)
    control <- run(body_control)
    stopifnot(inherits(control, "marginplyr_error"))
    cat(kind, direction, "\n")
    print(observed)
    cat("Body removal control: Package refusal\n")
    results[[paste(kind,direction)]] <- observed
  }
}
if ("--assert" %in% commandArgs(TRUE)) {
  stopifnot(all(vapply(results, inherits, logical(1), "marginplyr_error")))
}
stopifnot(all(vapply(results, is.data.frame, logical(1))))
stopifnot(identical(results[["total after_share"]]$derived, c(100,300,400)))
stopifnot(identical(results[["total source"]]$value, c(1,3,4)))
