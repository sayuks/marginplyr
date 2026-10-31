# Run from the repository root; --assert requests the documented refusals.
pkgload::load_all(".", quiet = TRUE)
assert_desired <- "--assert" %in% commandArgs(trailingOnly = TRUE)
capture <- function(expr) tryCatch(expr, error = identity)
input <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
removals <- alist(
  (get("rm"))(prior),
  (match.fun("rm"))(prior),
  (getFunction("rm"))(prior),
  do.call("rm", list("prior")),
  (get("remove"))(prior),
  (match.fun("remove"))(prior),
  do.call("remove", list("prior"))
)
canonical <- alist(rm(prior), (rm)(prior), base::rm(prior), remove(prior))
inputs <- list(frame = input, tibble = tibble::as_tibble(input),
               data_table = data.table::as.data.table(input))
missing_refusals <- controls <- 0L

run <- function(data, expression, kind, direction, shares = TRUE) {
  helper <- rlang::call2(paste0("share_of_", kind), quote(value))
  if (direction == "source") {
    dots <- if (shares) list(fraction = helper) else list()
    summarize_with_margins(
      data, prior = sum(x), value = !!expression, !!!dots,
      .grouping = rollup(g)
    )
  } else {
    summarize_with_margins(
      data, value = sum(x), prior = !!helper, derived = !!expression,
      .grouping = rollup(g)
    )
  }
}
for (data in inputs) {
  for (kind in c("parent", "total")) {
    for (direction in c("source", "later")) {
      for (removal in removals) {
        expression <- rlang::expr({
          prior <- 2
          !!removal
          sum(prior)
        })
        observed <- capture(run(data, expression, kind, direction))
        if (assert_desired) {
          stopifnot(inherits(observed, "marginplyr_error"))
          wanted <- if (direction == "source") {
            "depends on earlier summary alias"
          } else {
            "Ordinary summaries cannot use an earlier"
          }
          stopifnot(grepl(wanted, conditionMessage(observed), fixed = TRUE))
          next
        }
        stopifnot(is.data.frame(observed))
        expected <- if (direction == "source") c(1, 3, 4) else c(100, 300, 400)
        column <- if (direction == "source") "value" else "derived"
        stopifnot(identical(observed[[column]], expected))
        missing_refusals <- missing_refusals + 1L
        ordinary <- run(data, expression, kind, "source", shares = FALSE)
        stopifnot(identical(ordinary$value, c(1, 3, 4)))
        controls <- controls + 1L
      }
      for (removal in canonical) {
        expression <- rlang::expr({
          prior <- 2
          !!removal
          sum(prior)
        })
        observed <- capture(run(data, expression, kind, direction))
        stopifnot(inherits(observed, "marginplyr_error"))
        wanted <- if (direction == "source") {
          "depends on earlier summary alias"
        } else {
          "Ordinary summaries cannot use an earlier"
        }
        stopifnot(grepl(wanted, conditionMessage(observed), fixed = TRUE))
        controls <- controls + 1L
      }
      independent <- run(data, quote({prior <- 2; sum(prior)}), kind, direction)
      column <- if (direction == "source") "value" else "derived"
      stopifnot(identical(independent[[column]], c(2, 2, 2)))
      controls <- controls + 1L
    }
  }
}
cat("Indirect removal missing refusals:", missing_refusals, "\n")
cat("Canonical, no-share, and independent controls passed:", controls, "\n")
cat("Source values: 1,3,4; later derived values: 100,300,400\n")
if (assert_desired) stopifnot(missing_refusals == 0L)
