# Run from the repository root; --assert requests the documented refusals.
pkgload::load_all(".", quiet = TRUE)
assert_desired <- "--assert" %in% commandArgs(trailingOnly = TRUE)
capture <- function(expr) tryCatch(expr, error = identity)
input <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
mutations <- alist(
  (get("parent.env<-"))(outside, environment()),
  (match.fun("parent.env<-"))(outside, environment()),
  (getFunction("parent.env<-"))(outside, environment()),
  do.call("parent.env<-", list(outside, environment()))
)
canonical <- alist(
  parent.env(outside) <- environment(),
  base::`parent.env<-`(outside, environment())
)
inputs <- list(frame = input, tibble = tibble::as_tibble(input),
               data_table = data.table::as.data.table(input))
missing_refusals <- controls <- 0L

run <- function(data, mutation, kind, direction, shares = TRUE,
                external = FALSE) {
  outside <- new.env(parent = baseenv())
  if (external) outside$prior <- 7
  expression <- rlang::expr({
    !!mutation
    sum(get("prior", envir = outside, inherits = !!(!external)))
  })
  helper <- rlang::call2(paste0("share_of_", kind), quote(value))
  observed <- capture(if (direction == "source") {
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
  })
  parent.env(outside) <- baseenv()
  observed
}
for (data in inputs) {
  for (kind in c("parent", "total")) {
    for (direction in c("source", "later")) {
      for (mutation in mutations) {
        observed <- run(data, mutation, kind, direction)
        stopifnot(is.data.frame(observed))
        column <- if (direction == "source") "value" else "derived"
        expected <- if (direction == "source") c(1, 3, 4) else c(100, 300, 400)
        stopifnot(identical(observed[[column]], expected))
        missing_refusals <- missing_refusals + 1L
        ordinary <- run(data, mutation, kind, "source", shares = FALSE)
        stopifnot(identical(ordinary$value, c(1, 3, 4)))
        external <- run(data, mutation, kind, direction, external = TRUE)
        stopifnot(identical(external[[column]], c(7, 7, 7)))
        controls <- controls + 2L
      }
      for (mutation in canonical) {
        observed <- run(data, mutation, kind, direction)
        stopifnot(inherits(observed, "marginplyr_error"))
        wanted <- if (direction == "source") {
          "depends on earlier summary alias"
        } else {
          "Ordinary summaries cannot use an earlier"
        }
        stopifnot(grepl(wanted, conditionMessage(observed), fixed = TRUE))
        controls <- controls + 1L
      }
    }
  }
}
cat("Indirect reparenting missing refusals:", missing_refusals, "\n")
cat("Canonical, no-share, non-inheriting controls passed:", controls, "\n")
cat("Source values: 1,3,4; later derived values: 100,300,400\n")
if (assert_desired) stopifnot(missing_refusals == 0L)
