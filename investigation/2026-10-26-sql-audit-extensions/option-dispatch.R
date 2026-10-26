# Run from the checkout root. Exit 1 means a malformed audit option changed
# ordinary public-call behavior; exit 0 means both calls completed normally.
args <- commandArgs(TRUE)
if (length(args) > 1L) stop("Usage: Rscript option-dispatch.R [fresh-output.rds]")
if (length(args) && file.exists(args[[1L]])) stop("Output already exists")
pkgload::load_all(quiet = TRUE)

is.na.audit_option <- function(x) stop("audit option method ran")

main <- function() {
  old <- options(marginplyr.audit_sql = FALSE)
  on.exit(options(old), add = TRUE)
  run <- function(value) {
    options(marginplyr.audit_sql = value)
    tryCatch(
      inspect_grouping(data.frame(g = "a"), .grouping = rollup(g)),
      error = identity
    )
  }
  control <- run(FALSE)
  actual <- run(structure(FALSE, class = "audit_option"))
  describe <- function(x) {
    list(classes = class(x), message = if (inherits(x, "condition")) {
      conditionMessage(x)
    } else NULL)
  }
  evidence <- list(control = describe(control), actual = describe(actual))
  print(evidence)
  if (length(args)) saveRDS(evidence, args[[1L]])
  stopifnot(!inherits(control, "condition"))
  stopifnot(!inherits(actual, "condition"))
}
main()
