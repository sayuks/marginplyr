# Historical confirmation matrix. Exit 0 confirms the observed method errors;
# option-dispatch.R is the red-capable regression reproduction.
args <- commandArgs(TRUE)
stopifnot(length(args) <= 1L)
if (length(args) && file.exists(args[[1L]])) stop("Output already exists")
pkgload::load_all(quiet = TRUE)
is.na.audit_option <- function(x) stop("audit option method ran")

main <- function() {
  old <- options()
  on.exit(options(old), add = TRUE)
  local_input <- data.frame(g = "a", v = 1)
  remote <- dbplyr::tbl_lazy(local_input)
  verbs <- list(
    summarize = function(x) summarize_with_margins(
      x, total = sum(v), .grouping = rollup(g)
    ),
    summarise = function(x) summarise_with_margins(
      x, total = sum(v), .grouping = rollup(g)
    ),
    expand = function(x) expand_with_margins(x, .grouping = rollup(g)),
    nest = function(x) nest_with_margins(x, .grouping = rollup(g)),
    nest_by = function(x) nest_by_with_margins(x, .grouping = rollup(g)),
    inspect = function(x) inspect_grouping(x, .grouping = rollup(g))
  )
  entries <- list()
  for (input in c("local", "sql")) {
    x <- if (input == "local") local_input else remote
    for (name in names(verbs)) {
      options(marginplyr.audit_sql = FALSE)
      control <- tryCatch(verbs[[name]](x), error = identity)
      for (value in c(FALSE, TRUE)) {
        options(marginplyr.audit_sql = structure(value, class = "audit_option"))
        actual <- tryCatch(verbs[[name]](x), error = identity)
        entries[[length(entries) + 1L]] <- list(
          input = input, verb = name, value = value,
          control_error = inherits(control, "error"),
          actual_classes = class(actual),
          actual_message = if (inherits(actual, "condition")) {
            conditionMessage(actual)
          } else NULL
        )
      }
    }
  }
  stopifnot(length(entries) == 24L)
  stopifnot(all(vapply(entries, function(x) {
    identical(x$actual_message, "audit option method ran")
  }, logical(1))))
  successful_controls <- sum(!vapply(entries, `[[`, logical(1), "control_error"))
  stopifnot(successful_controls == 20L)
  cat("24 method errors;", successful_controls,
      "successful ordinary-FALSE controls; 4 pre-existing SQL nesting refusals.\n")
  if (length(args)) saveRDS(entries, args[[1L]])
}
main()
