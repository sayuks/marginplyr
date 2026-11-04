# Run from the source root. --assert requires both dependency refusals.
pkgload::load_all(quiet = TRUE)
run <- function() {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  inputs <- list(
    data.frame = data, tibble = tibble::as_tibble(data),
    data.table = data.table::as.data.table(data)
  )
  removals <- alist(
    implicit_rm = base::local(base::rm("prior", inherits = TRUE)),
    implicit_remove = base::local(base::remove(prior, inherits = 1L)),
    explicit_env = base::local(
      base::rm("prior", inherits = TRUE), envir = environment()
    ),
    inline = (function() base::rm("prior", inherits = TRUE))(),
    local_only = base::local({
      prior <- 2
      base::rm("prior", inherits = TRUE)
    })
  )
  statuses <- character()
  for (input in names(inputs)) {
    frame <- inputs[[input]]
    frozen <- serialize(frame, NULL)
    for (helper_name in c("share_of_parent", "share_of_total")) {
      helper <- rlang::call2(helper_name, quote(value))
      for (name in names(removals)) {
        removal <- removals[[name]]
        expr <- rlang::expr((function(prior = 1) {
          !!removal
          sum(prior)
        })())
        ordinary <- summarize_with_margins(
          frame, prior = sum(x), value = !!expr, .grouping = rollup(g)
        )
        expected_value <- if (name == "local_only") c(1, 1, 1) else c(1, 3, 4)
        stopifnot(identical(ordinary$value, expected_value))
        for (direction in c("after_share", "share_source")) {
          got <- tryCatch(
            if (direction == "after_share") {
              summarize_with_margins(
                frame, value = sum(x), prior = !!helper, derived = !!expr,
                .grouping = rollup(g)
              )
            } else {
              summarize_with_margins(
                frame, prior = sum(x), value = !!expr, ratio = !!helper,
                .grouping = rollup(g)
              )
            }, error = identity
          )
          if (inherits(got, "error")) {
            expected <- if (direction == "after_share") {
              "Ordinary summaries cannot use an earlier"
            } else {
              "depends on earlier summary alias"
            }
            stopifnot(inherits(got, "marginplyr_error"),
                      grepl(expected, conditionMessage(got), fixed = TRUE))
            status <- if (name == "local_only") "false_refusal" else "refused"
          } else {
            expected_ratio <- if (name == "local_only") rep(1, 3) else c(0.25, 0.75, 1)
            if (direction == "after_share") {
              stopifnot(identical(got$prior, c(0.25, 0.75, 1)))
              expected_derived <- if (name == "local_only") {
                rep(1, 3)
              } else {
                c(100, 300, 400)
              }
              stopifnot(identical(got$derived, expected_derived))
            } else {
              stopifnot(identical(got$value, expected_value),
                        identical(got$ratio, expected_ratio))
            }
            status <- if (name == "local_only") "local_control" else "missed"
          }
          statuses <- c(statuses, status)
          cat(input, helper_name, name, direction, status, "\n")
        }
      }
    }
    stopifnot(identical(serialize(frame, NULL), frozen))
  }
  print(table(statuses))
  if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
    stopifnot(all(statuses %in% c("refused", "local_control")))
  } else {
    stopifnot(sum(statuses == "refused") == 24L,
              sum(statuses == "missed") == 24L,
              sum(statuses == "local_control") == 12L)
  }
}
run()
