# Run from the source root. --assert requires the alias-dependency guard.
pkgload::load_all(quiet = TRUE)
run <- function() {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), z = c(7, 9))
  inputs <- list(
    data.frame = data, tibble = tibble::as_tibble(data),
    data.table = data.table::as.data.table(data)
  )
  dispatch <- alist(
    direct = tidyselect::peek_data(),
    do_call = base::do.call(tidyselect::peek_data, list()),
    do_call_name = base::do.call(
      "peek_data", list(), envir = asNamespace("tidyselect")
    ),
    exec = rlang::exec(tidyselect::peek_data),
    get_export = base::getExportedValue("tidyselect", "peek_data")(),
    inline = (function(fn) fn())(tidyselect::peek_data),
    named = callback()
  )
  callback <- function() tidyselect::peek_data()
  statuses <- character()
  for (input in names(inputs)) {
    frame <- inputs[[input]]
    frozen <- serialize(frame, NULL)
    for (helper_name in c("share_of_parent", "share_of_total")) {
      helper <- rlang::call2(helper_name, quote(value))
      for (selector_name in c("pick", "across")) {
        for (dispatch_name in names(dispatch)) {
          read <- dispatch[[dispatch_name]]
          selected <- rlang::expr(dplyr::all_of(
            if ((!!read)$prior[[1L]] < 2) "z" else "x"
          ))
          # An ordinary lexical function that returns the proxy is understood
          # by the existing guard, just as a direct peek_data() call is.
          independent <- rlang::expr(dplyr::all_of(
            if ((!!read)$z[[1L]] > 5) "x" else "z"
          ))
          wrap <- function(selection) {
            if (selector_name == "pick") {
              rlang::expr(sum(dplyr::pick(!!selection)))
            } else {
              rlang::expr(sum(dplyr::across(!!selection, identity)))
            }
          }
          dependent <- wrap(selected)
          ordinary <- summarize_with_margins(
            frame, prior = sum(x), value = !!dependent,
            .grouping = rollup(g)
          )
          stopifnot(identical(ordinary$value, c(7, 9, 4)))
          got <- tryCatch(summarize_with_margins(
            frame, prior = sum(x), value = !!dependent,
            ratio = !!helper, .grouping = rollup(g)
          ), error = identity)
          if (inherits(got, "error")) {
            stopifnot(inherits(got, "marginplyr_error"),
                      grepl("depends on earlier summary alias `prior`",
                            conditionMessage(got), fixed = TRUE))
            status <- "refused"
          } else {
            stopifnot(identical(got$value, ordinary$value),
                      identical(got$ratio, c(1.75, 2.25, 1)))
            status <- "missed"
          }
          statuses <- c(statuses, status)
          # Use a direct access to establish that reading an ordinary input
          # from the active proxy is allowed even beside an earlier alias.
          if (dispatch_name == "direct") {
            unrelated <- summarize_with_margins(
              frame, prior = sum(x), value = !!wrap(independent),
              ratio = !!helper, .grouping = rollup(g)
            )
            stopifnot(identical(unrelated$value, c(1, 3, 4)),
                      identical(unrelated$ratio, c(0.25, 0.75, 1)))
          }
          cat(input, helper_name, selector_name, dispatch_name, status, "\n")
        }
      }
    }
    stopifnot(identical(serialize(frame, NULL), frozen))
  }
  print(table(statuses))
  if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
    stopifnot(all(statuses == "refused"))
  } else {
    stopifnot(sum(statuses == "refused") == 24L,
              sum(statuses == "missed") == 60L)
  }
}
run()
