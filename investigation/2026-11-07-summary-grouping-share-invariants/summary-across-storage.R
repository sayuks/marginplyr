Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
mode <- commandArgs(trailingOnly = TRUE)
if (length(mode) == 0L) mode <- "observe"
stopifnot(mode %in% c("observe", "contract", "controls"))

as.list.margin_hunt_list_view <- function(x, ...) {
  if (nrow(x) > 0L && identical(attr(x, "view"), "refuse")) {
    stop("Positive-row list conversion is unavailable.", call. = FALSE)
  }
  columns <- unclass(x)
  attributes(columns) <- list(names = names(x))
  columns$x <- as.character(columns$x)
  columns$y <- seq_along(columns$y)
  columns
}

report <- function(data, helper, share, plan) {
  across <- rlang::expr(dplyr::across(
    tidyselect::where(is.numeric), length, .names = "count_{.col}"
  ))
  options <- if (plan == "rollup") rlang::quo(rollup(g)) else rlang::quo(NULL)
  extra <- switch(share,
    none = list(),
    direct = list(share = rlang::call2(helper, rlang::sym("total"))),
    selected = list(rlang::expr(dplyr::across(
      tidyselect::starts_with("total"), !!rlang::sym(helper), .names = "share"
    )))
  )
  summarize_with_margins(
    data, !!across, total = sum(x), !!!extra,
    .grouping = !!options, .sort = "last"
  )
}

ordinary <- function(data, plan) {
  if (plan == "unpartitioned") {
    return(dplyr::summarize(
      data, dplyr::across(tidyselect::where(is.numeric), length,
                         .names = "count_{.col}"),
      total = sum(x)
    ))
  }
  detail <- dplyr::summarize(
    data, dplyr::across(tidyselect::where(is.numeric), length,
                       .names = "count_{.col}"),
    total = sum(x), .by = g
  )
  grand <- dplyr::summarize(
    data, dplyr::across(!g & tidyselect::where(is.numeric), length,
                       .names = "count_{.col}"),
    total = sum(x)
  )
  dplyr::bind_rows(detail, dplyr::mutate(grand, g = "Total"))
}

fixture <- tibble::tibble(g = c("a", "b", "a"), x = c(1L, 2L, 3L), y = letters[1:3])
inputs <- list(as.data.frame(fixture), fixture, data.table::as.data.table(fixture))
observed <- 0L
controls <- 0L
for (input in inputs) {
  for (view in c("alter", "refuse")) {
    wrapped <- structure(input, class = c("margin_hunt_list_view", class(input)),
                         view = view)
    original <- serialize(wrapped, NULL)
    for (plan in c("unpartitioned", "rollup")) {
      helpers <- if (plan == "rollup") c("share_of_total", "share_of_parent") else "share_of_total"
      for (helper in helpers) {
        expected <- ordinary(wrapped, plan)
        no_share <- report(wrapped, helper, "none", plan)
        direct <- report(wrapped, helper, "direct", plan)
        plain <- report(input, helper, "selected", plan)
        stopifnot(identical(as.list(no_share), as.list(expected)))
        stopifnot(identical(as.list(direct), as.list(plain)))
        stopifnot(identical(direct$count_x, expected$count_x))
        stopifnot(identical(direct$share, if (plan == "rollup") c(4 / 6, 2 / 6, 1) else 1))
        controls <- controls + 3L
        if (mode != "controls") {
          actual <- tryCatch(report(wrapped, helper, "selected", plan), error = identity)
          if (mode == "contract") {
            stopifnot(identical(as.list(actual), as.list(direct)))
          } else if (view == "alter" && plan == "unpartitioned") {
            stopifnot(identical(actual$count_y, expected$count_x), is.null(actual$count_x))
            cat("Altered selection:", paste(names(actual), collapse = ", "), "\n")
          } else {
            stopifnot(inherits(actual, "error"))
            text <- if (view == "refuse") "Positive-row list conversion" else "Union branch"
            stopifnot(grepl(text, conditionMessage(actual), fixed = TRUE))
          }
          observed <- observed + 1L
        }
      }
    }
    stopifnot(identical(serialize(wrapped, NULL), original))
  }
}
cat("Observed discrepancies:", observed, "\n")
cat("Passing ordinary, named-share, and unwrapped controls:", controls, "\n")
