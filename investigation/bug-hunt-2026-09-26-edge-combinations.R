# Intended-result probes at 049428fc4029ed59707d955b31af87b55e89fa9d.
# Run from the repository root; an optional argument selects one probe.
suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))

probe_unpack <- function() {
  run <- function(margin, dynamic) {
    calls <- 0L
    make_fn <- function() {
      calls <<- calls + 1L
      offset <- calls
      function(x) x + offset
    }
    data <- tibble::tibble(x = 1)
    result <- if (!margin) {
      dplyr::summarise(
        data, dplyr::across(x, make_fn(), .unpack = identity(FALSE))
      )
    } else if (dynamic) {
      summarize_with_margins(
        data, dplyr::across(x, make_fn(), .unpack = identity(FALSE)),
        .grouping = grouping_set()
      )
    } else {
      summarize_with_margins(
        data, dplyr::across(x, make_fn(), .unpack = FALSE),
        .grouping = grouping_set()
      )
    }
    list(result = result, calls = calls)
  }
  control <- run(FALSE, TRUE)
  literal <- run(TRUE, FALSE)
  actual <- run(TRUE, TRUE)
  stopifnot(identical(literal, control), identical(control$calls, 1L))
  cat("dplyr .fns evaluations:", control$calls,
      "; dynamic Margin evaluations:", actual$calls, "\n")
  print(actual$result)
  stopifnot(identical(actual, control))
}

probe_selection <- function() {
  run <- function(share) {
    calls <- 0L
    select_column <- function() {
      calls <<- calls + 1L
      if (calls == 1L) "x" else "y"
    }
    data <- tibble::tibble(x = 1, y = 10)
    result <- if (share) {
      summarize_with_margins(
        data, total = sum(x),
        packed = dplyr::across(dplyr::all_of(select_column()), sum),
        p = share_of_total(total), .grouping = grouping_set()
      )
    } else {
      summarize_with_margins(
        data, total = sum(x),
        packed = dplyr::across(dplyr::all_of(select_column()), sum),
        p = 1, .grouping = grouping_set()
      )
    }
    list(result = result, calls = calls)
  }
  control <- run(FALSE)
  actual <- run(TRUE)
  stopifnot(identical(control$calls, 1L),
            identical(control$result$packed$x, 1))
  cat("Without share:", control$calls,
      "; with unrelated share:", actual$calls, "\n")
  print(actual$result)
  stopifnot(identical(actual, control))
}

probe_predicate <- function() {
  data <- tibble::tibble(x = 1, y = -10)
  control <- summarize_with_margins(
    data, total = sum(x),
    packed = dplyr::across(dplyr::where(~ all(.x > 0)), sum),
    p = 1, .grouping = grouping_set()
  )
  expected <- dplyr::summarise(
    data, total = sum(x),
    packed = dplyr::across(dplyr::where(~ all(.x > 0)), sum), p = 1
  )
  stopifnot(identical(control, expected))
  actual <- summarize_with_margins(
    data, total = sum(x),
    packed = dplyr::across(dplyr::where(~ all(.x > 0)), sum),
    p = share_of_total(total), .grouping = grouping_set()
  )
  stopifnot(identical(actual, expected))
}

probe_dimension <- function() {
  data <- tibble::tibble(.N = c("b", "a"), value = 1:2)
  source <- dtplyr::lazy_dt(data)
  canonical <- function(x) {
    x <- as.data.frame(x)
    x <- x[order(x$value, x$.N, na.last = TRUE), , drop = FALSE]
    row.names(x) <- NULL
    x
  }
  expected <- canonical(expand_with_margins(data, .grouping = rollup(.N)))
  control <- canonical(dplyr::collect(dplyr::union_all(
    source, dplyr::mutate(source, .N = "Total")
  )))
  stopifnot(identical(control, expected))
  missing <- dplyr::collect(expand_with_margins(
    source, .grouping = rollup(.N), .margin_label = NULL
  ))
  stopifnot(nrow(missing) == 4L, sum(is.na(missing$.N)) == 2L)
  unrelated <- tibble::tibble(g = c("b", "a"), .N = 1:2)
  stopifnot(nrow(dplyr::collect(expand_with_margins(
    dtplyr::lazy_dt(unrelated), .grouping = rollup(g)
  ))) == 4L)
  actual <- canonical(dplyr::collect(expand_with_margins(
    source, .grouping = rollup(.N)
  )))
  stopifnot(identical(actual, expected))
}

probe_share <- function() {
  data <- tibble::tibble(value = 2)
  source <- dtplyr::lazy_dt(data)
  expected_source <- tibble::tibble(.I = 2)
  stopifnot(identical(
    dplyr::collect(dplyr::summarise(source, .I = sum(value))),
    expected_source
  ))
  stopifnot(identical(
    dplyr::collect(summarize_with_margins(source, .I = sum(value))),
    expected_source
  ))
  stopifnot(identical(dplyr::collect(summarize_with_margins(
    source, total = sum(value), p = share_of_total(total)
  )), tibble::tibble(total = 2, p = 1)))
  expected <- summarize_with_margins(
    data, .I = sum(value), p = share_of_total(.I)
  )
  stopifnot(identical(expected, tibble::tibble(.I = 2, p = 1)))
  actual <- dplyr::collect(summarize_with_margins(
    source, .I = sum(value), p = share_of_total(.I)
  ))
  stopifnot(identical(actual, expected))
}

probe_case <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- tibble::tibble(g = "a", v = 1L)
  source <- dplyr::copy_to(con, data, "case_probe")
  control <- dplyr::collect(dplyr::summarise(
    dplyr::group_by(source, g), G = sum(v, na.rm = TRUE)
  ))
  stopifnot(identical(control$G, 1L))
  expected <- summarize_with_margins(
    data, G = sum(v), .grouping = rollup(g), .margin_label = NULL
  )
  actual <- tryCatch(
    dplyr::collect(summarize_with_margins(
      source, G = sum(v, na.rm = TRUE),
      .grouping = rollup(g), .margin_label = NULL
    )),
    marginplyr_error = identity
  )
  if (inherits(actual, "marginplyr_error")) {
    message <- conditionMessage(actual)
    stopifnot(
      grepl("(^|[^[:alnum:]_])g([^[:alnum:]_]|$)", message),
      grepl("(^|[^[:alnum:]_])G([^[:alnum:]_]|$)", message),
      grepl("renam", message, ignore.case = TRUE)
    )
    return(invisible(NULL))
  }
  print(actual)
  stopifnot(identical(actual$G, expected$G))
}

probe_internal_case <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- tibble::tibble(
    g = c("a", "b"), v = 1:2, `..MARGINPLYR_KEY_1` = c("WRONG", "WRONG")
  )
  source <- dplyr::copy_to(con, data, "internal_case_probe")
  run <- function(data) {
    summarize_with_margins(
      data, n = dplyr::n(), .grouping = rollup(g),
      .margin_label = NULL, .sort = "last"
    )
  }
  expected <- as.data.frame(run(data))
  actual <- as.data.frame(dplyr::collect(run(source)))
  print(actual)
  stopifnot(identical(actual, expected))
}

probe_dot_data <- function() {
  data <- tibble::tibble(g = "a", v = 1L)
  control <- dplyr::summarise(data, ".data" := dplyr::n(), .by = g)
  stopifnot(identical(control[[".data"]], 1L))
  actual <- summarize_with_margins(
    data, ".data" := dplyr::n(), .grouping = rollup(g)
  )
  stopifnot(identical(actual[[".data"]], c(1L, 1L)),
            identical(names(actual), c("g", ".data")))
}

probes <- list(
  unpack = probe_unpack,
  selection = probe_selection,
  predicate = probe_predicate,
  dimension = probe_dimension,
  share = probe_share,
  case = probe_case,
  internal_case = probe_internal_case,
  dot_data = probe_dot_data
)
selected <- commandArgs(trailingOnly = TRUE)
if (length(selected) > 0L) {
  stopifnot(all(selected %in% names(probes)))
  probes <- probes[selected]
}
passed <- vapply(names(probes), function(name) {
  cat("\n[", name, "]\n", sep = "")
  tryCatch({
    probes[[name]]()
    cat("PASS\n")
    TRUE
  }, error = function(error) {
    cat("FAIL:", conditionMessage(error), "\n")
    FALSE
  })
}, logical(1))
cat("\n", sum(passed), "/", length(passed),
    " intended-result probes passed.\n", sep = "")
if (!all(passed)) {
  quit(status = 1L)
}
