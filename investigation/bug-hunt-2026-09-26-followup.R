# Reproductions for bug-hunt-2026-09-26-followup.md; run from the repo root.
# Optional arguments select probes. Every probe asserts intended behavior.
pkgload::load_all(quiet = TRUE)

same <- function(actual, expected) {
  if (!identical(actual, expected)) {
    show <- function(x) paste(capture.output(dput(x)), collapse = " ")
    stop("Expected: ", show(expected), "\nActual:   ", show(actual),
         call. = FALSE)
  }
}

probes <- list(
  key = function() {
    data <- tibble::tibble(g = c("a", "b"), v = 1:2)
    frame <- function(x) {
      stats::setNames(data.frame(sum(x)), "..marginplyr_key_1")
    }
    expected <- summarize_with_margins(
      data, `..marginplyr_key_1` = sum(v), .grouping = grouping_set(g)
    )
    same(expected$g, data$g)
    public_frame <- function(x) data.frame(g = sum(x))
    public <- tryCatch(summarize_with_margins(
      data, public_frame(v), .grouping = grouping_set(g)
    ), error = identity)
    stopifnot(inherits(public, "marginplyr_error"))

    actual <- tryCatch(summarize_with_margins(
      data, frame(v), .grouping = grouping_set(g)
    ), error = identity)
    # A package collision refusal or an intact result both protect the keys.
    if (inherits(actual, "marginplyr_error")) return(invisible(NULL))
    print(actual)
    same(actual, expected)
  },
  names = function() {
    run <- function(margin = TRUE, share = TRUE) {
      data <- data.frame(v = 1L)
      calls <- 0L
      name <- function() {
        calls <<- calls + 1L
        paste0("out", calls)
      }
      result <- if (!margin) {
        dplyr::summarise(
          data, total = sum(v), dplyr::across(v, sum, .names = name()),
          ratio = 1, observed = calls
        )
      } else if (!share) {
        summarize_with_margins(
          data, total = sum(v), dplyr::across(v, sum, .names = name()),
          ratio = 1, observed = calls, .grouping = grouping_set()
        )
      } else {
        summarize_with_margins(
          data, total = sum(v), dplyr::across(v, sum, .names = name()),
          ratio = share_of_total(total), observed = calls,
          .grouping = grouping_set()
        )
      }
      list(result = result, calls = calls)
    }
    expected <- run(margin = FALSE)
    same(run(share = FALSE), expected)
    actual <- run()
    cat("Control calls:", expected$calls, "; with share:", actual$calls, "\n")
    same(actual, expected)
  },
  source = function() {
    data <- tibble::tibble(g = c("a", "b"), v = c(1, 3))
    duplicate <- rlang::quos(sum(v), max(v))
    names(duplicate) <- c("total", "total")
    control <- tryCatch(summarize_with_margins(
      data, !!!duplicate, p = share_of_total(total),
      .grouping = rollup(g)
    ), error = identity)
    stopifnot(inherits(control, "marginplyr_error"),
              grepl("defined exactly once", conditionMessage(control),
                    fixed = TRUE))
    actual <- tryCatch(summarize_with_margins(
      data, total = sum(v), tibble::tibble(total = max(v)),
      p = share_of_total(total), .grouping = rollup(g)
    ), error = identity)
    if (!inherits(actual, "marginplyr_error")) {
      print(actual)
      stop("An expanded frame redefined the share source without refusal.",
           call. = FALSE)
    }
    stopifnot(grepl("defined exactly once", conditionMessage(actual),
                   fixed = TRUE))
  },
  prior = function() {
    data <- data.frame(v = 1L)
    expected <- dplyr::summarise(
      data, total = sum(v),
      dplyr::across(total, identity, .names = "copy_{.col}"), p = 1
    )
    same(summarize_with_margins(
      data, total = sum(v),
      dplyr::across(total, identity, .names = "copy_{.col}"),
      p = 1, .grouping = grouping_set()
    ), expected)
    same(summarize_with_margins(
      data, total = sum(v),
      dplyr::across(dplyr::starts_with("tot"), identity,
                    .names = "copy_{.col}"),
      p = share_of_total(total), .grouping = grouping_set()
    ), expected)
    same(summarize_with_margins(
      data, total = sum(v),
      dplyr::across(total, identity, .names = "copy_{.col}"),
      p = share_of_total(total), .grouping = grouping_set()
    ), expected)
  },
  array = function() {
    data <- tibble::tibble(
      g = array(c(2L, 1L), dim = c(2L, 1L, 1L)), value = 1:2
    )
    expected <- dplyr::arrange(data, g)
    same(expected$value, c(2L, 1L))
    control <- data
    control$g <- matrix(control$g, nrow = 2L)
    same(expand_with_margins(control, .by = g, .sort = "last")$value,
         expected$value)
    same(expand_with_margins(data, .by = g, .sort = "last"), expected)
  },
  packed = function() {
    data <- tibble::tibble(
      g = tibble::tibble(
        m = matrix(c(NA_real_, NA_real_, NA_real_, 1), nrow = 2L),
        a = c(NA_real_, NA_real_)
      ),
      value = 1:2
    )
    expected <- dplyr::arrange(data, g)
    same(expected$value, c(2L, 1L))
    flat <- data
    flat$g <- tibble::tibble(
      m1 = data$g$m[, 1L], m2 = data$g$m[, 2L], a = data$g$a
    )
    same(expand_with_margins(flat, .by = g, .sort = "last")$value,
         expected$value)
    actual <- expand_with_margins(data, .by = g, .sort = "last")
    cat("Expected row order:", expected$value,
        "; actual:", actual$value, "\n")
    same(actual, expected)
  },
  dtplyr = function() {
    data <- tibble::tibble(g = c("x", "y"), .N = c(7, 9))
    original <- data
    source <- dtplyr::lazy_dt(data)
    before <- dplyr::collect(source)
    expected <- tibble::tibble(
      g = c("x", "y", "Total", "Total"), .N = c(7, 9, 7, 9)
    )
    same(expand_with_margins(data, .grouping = rollup(g)), expected)
    same(dplyr::collect(dplyr::union_all(
      source, dplyr::mutate(source, g = "Total")
    )), expected)
    missing <- expected
    missing$g[3:4] <- NA_character_
    same(dplyr::collect(expand_with_margins(
      source, .grouping = rollup(g), .margin_label = NULL
    )), missing)
    actual <- tryCatch(dplyr::collect(expand_with_margins(
      source, .grouping = rollup(g)
    )), error = identity)
    same(data, original)
    same(dplyr::collect(source), before)
    if (inherits(actual, "error")) stop(actual)
    same(actual, expected)
  }
)

selected <- commandArgs(trailingOnly = TRUE)
if (length(selected) == 0L) selected <- names(probes)
stopifnot(all(selected %in% names(probes)))
failures <- character()
for (name in selected) {
  cat("\n[", name, "]\n", sep = "")
  error <- tryCatch({
    probes[[name]]()
    NULL
  }, error = identity)
  if (is.null(error)) {
    cat("PASS\n")
  } else {
    failures <- c(failures, name)
    cat("FAIL: ", conditionMessage(error), "\n", sep = "")
  }
}
cat("\n", length(selected) - length(failures), "/", length(selected),
    " probes passed their intended-result assertions.\n", sep = "")
if (length(failures) > 0L) quit(status = 1L)
