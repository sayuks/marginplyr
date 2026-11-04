# Replay from the investigated repository root. --assert asks for desired
# behavior and exits nonzero when any of the three defects remains.
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(quiet = TRUE)
d <- data.frame(g = c("a", "b"), x = c(1, 3), y = c(2, 4))
desired <- c(0.25, 0.75, 1)
capture <- function(expr) tryCatch(suppressWarnings(force(expr)), error = identity)
ratios <- function(result) {
  if (inherits(result, "error")) return(conditionMessage(result))
  if (!"ratio" %in% names(result)) return("<no ratio column>")
  unname(result$ratio[match(c("a", "b", "Total"), result$g)])
}
is_desired <- function(result) identical(ratios(result), desired)
succeeded <- c(predicate = 0L, static_output = 0L, excluded_alias = 0L)
checked <- succeeded
record <- function(finding, label, as_desired, actual) {
  checked[[finding]] <<- checked[[finding]] + 1L
  succeeded[[finding]] <<- succeeded[[finding]] + as_desired
  cat(finding, label, if (as_desired) "DESIRED" else "DIVERGES",
      paste(actual, collapse = "/"), "\n")
}
backends <- list(
  data.frame = d,
  tibble = tibble::as_tibble(d),
  data.table = data.table::as.data.table(d),
  dtplyr = dtplyr::lazy_dt(d)
)
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
backends$DuckDB <- dplyr::copy_to(con, d, "share_findings_data", temporary = TRUE)
for (backend in names(backends)) {
  data <- backends[[backend]]
  for (share in c("share_of_parent", "share_of_total")) {
    # A real source has type double. The positional proxy used by the
    # share selection has type integer.
    calls <- 0L
    predicate <- function(col) {
      calls <<- calls + 1L
      is.integer(col)
    }
    where_alias <- tidyselect::where
    saved_predicate <- tidyselect::where(predicate)
    predicate_forms <- rlang::exprs(
      canonical = tidyselect::where(predicate),
      alias = where_alias(predicate),
      saved = .env$saved_predicate,
      negative = !.env$saved_predicate,
      anonymous = function(col) predicate(col)
    )
    for (selector in names(predicate_forms)) {
      calls <- 0L
      expr <- rlang::expr(dplyr::across(!!predicate_forms[[selector]],
                                       !!rlang::sym(share), .names = "ratio"))
      result <- capture(dplyr::collect(summarize_with_margins(
        data, total = sum(x), !!expr, .grouping = rollup(g),
        .check_share_source = FALSE
      )))
      rejected <- inherits(result, "error") && calls == 0L
      if (selector == "canonical") {
        stopifnot(rejected, grepl("name-based tidyselect", conditionMessage(result), fixed = TRUE))
      } else {
        record("predicate", paste(backend, share, selector), rejected,
               paste(calls, "predicate calls;", paste(ratios(result), collapse = "/")))
      }
    }

    # Every selector below has a static result x. A prior summary makes
    # ordinary across run in the later-summary path.
    selectors <- rlang::exprs(
      slash = x / y,
      one_of = tidyselect::one_of("x"),
      one_of_exclude = c(x, -tidyselect::one_of("prior")),
      intersection = x & !y,
      all_of = tidyselect::all_of("x")
    )
    for (selector in names(selectors)) {
      # Ordinary lazy dbplyr/dtplyr selections do not expose prior.
      if (selector == "one_of_exclude" && backend %in% c("dtplyr", "DuckDB")) next
      ordinary <- rlang::expr(dplyr::across(!!selectors[[selector]], sum,
                                           .names = "sum_{.col}"))
      request <- rlang::call2(share, quote(sum_x))
      control <- capture(dplyr::collect(summarize_with_margins(
        data, prior = sum(y), !!ordinary, .grouping = rollup(g)
      )))
      stopifnot(!inherits(control, "error"), "sum_x" %in% names(control))
      for (request_style in c("direct", "pattern")) {
        shares <- if (request_style == "direct") {
          setNames(list(request), "ratio")
        } else {
          list(rlang::expr(dplyr::across(tidyselect::starts_with("sum_"),
                                        !!rlang::sym(share), .names = "ratio")))
        }
        result <- capture(dplyr::collect(summarize_with_margins(
          data, prior = sum(y), !!ordinary, !!!shares,
          .grouping = rollup(g), .check_share_source = FALSE
        )))
        if (selector %in% c("intersection", "all_of")) stopifnot(is_desired(result))
        else record("static_output", paste(backend, share, selector, request_style),
                    is_desired(result), ratios(result))
      }
    }

    if (backend %in% c("dtplyr", "DuckDB")) next
    for (consumer in c("pick", "across", "if_any", "if_all")) {
      expected_source <- if (consumer %in% c("if_any", "if_all")) c(0, 1, 1) else c(1, 3, 4)
      expected_ratios <- expected_source / expected_source[[3L]]
      for (selector in c("x & !prior", "c(x, -prior)", "c(x, -all_of('prior'))",
                         "c(x, -any_of('prior'))", "prior")) {
        source <- switch(consumer,
          pick = sprintf("sum(dplyr::pick(%s))", selector),
          across = sprintf("sum(dplyr::across(%s, identity))", selector),
          if_any = sprintf("sum(dplyr::if_any(%s, ~ .x > 1))", selector),
          if_all = sprintf("sum(dplyr::if_all(%s, ~ .x > 1))", selector)
        )
        source <- parse(text = source, keep.source = FALSE)[[1L]]
        request <- rlang::call2(share, quote(source))
        if (selector != "prior") {
          control <- summarize_with_margins(
            data, prior = sum(y), source = !!source, .grouping = rollup(g)
          )
          stopifnot(all(control$source == expected_source))
        }
        result <- capture(summarize_with_margins(
          data, prior = sum(y), source = !!source, ratio = !!request,
          .grouping = rollup(g)
        ))
        if (selector == "prior") {
          stopifnot(inherits(result, "error"),
                    grepl("depends on earlier summary alias", conditionMessage(result), fixed = TRUE))
        } else if (grepl("all_of|any_of", selector)) stopifnot(identical(ratios(result), expected_ratios))
        else record("excluded_alias", paste(backend, share, consumer, selector),
                    identical(ratios(result), expected_ratios), ratios(result))
      }
    }
  }
}
DBI::dbDisconnect(con)
print(rbind(desired = succeeded, checked = checked))
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(identical(succeeded, checked))
}
