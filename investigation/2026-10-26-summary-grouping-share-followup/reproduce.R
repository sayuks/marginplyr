# Run from the repository root. This records unresolved defects, not fixes.
# Without --observe, every failed public-contract assertion makes the run red.
pkgload::load_all(".", quiet = TRUE)
observe <- "--observe" %in% commandArgs(trailingOnly = TRUE)
failures <- character()

capture <- function(code) {
  tryCatch(force(code), error = identity)
}

check <- function(id, actual, expected) {
  passed <- !inherits(actual, "error") && isTRUE(all.equal(actual, expected))
  cat(id, if (passed) "PASS\n" else "FAIL\n")
  if (inherits(actual, "error")) {
    cat("  ", conditionMessage(actual), "\n", sep = "")
  } else if (!passed) {
    print(actual)
  }
  if (!passed) failures <<- c(failures, id)
  invisible(passed)
}

# F1: the injected expression and its environment stay the same.
data <- data.frame(a = 1)
q <- rlang::quo(grouping_set(a))
plain <- inspect_grouping(data, .grouping = grouping_sets(!!q))
# jarl-ignore unnecessary_parentheses: Repeated pairs reproduce the recognition defect.
check("F1 injected constructor", capture(inspect_grouping(
  data, .grouping = grouping_sets(((!!q)))
)), plain)
forward_plain <- function(x) grouping_sets({{ x }})
forward_wrapped <- function(x) grouping_sets(({{ x }}))
check("F1 embracing wrapper", capture(inspect_grouping(
  data, .grouping = forward_wrapped(grouping_set(a))
)), inspect_grouping(data, .grouping = forward_plain(grouping_set(a))))

# F2: direct dplyr and public rename controls accept this source column name.
data <- data.frame(group = c("a", "b"), value = 1:2)
names(data)[1L] <- "..0"
expected <- dplyr::summarize(
  data, total = sum(value), .by = tidyselect::all_of("..0")
)
control <- dplyr::summarize(
  dplyr::group_by(data, dplyr::across(tidyselect::all_of("..0"))),
  total = sum(value), .groups = "drop"
)
stopifnot(identical(as.data.frame(control), expected))
renamed <- dplyr::rename(data, group = tidyselect::all_of("..0"))
workaround <- summarize_with_margins(
  renamed, total = sum(value), .grouping = grouping_set(group),
  .margin_label = NULL
)
workaround <- dplyr::rename(workaround, `..0` = group)
stopifnot(identical(workaround, expected))
check("F2 Grouping dimension", capture(summarize_with_margins(
  data, total = sum(value),
  .grouping = grouping_set(tidyselect::all_of("..0")),
  .margin_label = NULL
)), expected)
check("F2 fixed key", capture(summarize_with_margins(
  data, total = sum(value), .by = tidyselect::all_of("..0"),
  .margin_label = NULL
)), expected)

# The same generated-read failure reaches a valid named scalar share source.
data <- data.frame(g = c("a", "b"), value = c(1, 3))
for (source_name in c("..0", "..00")) {
  source <- rlang::set_names(rlang::quos(sum(value)), source_name)
  ordinary <- summarize_with_margins(
    data, !!!source, .grouping = rollup(g)
  )
  stopifnot(identical(ordinary[[source_name]], c(1, 3, 4)))
  for (kind in c("total", "parent")) {
    helper <- rlang::call2(paste0("share_of_", kind), rlang::sym(source_name))
    actual <- capture(summarize_with_margins(
      data, !!!source, share = !!helper, .grouping = rollup(g)
    ))
    check(paste("F2 share source", source_name, kind),
          if (inherits(actual, "error")) actual else actual$share,
          c(0.25, 0.75, 1))
  }
}

# F3: a named scalar containing a selector has no expanded-frame provenance.
plain <- summarize_with_margins(
  data, source = sum(value), share = share_of_total(source),
  .grouping = rollup(g)
)
stopifnot(identical(plain$source, c(1, 3, 4)))
stopifnot(identical(plain$share, c(0.25, 0.75, 1)))
for (selection in c("pick", "across")) {
  expr <- if (selection == "pick") {
    rlang::expr(sum(dplyr::pick(value)))
  } else {
    rlang::expr(sum(dplyr::across(value, identity)))
  }
  ordinary <- summarize_with_margins(
    data, source = !!expr, .grouping = rollup(g)
  )
  stopifnot(identical(ordinary$source, c(1, 3, 4)))
  for (kind in c("total", "parent")) {
    helper <- rlang::call2(paste0("share_of_", kind), rlang::sym("source"))
    actual <- capture(summarize_with_margins(
      data, source = !!expr, share = !!helper, .grouping = rollup(g)
    ))
    check(paste("F3", selection, kind),
          if (inherits(actual, "error")) actual else actual$share,
          c(0.25, 0.75, 1))
  }
}

cat("Failed public-contract checks: ", length(failures), "\n", sep = "")
if (!observe && length(failures)) {
  stop("Public-contract assertions failed: ", paste(failures, collapse = "; "),
       call. = FALSE)
}
