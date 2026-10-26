# Reproductions for bug-hunt-2026-10-26-expand-grouping.md.
# Run from the repository root; optional arguments select named probes.
pkgload::load_all(quiet = TRUE)

same <- function(actual, expected) {
  if (!identical(actual, expected)) {
    print(list(expected = expected, actual = actual))
    stop("The intended-result assertion failed.", call. = FALSE)
  }
}

arrow_source <- function() {
  arrow::Table$create(
    g = arrow::Array$create(
      list(1:2, 3:4),
      type = arrow::fixed_size_list_of(arrow::int32(), 2)
    ),
    payload = 1:2
  )
}

probes <- list(
  dtplyr = function() {
    data <- tibble::tibble(g = vctrs::list_of(1L, 2L), payload = 1:2)
    lazy <- dtplyr::lazy_dt(data)
    control <- dplyr::collect(dplyr::union_all(
      lazy,
      dplyr::mutate(lazy, g = vctrs::vec_init(g, length(g)))
    ))
    stopifnot(nrow(control) == 4L, inherits(control$g, "vctrs_list_of"))
    expected <- expand_with_margins(
      data, .grouping = rollup(g), .margin_label = NULL, .id = "sid"
    )
    actual <- dplyr::collect(expand_with_margins(
      lazy, .grouping = rollup(g), .margin_label = NULL, .id = "sid"
    ))
    same(dplyr::arrange(actual, sid, payload),
         dplyr::arrange(expected, sid, payload))
  },
  `arrow-one` = function() {
    data <- arrow_source()
    same(inspect_grouping(data, .grouping = grouping_set(g))$included, "(g)")
    expected <- dplyr::collect(dplyr::mutate(data, sid = 1L))
    expected <- dplyr::relocate(expected, g, sid)
    actual <- dplyr::collect(expand_with_margins(
      data, .grouping = grouping_set(g), .margin_label = NULL, .id = "sid"
    ))
    same(actual, expected)
  },
  `arrow-rollup` = function() {
    data <- arrow_source()
    missing <- arrow::Scalar$create(
      list(NULL), type = data$schema$GetFieldByName("g")$type
    )
    control <- dplyr::union_all(
      dplyr::mutate(data, sid = 1L),
      dplyr::mutate(data, g = missing, sid = 2L)
    )
    expected <- dplyr::collect(control)
    same(nrow(dplyr::collect(dplyr::compute(control))), 4L)
    expected <- dplyr::relocate(expected, g, sid)
    actual <- dplyr::collect(expand_with_margins(
      data, .grouping = rollup(g), .margin_label = NULL, .id = "sid"
    ))
    same(dplyr::arrange(actual, sid, payload),
         dplyr::arrange(expected, sid, payload))
  },
  `sqlite-types` = function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con))
    data <- dplyr::copy_to(con, tibble::tibble(
      g = c("a", "b"), fixed = rep(NA_real_, 2),
      payload = rep(NA_integer_, 2)
    ), "source")
    types <- function(x) vapply(dplyr::collect(x), typeof, character(1))
    expected <- c(fixed = "double", g = "character", payload = "integer")
    control <- dplyr::arrange(dplyr::mutate(data, g = as.character(g)), g)
    same(unname(types(control)[names(expected)]), unname(expected))
    same(unname(types(dplyr::compute(control))[names(expected)]),
         unname(expected))
    with_id <- expand_with_margins(
      data, .by = fixed, .grouping = grouping_set(g),
      .sort = "last", .id = "sid"
    )
    same(types(with_id)[names(expected)], expected)
    same(types(dplyr::compute(with_id))[names(expected)], expected)
    result <- expand_with_margins(
      data, .by = fixed, .grouping = grouping_set(g), .sort = "last"
    )
    observed <- list(direct = types(result),
                     materialized = types(dplyr::compute(result)))
    print(observed)
    same(observed$direct, expected)
    same(observed$materialized, expected)
  },
  `sqlite-id` = function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con))
    data <- dplyr::copy_to(
      con, tibble::tibble(g = c("b", "a"), payload = 1:2), "source"
    )
    safe <- expand_with_margins(
      data, .grouping = rollup(g), .margin_label = NULL,
      .id = "sid", .sort = "last"
    )
    same(nrow(dplyr::collect(dplyr::compute(safe))), 4L)
    result <- expand_with_margins(
      data, .grouping = rollup(g), .margin_label = NULL,
      .id = ".data", .sort = "last"
    )
    expected <- dplyr::collect(result)
    same(nrow(expected), 4L)
    same(expected[[".data"]], c(1L, 1L, 2L, 2L))
    before <- DBI::dbListTables(con)
    actual <- tryCatch(
      dplyr::collect(dplyr::compute(result, name = "materialized")),
      error = identity
    )
    if (inherits(actual, "error")) {
      same(DBI::dbListTables(con), before)
      stop(actual)
    }
    same(as.data.frame(actual), as.data.frame(expected))
  },
  predicates = function() {
    data <- tibble::tibble(a = c("x", "y"), value = 1:2)
    inputs <- list(local = data, dtplyr = dtplyr::lazy_dt(data),
                   Arrow = arrow::Table$create(data))
    for (kind in names(inputs)) {
      input <- inputs[[kind]]
      value_plan <- inspect_grouping(
        input, .grouping = grouping_set(tidyselect::where(~ any(.x == "x")))
      )
      type_plan <- inspect_grouping(
        input, .grouping = grouping_set(tidyselect::where(is.character))
      )
      cat(kind, "value predicate:", value_plan$included,
          "; type predicate:", type_plan$included, "\n")
      same(type_plan$included, "(a)")
    }
    for (input in inputs[c("local", "dtplyr")]) {
      print(dplyr::collect(summarize_with_margins(
        input, n = dplyr::n(), .by = tidyselect::where(~ any(.x == "x"))
      )))
    }
  }
)

selected <- commandArgs(trailingOnly = TRUE)
if (length(selected) == 0L) selected <- names(probes)
stopifnot(all(selected %in% names(probes)))
failed <- character()
for (name in selected) {
  cat("\nPROBE", name, "\n")
  tryCatch({
    probes[[name]]()
    cat("PASS", name, "\n")
  }, error = function(cnd) {
    failed <<- c(failed, name)
    cat("FAIL", name, ":", conditionMessage(cnd), "\n")
  })
}
if (length(failed) > 0L) {
  stop("Failed probes: ", paste(failed, collapse = ", "), call. = FALSE)
}
