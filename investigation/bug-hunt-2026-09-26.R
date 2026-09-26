# Reproductions for bug-hunt-2026-09-26.md. Run from the repository root.
# Each check asserts the intended public result, so unfixed defects exit red.
# Optional arguments select findings: sqlite, frame, unpack, join, projection.
pkgload::load_all(quiet = TRUE)

same <- function(actual, expected) {
  if (!identical(actual, expected)) {
    show <- function(x) paste(capture.output(dput(x)), collapse = " ")
    stop("Expected: ", show(expected), "\nActual:   ", show(actual),
         call. = FALSE)
  }
}

probes <- list(
  sqlite = function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con))
    source <- dplyr::copy_to(con, data.frame(g = 1L), "source")

    # Typed-missing labels are the passing control for the same materializer.
    control <- expand_with_margins(
      source, .grouping = rollup(g), .margin_label = NULL, .id = "sid"
    )
    same(dplyr::collect(dplyr::compute(control, name = "control"))$g,
         dplyr::collect(control)$g)

    query <- expand_with_margins(
      source, .grouping = rollup(g), .margin_label = "01", .id = "sid"
    )
    expected <- dplyr::collect(query)$g
    same(expected, c("1", "01"))
    saved <- dplyr::compute(query, name = "report")
    print(DBI::dbGetQuery(con, "PRAGMA temp.table_info(report)"))
    print(DBI::dbGetQuery(
      con, "SELECT typeof(g) AS storage, quote(g) AS value FROM temp.report"
    ))
    same(dplyr::collect(saved)$g, expected)
  },
  frame = function() {
    data <- tibble::tibble(g = "a", x = 1)
    expected <- dplyr::summarise(
      data, tibble::tibble(g = NULL, total = sum(x)), .by = g
    )
    # The same frame behind an ordinary function bypasses static prediction.
    frame <- function(x) tibble::tibble(g = NULL, total = sum(x))
    same(summarize_with_margins(
      data, frame(x), .grouping = grouping_set(g)
    ), expected)
    actual <- summarize_with_margins(
      data, tibble::tibble(g = NULL, total = sum(x)),
      .grouping = grouping_set(g)
    )
    same(actual, expected)
  },
  unpack = function() {
    data <- tibble::tibble(g = "a", x = 1)
    run <- function(margin) {
      count <- 0L
      unpack <- function() {
        count <<- count + 1L
        FALSE
      }
      fn <- function(x) sum(x) + count
      result <- if (margin) {
        summarize_with_margins(
          data, dplyr::across(x, fn, .unpack = unpack()),
          .grouping = grouping_set(g)
        )
      } else {
        dplyr::summarise(
          data, dplyr::across(x, fn, .unpack = unpack()), .by = g
        )
      }
      list(result = result, evaluations = count)
    }
    expected <- run(FALSE)
    actual <- run(TRUE)
    same(expected$evaluations, 1L)
    same(expected$result$x, 2)
    cat("dplyr evaluations:", expected$evaluations,
        "; Margin evaluations:", actual$evaluations, "\n")
    same(actual$result$x, expected$result$x)
    same(actual$evaluations, expected$evaluations)
  },
  join = function() {
    data <- tibble::tibble(`a<b` = c("A", "A"), g = c("x", "y"),
                           value = c(1, 3))
    source <- dtplyr::lazy_dt(data)
    # Neither the source nor ordinary Margin aggregation refuses this name.
    same(names(dplyr::collect(summarize_with_margins(
      source, total = sum(value), .by = `a<b`, .grouping = rollup(g)
    ))), c("a<b", "g", "total"))
    errors <- character()
    for (helper in c("share_of_parent", "share_of_total")) {
      share <- rlang::call2(helper, rlang::sym("total"))
      query <- rlang::inject(summarize_with_margins(
        source, total = sum(value), fraction = !!share,
        .by = `a<b`, .grouping = rollup(g)
      ))
      error <- tryCatch({
        same(dplyr::collect(query)$fraction, c(0.25, 0.75, 1))
        NULL
      }, error = identity)
      if (!is.null(error)) {
        errors <- c(errors, paste0(helper, ": ", conditionMessage(error)))
      }
    }
    if (length(errors) > 0L) stop(paste(errors, collapse = "\n"), call. = FALSE)
  },
  projection = function() {
    data <- tibble::tibble(g = c("x", "y"), .BY = c(1, 3))
    source <- dtplyr::lazy_dt(data)
    control <- expand_with_margins(
      source, .grouping = rollup(g), .sort = "last", .id = "set"
    )
    same(names(dplyr::collect(control)), c("g", "set", ".BY"))
    query <- expand_with_margins(
      source, .grouping = rollup(g), .sort = "last"
    )
    same(as.character(dplyr::tbl_vars(query)), c("g", ".BY"))
    same(names(dplyr::collect(query)), c("g", ".BY"))
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
    " findings passed their intended-result assertions.\n", sep = "")
if (length(failures) > 0L) quit(status = 1L)
