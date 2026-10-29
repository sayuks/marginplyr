# Contextual-share investigation evidence at the checked-out snapshot.
# Default mode reports established contract failures; --assert exits nonzero
# when either focused successful-call/refusal assertion is contradicted.
args <- commandArgs(trailingOnly = TRUE)
options(error = function() {
  traceback(10L)
  quit(status = 1L)
})
assert_findings <- "--assert" %in% args
focused_only <- "--focused" %in% args
roots <- args[!grepl("^--", args)]
root <- if (length(roots)) roots[[1L]] else getwd()
suppressPackageStartupMessages(pkgload::load_all(root, quiet = TRUE))
suppressPackageStartupMessages(library(dplyr))
cat(
  "Snapshot:",
  system2("git", c("-C", root, "rev-parse", "HEAD"), stdout = TRUE), "\n"
)
for (package in c(
  "R", "dplyr", "dbplyr", "dtplyr", "data.table", "RSQLite", "duckdb"
)) {
  version <- if (package == "R") getRversion() else packageVersion(package)
  cat(package, as.character(version), "\n")
}
capture <- function(code) {
  tryCatch(
    list(value = force(code)), error = function(error) list(error = error)
  )
}
verdict <- function(answer) {
  if (is.null(answer$error)) {
    "accepted"
  } else {
    paste(class(answer$error)[[1L]], conditionMessage(answer$error))
  }
}
findings <- 0L
cat("\nF-S1: dtplyr .BY source-input protection\n")
input <- tibble(g = c("a", "b"), .BY = c(1, 3))
original <- input
source <- dtplyr::lazy_dt(input)
ordinary <- collect(summarise(source, value = sum(.BY), .by = g))
control <- collect(summarize_with_margins(
  source, value = sum(.BY), .grouping = rollup(g)
))
stopifnot(
  identical(ordinary$value, c(1, 3)), identical(control$value, c(1, 3, 4))
)
cat("ordinary dtplyr and no-share Margin controls: PASS\n")
for (helper in c("share_of_parent", "share_of_total")) {
  for (form in c("direct", "across")) {
    dots <- if (form == "direct") {
      list(value = quote(sum(.BY)))
    } else {
      list(quote(across(.BY, sum, .names = "value")))
    }
    share <- rlang::call2(helper, quote(value))
    query <- summarize_with_margins(
      source, !!!dots, fraction = !!share, .grouping = rollup(g)
    )
    stopifnot(inherits(query, "dtplyr_step"))
    answer <- capture(collect(query))
    correct <- is.null(answer$error) &&
      identical(answer$value$fraction, c(0.25, 0.75, 1))
    findings <- findings + !correct
    cat(helper, form, verdict(answer), "\n")
  }
}
stopifnot(identical(input, original), identical(collect(source), original))
renamed <- rename(input, x = .BY)
renamed_control <- collect(summarize_with_margins(
  dtplyr::lazy_dt(renamed), value = sum(x),
  fraction = share_of_total(value), .grouping = rollup(g)
))
stopifnot(identical(renamed_control$fraction, c(0.25, 0.75, 1)))
identity_value <- function(value) value
wrapped_control <- capture(collect(summarise(
  source, value = identity_value(sum(.BY)), .by = g
)))
cat("upstream expression-shape control:", verdict(wrapped_control), "\n")
if ("--trace" %in% args) {
  old_options <- options(datatable.verbose = TRUE)
  invisible(collect(summarise(source, value = sum(.BY), .by = g)))
  invisible(capture(collect(summarise(
    source, value = identity_value(sum(.BY)), .by = g
  ))))
  options(old_options)
}
print(show_query(summarize_with_margins(
  source, value = sum(.BY), fraction = share_of_total(value),
  .grouping = rollup(g)
)))
cat("\nF-S2: computed parenthesized data-pronoun source reads\n")
input <- tibble(g = c("a", "b"), x = c(1, 3), first = c(100, 300))
key <- "first"
# jarl-ignore unnecessary_parentheses: Repeated pairs are syntax under test.
reads <- list(
  computed = quote((.data)[[paste0("fir", "st")]]),
  variable = quote((.data)[[key]]),
  repeated = quote(((.data))[[key]])
)
for (helper in c("share_of_parent", "share_of_total")) {
  share <- rlang::call2(helper, quote(value))
  for (name in names(reads)) {
    read <- reads[[name]]
    for (direction in c("source", "derived")) {
      answer <- capture(if (direction == "source") {
        summarize_with_margins(
          input, first = sum(x), value = !!read, fraction = !!share,
          .grouping = rollup(g)
        )
      } else {
        summarize_with_margins(
          input, value = sum(x), first = !!share, derived = sum(!!read),
          .grouping = rollup(g)
        )
      })
      correct <- inherits(answer$error, "marginplyr_error")
      findings <- findings + !correct
      cat(helper, name, direction, verdict(answer), "\n")
      if (is.null(answer$error)) print(answer$value)
    }
  }
  for (read in list(quote((.data)[["first"]]), quote(.data[["first"]]))) {
    answer <- capture(summarize_with_margins(
      input, value = sum(x), first = !!share, derived = sum(!!read),
      .grouping = rollup(g)
    ))
    stopifnot(inherits(answer$error, "marginplyr_error"))
  }
}
independent <- summarize_with_margins(
  input, value = sum(x), first = share_of_total(value),
  independent = sum((.data)[[paste0("x")]]), .grouping = rollup(g)
)
stopifnot(identical(independent$independent, c(1, 3, 4)))
cat("literal/bare refusals and independent dynamic input reads: PASS\n")

if (!focused_only) {
  cat("\nIndependent result-relative share oracle\n")
  set.seed(20261029)
  con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  con_duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), ":memory:")
  checks <- 0L
  rows_checked <- 0L
  same_key <- function(x, value) {
    if (is.na(value)) is.na(x) else !is.na(x) & x == value
  }
  check_ratio <- function(result, plan, column, source_column, parent) {
    for (row in seq_len(nrow(result))) {
      occurrence <- as.integer(result$set[[row]])
      included <- plan$included[[occurrence]]
      if (!parent) {
        targets <- which(lengths(plan$included) == 0L)
        target <- if (occurrence %in% targets) NA_integer_ else targets[[1L]]
      } else {
        targets <- which(
          seq_len(nrow(plan)) > occurrence &
            lengths(plan$included) < length(included)
        )
        target <- if (length(targets)) targets[[1L]] else NA_integer_
      }
      expected <- 1
      if (!is.na(target)) {
        matching <- result$set == target
        for (key_name in c(plan$fixed[[target]], plan$included[[target]])) {
          matching <- matching & same_key(
            result[[key_name]], result[[key_name]][[row]]
          )
        }
        denominator <- result[[source_column]][matching]
        stopifnot(length(denominator) == 1L)
        numerator <- result[[source_column]][[row]]
        expected <- if (
          is.na(numerator) || is.na(denominator) || denominator == 0
        ) {
          NA_real_
        } else {
          as.double(numerator) / as.double(denominator)
        }
      }
      actual <- result[[column]][[row]]
      stopifnot(
        is.double(result[[column]]),
        isTRUE(all.equal(actual, expected, tolerance = 1e-12))
      )
      rows_checked <<- rows_checked + 1L
    }
  }
  for (fixture in seq_len(14L)) {
    cat("Fixture", fixture, "\n")
    size <- if (fixture == 1L) 0L else sample(1:16, 1L)
    input <- tibble(
      fixed = sample(c("A", "B", NA_character_), size, replace = TRUE),
      g = sample(c(1, 1 + 1e-14, 2, NA_real_), size, replace = TRUE),
      h = sample(c("u", "v", NA_character_), size, replace = TRUE),
      x = sample(c(-4, -1, 0, 1, 4, NA_real_), size, replace = TRUE)
    )
    plans <- list(
      rollup = rollup(g, h), composite = rollup(c(g, h)), cube = cube(g, h),
      repeated = rollup(g, g, h),
      unordered = grouping_sets(
        grouping_set(), grouping_set(g, h), grouping_set(g), grouping_set()
      ),
      product = grouping_spec(rollup(g), cube(h))
    )
    sources <- list(
      local = input, dtplyr = dtplyr::lazy_dt(input),
      sqlite = copy_to(con_sqlite, input, paste0("fixture_", fixture)),
      duckdb = copy_to(con_duck, input, paste0("fixture_", fixture))
    )
    for (plan_name in names(plans)) {
      specification <- plans[[plan_name]]
      parent <- plan_name %in% c("rollup", "composite", "repeated")
      for (fixed in c(FALSE, TRUE)) {
        by <- if (fixed) quote(fixed) else NULL
        plan <- inspect_grouping(
          input, .by = !!by, .grouping = specification,
          .duplicates = "keep", .format = "list"
        )
        extras <- if (parent) {
          rlang::quos(p = share_of_parent(value), q = share_of_parent(count))
        } else {
          list()
        }
        for (backend in names(sources)) {
          cat(fixture, plan_name, fixed, backend, "\n")
          query <- summarize_with_margins(
            sources[[backend]], value = sum(x, na.rm = TRUE), count = n(),
            s = share_of_total(value), t = share_of_total(count), !!!extras,
            .by = !!by, .grouping = specification,
            .duplicates = "keep", .id = "set",
            .margin_label = NULL, .check_share_source = FALSE
          )
          result <- if (backend == "local") query else collect(query)
          check_ratio(result, plan, "s", "value", FALSE)
          check_ratio(result, plan, "t", "count", FALSE)
          if (parent) {
            check_ratio(result, plan, "p", "value", TRUE)
            check_ratio(result, plan, "q", "count", TRUE)
          }
          checks <- checks + 1L
        }
      }
    }
  }
  DBI::dbDisconnect(con_sqlite)
  DBI::dbDisconnect(con_duck, shutdown = TRUE)
  cat(
    "Oracle PASS:", checks, "results;",
    rows_checked, "row/source ratios checked\n"
  )
}
cat("Focused contract mismatches:", findings, "\n")
if (assert_findings && findings > 0L) quit(status = 1L)
