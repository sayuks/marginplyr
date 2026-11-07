# Replay from the package root. Set --assert-label or --assert-id to require
# the corresponding desired behavior after checking all observed controls.
pkgload::load_all(".", quiet = TRUE)

intercept_namespace <- function(root, target, replacement) {
  lookup <- base::`::`
  hits <- new.env(parent = emptyenv())
  hits$count <- 0L
  root[["::"]] <- function(package, name) {
    package <- substitute(package)
    name <- substitute(name)
    key <- paste(as.character(package), as.character(name), sep = "::")
    if (identical(key, target)) {
      hits$count <- hits$count + 1L
      return(replacement)
    }
    eval(as.call(list(lookup, package, name)))
  }
  hits
}

capture_error <- function(expr) tryCatch(expr, error = identity)
source_in <- function(root, data) {
  root$input_data <- data
  evalq(dtplyr::lazy_dt(input_data), root)
}
ordinary_matches <- function(input, values) {
  identical(dplyr::collect(input), values) &&
    identical(dplyr::collect(dplyr::mutate(input)), values) &&
    identical(
      dplyr::collect(dplyr::union_all(input, input)),
      dplyr::bind_rows(values, values)
    )
}

collision <- function() {
  root <- new.env(parent = globalenv())
  data <- data.table::data.table(g = "Total", v = 7L)
  input <- source_in(root, data)
  original <- serialize(data, NULL)
  ordinary <- dplyr::collect(input)
  expand <- function() expand_with_margins(
    input, .grouping = rollup(g), .check_margin_label = TRUE
  )
  hits <- intercept_namespace(
    root, "base::as.character", function(x) rep("unrelated", length(x))
  )
  result <- capture_error(expand())
  invoked_during_check <- hits$count
  stopifnot(
    inherits(result, "dtplyr_step"),
    identical(invoked_during_check, 1L),
    identical(dplyr::collect(result)$g, c("Total", "Total")),
    ordinary_matches(input, ordinary),
    identical(hits$count, invoked_during_check)
  )
  rm(list = "::", envir = root)
  restored <- capture_error(expand())
  stopifnot(
    inherits(restored, "marginplyr_error"),
    grepl("already present", conditionMessage(restored), fixed = TRUE),
    identical(serialize(data, NULL), original)
  )
  cat("Collision: 1 generated callback; colliding expansion accepted;",
      "native controls unchanged; removing binding restores rejection.\n")
  inherits(result, "marginplyr_error") && invoked_during_check == 0L
}

omission <- function() {
  root <- new.env(parent = globalenv())
  data <- data.table::data.table(g = "a", v = 7L)
  input <- source_in(root, data)
  original <- serialize(data, NULL)
  ordinary <- dplyr::collect(input)
  query <- expand_with_margins(input, .grouping = rollup(g), .id = "sid")
  expected <- tibble::tibble(g = c("a", "Total"), sid = 1:2, v = c(7L, 7L))
  hits <- intercept_namespace(
    root, "base::rep", function(x, times) rep("MASK", times)
  )
  actual <- dplyr::collect(query)
  stopifnot(
    identical(actual, dplyr::mutate(expected, g = c("a", "MASK"))),
    ordinary_matches(input, ordinary),
    identical(hits$count, 1L)
  )
  rm(list = "::", envir = root)
  stopifnot(
    identical(dplyr::collect(query), expected),
    identical(serialize(data, NULL), original)
  )
  cat("Omission: 1 generated callback; labels a/MASK instead of a/Total;",
      "native controls unchanged; the same query recovers.\n")
  identical(actual, expected) && hits$count == 0L
}

zero_column_id <- function() {
  root <- new.env(parent = globalenv())
  data <- data.table::data.table()
  input <- source_in(root, data)
  original <- serialize(data, NULL)
  ordinary <- dplyr::collect(input)
  query <- expand_with_margins(input, .grouping = grouping_set(), .id = "sid")
  expected <- tibble::tibble(sid = integer())
  hits <- intercept_namespace(root, "base::rep", function(...) 999L)
  actual <- dplyr::collect(query)
  stopifnot(
    identical(actual, tibble::tibble(sid = 999L)),
    identical(nrow(ordinary), 0L),
    identical(ncol(ordinary), 0L),
    identical(dplyr::collect(input), ordinary),
    identical(hits$count, 1L)
  )
  rm(list = "::", envir = root)
  stopifnot(
    identical(dplyr::collect(query), expected),
    identical(serialize(data, NULL), original)
  )
  cat("Identifier: 1 generated callback; invented row sid=999;",
      "native input stays empty; the same query recovers.\n")
  identical(actual, expected) && hits$count == 0L
}

label_check <- collision()
label_value <- omission()
identifier <- zero_column_id()
args <- commandArgs(trailingOnly = TRUE)
if ("--assert-label" %in% args) {
  stopifnot(label_check, label_value)
}
if ("--assert-id" %in% args) {
  stopifnot(identifier)
}
