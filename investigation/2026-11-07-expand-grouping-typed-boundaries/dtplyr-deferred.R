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

is_collision <- function(x) {
  inherits(x, "marginplyr_error") &&
    grepl("already present", conditionMessage(x), fixed = TRUE)
}

collision <- function(require_contract) {
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
    ordinary_matches(input, ordinary),
    identical(hits$count, invoked_during_check)
  )
  observed_displays <- if (inherits(result, "dtplyr_step")) {
    dplyr::collect(result)$g
  }
  rm(list = "::", envir = root)
  stopifnot(
    is_collision(capture_error(expand())),
    identical(serialize(data, NULL), original)
  )
  if (require_contract) {
    stopifnot(is_collision(result), invoked_during_check == 0L)
    return(invisible(NULL))
  }
  stopifnot(
    identical(invoked_during_check, 1L),
    identical(observed_displays, c("Total", "Total"))
  )
  cat("Collision: 1 generated callback; colliding expansion accepted;",
      "native controls unchanged; removing binding restores rejection.\n")
}

omission <- function(require_contract) {
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
  collect_ordered <- function(x) dplyr::arrange(dplyr::collect(x), sid)
  actual <- collect_ordered(query)
  stopifnot(
    ordinary_matches(input, ordinary)
  )
  rm(list = "::", envir = root)
  stopifnot(
    identical(collect_ordered(query), expected),
    identical(serialize(data, NULL), original)
  )
  if (require_contract) {
    stopifnot(identical(actual, expected), hits$count == 0L)
    return(invisible(NULL))
  }
  stopifnot(
    identical(actual, dplyr::mutate(expected, g = c("a", "MASK"))),
    identical(hits$count, 1L)
  )
  cat("Omission: 1 generated callback; labels a/MASK instead of a/Total;",
      "native controls unchanged; the same query recovers.\n")
}

zero_column_id <- function(require_contract) {
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
    identical(nrow(ordinary), 0L),
    identical(ncol(ordinary), 0L),
    identical(dplyr::collect(input), ordinary)
  )
  rm(list = "::", envir = root)
  stopifnot(
    identical(dplyr::collect(query), expected),
    identical(serialize(data, NULL), original)
  )
  if (require_contract) {
    stopifnot(identical(actual, expected), hits$count == 0L)
    return(invisible(NULL))
  }
  stopifnot(
    identical(actual, tibble::tibble(sid = 999L)),
    identical(hits$count, 1L)
  )
  cat("Identifier: 1 generated callback; invented row sid=999;",
      "native input stays empty; the same query recovers.\n")
}

mode <- commandArgs(trailingOnly = TRUE)
if (identical(mode, "--assert-label")) {
  collision(require_contract = TRUE)
  omission(require_contract = TRUE)
} else if (identical(mode, "--assert-id")) {
  zero_column_id(require_contract = TRUE)
} else {
  collision(require_contract = FALSE)
  omission(require_contract = FALSE)
  zero_column_id(require_contract = FALSE)
}
