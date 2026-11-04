pkgload::load_all(quiet = TRUE)

probe <- function(input, cache_name, with_caller_copy = FALSE) {
  calls <- 0L
  if (with_caller_copy) {
    copy <- function(x) {
      calls <<- calls + 1L
      stop("the caller's unrelated copy")
    }
  }
  source <- dtplyr::lazy_dt(input)
  source <- dplyr::compute(dplyr::filter(source, TRUE), name = cache_name)
  expected <- tibble::as_tibble(input)
  before <- dplyr::collect(source)
  stopifnot(identical(before, expected), calls == 0L)
  ordinary_union <- dplyr::collect(dplyr::union_all(source, source))
  stopifnot(identical(ordinary_union, dplyr::bind_rows(expected, expected)),
            calls == 0L)

  query <- expand_with_margins(source, .margin_label = NULL)
  stopifnot(inherits(query, "dtplyr_step"), calls == 0L)
  result <- tryCatch(dplyr::collect(query), error = identity)
  stopifnot(identical(dplyr::collect(source), before))
  list(result = result, calls = calls)
}

input <- tibble::tibble(g = "a")
ordinary_cache <- probe(input, "ordinary_cache", TRUE)
unresolved_copy <- probe(input, "copy")
caller_copy <- probe(input, "copy", TRUE)
print(ordinary_cache)
print(unresolved_copy)
print(caller_copy)
stopifnot(identical(ordinary_cache$result, input), ordinary_cache$calls == 0L)
if ("--assert" %in% commandArgs(TRUE)) {
  stopifnot(identical(unresolved_copy$result, input),
            identical(caller_copy$result, input), caller_copy$calls == 0L)
}
