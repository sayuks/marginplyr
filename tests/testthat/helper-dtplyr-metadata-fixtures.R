# A derived step whose caller bracket changes the zero-row grouping type.
# The caller has made dtplyr available. Recorded bracket calls observe backend
# dispatch because the public result cannot show whether a callback ran early.
dtplyr_caller_bracket_fixture <- function() {
  fixture <- new.env(parent = emptyenv())
  fixture$source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  fixture$before <- data.table::copy(fixture$source)
  fixture$calls <- integer()
  binding_env <- new.env(parent = environment())
  binding_env$`[` <- function(x, ...) {
    fixture$calls <- c(fixture$calls, nrow(x))
    result <- base::`[`(x, ...)
    if (nrow(result) == 0L) {
      data.table::set(result, j = "g", value = integer())
    }
    result
  }
  caller <- new.env(parent = binding_env)
  caller$`[` <- NULL
  fixture$step <- eval(quote(dplyr::mutate(
    dtplyr::lazy_dt(fixture$source), derived = value + 1L
  )), caller)
  fixture
}

# Collect a Nest result and normalize its public cells for comparison across
# the dtplyr and local backends, retaining row order, names, types and values.
dtplyr_metadata_nest_result <- function(x) {
  x <- dplyr::ungroup(dplyr::collect(x))
  x$data <- lapply(x$data, tibble::as_tibble)
  x
}
