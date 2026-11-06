# A derived step whose caller bracket changes the zero-row grouping type.
# The caller has made dtplyr available. Recorded bracket calls observe backend
# dispatch because the public result cannot show whether a callback ran early.
dtplyr_caller_bracket_fixture <- function(binding = "[") {
  fixture <- new.env(parent = emptyenv())
  fixture$source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  fixture$before <- data.table::copy(fixture$source)
  fixture$calls <- integer()
  binding_env <- new.env(parent = environment())
  ordinary <- if (binding == "[") base::`[` else get(
    "[.data.table", envir = asNamespace("data.table")
  )
  binding_env[[binding]] <- function(x, ...) {
    fixture$calls <- c(fixture$calls, nrow(x))
    result <- ordinary(x, ...)
    if (nrow(result) == 0L) {
      data.table::set(result, j = "g", value = integer())
    }
    result
  }
  caller <- new.env(parent = binding_env)
  caller[[binding]] <- NULL
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

# A root subclass with a lexical S3 method, independent of the head generic
# and generated-bracket bindings. The caller has made dtplyr available.
local_dtplyr_root_subclass <- function(dispatch, mutating = FALSE,
                                       .env = parent.frame()) {
  fixture <- new.env(parent = emptyenv())
  fixture$source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  class(fixture$source) <- c("marginplyr_test_root", class(fixture$source))
  fixture$before <- serialize(fixture$source, NULL)
  fixture$calls <- integer()
  method <- function(x, ...) {
    fixture$calls <- c(fixture$calls, nrow(x))
    if (mutating) {
      data.table::set(x, j = "g", value = rep("changed", nrow(x)))
    }
    out <- NextMethod()
    if (!mutating && nrow(out) == 0L) {
      data.table::set(out, j = "g", value = integer())
    }
    out
  }
  name <- paste0(dispatch, ".marginplyr_test_root")
  # GlobalEnv is the S3 lookup boundary, not the test evaluation environment.
  stopifnot(!exists(name, envir = .GlobalEnv, inherits = FALSE))
  rlang::local_bindings(!!!setNames(list(method), name),
                        .env = .GlobalEnv, .frame = .env)
  fixture
}
