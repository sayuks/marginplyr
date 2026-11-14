# An input marginplyr does not recognise is admitted when dplyr can group it,
# and is planned as the `other` backend kind. This one forwards each dplyr verb
# to the tibble it holds as a list of columns. The class is the test's own, so
# the methods registered for it reach no other test.
other_backend_input <- local({
  registered <- FALSE
  wrap <- function(data) {
    structure(as.list(dplyr::ungroup(data)),
              class = c("marginplyr_test_other", "list"))
  }
  unwrap <- function(x) tibble::as_tibble(unclass(x))
  register <- function() {
    dplyr_ns <- asNamespace("dplyr")
    verbs <- c("ungroup", "select", "mutate", "summarise", "filter",
               "group_by", "arrange", "distinct", "rename", "transmute",
               "relocate", "slice", "count", "compute", "collapse")
    for (verb in verbs) {
      local({
        fn <- getExportedValue("dplyr", verb)
        registerS3method(verb, "marginplyr_test_other", function(.data, ...) {
          wrap(fn(unwrap(.data), ...))
        }, envir = dplyr_ns)
      })
    }
    registerS3method("union_all", "marginplyr_test_other", function(x, y, ...) {
      wrap(dplyr::union_all(unwrap(x), unwrap(y), ...))
    }, envir = dplyr_ns)
    registerS3method("group_vars", "marginplyr_test_other", function(x) {
      dplyr::group_vars(unwrap(x))
    }, envir = dplyr_ns)
    registerS3method("tbl_vars", "marginplyr_test_other", function(x) {
      dplyr::tbl_vars(unwrap(x))
    }, envir = dplyr_ns)
    registerS3method("collect", "marginplyr_test_other", function(x, ...) {
      unwrap(x)
    }, envir = dplyr_ns)
  }
  function(data) {
    if (!registered) {
      register()
      registered <<- TRUE
    }
    wrap(data)
  }
})

test_that("an unrecognised input reads context names from its names", {
  data <- tibble::tibble(region = c("E", "W"), value = 1:2)
  input <- other_backend_input(data)
  expect_identical(grouping_backend(input)$kind, "other")
  selections <- rlang::quos(
    tidyselect::peek_vars(),
    names(tidyselect::peek_data())
  )
  for (selection in selections) {
    expect_identical(
      inspect_grouping(input, .grouping = grouping_set(!!selection)),
      inspect_grouping(data, .grouping = grouping_set(region, value))
    )
  }
})
