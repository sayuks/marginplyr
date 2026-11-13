# A backend marginplyr does not recognise: a named list of columns whose dplyr
# verbs run on the tibble it holds. It is neither a data frame nor a lazy
# table, so `grouping_backend()` classifies it as `other`.
other_backend_frame <- function(data) {
  structure(
    as.list(dplyr::ungroup(data)),
    group_names = dplyr::group_vars(data),
    class = c("marginplyr_test_other_backend", "list")
  )
}

other_backend_data <- function(x) {
  if (!inherits(x, "marginplyr_test_other_backend")) {
    return(x)
  }
  dplyr::group_by(
    tibble::as_tibble(unclass(x)[names(x)]),
    !!!rlang::syms(attr(x, "group_names"))
  )
}

register_other_backend <- function() {
  joins <- c("left_join", "union_all")
  verbs <- c(joins, "group_by", "ungroup", "summarise", "mutate", "select",
             "rename", "arrange", "filter", "transmute", "distinct",
             "relocate")
  for (verb in verbs) {
    local({
      fun <- getExportedValue("dplyr", verb)
      method <- if (verb %in% joins) {
        function(x, y, ...) {
          other_backend_frame(fun(other_backend_data(x),
                                  other_backend_data(y), ...))
        }
      } else {
        function(.data, ...) {
          other_backend_frame(fun(other_backend_data(.data), ...))
        }
      }
      registerS3method(verb, "marginplyr_test_other_backend", method,
                       envir = asNamespace("dplyr"))
    })
  }
  registerS3method(
    "group_vars", "marginplyr_test_other_backend",
    function(x) attr(x, "group_names"), envir = asNamespace("dplyr")
  )
  registerS3method(
    "tbl_vars", "marginplyr_test_other_backend",
    function(x) names(unclass(x)), envir = asNamespace("dplyr")
  )
  registerS3method(
    "collect", "marginplyr_test_other_backend",
    function(x, ...) dplyr::ungroup(other_backend_data(x)),
    envir = asNamespace("dplyr")
  )
}

test_that("an unrecognised backend joins share denominators on missing keys", {
  register_other_backend()
  data <- tibble::tibble(
    f = c(NA, NA, "P"), g = c("a", "b", "a"), x = c(1, 3, 4)
  )
  source <- other_backend_frame(data)
  expect_identical(grouping_backend(source)$kind, "other")
  summarize <- function(data, ...) {
    summarize_with_margins(
      data, value = sum(x), parent = share_of_parent(value),
      total = share_of_total(value), .grouping = rollup(f, g), ...
    )
  }
  expected <- summarize(data)
  # Its dialect cannot be asked whether it refuses a non-numeric source.
  actual <- dplyr::collect(summarize(source, .check_share_source = FALSE))
  expect_equal(actual, expected)
  expect_equal(actual$parent[is.na(actual$f) & actual$g != "Total"],
               c(0.25, 0.75))
})
