# An input marginplyr does not recognise: a named list of columns whose dplyr
# verbs run on the grouped tibble it holds. It is neither a data frame nor a
# lazy table, so `grouping_backend()` classifies it as `other`. The class is
# the tests' own, so the methods registered for it on first use reach no other
# input.
other_backend_input <- local({
  registered <- FALSE
  wrap <- function(data) {
    structure(
      as.list(dplyr::ungroup(data)),
      group_names = dplyr::group_vars(data),
      class = c("marginplyr_test_other_backend", "list")
    )
  }
  # A join's other input may be an ordinary frame.
  unwrap <- function(x) {
    if (!inherits(x, "marginplyr_test_other_backend")) {
      return(x)
    }
    dplyr::group_by(
      tibble::as_tibble(unclass(x)[names(x)]),
      !!!rlang::syms(attr(x, "group_names"))
    )
  }
  register <- function() {
    dplyr_ns <- asNamespace("dplyr")
    joins <- c("left_join", "union_all")
    verbs <- c(joins, "group_by", "ungroup", "summarise", "mutate", "select",
               "rename", "arrange", "filter", "transmute", "distinct",
               "relocate", "slice", "count", "compute", "collapse")
    for (verb in verbs) {
      local({
        fun <- getExportedValue("dplyr", verb)
        method <- if (verb %in% joins) {
          function(x, y, ...) wrap(fun(unwrap(x), unwrap(y), ...))
        } else {
          function(.data, ...) wrap(fun(unwrap(.data), ...))
        }
        registerS3method(verb, "marginplyr_test_other_backend", method,
                         envir = dplyr_ns)
      })
    }
    registerS3method(
      "group_vars", "marginplyr_test_other_backend",
      function(x) attr(x, "group_names"), envir = dplyr_ns
    )
    registerS3method(
      "tbl_vars", "marginplyr_test_other_backend",
      function(x) names(unclass(x)), envir = dplyr_ns
    )
    registerS3method(
      "collect", "marginplyr_test_other_backend",
      function(x, ...) dplyr::ungroup(unwrap(x)), envir = dplyr_ns
    )
  }
  function(data) {
    if (!registered) {
      register()
      registered <<- TRUE
    }
    wrap(data)
  }
})
