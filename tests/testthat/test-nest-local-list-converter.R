test_that("local Nest cells ignore registered list converters", {
  local({
    input <- tibble::tibble(rid = 17L)
    before <- serialize(input, NULL)
    ns <- asNamespace("tibble")
    stock <- utils::getS3method("as_tibble", "list", envir = ns)
    on.exit(registerS3method("as_tibble", "list", stock, envir = ns))
    calls <- 0L
    replacement <- function(x, ...) {
      calls <<- calls + 1L
      if ("rid" %in% names(x)) x$rid <- rep.int(-99L, length(x$rid))
      stock(x, ...)
    }
    registerS3method("as_tibble", "list", replacement, envir = ns)
    expect_identical(tibble::as_tibble(list(rid = 17L))$rid, -99L)
    expect_identical(calls, 1L)
    calls <- 0L
    expect_identical(dplyr::nest_by(input)$data[[1L]]$rid, 17L)
    expect_identical(calls, 0L)
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      result <- verb(input)
      expect_identical(result$data[[1L]]$rid, 17L)
      expect_identical(calls, 0L)
    }
    expect_identical(serialize(input, NULL), before)
    registerS3method("as_tibble", "list", stock, envir = ns)
    expect_identical(tibble::as_tibble(list(rid = 17L))$rid, 17L)
  })
})

test_that("registered list converters leave empty local Nest cells intact", {
  local({
    sources <- list(
      tibble::tibble(rid = integer(), day = as.Date(character())),
      tibble::new_tibble(list(), nrow = 0L)
    )
    ns <- asNamespace("tibble")
    stock <- utils::getS3method("as_tibble", "list", envir = ns)
    on.exit(registerS3method("as_tibble", "list", stock, envir = ns))
    calls <- 0L
    registerS3method("as_tibble", "list", function(x, ...) {
      calls <<- calls + 1L
      stock(x, ...)
    }, envir = ns)
    for (input in sources) {
      before <- serialize(input, NULL)
      ordinary <- nest_with_margins(input, .id = "set")
      expect_identical(nrow(ordinary), 0L)
      expect_identical(ordinary$set, integer())
      expect_false(inherits(ordinary, "grouped_df"))
      rowwise <- nest_by_with_margins(input, .id = "set")
      expect_s3_class(rowwise, "rowwise_df")
      expect_identical(dplyr::group_vars(rowwise), "set")
      expect_identical(rowwise$set, 1L)
      expect_identical(nrow(rowwise$data[[1L]]), 0L)
      expect_identical(as.list(rowwise$data[[1L]]), as.list(input))
      expect_s3_class(rowwise$data[[1L]], "tbl_df")
      expect_identical(serialize(input, NULL), before)
    }
    expect_identical(calls, 0L)
  })
})

test_that("registered list converters preserve Nest plans and kept keys", {
  skip_if_suggest_absent("tidyr")
  local({
    plain <- data.frame(
      fixed = rep("p", 3L), district = c("north", "north", "south"),
      rid = c(41L, 67L, 89L), day = as.Date(c("2026-01-01", "2026-01-02",
                                              "2026-01-03"))
    )
    sources <- list(plain, tibble::as_tibble(plain))
    native_classes <- lapply(sources, function(input) {
      class(tidyr::nest(input, data = dplyr::everything()))
    })
    ns <- asNamespace("tibble")
    stock <- utils::getS3method("as_tibble", "list", envir = ns)
    on.exit(registerS3method("as_tibble", "list", stock, envir = ns))
    calls <- 0L
    registerS3method("as_tibble", "list", function(x, ...) {
      calls <<- calls + 1L
      if ("rid" %in% names(x)) x$rid <- rep.int(-99L, length(x$rid))
      stock(x, ...)
    }, envir = ns)
    for (i in seq_along(sources)) {
      input <- sources[[i]]
      before <- serialize(input, NULL)
      for (verb in list(nest_with_margins, nest_by_with_margins)) {
        for (plan in c("included", "rollup", "keyless")) {
          for (keep in c(FALSE, TRUE)) {
            if (plan == "keyless") {
              result <- verb(input, .id = "set", .key = "detail",
                             .keep = keep)
              outer <- "set"
              sets <- 1L
              expected <- list(list(
                fixed = c("p", "p", "p"),
                district = c("north", "north", "south"),
                rid = c(41L, 67L, 89L),
                day = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03"))
              ))
            } else {
              total <- plan == "rollup"
              spec <- if (total) rollup(district) else grouping_set(district)
              result <- verb(
                input, .by = fixed, .grouping = spec, .id = "set",
                .key = "detail", .keep = keep, .margin_label = NULL,
                .sort = "last"
              )
              outer <- c("fixed", "district", "set")
              sets <- c(1L, 1L, 2L[total])
              expect_identical(result$fixed, rep("p", length(sets)))
              expect_identical(result$district,
                               c("north", "south", NA_character_[total]))
              expected <- list(
                list(rid = c(41L, 67L),
                     day = as.Date(c("2026-01-01", "2026-01-02"))),
                list(rid = 89L, day = as.Date("2026-01-03"))
              )
              if (total) {
                expected[[3L]] <- list(
                  rid = c(41L, 67L, 89L),
                  day = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03"))
                )
              }
              if (keep) {
                keys <- list(c("north", "north"), "south",
                             c("north", "north", "south"))
                expected <- Map(function(key, cell) {
                  c(list(fixed = rep("p", length(key)), district = key), cell)
                }, keys[seq_along(expected)], expected)
              }
            }
            expect_named(result, c(outer, "detail"))
            expect_identical(result$set, sets)
            expect_identical(lapply(result$detail, as.list), expected)
            expect_true(all(vapply(result$detail, inherits, logical(1),
                                   what = "tbl_df")))
            if (identical(verb, nest_by_with_margins)) {
              expect_s3_class(result, "rowwise_df")
              expect_identical(dplyr::group_vars(result), outer)
            } else {
              expect_identical(class(result), native_classes[[i]])
              expect_identical(dplyr::group_vars(result), character())
            }
          }
        }
      }
      expect_identical(serialize(input, NULL), before)
    }
    expect_identical(calls, 0L)
  })
})

test_that("registered list converters preserve payload-free local Nest rows", {
  local({
    input <- tibble::tibble(district = c("north", "north", "south"))
    before <- serialize(input, NULL)
    ns <- asNamespace("tibble")
    stock <- utils::getS3method("as_tibble", "list", envir = ns)
    on.exit(registerS3method("as_tibble", "list", stock, envir = ns))
    calls <- 0L
    registerS3method("as_tibble", "list", function(x, ...) {
      calls <<- calls + 1L
      stock(x, ...)
    }, envir = ns)
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      result <- verb(input, .grouping = rollup(district), .id = "set",
                     .margin_label = NULL, .sort = "last")
      expect_identical(result$district, c("north", "south", NA_character_))
      expect_identical(result$set, c(1L, 1L, 2L))
      expect_identical(lapply(result$data, dim),
                       list(c(2L, 0L), c(1L, 0L), c(3L, 0L)))
      expect_true(all(vapply(result$data, inherits, logical(1),
                             what = "tbl_df")))
    }
    expect_identical(calls, 0L)
    expect_identical(serialize(input, NULL), before)
  })
})
