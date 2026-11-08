# Replace one frame converter for the callback, restoring both exact S3
# registrations afterwards. Removing the tibble method exposes inheritance.
with_nest_frame_converter <- function(class, code) {
  ns <- asNamespace("data.table")
  registry <- get(".__S3MethodsTable__.", envir = ns)
  classes <- c("tbl_df", "data.frame")
  original <- lapply(classes, function(class) {
    utils::getS3method("as.data.table", class, optional = TRUE, envir = ns)
  })
  on.exit({
    for (i in seq_along(classes)) {
      method <- paste0("as.data.table.", classes[[i]])
      if (is.null(original[[i]])) {
        if (exists(method, envir = registry, inherits = FALSE)) {
          rm(list = method, envir = registry)
        }
      } else {
        registerS3method("as.data.table", classes[[i]], original[[i]],
                         envir = ns)
      }
    }
  })
  if (exists("as.data.table.tbl_df", envir = registry, inherits = FALSE)) {
    rm("as.data.table.tbl_df", envir = registry)
  }
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  registerS3method("as.data.table", class, function(x, ...) {
    calls$n <- calls$n + 1L
    data.table::data.table(injected = character())
  }, envir = ns)
  code(calls)
}

test_that("empty dtplyr Nest cells ignore registered frame converters", {
  skip_if_suggest_absent("dtplyr")
  # data.table is imported by dtplyr; it is not a second optional backend.
  source <- data.table::data.table(rid = integer(), day = as.Date(character()))
  before <- serialize(source, NULL)
  input <- dtplyr::lazy_dt(source, immutable = TRUE)
  expected <- list(rid = integer(), day = as.Date(character()))
  for (class in c("tbl_df", "data.frame")) {
    with_nest_frame_converter(class, function(calls) {
      expect_identical(
        names(data.table::as.data.table(tibble::as_tibble(source))), "injected"
      )
      expect_identical(calls$n, 1L)
      calls$n <- 0L
      expect_identical(as.list(dplyr::collect(input)), expected)
      result <- nest_by_with_margins(input, .id = "set")
      expect_s3_class(result, "rowwise_df")
      expect_identical(dplyr::group_vars(result), "set")
      expect_identical(result$set, 1L)
      expect_identical(nrow(result), 1L)
      expect_s3_class(result$data[[1L]], "data.table")
      expect_identical(nrow(result$data[[1L]]), 0L)
      expect_identical(as.list(result$data[[1L]]), expected)
      expect_identical(calls$n, 0L)
    })
    expect_identical(as.list(nest_by_with_margins(input)$data[[1L]]), expected)
    expect_identical(
      as.list(data.table::as.data.table(tibble::as_tibble(source))), expected
    )
    expect_identical(serialize(source, NULL), before)
  }
})

test_that("registered frame converters preserve empty dtplyr Nest options", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::as.data.table(tibble::tibble(
    rid = 1:2, day = as.Date(c("2026-01-01", "2026-01-02")),
    .SD = c("a", "b"), `..0` = c(3L, 5L), `..01` = c(TRUE, FALSE),
    .name_repair = "minimal"
  ))
  expected <- list(rid = integer(), day = as.Date(character()),
                   .SD = character(), `..0` = integer(), `..01` = logical())
  for (class in c("tbl_df", "data.frame")) {
    with_nest_frame_converter(class, function(calls) {
      for (filtered in c(FALSE, TRUE)) {
        data <- if (filtered) source else source[0L]
        before <- serialize(data, NULL)
        evaluations <- 0L
        discard <- function(rid) {
          evaluations <<- evaluations + 1L
          rep(FALSE, length(rid))
        }
        input <- dtplyr::lazy_dt(data, immutable = TRUE)
        if (filtered) input <- dplyr::filter(input, discard(rid))
        for (grouping in list(list(), list(.grouping = grouping_set()))) {
          for (keep in c(FALSE, TRUE)) {
            for (sort in c("none", "first", "last")) {
              for (custom in c(FALSE, TRUE)) {
                id <- if (custom) "occurrence" else NULL
                key <- if (custom) "payload" else "data"
                evaluations <- 0L
                result <- rlang::exec(
                  nest_by_with_margins, input, !!!grouping,
                  .keep = keep, .sort = sort, .key = key, .id = id
                )
                expect_s3_class(result, "rowwise_df")
                expect_named(result, c(id, key))
                expect_identical(dplyr::group_vars(result), as.character(id))
                expect_identical(nrow(result), 1L)
                if (custom) expect_identical(result$occurrence, 1L)
                expect_s3_class(result[[key]][[1L]], "data.table")
                expect_identical(nrow(result[[key]][[1L]]), 0L)
                expect_identical(as.list(result[[key]][[1L]]), expected)
                expect_identical(evaluations, as.integer(filtered))
                expect_identical(calls$n, 0L)
                expect_identical(serialize(data, NULL), before)
              }
            }
          }
        }
      }
    })
  }
})

test_that("frame registrations leave adjacent Nest behavior intact", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(
    district = c("E", "E", "W"), rid = c(41L, 67L, 89L),
    day = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03"))
  )
  before <- serialize(source, NULL)
  empty <- dtplyr::lazy_dt(source[0L], immutable = TRUE)
  populated <- dtplyr::lazy_dt(source, immutable = TRUE)
  local_empty <- tibble::tibble(rid = integer(), day = as.Date(character()))
  no_columns <- dtplyr::lazy_dt(data.table::data.table(), immutable = TRUE)
  keys_only <- dtplyr::lazy_dt(source[, "district"], immutable = TRUE)
  for (class in c("tbl_df", "data.frame")) {
    with_nest_frame_converter(class, function(calls) {
      local_result <- nest_by_with_margins(local_empty, .id = "set")
      expect_identical(local_result$set, 1L)
      expect_s3_class(local_result$data[[1L]], "tbl_df")
      expect_identical(as.list(local_result$data[[1L]]), as.list(local_empty))
      no_payload <- nest_by_with_margins(no_columns, .id = "set")
      expect_identical(no_payload$set, 1L)
      expect_identical(dim(no_payload$data[[1L]]), c(0L, 0L))
      expect_s3_class(no_payload$data[[1L]], "tbl_df")
      for (verb in list(nest_with_margins, nest_by_with_margins)) {
        for (keep in c(FALSE, TRUE)) {
          keyed_empty <- verb(empty, .by = district, .keep = keep, .id = "set")
          keyed_empty <- dplyr::collect(keyed_empty)
          expect_identical(nrow(keyed_empty), 0L)
          expect_named(keyed_empty, c("district", "set", "data"))
          expect_identical(keyed_empty$set, integer())
          result <- verb(populated, .by = district, .keep = keep, .id = "set")
          if (identical(verb, nest_with_margins)) {
            expect_s3_class(result, "dtplyr_step")
          } else {
            expect_s3_class(result, "rowwise_df")
            expect_identical(dplyr::group_vars(result), c("district", "set"))
          }
          result <- dplyr::collect(result)
          expect_identical(result$district, c("E", "W"))
          expect_identical(result$set, c(1L, 1L))
          expected <- list(
            list(rid = c(41L, 67L),
                 day = as.Date(c("2026-01-01", "2026-01-02"))),
            list(rid = 89L, day = as.Date("2026-01-03"))
          )
          if (keep) {
            expected <- Map(function(key, cell) c(list(district = key), cell),
                            list(c("E", "E"), "W"), expected)
          }
          expect_identical(lapply(result$data, as.list), expected)
          expect_true(all(vapply(result$data, inherits, logical(1),
                                 what = "data.table")))
        }
        keyless <- dplyr::collect(verb(populated))
        expect_identical(lapply(keyless$data, as.list), list(list(
          district = c("E", "E", "W"), rid = c(41L, 67L, 89L),
          day = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03"))
        )))
        payload_free <- dplyr::collect(verb(keys_only, .by = district))
        expect_identical(payload_free$district, c("E", "W"))
        expect_identical(lapply(payload_free$data, dim),
                         list(c(2L, 0L), c(1L, 0L)))
        expect_true(all(vapply(payload_free$data, inherits, logical(1),
                               what = "tbl_df")))
      }
      ordinary <- nest_with_margins(empty, .id = "set")
      expect_s3_class(ordinary, "dtplyr_step")
      expect_identical(nrow(dplyr::collect(ordinary)), 0L)
      expect_identical(nrow(nest_with_margins(local_empty)), 0L)
      expect_identical(calls$n, 0L)
    })
    expect_identical(serialize(source, NULL), before)
  }
})

test_that("frame registrations keep empty ordinary dtplyr Nest deferred", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(rid = 1:2)
  before <- serialize(source, NULL)
  for (class in c("tbl_df", "data.frame")) {
    with_nest_frame_converter(class, function(calls) {
      evaluations <- 0L
      discard <- function(rid) {
        evaluations <<- evaluations + 1L
        rep(FALSE, length(rid))
      }
      input <- dplyr::filter(dtplyr::lazy_dt(source, immutable = TRUE),
                             discard(rid))
      for (grouping in list(list(), list(.grouping = grouping_set()))) {
        evaluations <- 0L
        result <- rlang::exec(
          nest_with_margins, input, !!!grouping, .id = "set"
        )
        expect_s3_class(result, "dtplyr_step")
        expect_identical(evaluations, 0L)
        result <- dplyr::collect(result)
        expect_identical(nrow(result), 0L)
        expect_identical(result$set, integer())
        expect_identical(evaluations, 1L)
        expect_identical(calls$n, 0L)
      }
    })
    expect_identical(serialize(source, NULL), before)
  }
})
