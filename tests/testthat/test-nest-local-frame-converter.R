# Replace only the converter receiving generated Nest bookkeeping columns.
with_nest_local_frame <- function(code) {
  ns <- asNamespace("tibble")
  original <- utils::getS3method("as_tibble", "data.frame", envir = ns)
  stock <- utils::getFromNamespace("as_tibble.data.frame", "tibble")
  on.exit(registerS3method("as_tibble", "data.frame", original, envir = ns))
  registerS3method("as_tibble", "data.frame", function(x, ...) {
    if (any(startsWith(names(x), "..marginplyr_nest_"))) {
      if ("rid" %in% names(x)) x$rid <- rep.int(-99L, nrow(x))
      if ("fixed" %in% names(x)) x$fixed <- rep("merged", nrow(x))
      if ("district" %in% names(x)) x$district <- rep("changed", nrow(x))
    }
    stock(x, ...)
  }, envir = ns)
  code()
}

test_that("local Nest cells preserve values under frame registration", {
  input <- tibble::tibble(rid = 17L)
  before <- serialize(input, NULL)
  original <- utils::getS3method("as_tibble", "data.frame",
                                 envir = asNamespace("tibble"))
  with_nest_local_frame(function() {
    expect_identical(tibble::as_tibble(input)$rid, 17L)
    expect_identical(dplyr::nest_by(input)$data[[1L]]$rid, 17L)
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      expect_identical(verb(input)$data[[1L]]$rid, 17L)
    }
    expect_identical(serialize(input, NULL), before)
  })
  expect_identical(utils::getS3method("as_tibble", "data.frame",
                                      envir = asNamespace("tibble")), original)
  expect_identical(tibble::as_tibble(input)$rid, 17L)
})

# Known memberships for each public plan, independent of the fold's grouping.
expect_local_nest_frame_plans <- function(input) {
  before <- serialize(input, NULL)
  cases <- list(
    keyless = list(
      by = character(), spec = grouping_set(),
      outer = list(set = 1L), rows = list(1:3)
    ),
    fixed = list(
      by = "fixed", spec = grouping_set(),
      outer = list(fixed = c("p", "q"), set = c(1L, 1L)),
      rows = list(1:2, 3L)
    ),
    included = list(
      by = "fixed", spec = grouping_set(dplyr::all_of("district")),
      outer = list(fixed = c("p", "p", "q"),
                   district = c("north", "south", "north"),
                   set = c(1L, 1L, 1L)),
      rows = list(1L, 2L, 3L)
    ),
    rollup = list(
      by = "fixed", spec = rollup(dplyr::all_of("district")),
      outer = list(fixed = c("p", "p", "q", "p", "q"),
                   district = c("north", "south", "north", NA, NA),
                   set = c(1L, 1L, 1L, 2L, 2L)),
      rows = list(1L, 2L, 3L, 1:2, 3L)
    ),
    mixed = list(
      by = "fixed", spec = grouping_sets(
        grouping_set(dplyr::all_of("district")),
        grouping_set(dplyr::all_of("zone"))
      ),
      outer = list(fixed = c("p", "p", "q", "p", "q"),
                   district = c("north", "south", "north", NA, NA),
                   zone = c(NA, NA, NA, "x", "y"),
                   set = c(1L, 1L, 1L, 2L, 2L)),
      rows = list(1L, 2L, 3L, 1:2, 3L)
    )
  )
  columns <- list(fixed = c("p", "p", "q"),
                  district = c("north", "south", "north"),
                  zone = c("x", "x", "y"), rid = c(41L, 67L, 89L))
  with_nest_local_frame(function() {
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      for (case in cases) {
        for (keep in c(FALSE, TRUE)) {
          result <- verb(input, .by = dplyr::all_of(case$by),
                         .grouping = case$spec, .keep = keep,
                         .id = "set", .key = "detail", .margin_label = NULL)
          result <- dplyr::arrange(result, !!!rlang::syms(c(
            "set", setdiff(names(case$outer), "set")
          )))
          expect_named(result, c(names(case$outer), "detail"))
          for (key in names(case$outer)) {
            expect_identical(result[[key]], case$outer[[key]])
          }
          cell_cols <- if (keep) {
            names(columns)
          } else {
            setdiff(names(columns), names(case$outer))
          }
          expected <- lapply(case$rows, function(rows) {
            lapply(columns[cell_cols], function(column) column[rows])
          })
          expect_identical(lapply(result$detail, as.list), expected)
          memberships <- lapply(case$rows, function(rows) {
            sort(columns$rid[rows])
          })
          expect_identical(lapply(result$detail, function(cell) sort(cell$rid)),
                           memberships)
          expect_true(all(vapply(result$detail, inherits, logical(1),
                                 what = "tbl_df")))
          if (identical(verb, nest_by_with_margins)) {
            expect_s3_class(result, "rowwise_df")
            expect_identical(dplyr::group_vars(result), names(case$outer))
          } else {
            expect_false(inherits(result, "rowwise_df"))
            expect_identical(dplyr::group_vars(result), character())
          }
        }
      }
    }
    expect_identical(serialize(input, NULL), before)
  })
}

test_that("frame registrations preserve local Nest plans and kept keys", {
  input <- data.frame(fixed = c("p", "p", "q"),
                      district = c("north", "south", "north"),
                      zone = c("x", "x", "y"), rid = c(41L, 67L, 89L))
  expect_local_nest_frame_plans(input)
  expect_local_nest_frame_plans(tibble::as_tibble(input))
})

test_that("frame registrations preserve eager data.table Nest plans", {
  skip_if_suggest_absent("data.table")
  input <- data.table::data.table(fixed = c("p", "p", "q"),
                                  district = c("north", "south", "north"),
                                  zone = c("x", "x", "y"),
                                  rid = c(41L, 67L, 89L))
  expect_local_nest_frame_plans(input)
})

test_that("frame registrations cannot merge fixed Nest partitions", {
  input <- tibble::tibble(fixed = c("p", "q"), district = c("north", "south"),
                          rid = c(41L, 89L))
  with_nest_local_frame(function() {
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      result <- verb(input, .by = fixed, .grouping = rollup(district),
                     .keep = TRUE, .id = "set", .margin_label = NULL)
      result <- dplyr::arrange(result, !!!rlang::syms(c("set", "fixed")))
      expect_identical(nrow(result), 4L)
      expect_identical(result$fixed, c("p", "q", "p", "q"))
      expect_identical(result$district, c("north", "south", NA, NA))
      expect_identical(result$set, c(1L, 1L, 2L, 2L))
      expect_identical(lapply(result$data, function(cell) cell$rid),
                       list(41L, 89L, 41L, 89L))
      expect_identical(lapply(result$data, function(cell) cell$fixed),
                       list("p", "q", "p", "q"))
    }
  })
})

test_that("frame registrations preserve empty and payload-free Nest rows", {
  with_nest_local_frame(function() {
    for (input in list(tibble::tibble(rid = integer()),
                       tibble::new_tibble(list(), nrow = 0L))) {
      ordinary <- nest_with_margins(input, .id = "set")
      expect_identical(nrow(ordinary), 0L)
      expect_identical(ordinary$set, integer())
      expect_identical(dplyr::group_vars(ordinary), character())
      rowwise <- nest_by_with_margins(input, .id = "set")
      expect_identical(rowwise$set, 1L)
      expect_s3_class(rowwise, "rowwise_df")
      expect_identical(dplyr::group_vars(rowwise), "set")
      expect_identical(nrow(rowwise$data[[1L]]), 0L)
      expect_identical(as.list(rowwise$data[[1L]]), as.list(input))
      expect_s3_class(rowwise$data[[1L]], "tbl_df")
    }
    input <- tibble::tibble(district = c("north", "north", "south"))
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      result <- verb(input, .grouping = rollup(district), .id = "set",
                     .margin_label = NULL)
      expect_identical(result$district, c("north", "south", NA_character_))
      expect_identical(result$set, c(1L, 1L, 2L))
      expect_identical(lapply(result$data, dim),
                       list(c(2L, 0L), c(1L, 0L), c(3L, 0L)))
      expect_true(all(vapply(result$data, inherits, logical(1),
                             what = "tbl_df")))
    }
  })
})

test_that("caller-owned subclass conversion retains ordinary dispatch", {
  local({
    input <- structure(data.frame(rid = 17L),
                       class = c("margin_frame_input", "data.frame"))
    before <- serialize(input, NULL)
    ns <- asNamespace("tibble")
    registry <- get(".__S3MethodsTable__.", envir = ns)
    original <- utils::getS3method("as_tibble", "margin_frame_input",
                                   optional = TRUE, envir = ns)
    on.exit({
      if (is.null(original)) {
        rm("as_tibble.margin_frame_input", envir = registry)
      } else {
        registerS3method("as_tibble", "margin_frame_input", original,
                         envir = ns)
      }
    })
    stock <- utils::getFromNamespace("as_tibble.data.frame", "tibble")
    registerS3method("as_tibble", "margin_frame_input", function(x, ...) {
      x$rid <- x$rid + 100L
      stock(x, ...)
    }, envir = ns)
    with_nest_local_frame(function() {
      expect_identical(tibble::as_tibble(input)$rid, 117L)
      for (verb in list(nest_with_margins, nest_by_with_margins)) {
        expect_identical(verb(input)$data[[1L]]$rid, 17L)
      }
      expect_identical(tibble::as_tibble(input)$rid, 117L)
      expect_identical(serialize(input, NULL), before)
    })
  })
})
