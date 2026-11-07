test_that("dtplyr observed label checks preserve an .SD dimension", {
  skip_if_suggest_absent("dtplyr")
  input <- data.table::data.table(dimension = c("a", "b"), value = c(1, 3))
  data.table::setnames(input, "dimension", ".SD")
  before <- data.table::copy(input)
  source <- dtplyr::lazy_dt(input, immutable = TRUE) |>
    dplyr::filter(TRUE)
  query <- summarize_with_margins(
    source, total = sum(value), .grouping = rollup(.SD),
    .check_margin_label = TRUE, .sort = "last", .id = "set"
  )
  expect_s3_class(query, "dtplyr_step")
  expect_identical(input, before)
  expect_identical(dplyr::collect(query), tibble::tibble(
    .SD = c("a", "b", "Total"), set = c(1L, 1L, 2L), total = c(1, 3, 4)
  ))
  expect_identical(input, before)
})

# Exercise the four Margin verbs with the same label-check options.
generated_label_verbs <- list(
  summarize_with_margins = function(data, ...) {
    summarize_with_margins(data, total = sum(value), ...)
  },
  expand_with_margins = function(data, ...) expand_with_margins(data, ...),
  nest_with_margins = function(data, ...) nest_with_margins(data, ...),
  nest_by_with_margins = function(data, ...) nest_by_with_margins(data, ...)
)

# Compare outer columns and nested cell values through ordinary local tibbles.
generated_label_result <- function(result) {
  if (inherits(result, "dtplyr_step")) result <- dplyr::collect(result)
  result <- tibble::as_tibble(dplyr::ungroup(result))
  if ("data" %in% names(result)) {
    result$data <- lapply(result$data, tibble::as_tibble)
  }
  result
}

test_that("dtplyr checks .SD on empty inputs and multiple dimensions", {
  skip_if_suggest_absent("dtplyr")
  for (size in c(0L, 2L)) {
    input <- data.table::data.table(
      dimension = c("a", "b")[seq_len(size)],
      other = rep("x", size), value = c(1, 3)[seq_len(size)]
    )
    data.table::setnames(input, c("dimension", "other"), c(".SD", "g"))
    before <- data.table::copy(input)
    source <- dtplyr::lazy_dt(input, immutable = TRUE) |> dplyr::filter(TRUE)
    for (dimensions in list(".SD", c(".SD", "g"))) {
      spec <- rollup(dplyr::all_of(dimensions))
      for (verb in names(generated_label_verbs)) {
        run <- generated_label_verbs[[verb]]
        query <- run(source, .grouping = spec, .check_margin_label = TRUE,
                     .sort = "last", .id = "set")
        if (verb == "nest_by_with_margins") {
          expect_s3_class(query, "rowwise_df")
        } else {
          expect_s3_class(query, "dtplyr_step")
        }
        expect_identical(input, before)
        actual <- generated_label_result(query)
        expected <- generated_label_result(run(
          tibble::as_tibble(before), .grouping = spec,
          .check_margin_label = TRUE, .sort = "last", .id = "set"
        ))
        expect_identical(
          actual, expected,
          info = paste(verb, size, paste(dimensions, collapse = ","))
        )
        expect_identical(input, before)
        expect_identical(generated_label_result(query), actual)
      }
    }
  }
})

test_that("dtplyr collision conditions name the caller's checked dimensions", {
  skip_if_suggest_absent("dtplyr")
  dimensions <- c(".SD", "g")
  for (colliding in list(dimensions[1L], dimensions[2L], dimensions)) {
    input <- data.table::data.table(dimension = "a", other = "b", value = 2L)
    data.table::setnames(input, c("dimension", "other"), dimensions)
    for (name in colliding) data.table::set(input, j = name, value = "Total")
    before <- data.table::copy(input)
    source <- dtplyr::lazy_dt(input, immutable = TRUE) |> dplyr::filter(TRUE)
    for (verb in names(generated_label_verbs)) {
      run <- generated_label_verbs[[verb]]
      error <- tryCatch(run(
        source, .grouping = rollup(dplyr::all_of(dimensions)),
        .check_margin_label = TRUE
      ), error = identity)
      expect_s3_class(error, "marginplyr_error")
      message <- conditionMessage(error)
      expect_match(message, "already present in grouping", fixed = TRUE)
      for (name in dimensions) {
        expect_identical(grepl(paste0("`", name, "`"), message, fixed = TRUE),
                         name %in% colliding)
      }
      expect_identical(error$call[[1L]], as.name(verb))
      expect_identical(input, before)
      unchecked <- run(source, .grouping = rollup(dplyr::all_of(dimensions)),
                       .check_margin_label = FALSE, .id = "set")
      expect_no_error(generated_label_result(unchecked))
      expect_identical(input, before)
    }
  }
})

test_that("declared .SD collisions do not depend on the observed opt-in", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(
    .SD = factor("a", levels = c("a", "Total")), value = 2L
  )
  source <- dtplyr::lazy_dt(input, immutable = TRUE)
  for (run in generated_label_verbs) {
    for (check in c(FALSE, TRUE)) {
      error <- tryCatch(run(source, .grouping = rollup(.SD),
                            .check_margin_label = check), error = identity)
      expect_s3_class(error, "marginplyr_error")
      message <- conditionMessage(error)
      expect_match(message, "already a factor level", fixed = TRUE)
      expect_match(message, "`.SD`", fixed = TRUE)
      expect_false(grepl(".check_margin_label", message, fixed = TRUE))
    }
  }
  expect_identical(dplyr::collect(source), input)
})

test_that(".SD checking reads dtplyr rows at the requested boundary", {
  skip_if_suggest_absent("dtplyr")
  for (verb in names(generated_label_verbs)) {
    for (check in c(FALSE, TRUE)) {
      reads <- 0L
      visit <- function(x) {
        reads <<- reads + length(x)
        rep(TRUE, length(x))
      }
      source <- dtplyr::lazy_dt(tibble::tibble(.SD = "a", value = 2L)) |>
        dplyr::filter(visit(value))
      result <- generated_label_verbs[[verb]](
        source, .grouping = rollup(.SD), .check_margin_label = check
      )
      if (check || verb == "nest_by_with_margins") {
        expect_gt(reads, 0L)
      } else {
        expect_identical(reads, 0L)
      }
      expect_no_error(generated_label_result(result))
      expect_gt(reads, 0L)
    }
  }
})

test_that("dtplyr collision checks keep generated helper lookup private", {
  skip_if_suggest_absent("dtplyr")
  root <- new.env(parent = globalenv())
  root$source <- data.table::data.table(g = "Total", value = 7L)
  before <- serialize(root$source, NULL)
  input <- evalq(
    dplyr::filter(dtplyr::lazy_dt(source), base::identity(value) > 0L), root
  )
  authored_calls <- 0L
  generated_calls <- 0L
  root$`::` <- function(package, name) {
    package <- as.character(substitute(package))
    name <- as.character(substitute(name))
    if (identical(c(package, name), c("base", "identity"))) {
      authored_calls <<- authored_calls + 1L
    }
    if (paste0(package, "::", name) %in%
          c("base::as.character", "base::sum", "dplyr::if_else")) {
      generated_calls <<- generated_calls + 1L
      return(function(...) 0L)
    }
    getExportedValue(package, name)
  }
  query <- expand_with_margins(input, .grouping = rollup(g), .id = "set")
  expect_s3_class(query, "dtplyr_step")
  expect_identical(authored_calls, 0L)
  expect_error(
    expand_with_margins(
      input, .grouping = rollup(g), .check_margin_label = TRUE
    ),
    "already present in grouping", class = "marginplyr_error"
  )
  expect_identical(authored_calls, 1L)
  expect_identical(generated_calls, 0L)
  expect_identical(serialize(root$source, NULL), before)
})

test_that("omitted dtplyr dimensions ignore late namespace dispatch", {
  skip_if_suggest_absent("dtplyr")
  root <- new.env(parent = globalenv())
  root$source <- data.table::data.table(g = c("a", "b"), value = c(7L, 11L))
  before <- serialize(root$source, NULL)
  input <- evalq(dtplyr::lazy_dt(source), root)
  query <- expand_with_margins(
    input, .grouping = rollup(g), .id = "set", .margin_label = "All"
  )
  expect_s3_class(query, "dtplyr_step")
  calls <- 0L
  root$`::` <- function(package, name) {
    package <- as.character(substitute(package))
    name <- as.character(substitute(name))
    if (identical(package, "base") && name %in% c("rep", "length")) {
      calls <<- calls + 1L
      if (name == "rep") return(function(x, times) rep("MASK", times))
    }
    getExportedValue(package, name)
  }
  expected <- tibble::tibble(
    g = c("a", "b", "All", "All"), set = c(1L, 1L, 2L, 2L),
    value = c(7L, 11L, 7L, 11L)
  )
  collect_expansion <- function() {
    dplyr::arrange(dplyr::collect(query), set, value)
  }
  expect_identical(collect_expansion(), expected)
  expect_identical(calls, 0L)
  rm("::", envir = root)
  expect_identical(collect_expansion(), expected)
  expect_identical(serialize(root$source, NULL), before)
})
