# Install a selective caller namespace operator without changing native lookups.
nest_namespace_dispatcher <- function(root, target, payload = "weight",
                                      replacement = -311L) {
  root$calls <- character()
  function(pkg, name) {
    package <- as.character(substitute(pkg))
    member <- as.character(substitute(name))
    selected <- paste(package, member, sep = "::")
    if (selected %in% target) {
      root$calls <- c(root$calls, selected)
      return(switch(member,
        length = base::length,
        min = function(...) 0L,
        seq_len = function(...) integer(),
        vec_rep = function(value, size) paste0("row_", seq_len(size)),
        vec_slice = function(x, i, ...) rep("SHADOW", length(i)),
        list = function(...) {
          columns <- base::list(...)
          if (payload %in% names(columns)) {
            columns[[payload]] <- rep(replacement, length(columns[[payload]]))
          }
          columns
        },
        as.data.table = function(x, ...) {
          x[[payload]] <- rep(replacement, length(x[[payload]]))
          getExportedValue("data.table", "as.data.table")(x, ...)
        },
        stop("GENERATED_NEST_NAMESPACE_INTERCEPT")
      ))
    }
    getExportedValue(package, member)
  }
}

test_that("generated nesting keys and payload ignore caller namespace lookup", {
  skip_if_suggest_absent("dtplyr")
  targets <- c("base::min", "base::seq_len", "vctrs::vec_slice",
               "base::list", "data.table::as.data.table")
  for (target in targets) {
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      for (early in c(TRUE, FALSE)) {
        root <- new.env(parent = globalenv())
        root$source <- data.table::data.table(
          district = c("north", "south"), weight = c(41L, 67L)
        )
        before <- serialize(root$source, NULL)
        dispatcher <- nest_namespace_dispatcher(root, target)
        if (early) root$`::` <- dispatcher
        input <- evalq(dtplyr::lazy_dt(source, immutable = TRUE), root)
        query <- verb(input, .grouping = grouping_set(district),
                      .margin_label = NULL)
        if (!early) root$`::` <- dispatcher
        actual <- dplyr::collect(query)
        expect_identical(actual$district, c("north", "south"), info = target)
        expect_identical(lapply(actual$data, as.list), list(
          list(weight = 41L), list(weight = 67L)
        ), info = target)
        expect_identical(names(actual), c("district", "data"))
        expect_identical(root$calls, character(), info = target)
        expect_identical(dplyr::collect(input)$weight, c(41L, 67L))
        expect_identical(dplyr::collect(dplyr::count(input, district))$n,
                         c(1L, 1L))
        rm("::", envir = root)
        expect_identical(dplyr::collect(query), actual)
        expect_identical(serialize(root$source, NULL), before)
      }
    }
  }
})

test_that("late converter lookup retains the original payload witness", {
  skip_if_suggest_absent("dtplyr")
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    root <- new.env(parent = globalenv())
    root$source <- tibble::tibble(
      region = c("east", "west"), units = c(13L, 29L)
    )
    before <- serialize(root$source, NULL)
    input <- evalq(dtplyr::lazy_dt(source, immutable = TRUE), root)
    query <- verb(input, .grouping = grouping_set(region), .margin_label = NULL)
    root$`::` <- nest_namespace_dispatcher(
      root, "data.table::as.data.table", "units", -99L
    )
    actual <- dplyr::collect(query)
    expect_identical(actual$region, c("east", "west"))
    expect_identical(lapply(actual$data, as.list),
                     list(list(units = 13L), list(units = 29L)))
    expect_identical(root$calls, character())
    expect_identical(dplyr::collect(input), root$source)
    rm("::", envir = root)
    expect_identical(dplyr::collect(query), actual)
    expect_identical(serialize(root$source, NULL), before)
  }
})

test_that("nesting namespace isolation preserves plans keep and order", {
  skip_if_suggest_absent("dtplyr")
  options <- expand.grid(
    early = c(TRUE, FALSE), rollup = c(TRUE, FALSE), keep = c(TRUE, FALSE),
    sort = c("none", "first", "last"), stringsAsFactors = FALSE
  )
  targets <- c("base::min", "base::seq_len", "vctrs::vec_size",
               "vctrs::vec_slice", "base::list", "data.table::as.data.table",
               "base::length", "vctrs::vec_rep")
  for (i in seq_len(nrow(options))) {
    option <- options[i, ]
    info <- as.character(i)
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      root <- new.env(parent = globalenv())
      root$source <- tibble::tibble(
        district = c("north", "north", "south"), weight = c(41L, 89L, 67L)
      )
      before <- serialize(root$source, NULL)
      dispatcher <- nest_namespace_dispatcher(root, targets)
      if (option$early) root$`::` <- dispatcher
      root$getExportedValue <- function(...) {
        stop("CALLER_EXPORT_LOOKUP_INTERCEPT")
      }
      input <- evalq(dtplyr::lazy_dt(source, immutable = TRUE), root)
      spec <- if (option$rollup) rollup(district) else grouping_set(district)
      query <- verb(input, .grouping = spec, .margin_label = NULL, .id = "set",
                    .keep = option$keep, .sort = option$sort)
      if (identical(verb, nest_with_margins)) {
        expect_s3_class(query, "dtplyr_step")
      } else {
        expect_s3_class(query, "rowwise_df")
        expect_identical(dplyr::group_vars(query), c("district", "set"))
      }
      if (!option$early) root$`::` <- dispatcher
      actual <- dplyr::ungroup(dplyr::collect(query))
      if (option$sort == "none") {
        actual <- dplyr::arrange(actual, .data$set, .data$district)
      }
      keys <- c("north", "south", NA_character_[option$rollup])
      sets <- c(1L, 1L, 2L[option$rollup])
      cells <- list(list(weight = c(41L, 89L)), list(weight = 67L))
      if (option$rollup) {
        cells <- c(cells, list(list(weight = c(41L, 89L, 67L))))
      }
      if (option$keep) {
        districts <- list(c("north", "north"), "south")
        if (option$rollup) {
          districts <- c(districts, list(c("north", "north", "south")))
        }
        cells <- Map(
          function(district, cell) c(list(district = district), cell),
          districts, cells
        )
      }
      positions <- if (option$rollup && option$sort == "first") {
        c(3L, 1L, 2L)
      } else {
        seq_along(keys)
      }
      expect_identical(actual$district, keys[positions], info = info)
      expect_identical(actual$set, sets[positions], info = info)
      expect_identical(lapply(actual$data, as.list), cells[positions],
                       info = info)
      expect_identical(names(actual), c("district", "set", "data"))
      expect_identical(root$calls, character(), info = info)
      expect_identical(serialize(root$source, NULL), before)
    }
  }
})

test_that("nesting keeps authored namespace lookup and collection boundaries", {
  skip_if_suggest_absent("dtplyr")
  root <- new.env(parent = globalenv())
  root$source <- tibble::tibble(
    partition = c("a", "a", "b"), district = c("north", "south", "north"),
    weight = c(41L, 67L, 89L)
  )
  before <- serialize(root$source, NULL)
  reads <- 0L
  dispatcher <- function(pkg, name) {
    package <- as.character(substitute(pkg))
    member <- as.character(substitute(name))
    if (package == "base" && member == "identity") {
      return(function(value) {
        if (length(value) > 0L) {
          reads <<- reads + 1L
        }
        value
      })
    }
    if (paste(package, member, sep = "::") %in% c(
      "base::list", "base::min", "base::seq_len", "vctrs::vec_slice",
      "vctrs::vec_size", "data.table::as.data.table", "base::length",
      "vctrs::vec_rep"
    )) stop("GENERATED_NEST_NAMESPACE_INTERCEPT")
    getExportedValue(package, member)
  }
  root$`::` <- dispatcher
  input <- evalq(dplyr::filter(
    dtplyr::lazy_dt(source, immutable = TRUE), base::identity(weight) > 0L
  ), root)
  query <- nest_with_margins(
    input, .by = partition, .grouping = rollup(district), .margin_label = NULL,
    .id = "set", .sort = "last"
  )
  expect_s3_class(query, "dtplyr_step")
  expect_identical(reads, 0L)
  actual <- dplyr::collect(query)
  expect_gt(reads, 0L)
  expect_identical(actual$partition, c("a", "a", "a", "b", "b"))
  expect_identical(actual$district, c("north", "south", NA, "north", NA))
  expect_identical(actual$set, c(1L, 1L, 2L, 1L, 2L))
  expect_identical(lapply(actual$data, as.list), list(
    list(weight = 41L), list(weight = 67L), list(weight = c(41L, 67L)),
    list(weight = 89L), list(weight = 89L)
  ))

  reads <- 0L
  rm("::", envir = root)
  # Rebind while the row-wise verb reads its source, after query construction.
  root$observe <- function(value) {
    if (length(value) > 0L) {
      reads <<- reads + 1L
      root$`::` <- dispatcher
    }
    rep(TRUE, length(value))
  }
  input <- evalq(dplyr::filter(
    dtplyr::lazy_dt(source, immutable = TRUE), observe(weight)
  ), root)
  rowwise <- nest_by_with_margins(
    input, .by = partition, .grouping = rollup(district), .margin_label = NULL,
    .id = "set", .sort = "last"
  )
  expect_gt(reads, 0L)
  expect_identical(root$`::`, dispatcher)
  expect_s3_class(rowwise, "rowwise_df")
  expect_identical(dplyr::group_vars(rowwise),
                   c("partition", "district", "set"))
  expect_identical(dplyr::ungroup(rowwise), actual)
  expect_identical(serialize(root$source, NULL), before)
})

test_that("nesting namespace isolation keeps empty cells and identities", {
  skip_if_suggest_absent("dtplyr")
  sources <- list(
    tibble::tibble(district = c("north", "north")),
    tibble::tibble(district = c("north", "north"), .SD = c(41L, 67L)),
    tibble::tibble(
      district = structure(
        c(1L, 2L, NA_integer_), levels = c("north", NA_character_),
        class = "factor"
      ),
      weight = c(41L, 67L, 89L)
    )
  )
  targets <- list("dplyr::tibble", "data.table::setnames",
                  c("base::as.integer", "base::unclass"))
  for (i in seq_along(sources)) {
    for (target in targets[[i]]) {
      for (verb in list(nest_with_margins, nest_by_with_margins)) {
        root <- new.env(parent = globalenv())
        root$source <- sources[[i]]
        before <- serialize(root$source, NULL)
        root$`::` <- nest_namespace_dispatcher(root, target)
        input <- evalq(dtplyr::lazy_dt(source, immutable = TRUE), root)
        actual <- dplyr::collect(verb(
          input, .grouping = grouping_set(district), .margin_label = NULL
        ))
        expect_identical(root$calls, character(), info = target)
        expect_identical(names(actual), c("district", "data"))
        if (i == 1L) {
          expect_identical(actual$district, "north")
          expect_identical(lapply(actual$data, dim), list(c(2L, 0L)))
        } else if (i == 2L) {
          expect_identical(actual$district, "north")
          expect_identical(lapply(actual$data, as.list),
                           list(list(.SD = c(41L, 67L))))
        } else {
          # No Margin order was requested; match the cells by literal payload.
          actual <- actual[order(vapply(actual$data, function(cell) {
            cell$weight
          }, integer(1))), ]
          expect_identical(actual$district, sources[[i]]$district)
          expect_identical(lapply(actual$data, as.list), list(
            list(weight = 41L), list(weight = 67L), list(weight = 89L)
          ))
        }
        expect_identical(serialize(root$source, NULL), before)
      }
    }
  }
})
