test_that("included dtplyr dimensions ignore generated namespace dispatch", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; this remains a one-backend test.
  for (region in list("north", c("east", "west"))) {
    caller <- new.env(parent = environment())
    values <- if (length(region) == 1L) 13L else c(17L, 43L)
    caller$source <- data.table::data.table(region = region, value = values)
    before <- serialize(caller$source, NULL)
    input <- evalq(dtplyr::lazy_dt(source), caller)
    query <- expand_with_margins(input, .grouping = grouping_set(region))
    calls <- 0L
    caller$`:::` <- function(package, name) {
      package <- as.character(substitute(package))
      name <- as.character(substitute(name))
      if (identical(c(package, name),
                    c("marginplyr", "margin_display_character"))) {
        calls <<- calls + 1L
        return(function(x) paste0("changed:", x))
      }
      getFromNamespace(name, package)
    }
    expected <- tibble::tibble(region = region, value = values)
    expect_identical(dplyr::collect(input), expected)
    expect_identical(dplyr::collect(dplyr::mutate(input, value = value + 0L)),
                     expected)
    expect_identical(dplyr::collect(dplyr::union_all(input, input)),
                     dplyr::bind_rows(expected, expected))
    expect_identical(calls, 0L)
    expect_identical(dplyr::collect(query), expected)
    expect_identical(calls, 0L)
    rm(":::", envir = caller)
    expect_identical(dplyr::collect(query), expected)
    expect_identical(serialize(caller$source, NULL), before)
  }
})

test_that("generated dtplyr labels retain rows across deferred source graphs", {
  skip_if_suggest_absent("dtplyr")
  cases <- expand.grid(
    target = c("margin_display_character", "encode_factor_for_margin",
               "reconstruct_factor_vector"),
    graph = c("root", "filter", "cache", "renamed_root"),
    plan = c("single", "rollup", "repeated"),
    sort = c("none", "first", "last"), timing = c("early", "late"),
    mode = c("count", "corrupt", "error"), stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(cases))) local({
    case <- cases[i, ]
    info <- paste(unlist(case), collapse = "/")
    caller <- new.env(parent = environment())
    is_factor <- case$target != "margin_display_character"
    dimension <- if (is_factor) {
      ordered(c("north", "south"), c("south", "north", "unused"))
    } else {
      c("north", "south")
    }
    caller$source <- data.table::data.table(g = dimension, value = c(13L, 29L))
    before <- serialize(caller$source, NULL)
    row_reads <- 0L
    caller$visit <- function(x) {
      row_reads <<- row_reads + length(x)
      rep(TRUE, length(x))
    }
    input <- evalq(dtplyr::lazy_dt(source), caller)
    input <- switch(case$graph,
      root = input,
      filter = dplyr::filter(input, visit(value)),
      cache = dplyr::compute(input, name = "copy"),
      renamed_root = dplyr::rename(
        dplyr::rename(input, key = g), `Group key` = key
      )
    )
    key <- if (case$graph == "renamed_root") "Group key" else "g"
    column <- rlang::sym(key)
    spec <- switch(case$plan,
      single = grouping_set(!!column), rollup = rollup(!!column),
      repeated = grouping_sets(grouping_set(!!column), grouping_set(),
                               grouping_set(!!column))
    )
    calls <- 0L
    dispatcher <- function(package, name) {
      package <- as.character(substitute(package))
      name <- as.character(substitute(name))
      if (identical(c(package, name), c("marginplyr", case$target))) {
        calls <<- calls + 1L
        if (case$mode == "error") {
          return(function(...) stop("Caller label ran."))
        }
        if (case$mode == "corrupt") return(function(x, ...) {
          if (case$target == "reconstruct_factor_vector") {
            return(ordered(rep("MASK", length(x)), "MASK"))
          }
          rep("MASK", length(x))
        })
      }
      getFromNamespace(name, package)
    }
    if (case$timing == "early") caller$`:::` <- dispatcher
    position <- if (case$timing == "early") "first" else "last"
    query <- expand_with_margins(
      input, .grouping = spec, .duplicates = "keep", .id = "set",
      .margin_label = "All", .margin_label_position = position,
      .sort = case$sort
    )
    expect_s3_class(query, "dtplyr_step")
    expect_identical(calls, 0L, info = info)
    expect_identical(row_reads, 0L, info = info)
    if (case$timing == "late") caller$`:::` <- dispatcher
    caller$getFromNamespace <- function(...) stop("Caller accessor ran.")
    native <- tibble::tibble(!!key := dimension, value = c(13L, 29L))
    expect_identical(dplyr::collect(input), native, info = info)
    expect_identical(dplyr::collect(dplyr::mutate(input, value = value + 0L)),
                     native, info = info)
    expect_identical(dplyr::collect(dplyr::union_all(input, input)),
                     dplyr::bind_rows(native, native), info = info)
    expect_identical(calls, 0L, info = info)
    values <- switch(case$plan,
      single = c("north", "south"),
      rollup = c("north", "south", "All", "All"),
      repeated = c("north", "south", "All", "All", "north", "south")
    )
    if (is_factor) {
      levels <- if (position == "first") {
        c("All", "south", "north", "unused")
      } else {
        c("south", "north", "unused", "All")
      }
      values <- ordered(values, levels)
    }
    count <- switch(case$plan, single = 1L, rollup = 2L, repeated = 3L)
    expected <- tibble::tibble(
      !!key := values, set = rep(seq_len(count), each = 2L),
      value = rep(c(13L, 29L), count)
    )
    actual <- dplyr::collect(query)
    expect_identical(dplyr::arrange(actual, set, value), expected, info = info)
    order <- if (case$sort == "none") {
      seq_len(nrow(expected))
    } else {
      switch(case$plan,
        single = if (is_factor) c(2L, 1L) else 1:2,
        rollup = if (case$sort == "first") {
          if (is_factor) c(3L, 4L, 2L, 1L) else c(3L, 4L, 1L, 2L)
        } else {
          if (is_factor) c(2L, 1L, 3L, 4L) else 1:4
        },
        repeated = if (case$sort == "first") {
          if (is_factor) {
            c(3L, 4L, 2L, 6L, 1L, 5L)
          } else {
            c(3L, 4L, 1L, 5L, 2L, 6L)
          }
        } else {
          if (is_factor) {
            c(2L, 6L, 1L, 5L, 3L, 4L)
          } else {
            c(1L, 5L, 2L, 6L, 3L, 4L)
          }
        }
      )
    }
    # Payload order within an equal Margin key is unspecified.
    expect_identical(
      dplyr::distinct(dplyr::select(actual, dplyr::all_of(c(key, "set")))),
      dplyr::distinct(dplyr::select(
        expected[order, ], dplyr::all_of(c(key, "set"))
      )),
      info = info
    )
    expect_identical(calls, 0L, info = info)
    rm(":::", "getFromNamespace", envir = caller)
    expect_identical(dplyr::collect(query), actual, info = info)
    expect_identical(serialize(caller$source, NULL), before, info = info)
  })
})

test_that("typed factor labels retain NA levels and missing codes lazily", {
  skip_if_suggest_absent("dtplyr")
  counter <- local_dtplyr_proxy_counter()
  for (label in list("All", NULL, NA_character_)) {
    for (ordered in c(FALSE, TRUE)) {
      caller <- new.env(parent = environment())
      levels <- if (is.null(label)) {
        c("south", "north", "unused", NA)
      } else {
        c("south", "north", "unused")
      }
      dimension <- structure(
        c(2L, 1L, NA_integer_), levels = levels,
        class = if (ordered) c("ordered", "factor") else "factor"
      )
      caller$source <- data.table::data.table(
        g = dimension, value = c(13L, 29L, 41L)
      )
      before <- serialize(caller$source, NULL)
      input <- evalq(dtplyr::lazy_dt(source), caller)
      caller$`:::` <- function(...) stop("Caller namespace ran.")
      counter$n <- 0L
      counter$source_evaluations <- 0L
      query <- expand_with_margins(
        input, .grouping = rollup(g), .margin_label = label, .id = "set"
      )
      expect_identical(counter$n, 1L)
      expect_identical(counter$source_evaluations, 0L)
      expected_levels <- if (identical(label, "All")) {
        c(levels, "All")
      } else {
        levels
      }
      codes <- if (identical(label, "All")) {
        c(2L, 1L, NA, 4L, 4L, 4L)
      } else {
        c(2L, 1L, NA, NA, NA, NA)
      }
      expected <- tibble::tibble(
        g = structure(as.integer(codes), levels = expected_levels,
                      class = class(dimension)),
        set = rep(1:2, each = 3L), value = rep(c(13L, 29L, 41L), 2L)
      )
      expect_identical(dplyr::collect(query), expected)
      expect_identical(serialize(caller$source, NULL), before)
    }
  }
})

test_that("authored label namespace expressions retain ordinary root lookup", {
  skip_if_suggest_absent("dtplyr")
  for (operator in c("::", ":::")) {
    for (timing in c("early", "late")) {
      caller <- new.env(parent = environment())
      caller$source <- data.table::data.table(
        g = c("north", "south"), value = c(13L, 29L)
      )
      input <- evalq(dtplyr::lazy_dt(source), caller)
      calls <- 0L
      package <- if (operator == "::") "base" else "marginplyr"
      member <- if (operator == "::") "identity" else "margin_display_character"
      authored <- rlang::call2(
        rlang::call2(operator, rlang::sym(package), rlang::sym(member)),
        quote(g)
      )
      input <- dplyr::filter(input, !!authored == g)
      dispatcher <- function(pkg, name) {
        pkg <- as.character(substitute(pkg))
        name <- as.character(substitute(name))
        if (identical(c(pkg, name), c(package, member))) {
          calls <<- calls + 1L
          return(function(x) x)
        }
        if (operator == "::") {
          getExportedValue(pkg, name)
        } else {
          getFromNamespace(name, pkg)
        }
      }
      if (timing == "early") assign(operator, dispatcher, caller)
      query <- expand_with_margins(input, .grouping = rollup(g), .id = "set")
      expect_identical(calls, 0L)
      if (timing == "late") assign(operator, dispatcher, caller)
      expect_identical(dplyr::collect(input), tibble::tibble(
        g = c("north", "south"), value = c(13L, 29L)
      ))
      expect_identical(calls, 1L)
      calls <- 0L
      expect_identical(dplyr::collect(query), tibble::tibble(
        g = c("north", "south", "Total", "Total"), set = c(1L, 1L, 2L, 2L),
        value = c(13L, 29L, 13L, 29L)
      ))
      expect_identical(calls, 1L)
    }
  }
})

test_that("deferred dtplyr factor identity ignores namespace dispatch", {
  skip_if_suggest_absent("dtplyr")
  for (target in c("encode_factor_for_margin", "reconstruct_factor_vector")) {
    caller <- new.env(parent = environment())
    dimension <- ordered(c("north", "south"), c("south", "north", "unused"))
    caller$source <- data.table::data.table(g = dimension, value = c(13L, 29L))
    input <- evalq(dtplyr::lazy_dt(source), caller)
    calls <- 0L
    caller$`:::` <- function(package, name) {
      package <- as.character(substitute(package))
      name <- as.character(substitute(name))
      if (identical(c(package, name), c("marginplyr", target))) {
        calls <<- calls + 1L
        return(function(x, ...) {
          if (target == "encode_factor_for_margin") {
            return(rep("south", length(x)))
          }
          ordered(rep("south", length(x)), c("south", "north", "unused", "All"))
        })
      }
      getFromNamespace(name, package)
    }
    query <- expand_with_margins(
      input, .grouping = grouping_set(g), .margin_label = "All"
    )
    expect_identical(calls, 0L)
    expect_identical(dplyr::collect(query), tibble::tibble(
      g = ordered(c("north", "south"), c("south", "north", "unused", "All")),
      value = c(13L, 29L)
    ))
    expect_identical(calls, 0L)
  }
})
