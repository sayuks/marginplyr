# Independent ordered-set oracle. Run from the package source directory.
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))

select_call <- function(keys) rlang::call2("all_of", keys)

element <- function(keys) {
  list(expr = rlang::call2("grouping_set", select_call(keys)), sets = list(keys))
}

unit_family <- function(units, cube) {
  args <- lapply(units, function(keys) {
    if (length(keys) == 1L) select_call(keys) else element(keys)$expr
  })
  n <- length(units)
  choices <- if (cube) {
    unlist(lapply(n:0L, function(k) {
      utils::combn(seq_len(n), k, simplify = FALSE)
    }), recursive = FALSE)
  } else {
    lapply(n:0L, seq_len)
  }
  sets <- lapply(choices, function(i) unique(as.character(unlist(units[i]))))
  list(expr = rlang::call2(if (cube) "cube" else "rollup", !!!args),
       sets = sets)
}

compose <- function(parts, product) {
  expr <- rlang::call2(if (product) "grouping_spec" else "grouping_sets",
                       !!!lapply(parts, "[[", "expr"))
  families <- lapply(parts, "[[", "sets")
  if (product) {
    # The last index varies first. No package plan or algebra is consulted.
    indices <- expand.grid(rev(lapply(families, seq_along)))
    sets <- lapply(seq_len(nrow(indices)), function(row) {
      chosen <- Map("[[", families, rev(as.list(indices[row, ])))
      unique(as.character(unlist(chosen)))
    })
  } else {
    sets <- do.call(c, families)
  }
  list(expr = expr, sets = sets)
}

reference <- function(sets, fixed, policy, format) {
  dimensions <- unique(as.character(unlist(sets)))
  sets <- lapply(sets, function(keys) intersect(dimensions, keys))
  repeated <- duplicated(sets)
  if (policy == "error" && any(repeated)) return(NULL)
  if (policy == "drop") sets <- sets[!repeated]
  bits <- lapply(sets, function(keys) {
    bit <- as.integer(!dimensions %in% keys)
    if (length(bit)) stats::setNames(bit, dimensions) else bit
  })
  id <- vapply(bits, function(bit) {
    if (length(bit) > 31L) return(NA_integer_)
    Reduce(function(x, b) 2L * x + b, unname(bit), init = 0L)
  }, integer(1))
  fixed <- rep(list(fixed), length(sets))
  omitted <- lapply(sets, function(keys) setdiff(dimensions, keys))
  if (format == "text") {
    show_columns <- function(cols) sprintf("(%s)", toString(cols))
    fixed <- vapply(fixed, show_columns, character(1))
    sets <- vapply(sets, show_columns, character(1))
    omitted <- vapply(omitted, show_columns, character(1))
    bits <- vapply(bits, function(bit) {
      if (length(bit)) toString(sprintf("%s=%d", names(bit), bit)) else "()"
    }, character(1))
  }
  tibble::tibble(set_id = seq_along(sets), fixed = fixed, included = sets,
                 omitted = omitted, grouping_bits = bits, grouping_id = id)
}

main <- function() {
  set.seed(20261107L)
  options(marginplyr.audit_sql = TRUE)
  duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)
  sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(sqlite), add = TRUE)
  dimensions <- c("region, west", "store=code", "品目", "day two")
  data <- tibble::as_tibble(stats::setNames(
    list(c("W", "W"), c("X", "Y"), c("p", NA), 1:2, c(TRUE, FALSE)),
    c("fixed key", dimensions)
  ))
  source_calls <- 0L
  source_predicate <- function(x) {
    source_calls <<- source_calls + length(x)
    rep(TRUE, length(x))
  }
  source_table <- data.table::as.data.table(data)
  source_bytes <- serialize(source_table, NULL)
  lazy <- dtplyr::lazy_dt(source_table, immutable = FALSE)
  backends <- list(
    tibble = data, empty = data[0, ], dtplyr = filter(lazy, source_predicate(`day two`)),
    arrow = arrow::arrow_table(data),
    duckdb = copy_to(duck, data, "inspection_matrix"),
    sqlite = copy_to(sqlite, data, "inspection_matrix")
  )
  random_set <- function(empty = TRUE) {
    sample(dimensions, sample(seq.int(as.integer(!empty), 3L), 1L))
  }
  family <- function() switch(sample.int(5L, 1L),
    element(random_set()),
    unit_family(replicate(3L, random_set(FALSE), simplify = FALSE), FALSE),
    unit_family(replicate(3L, random_set(FALSE), simplify = FALSE), TRUE),
    compose(list(element(random_set()), element(random_set())), FALSE),
    compose(list(element(random_set()),
                 unit_family(list(random_set(FALSE), random_set(FALSE)), TRUE)), TRUE)
  )
  cases <- lapply(seq_len(96L), function(i) {
    if (i %% 3L == 0L) family() else compose(list(family(), family()), i %% 3L == 1L)
  })
  results <- matrix(0L, nrow = length(backends), ncol = 2L,
                    dimnames = list(names(backends), c("exact", "duplicate")))
  for (i in seq_along(cases)) {
    case <- cases[[i]]
    fixed <- if (i %% 2L) "fixed key" else character()
    spec <- eval(case$expr)
    for (policy in c("error", "drop", "keep")) {
      for (format in c("list", "text")) {
        want <- reference(case$sets, fixed, policy, format)
        for (name in names(backends)) {
          actual <- tryCatch(inspect_grouping(
            backends[[name]], .by = all_of(fixed), .grouping = spec,
            .duplicates = policy, .format = format
          ), error = identity)
          if (is.null(want)) {
            stopifnot(inherits(actual, "marginplyr_error"),
                      grepl("Duplicate grouping sets", conditionMessage(actual),
                            fixed = TRUE))
            results[name, "duplicate"] <- results[name, "duplicate"] + 1L
          } else {
            if (!identical(want, actual)) {
              print(list(case = i, backend = name, policy = policy,
                         format = format, spec = case$expr, want = want, got = actual))
              stop("Independent plan comparison failed.")
            }
            results[name, "exact"] <- results[name, "exact"] + 1L
          }
          stopifnot(nrow(last_sent_queries()) == 0L)
        }
      }
    }
  }
  print(results)
  stopifnot(source_calls == 0L, identical(serialize(source_table, NULL), source_bytes))

  # Compare width boundaries without making a wide cube.
  width_checks <- 0L
  for (n in c(0L, 1L, 2L, 30L, 31L, 32L, 64L, 96L)) {
    columns <- if (n) paste0("d ", seq_len(n)) else character()
    fixed <- "fixed"
    wide <- tibble::as_tibble(stats::setNames(as.list(rep(1L, n + 1L)),
                                            c(fixed, columns)))
    sets <- lapply(rev(seq.int(0L, n)), function(k) head(columns, k))
    spec <- if (n) rollup(all_of(columns)) else grouping_set()
    for (format in c("list", "text")) {
      expected <- reference(sets, fixed, "error", format)
      actual <- inspect_grouping(wide, .by = all_of(fixed), .grouping = spec,
                                 .format = format)
      stopifnot(identical(expected, actual))
      width_checks <- width_checks + 1L
    }
  }
  # A reader already advanced by its caller must retain the remaining batches.
  first <- arrow::record_batch(data[1L, ])
  second <- arrow::record_batch(data[2L, ])
  reader <- arrow::RecordBatchReader$create(first, second)
  stopifnot(identical(as.list(as.data.frame(reader$read_next_batch())), as.list(data[1L, ])))
  expected <- inspect_grouping(data[2L, ], .grouping = grouping_set(where(is.character)),
                               .format = "list")
  for (i in seq_len(3L)) {
    actual <- inspect_grouping(reader, .grouping = grouping_set(where(is.character)),
                               .format = "list")
    stopifnot(identical(expected, actual))
  }
  stopifnot(identical(as.list(as.data.frame(reader$read_next_batch())), as.list(data[2L, ])),
            is.null(reader$read_next_batch()))
  typed <- grouping_set(where(is.integer))
  for (name in c("tibble", "empty", "dtplyr", "arrow", "duckdb")) {
    x <- backends[[name]]
    actual <- inspect_grouping(x, .grouping = typed, .format = "list")
    stopifnot(identical(actual, reference(list("品目"), character(),
                                         "error", "list")))
    expected_queries <- if (name == "duckdb") "selection_proxy" else character()
    stopifnot(identical(last_sent_queries()$purpose, expected_queries))
  }
  refused <- tryCatch(inspect_grouping(backends$sqlite, .grouping = typed), error = identity)
  stopifnot(inherits(refused, "marginplyr_error"), nrow(last_sent_queries()) == 0L,
            source_calls == 0L, identical(serialize(source_table, NULL), source_bytes))
  cat("Width checks:", width_checks, "\n")
  cat("Typed controls:", 6L, "\n")
  cat("Repeated advanced-reader inspections:", 3L, "\n")
  cat("dtplyr source predicate elements:", source_calls, "\n")
  print(sessionInfo())
}
main()
