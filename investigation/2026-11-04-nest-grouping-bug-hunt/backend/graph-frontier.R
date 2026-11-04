# Dated finite oracle: three-input graphs, nested aliases and effectful predicates.
# Point MARGINPLYR_HUNT_SOURCE to an archive of the investigated revision.
Sys.setenv(TESTTHAT_PARALLEL = "false")
# Native oracle reads must not add data.table secondary indexes to the roots.
options(datatable.auto.index = FALSE)
source_path <- Sys.getenv("MARGINPLYR_HUNT_SOURCE", ".")
pkgload::load_all(source_path, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))

observe <- function(expr) {
  warnings <- character()
  value <- tryCatch(withCallingHandlers(force(expr), warning = function(cnd) {
    warnings <<- c(warnings, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  }), error = identity)
  list(value = value, warnings = warnings)
}
normalize <- function(x, sorted) {
  result <- tibble::as_tibble(x)
  result$data <- lapply(result$data, tibble::as_tibble)
  if (!sorted && nrow(result)) {
    key <- vapply(seq_len(nrow(result)), function(i) {
      paste(result$set[[i]], paste(result$data[[i]]$row_id, collapse = ","))
    }, character(1))
    result <- result[order(key), ]
  }
  result
}
counts <- c(graphs = 0L, native_errors = 0L, plans_matched = 0L,
            metadata_refusals = 0L, name_plans_matched = 0L,
            nests_matched = 0L, nesting_refusals = 0L,
            unexpected_errors = 0L, mismatches = 0L,
            construction_callbacks = 0L, source_mutations = 0L,
            warnings = 0L)
records <- list()
for (size in c(0L, 1L, 4L)) {
  source <- data.table::data.table(
    k = rep(c(1L, 1L, 2L, NA_integer_), length.out = size),
    g = rep(c("a", NA_character_, "b", "a"), length.out = size),
    row_id = seq_len(size), v = as.double(seq_len(size))
  )
  source_b <- data.table::data.table(k = c(1L, 2L, NA_integer_),
                                    h = c("x", NA, "y"), w = 11:13)
  source_c <- data.table::data.table(k = c(1L, 3L), z = c(1.5, 2.5))
  sources <- list(source, source_b, source_c)
  snapshots <- lapply(sources, serialize, connection = NULL)
  callbacks <- 0L
  counted <- function(x) { callbacks <<- callbacks + 1L; rep(TRUE, length(x)) }
  a <- dtplyr::lazy_dt(source)
  b <- dtplyr::lazy_dt(source_b)
  cc <- dtplyr::lazy_dt(source_c)
  filtered <- a |> filter(counted(v))
  graphs <- list(
    root = a,
    filter = filtered,
    joined = left_join(filtered, b, by = "k", relationship = "many-to-many"),
    double_join = left_join(left_join(filtered, b, by = "k", relationship = "many-to-many"), cc, by = "k"),
    anti_join = left_join(anti_join(filtered, b |> filter(k == 2L), by = "k"), cc, by = "k"),
    join_set = union_all(inner_join(filtered, b, by = "k", relationship = "many-to-many"), inner_join(a, b |> filter(k == 1L), by = "k", relationship = "many-to-many")),
    set_join = left_join(union_all(filtered, a |> filter(k == 2L)), cc, by = "k"),
    cached_join = left_join(compute(left_join(filtered, b, by = "k", relationship = "many-to-many")), cc, by = "k"),
    renamed_cache = compute(filtered) |> rename(v2 = v) |> compute() |> rename(v = v2),
    ordered_cache = compute(filtered) |> arrange(desc(v)) |> compute() |> arrange(v),
    removed_cache = compute(filtered) |> select(-v) |> compute(),
    cache_set_cache = union_all(compute(filtered), compute(a |> filter(k == 2L))) |> compute(),
    arithmetic = a |> mutate(z = (v / 2 + row_id) * -1) |> rename(value = z),
    duplicated_copy = a |> mutate(copy = v, copy = copy / 2, g2 = g),
    empty_union = union_all(filtered |> filter(FALSE), a |> filter(k == 2L)),
    intersect = intersect(filtered, a |> filter(k == 1L)),
    semi = semi_join(filtered, b, by = "k")
  )
  for (graph in names(graphs)) {
    q <- graphs[[graph]]
    counts[["graphs"]] <- counts[["graphs"]] + 1L
    for (typed in c(FALSE, TRUE)) {
      callbacks <- 0L
      actual <- observe(if (typed) inspect_grouping(q, .by = where(is.character), .format = "list") else inspect_grouping(q, .grouping = rollup(g), .format = "list"))
      counts[["construction_callbacks"]] <- counts[["construction_callbacks"]] + callbacks
      counts[["warnings"]] <- counts[["warnings"]] + length(actual$warnings)
      native <- observe(collect(q))
      if (inherits(native$value, "error")) { counts[["native_errors"]] <- counts[["native_errors"]] + 1L; next }
      expected <- if (typed) inspect_grouping(native$value, .by = where(is.character), .format = "list") else inspect_grouping(native$value, .grouping = rollup(g), .format = "list")
      if (inherits(actual$value, "marginplyr_error")) {
        counts[["metadata_refusals"]] <- counts[["metadata_refusals"]] + 1L
      } else if (inherits(actual$value, "error")) {
        counts[["unexpected_errors"]] <- counts[["unexpected_errors"]] + 1L
        cat("INSPECTION ERROR", size, graph, typed, conditionMessage(actual$value), "\n")
      } else {
        counts[[if (typed) "plans_matched" else "name_plans_matched"]] <- counts[[if (typed) "plans_matched" else "name_plans_matched"]] + as.integer(identical(actual$value, expected))
        if (!identical(actual$value, expected)) counts[["mismatches"]] <- counts[["mismatches"]] + 1L
      }
    }
    native <- collect(q)
    for (keep in c(FALSE, TRUE)) for (sort in c("none", "last", "first")) for (verb in c("nest_with_margins", "nest_by_with_margins")) {
      fun <- get(verb, asNamespace("marginplyr"))
      callbacks <- 0L
      actual <- observe(fun(q, .grouping = rollup(g), .margin_label = NA_character_, .keep = keep, .id = "set", .sort = sort))
      if (verb == "nest_with_margins") counts[["construction_callbacks"]] <- counts[["construction_callbacks"]] + callbacks
      counts[["warnings"]] <- counts[["warnings"]] + length(actual$warnings)
      if (inherits(actual$value, "marginplyr_error")) {
        counts[["nesting_refusals"]] <- counts[["nesting_refusals"]] + 1L
        next
      }
      if (!inherits(actual$value, "error")) actual <- observe(collect(actual$value))
      if (inherits(actual$value, "error")) {
        counts[["unexpected_errors"]] <- counts[["unexpected_errors"]] + 1L
        cat("NEST ERROR", size, graph, keep, sort, verb, conditionMessage(actual$value), "\n")
        next
      }
      expected <- fun(native, .grouping = rollup(g), .margin_label = NA_character_, .keep = keep, .id = "set", .sort = sort)
      match <- isTRUE(all.equal(normalize(actual$value, sort != "none"), normalize(expected, sort != "none")))
      counts[[if (match) "nests_matched" else "mismatches"]] <- counts[[if (match) "nests_matched" else "mismatches"]] + 1L
      if (!match) cat("MISMATCH", size, graph, keep, sort, verb, "\n")
    }
    changed <- !identical(lapply(sources, serialize, connection = NULL), snapshots)
    counts[["source_mutations"]] <- counts[["source_mutations"]] + as.integer(changed)
  }
}
print(counts)
print(R.version.string)
print(vapply(c("dplyr", "dtplyr", "data.table", "vctrs", "tibble", "tidyr"), function(p) as.character(packageVersion(p)), character(1)))
stopifnot(counts[["unexpected_errors"]] == 0L, counts[["mismatches"]] == 0L,
          counts[["construction_callbacks"]] == 0L, counts[["source_mutations"]] == 0L,
          counts[["warnings"]] == 0L)
