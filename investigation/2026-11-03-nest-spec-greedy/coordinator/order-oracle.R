# Independent finite oracle for direct nesting order and source memberships.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
set.seed(20261103)
sets <- list("b", c("b", "a"), character(), "a")
spec <- grouping_sets(grouping_set(b), grouping_set(b, a), grouping_set(), grouping_set(a))
types <- list(
  character = function(x) c("z", "a", NA_character_)[x],
  numeric = function(x) c(10, 2, NA_real_)[x],
  factor = function(x) factor(c("z", "a", NA_character_)[x], levels = c("z", "a")),
  date = function(x) as.Date(c("2025-01-03", "2020-01-01", NA_character_))[x],
  list = function(x) list(2L, NA_integer_, NULL)[x],
  matrix = function(x) cbind(c(2L, 1L, NA_integer_)[x], c(NA_integer_, 2L, NA_integer_)[x]),
  packed = function(x) tibble::tibble(x = c(2L, 1L, NA_integer_)[x], y = c(NA_integer_, 2L, NA_integer_)[x])
)
records <- list()
for (iteration in seq_len(45L)) {
  n <- sample(1:9, 1)
  ta <- names(types)[(iteration - 1L) %% length(types) + 1L]
  tb <- names(types)[(iteration + 1L) %% length(types) + 1L]
  x <- tibble::tibble(p = sample(c(2L, 1L, NA_integer_), n, TRUE),
                      a = types[[ta]](sample(1:3, n, TRUE)),
                      b = types[[tb]](sample(1:3, n, TRUE)), row_id = seq_len(n))
  for (fixed in c(FALSE, TRUE)) for (verb in c("nest_with_margins", "nest_by_with_margins")) {
    for (label in c("missing", "display")) {
      # Structured dimensions only admit typed missing labels.
      if (label == "display" && any(c(ta, tb) %in% c("list", "matrix", "packed"))) next
      lab <- if (label == "missing") NULL else "Total"
      parts <- lapply(seq_along(sets), function(s) {
        selected <- c(if (fixed) "p", sets[[s]])
        locs <- if (!length(selected)) list(seq_len(n)) else vctrs::vec_group_loc(x[selected])$loc
        dplyr::bind_rows(lapply(locs, function(loc) {
          row <- x[loc[1L], c(if (fixed) "p", "b", "a"), drop = FALSE]
          for (d in c("b", "a")) {
            if (!d %in% sets[[s]]) row[[d]] <- if (is.null(lab)) vctrs::vec_init(x[[d]], 1L) else if (is.factor(x[[d]])) factor(lab, levels = c(levels(x[[d]]), lab)) else lab
            else if (!is.null(lab) && !is.factor(x[[d]])) row[[d]] <- as.character(row[[d]])
            if (!is.null(lab) && is.factor(row[[d]])) row[[d]] <- factor(as.character(row[[d]]), levels = c(levels(x[[d]]), lab))
          }
          row$sid <- s
          row$members <- list(loc)
          row
        }))
      })
      expected <- dplyr::bind_rows(parts)
      for (sorting in c("first", "last")) for (source_kind in c("tibble", "plain", "grouped")) {
        if (source_kind == "grouped" && !fixed) next
        key <- list()
        if (fixed) {
          key$p_missing <- vctrs::vec_detect_missing(expected$p)
          key$p <- expected$p
        }
        for (d in c("b", "a")) {
          bits <- vapply(expected$sid, function(s) as.integer(!d %in% sets[[s]]), integer(1))
          key[[paste0(d, "_bit")]] <- if (sorting == "first") -bits else bits
          value <- expected[[d]]
          key[[paste0(d, "_missing")]] <- if (is.matrix(value)) apply(is.na(value), 1L, all) else if (is.data.frame(value)) apply(is.na(value), 1L, all) else vctrs::vec_detect_missing(value)
          key[[d]] <- value
        }
        key$sid <- expected$sid
        want <- expected[vctrs::vec_order(tibble::as_tibble(key)), ]
        source <- switch(source_kind, tibble = x, plain = as.data.frame(x), grouped = dplyr::group_by(x, p))
        before <- serialize(source, NULL)
        args <- list(.data = source, .grouping = spec, .margin_label = lab, .sort = sorting, .id = "sid", .keep = TRUE)
        args$.by <- if (fixed && source_kind != "grouped") "p" else NULL
        actual <- rlang::exec(get(verb), !!!args)
        members <- lapply(actual$data, function(cell) cell$row_id)
        outer_names <- c(if (fixed) "p", "b", "a", "sid")
        key_ok <- all(vapply(outer_names, function(col) identical(actual[[col]], want[[col]]), logical(1)))
        cells_ok <- all(vapply(seq_along(members), function(i) {
          cell <- actual$data[[i]]
          all(vapply(names(x), function(col) identical(cell[[col]], vctrs::vec_slice(x[[col]], members[[i]])), logical(1)))
        }, logical(1)))
        group_ok <- if (verb == "nest_with_margins") !dplyr::is_grouped_df(actual) else inherits(actual, "rowwise_df") && identical(dplyr::group_vars(actual), outer_names)
        ok <- identical(members, want$members) && key_ok && cells_ok && group_ok && identical(serialize(source, NULL), before)
        records[[length(records) + 1L]] <- data.frame(iteration, ta, tb, fixed, source_kind, verb, label, sorting, ok, rows = nrow(actual))
        if (!ok) {
          print(tail(records, 1)); print(actual); print(want)
          stop("Independent order/membership oracle mismatch")
        }
      }
    }
  }
}
results <- dplyr::bind_rows(records)
write.csv(results, file.path(Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempdir()), "order-oracle-results.csv"), row.names = FALSE)
cat("PASS", nrow(results), "direct result order/membership comparisons\n")
print(sessionInfo())
