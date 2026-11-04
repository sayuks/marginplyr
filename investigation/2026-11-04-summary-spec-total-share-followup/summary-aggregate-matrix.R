pkgload::load_all(quiet = TRUE)

# Compare public summary results with an independent dplyr branch and order oracle.
# Source keys include source-missing rows that look like an omitted key.
main <- function() {
  sq <- DBI::dbConnect(RSQLite::SQLite())
  on.exit(DBI::dbDisconnect(sq), add = TRUE)
  duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)
  df <- data.frame(
    p = c("p", "p", "p", "p", NA, NA, "q", "q"),
    g = c("a", "b", "a", NA, "a", "b", NA, "b"),
    h = c(10L, 2L, NA, 10L, 2L, NA, 2L, 10L)
  )
  inputs <- list(
    local = identity,
    dtplyr = dtplyr::lazy_dt,
    Arrow = arrow::Table$create,
    SQLite = function(data) dplyr::copy_to(sq, data, "probe", overwrite = TRUE),
    DuckDB_native = function(data) dplyr::copy_to(duck, data, "probe", overwrite = TRUE),
    DuckDB_portable = function(data) dplyr::copy_to(duck, data, "probe", overwrite = TRUE)
  )
  oracle <- function(data, by, sort, label) {
    keys <- list(c("g", "h"), "g", character())
    parts <- lapply(seq_along(keys), function(id) {
      key <- keys[[id]]
      part <- dplyr::summarize(
        data, n = dplyr::n(), v = 19L,
        .by = dplyr::all_of(c(by, key))
      )
      for (col in c("g", "h")) {
        if (col %in% key && !is.null(label)) part[[col]] <- as.character(part[[col]])
        if (!col %in% key) {
          value <- if (is.null(label)) data[[col]][NA_integer_] else label
          part[[col]] <- rep(value, nrow(part))
        }
      }
      part$sid <- rep(id, nrow(part))
      part$bits <- rep(c(0L, 1L, 3L)[[id]], nrow(part))
      part
    })
    out <- dplyr::bind_rows(parts)[c(by, "g", "h", "sid", "n", "v", "bits")]
    missing_pair <- function(x) list(is.na(x), x)
    sign <- if (sort == "first") -1L else 1L
    ordering <- c(
      if (length(by)) missing_pair(out$p) else list(),
      list(sign * (out$sid == 3L)), missing_pair(out$g),
      list(sign * (out$sid != 1L)), missing_pair(out$h),
      list(out$sid)
    )
    ord <- do.call(order, ordering)
    out[ord, , drop = FALSE]
  }
  normalize <- function(data) {
    data <- as.data.frame(data)
    rownames(data) <- NULL
    for (name in intersect(c("sid", "n", "v", "bits"), names(data))) {
      if (!is.numeric(data[[name]]) && !inherits(data[[name]], "integer64")) {
        cat("non-numeric:", name, typeof(data[[name]]), "\n")
        stopifnot(nrow(data) == 0L, name %in% c("n", "v"))
      }
      data[[name]] <- as.integer(data[[name]])
    }
    data
  }
  counts <- list()
  for (backend in names(inputs)) {
    for (size in c(0L, nrow(df))) {
      data <- df[seq_len(size), , drop = FALSE]
      input <- inputs[[backend]](data)
      for (by in list(character(), "p")) {
        for (label in list(NULL, "Total")) {
          for (sort in c("first", "last")) {
            expected <- normalize(oracle(data, by, sort, label))
            received <- tryCatch(dplyr::collect(summarize_with_margins(
              input, n = dplyr::n(), v = 19L, bits = grouping_id(g, h),
              .by = dplyr::all_of(by), .grouping = rollup(g, h),
              .margin_label = label, .id = "sid", .sort = sort,
              .duplicates = if (backend == "DuckDB_portable") "keep" else "error"
            )), error = identity)
            if (inherits(received, "error")) {
              actual <- paste(class(received)[[1L]], conditionMessage(received))
            } else {
              actual <- tryCatch(normalize(received), error = identity)
            }
            same <- identical(actual, expected)
            tag <- data.frame(backend = backend, size = size,
                              fixed = length(by) > 0L, label = !is.null(label),
                              sort = sort, matches = same)
            counts[[length(counts) + 1L]] <- tag
            if (!same) {
              print(tag)
              print(actual)
              print(expected)
            }
          }
        }
      }
    }
  }
  rows <- do.call(rbind, counts)
  print(with(rows, table(backend, matches)))
  stopifnot(all(rows$matches))
}
main()
