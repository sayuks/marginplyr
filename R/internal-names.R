# SQLite and DuckDB resolve ASCII identifier case equivalently. Keep the
# caller's spelling everywhere else, including in diagnostics and output.
margin_identifier_key <- function(names, backend = NULL) {
  if (is.null(backend) || !inherits(backend, "marginplyr_backend") ||
        !(identical(backend$kind, "duckdb") ||
            inherits(backend$dialect, "sql_dialect_sqlite"))) {
    return(names)
  }
  chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", names)
}

# Only names that coexist in the public result belong here. A summary may
# replace an unrelated input payload; checking every source name would refuse
# ordinary summary replacement.
check_margin_sql_public_names <- function(names, backend) {
  keys <- margin_identifier_key(names, backend)
  for (i in seq_along(keys)) {
    earlier <- match(keys[[i]], keys[seq_len(i - 1L)])
    if (!is.na(earlier) && !identical(names[[earlier]], names[[i]])) {
      # Read only from the cli template below, which codetools cannot see.
      first <- names[[earlier]] # nolint: object_usage_linter.
      # Read only from the cli template below, which codetools cannot see.
      second <- names[[i]] # nolint: object_usage_linter.
      abort_marginplyr(paste0(
        "SQL result columns {.var {first}} and {.var {second}} have ",
        "equivalent identifiers. Rename one of these columns."
      ))
    }
  }
  invisible(NULL)
}

new_margin_internal_names <- function(count, used_names, prefix,
                                      backend = NULL) {
  stopifnot(
    length(count) == 1L,
    is.numeric(count),
    !is.na(count),
    count >= 0L,
    is.character(used_names),
    length(prefix) == 1L,
    is.character(prefix),
    !is.na(prefix)
  )

  result <- character(count)
  for (i in seq_len(count)) {
    candidate <- paste0(prefix, i)
    while (margin_identifier_key(candidate, backend) %in%
             margin_identifier_key(c(used_names, result), backend)) {
      candidate <- paste0(candidate, "_")
    }
    result[[i]] <- candidate
  }
  result
}
