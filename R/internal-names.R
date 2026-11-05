# Fold only the ASCII letters that SQLite and DuckDB equate in identifiers.
sql_ascii_lower <- function(names) {
  chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", names)
}

# SQLite and DuckDB resolve ASCII identifier case equivalently. Keep the
# caller's spelling everywhere else, including in diagnostics and output.
margin_identifier_key <- function(names, backend = NULL) {
  if (is.null(backend) || !inherits(backend, "marginplyr_backend") ||
        !(identical(backend$kind, "duckdb") ||
            inherits(backend$dialect, "sql_dialect_sqlite"))) {
    return(names)
  }
  sql_ascii_lower(names)
}

# Compare coexisting public result names under ADR 0032's SQL identifier rule.
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

# Allocate private names distinct from the coexisting columns. PostgreSQL
# candidates retain their distinguishing suffix within its 63-byte limit.
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
    attempt <- i
    suffix <- i
    candidate <- margin_internal_name_candidate(prefix, suffix, backend)
    while (margin_identifier_key(candidate, backend) %in%
             margin_identifier_key(c(used_names, result), backend)) {
      suffix <- if (inherits(backend$dialect, "sql_dialect_postgres") &&
                      nchar(enc2utf8(candidate), type = "bytes") >= 63L) {
        attempt <- attempt + 1L
        attempt
      } else {
        paste0(suffix, "_")
      }
      candidate <- margin_internal_name_candidate(prefix, suffix, backend)
    }
    result[[i]] <- candidate
  }
  result
}

# Keep the longest complete UTF-8 prefix that leaves room for the suffix on
# PostgreSQL; other backends retain the supplied prefix unchanged.
margin_internal_name_candidate <- function(prefix, suffix, backend) {
  if (inherits(backend$dialect, "sql_dialect_postgres")) {
    prefix <- enc2utf8(prefix)
    budget <- 63L - nchar(as.character(suffix), type = "bytes")
    while (nchar(prefix, type = "bytes") > budget) {
      prefix <- substr(prefix, 1L, nchar(prefix) - 1L)
    }
  }
  paste0(prefix, suffix)
}
