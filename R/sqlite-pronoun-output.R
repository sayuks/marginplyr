# Quote generated SQLite references to a public `.env` output (ADR 0031).
# The caller passes an unrendered lazy result with that output.
sqlite_env_output_query <- function(result) {
  result$lazy_query <- sqlite_env_select_query(result$lazy_query)
  result
}

# Quote generated references in a lazy query and its nested inputs.
# The caller passes a dbplyr query from a SQLite Margin summary.
sqlite_env_select_query <- function(query) {
  if (inherits(query, "lazy_select_query")) {
    if (!identical(query$select_operation, "summarise")) {
      query$select$expr <- lapply(query$select$expr, sqlite_env_reference)
      if (length(query$order_by) > 0L) {
        query$order_by <- lapply(query$order_by, sqlite_env_reference)
      }
    }
  }
  for (field in c("x", "y")) {
    if (inherits(query[[field]], "lazy_query")) {
      query[[field]] <- sqlite_env_select_query(query[[field]])
    }
  }
  if (inherits(query, "lazy_union_query")) {
    query$unions$table <- lapply(
      query$unions$table, sqlite_env_output_query
    )
  }
  query
}

# Quote bare `.env` in a generated select or order expression.
# The caller has excluded summary nodes; pronoun access stays lexical.
sqlite_env_reference <- function(expr) {
  if (identical(expr, as.name(".env"))) {
    return(dbplyr::ident(".env"))
  }
  if (!is.call(expr) || length(expr) < 2L ||
        (rlang::is_call(expr, c("$", "[[")) &&
           identical(expr[[2L]], as.name(".env")))) {
    return(expr)
  }
  for (i in seq.int(2L, length(expr))) {
    expr[[i]] <- sqlite_env_reference(expr[[i]])
  }
  expr
}
