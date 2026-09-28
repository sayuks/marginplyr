# Quote dbplyr's generated pass-through projections for a public `.env` column.
# The caller has a lazy SQLite query; caller summary expressions stay intact.
sqlite_env_output_query <- function(result) {
  result$lazy_query <- sqlite_env_select_query(result$lazy_query)
  result
}

# dbplyr represents a pass-through column as a bare symbol. Its SQL translator
# evaluates `.env` as the tidy-evaluation pronoun, so generated references
# need an identifier instead. Nested selects and union arms can each carry one.
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

# Keep lexical pronoun access intact while quoting bare column references.
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
