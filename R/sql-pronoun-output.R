# Names whose generated bare SQL references have an R evaluation meaning.
# Public names are already validated against the backend's own restrictions.
sql_name_needs_quoting <- function(names) {
  names == ".env" | grepl("^\\.\\.-?[0-9]+$", names)
}

# Quote SQL-generated references to names R reads as pronouns or dot arguments.
# The caller passes an unrendered lazy result; summary expressions stay lexical.
sql_quote_generated_columns <- function(result) {
  result$lazy_query <- sql_quote_generated_query(result$lazy_query)
  result
}

# Quote generated references in a lazy query and its nested inputs.
# The caller passes a dbplyr query from a SQL Margin result.
sql_quote_generated_query <- function(query) {
  if (inherits(query, "lazy_select_query")) {
    if (!identical(query$select_operation, "summarise")) {
      query$select$expr <- lapply(
        query$select$expr, sql_quote_generated_reference
      )
      if (length(query$order_by) > 0L) {
        query$order_by <- lapply(query$order_by, sql_quote_generated_reference)
      }
    }
  }
  for (field in c("x", "y")) {
    if (inherits(query[[field]], "lazy_query")) {
      query[[field]] <- sql_quote_generated_query(query[[field]])
    }
  }
  if (inherits(query, "lazy_union_query")) {
    query$unions$table <- lapply(
      query$unions$table, sql_quote_generated_columns
    )
  }
  if (inherits(query, "lazy_multi_join_query")) {
    query$joins$table <- lapply(
      query$joins$table, sql_quote_generated_query
    )
  }
  query
}

# Quote bare pronoun and dot-argument names in generated SQL expressions.
# The caller has excluded summary nodes; pronoun access stays lexical.
sql_quote_generated_reference <- function(expr) {
  if (rlang::is_symbol(expr) &&
        sql_name_needs_quoting(rlang::as_string(expr))) {
    return(dbplyr::ident(rlang::as_string(expr)))
  }
  if (!is.call(expr) || length(expr) < 2L ||
        (rlang::is_call(expr, c("$", "[[")) &&
           identical(expr[[2L]], as.name(".env")))) {
    return(expr)
  }
  for (i in seq.int(2L, length(expr))) {
    expr[[i]] <- sql_quote_generated_reference(expr[[i]])
  }
  expr
}
