# Boolean-word aliases are not retained in SQLite subqueries or CREATE TABLE AS.
# The caller supplies public names from a prepared live SQLite result.
sqlite_boolean_columns <- function(names) {
  names[tolower(names) %in% c("true", "false")]
}

# Keep Boolean-word outputs private below the direct result boundary (#826).
# The source query remains untouched; aliases are allocated against all known
# source and result names. `public` leaves only the outermost aliases public.
sqlite_boolean_query <- function(result, source, aliases, public = TRUE) {
  rewrite <- function(query, public) {
    unchanged <- list(query = query, aliases = character())
    if (identical(query, source) ||
          !inherits(query, c("lazy_select_query", "lazy_union_query",
                             "lazy_multi_join_query"))) {
      return(unchanged)
    }
    input <- rewrite(query$x, public && inherits(query, "lazy_union_query"))
    query$x <- input$query
    if (inherits(query, "lazy_union_query")) {
      query$unions$table <- lapply(query$unions$table, function(table) {
        table$lazy_query <- rewrite(table$lazy_query, public)$query
        table
      })
      return(sqlite_boolean_query_state(query, input$aliases))
    }
    if (inherits(query, "lazy_multi_join_query")) {
      inputs <- c(
        list(input), lapply(query$joins$table, rewrite, public = FALSE)
      )
      query$joins$table <- lapply(inputs[-1L], `[[`, "query")
      for (i in seq_along(inputs)) {
        positions <- which(query$vars$table == i)
        query$vars$var[positions] <- sqlite_boolean_names(
          query$vars$var[positions], inputs[[i]]$aliases
        )
      }
      output_aliases <- aliases[intersect(names(aliases), query$vars$name)]
      if (!public) {
        query$vars$name <- sqlite_boolean_names(query$vars$name, output_aliases)
      }
      # Share joins match private keys, so ON needs no public rename.
      return(sqlite_boolean_query_state(
        query, if (public) character() else output_aliases
      ))
    }
    query$select$expr <- lapply(query$select$expr, sqlite_boolean_reference,
                                aliases = input$aliases)
    for (field in c("where", "group_by", "order_by")) {
      query[[field]] <- lapply(query[[field]], sqlite_boolean_reference,
                               aliases = input$aliases)
    }
    query$select$group_vars <- lapply(
      query$select$group_vars, sqlite_boolean_names, aliases = input$aliases
    )
    query$select$order_vars <- lapply(query$select$order_vars, function(terms) {
      lapply(terms, sqlite_boolean_reference, aliases = input$aliases)
    })
    output_aliases <- aliases[intersect(names(aliases), query$select$name)]
    if (!public) {
      positions <- match(names(output_aliases), query$select$name)
      query$select$name[positions] <- unname(output_aliases)
    }
    sqlite_boolean_query_state(
      query, if (public) character() else output_aliases
    )
  }
  result$lazy_query <- rewrite(result$lazy_query, public)$query
  result
}

# Rewrite column references after their input acquired private aliases.
# The caller supplies dbplyr expressions after pronoun and lexical evaluation.
sqlite_boolean_reference <- function(expr, aliases) {
  if (is.symbol(expr) && as.character(expr) %in% names(aliases)) {
    return(rlang::sym(aliases[[as.character(expr)]]))
  }
  if (!is.call(expr) || length(expr) < 2L) {
    return(expr)
  }
  for (i in seq.int(2L, length(expr))) {
    expr[[i]] <- sqlite_boolean_reference(expr[[i]], aliases)
  }
  expr
}

# Rename known column names, leaving names absent from the alias map intact.
sqlite_boolean_names <- function(names, aliases) {
  positions <- match(names, names(aliases))
  names[!is.na(positions)] <- unname(aliases[positions[!is.na(positions)]])
  names
}

# Keep renamed query outputs aligned with dbplyr's grouping and window metadata.
# The caller supplies the alias map that the parent query must use.
sqlite_boolean_query_state <- function(query, aliases) {
  query$group_vars <- sqlite_boolean_names(query$group_vars, aliases)
  query$order_vars <- lapply(query$order_vars, sqlite_boolean_reference,
                             aliases = aliases)
  list(query = query, aliases = aliases)
}
