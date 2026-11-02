# Boolean-word aliases are not retained in SQLite subqueries or CREATE TABLE AS.
# The caller supplies public names from a prepared live SQLite result.
sqlite_boolean_columns <- function(names) {
  names[tolower(names) %in% c("true", "false")]
}

# Expose public names through one inlinable projection over private outputs.
# Binary dbplyr verbs can consume either input without the result's S3 class.
# The caller holds a completed live SQLite Margin query with Boolean names.
sqlite_boolean_public_query <- function(result) {
  aliases <- sqlite_boolean_query_aliases(result)
  public <- intersect(names(aliases), dbplyr::op_vars(result$lazy_query))
  result <- sqlite_boolean_query(result, NULL, aliases, public = FALSE)
  # Direct execution uses the companion query; this graph delegates ordering.
  if (inherits(result$lazy_query, "lazy_select_query")) {
    result$lazy_query$order_by <- NULL
  }
  result$lazy_query <- dbplyr::lazy_select_query(result$lazy_query)
  result <- dplyr::rename(result, dplyr::all_of(aliases[public]))
  class(result$lazy_query) <- c("lazy_margin_public_query",
                                class(result$lazy_query))
  result
}

# Keep the public projection symbolic until joins can inline it. Once built,
# keep it opaque so CTE flattening cannot lift its public Boolean aliases.
# The caller supplies a public projection over a privately named SQLite query.
#' @exportS3Method dbplyr::sql_build
#' @noRd
sql_build.lazy_margin_public_query <- function(op, con, ...,
                                               sql_options = NULL) {
  op <- sql_quote_generated_query(op)
  query <- NextMethod()
  sql <- dbplyr::sql_render(query, con = con, sql_options = sql_options)
  relation <- dplyr::tbl(con, sql, vars = dbplyr::op_vars(op))
  dbplyr::sql_build(relation, con = con, sql_options = sql_options)
}

# Render a root public projection without lifting its aliases into a CTE.
# Its built query is already opaque SQL, including any derived predicates.
#' @exportS3Method dbplyr::sql_render
#' @noRd
sql_render.lazy_margin_public_query <- function(query, con = NULL, ...,
                                                sql_options = NULL,
                                                subquery = FALSE, lvl = 0) {
  built <- dbplyr::sql_build(query, con = con, sql_options = sql_options)
  dbplyr::sql_render(built, con = con, ..., sql_options = sql_options,
                     subquery = subquery, lvl = lvl)
}

# Repair aliases at final rendering after ordinary verbs add query layers.
# The caller holds a derived live SQLite result; types and order stay delegated.
sqlite_boolean_derived_query <- function(result) {
  aliases <- sqlite_boolean_query_aliases(result)
  if (length(aliases) == 0L) {
    return(result)
  }
  result <- sqlite_boolean_query(result, source = NULL, aliases = aliases)
  sql_quote_generated_columns(result)
}

# Allocate Boolean-word aliases against every output in the traversable graph.
# The caller supplies a live SQLite result before any output has been renamed.
sqlite_boolean_query_aliases <- function(result) {
  columns <- sqlite_boolean_query_columns(result$lazy_query)
  boolean <- sqlite_boolean_columns(columns)
  stats::setNames(new_margin_internal_names(
    length(boolean), columns, prefix = "..marginplyr_boolean_",
    backend = grouping_backend(result)
  ), boolean)
}

# All output names in the select/union graph the alias rewrite can traverse.
# Include hidden intermediate outputs when allocating fresh private aliases.
sqlite_boolean_query_columns <- function(query) {
  columns <- dbplyr::op_vars(query)
  if (inherits(query, c("lazy_select_query", "lazy_union_query"))) {
    columns <- c(columns, sqlite_boolean_query_columns(query$x))
  }
  if (inherits(query, "lazy_union_query")) {
    columns <- c(columns, unlist(lapply(query$unions$table, function(table) {
      sqlite_boolean_query_columns(table$lazy_query)
    }), use.names = FALSE))
  }
  unique(columns)
}

# Keep Boolean-word outputs private below the outermost result projection.
# The source query remains untouched; aliases are allocated against all known
# source and result names. `public` leaves only the outermost aliases public.
sqlite_boolean_query <- function(result, source, aliases, public = TRUE) {
  rewrite <- function(query, public) {
    unchanged <- list(query = query, aliases = character())
    if (identical(query, source) ||
          !inherits(query, c("lazy_select_query", "lazy_union_query"))) {
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
