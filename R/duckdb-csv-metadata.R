# SQL lexical tokens for reader metadata guards, using ASCII identifier keys.
# Quoted values stay opaque and nested comments are discarded.
duckdb_metadata_tokens <- function(sql) {
  pattern <- paste0(
    "(?s)",
    "(?<comment>/\\*(?:[^*/]|/(?!\\*)|\\*(?!/)|(?&comment))*\\*/)",
    "|--[^\\r\\n]*",
    "|[eE]'(?:[^'\\\\]|\\\\.|'')*'",
    "|'(?:[^']|'')*'",
    "|(?<tag>\\$[A-Za-z_0-9]*\\$).*?\\k<tag>",
    '|"(?:[^"]|"")*"',
    "|[[:alpha:]_][[:alnum:]_$]*|:=|[^[:space:]]"
  )
  tokens <- regmatches(sql, gregexpr(pattern, sql, perl = TRUE))[[1L]]
  tokens <- tokens[!startsWith(tokens, "/*") & !startsWith(tokens, "--")]
  quoted <- startsWith(tokens, '"')
  tokens[quoted] <- gsub(
    '""', '"', substring(tokens[quoted], 2L, nchar(tokens[quoted]) - 1L),
    fixed = TRUE
  )
  structure(sql_ascii_lower(tokens), quoted = quoted)
}

# Remove only parentheses enclosing a complete token expression.
duckdb_metadata_unwrap <- function(tokens) {
  while (length(tokens) > 2L && tokens[[1L]] == "(" &&
           utils::tail(tokens, 1L) == ")") {
    depth <- cumsum((tokens == "(") - (tokens == ")"))
    if (any(depth[-length(depth)] == 0L) || utils::tail(depth, 1L) != 0L) break
    tokens <- tokens[-c(1L, length(tokens))]
  }
  tokens
}

# Recognize stored-view literal reader arguments without evaluating them.
# The caller has already ruled out macros in this view's function heads.
duckdb_stored_reader_arg <- function(arg) {
  arg <- duckdb_metadata_unwrap(arg)
  if (length(arg) < 3L || !arg[[2L]] %in% c("=", ":=")) return(arg)
  value <- duckdb_metadata_unwrap(arg[-c(1L, 2L)])
  if (length(value) == 6L &&
        identical(value[c(1L, 2L, 4L, 5L, 6L)],
                  c("cast", "(", "as", "boolean", ")"))) {
    literal <- value[[3L]]
    if (literal %in% c("false", "'false'", "'f'")) value <- "false"
    if (literal %in% c("true", "'true'", "'t'")) value <- "true"
  }
  if (arg[[1L]] == "columns") {
    open <- match("(", value)
    heads <- list("struct_pack", c("main", ".", "struct_pack"),
                  c("system", ".", "main", ".", "struct_pack"))
    if (!is.na(open) && utils::tail(value, 1L) == ")" &&
          any(vapply(heads, identical, logical(1),
                     value[seq_len(open - 1L)]))) {
      entries <- value[seq.int(open + 1L, length(value) - 1L)]
      entries[entries == ":="] <- ":"
      value <- c("{", entries, "}")
    }
  }
  c(arg[1:2], value)
}

# Count columns in a literal JSON schema; expressions leave the count unknown.
# DuckDB's JSON BindReader disables type inference for these columns and also
# resolves automatic record inference when more than one column is supplied.
duckdb_json_schema_size <- function(arg) {
  if (length(arg) < 7L || arg[[1L]] != "columns" ||
        !arg[[2L]] %in% c("=", ":=") || arg[[3L]] != "{" ||
        utils::tail(arg, 1L) != "}") {
    return(0L)
  }
  entries <- arg[seq.int(4L, length(arg) - 1L)]
  if ((length(entries) + 1L) %% 4L != 0L) {
    return(0L)
  }
  starts <- seq.int(1L, length(entries), by = 4L)
  valid <- vapply(starts, function(i) {
    entries[[i + 1L]] == ":" &&
      grepl("^'([^']|'')*'$", entries[[i + 2L]]) &&
      (i == 1L || entries[[i - 1L]] == ",")
  }, logical(1))
  if (all(valid)) length(starts) else 0L
}

# Return multipart relation names outside locally bound CTE references.
# Scalar function arguments have their own scope and do not name relations.
# The positions attribute retains source starts before CTE filtering.
duckdb_metadata_relations <- function(tokens, functions = FALSE) {
  bindings <- duckdb_metadata_ctes(tokens)
  query_starts <- attr(bindings, "query_starts")
  frames <- list(list(
    query = TRUE, table = FALSE, expect = FALSE, start = TRUE
  ))
  relations <- integer()
  quoted <- attr(tokens, "quoted")
  # PIVOT_WIDER and PIVOT_LONGER are DuckDB's synonyms for PIVOT and UNPIVOT.
  sources <- c("from", "pivot", "pivot_wider", "unpivot", "pivot_longer")
  # SUMMARIZE, DESCRIBE and SHOW bind their source to report its columns.
  statements <- c("table", "summarize", "describe", "show")
  for (i in seq_along(tokens)) {
    token <- tokens[[i]]
    n <- length(frames)
    frame <- frames[[n]]
    # WITH's final definition precedes the main query's TABLE/SELECT position.
    if (i %in% query_starts) frame$start <- TRUE
    qualified <- grepl("^[[:alpha:]_][[:alnum:]_$]*$", token) &&
      ((i > 1L && tokens[[i - 1L]] == "." && !quoted[[i - 1L]]) ||
         (frame$expect && i < length(tokens) &&
            tokens[[i + 1L]] == "." && !quoted[[i + 1L]]))
    # Consume quoted and qualified names before SQL scope handling.
    if (quoted[[i]] || qualified) {
      if (frame$expect) relations <- c(relations, i)
      frame$expect <- frame$start <- FALSE
      frames[[n]] <- frame
      next
    }
    # ONLY/LATERAL prefix a source without supplying the relation itself.
    if (frame$expect && token %in% c("only", "lateral")) next
    if (token == "(") {
      frames[[n]]$expect <- FALSE
      frames[[n + 1L]] <- list(
        query = FALSE, table = frame$expect, expect = frame$expect, start = TRUE
      )
      next
    }
    if (token == ")" && n > 1L) {
      frames[[n]] <- NULL
      next
    }
    if (token == "select" || (token %in% c("with", "values") && frame$start)) {
      frame$query <- TRUE
      frame$table <- frame$expect <- FALSE
    } else if ((token %in% sources && (frame$query || frame$start)) ||
                 (token %in% statements && frame$start) ||
                 (token == "join" && frame$table)) {
      frame$table <- frame$expect <- TRUE
    } else if (token %in% c(
      "where", "group", "having", "qualify", "order", "limit", "offset",
      "union", "intersect", "except", "on", "using"
    )) {
      frame$expect <- FALSE
      # ON ends a join condition's source position, not its enclosing FROM.
      if (!token %in% c("on", "using")) frame$table <- FALSE
    } else if (token == "," && frame$table) {
      frame$expect <- TRUE
    } else if (frame$expect) {
      relations <- c(relations, i)
      frame$expect <- FALSE
    }
    frame$start <- token %in% c("union", "intersect", "except") ||
      (frame$start && token %in% c("all", "distinct"))
    frames[[n]] <- frame
  }
  positions <- relations
  relations <- relations[!vapply(relations, function(position) {
    parts <- duckdb_relation_parts(tokens, position, functions)
    length(parts) == 1L && !isTRUE(attr(parts, "function")) &&
      any(vapply(bindings, function(binding) {
        parts[[1L]] == binding$name && position >= binding$start &&
          position <= binding$end
      }, logical(1)))
  }, logical(1))]
  paths <- lapply(relations, function(position) {
    duckdb_relation_parts(tokens, position, functions)
  })
  structure(paths[lengths(paths) > 0L], positions = positions)
}

# Find each CTE's visible interval within its enclosing SQL scope. A normal
# CTE becomes visible after its definition; RECURSIVE admits self-reference.
# Retain token positions of main-query starts and CTE column declarations.
duckdb_metadata_ctes <- function(tokens) {
  quoted <- attr(tokens, "quoted")
  opens <- which(tokens == "(" & !quoted)
  closes <- which(tokens == ")" & !quoted)
  depth <- cumsum((tokens == "(" & !quoted) - (tokens == ")" & !quoted))
  end_of <- function(start) {
    ends <- closes[closes > start & depth[closes] == depth[[start]] - 1L]
    if (length(ends)) ends[[1L]] else length(tokens) + 1L
  }
  bindings <- list()
  query_starts <- integer()
  column_declarations <- integer()
  for (start in which(tokens == "with" & !quoted)) {
    parents <- opens[opens < start & vapply(opens, end_of, integer(1)) > start]
    scope_end <- if (length(parents)) end_of(utils::tail(parents, 1L)) else
      length(tokens) + 1L
    position <- start + 1L
    recursive <- position <= length(tokens) && tokens[[position]] == "recursive"
    if (recursive) position <- position + 1L
    while (position < scope_end) {
      name <- if (quoted[[position]]) tokens[[position]] else
        duckdb_reader_relation_path(tokens[[position]])
      position <- position + 1L
      if (position < scope_end && tokens[[position]] == "(" &&
            !quoted[[position]]) {
        column_declarations <- c(column_declarations, position - 1L)
        position <- end_of(position) + 1L
      }
      if (position >= scope_end || tokens[[position]] != "as" ||
            quoted[[position]]) break
      position <- position + 1L
      if (position < scope_end && tokens[[position]] == "not") {
        position <- position + 1L
      }
      if (position < scope_end && tokens[[position]] == "materialized") {
        position <- position + 1L
      }
      if (position >= scope_end || tokens[[position]] != "(" ||
            quoted[[position]]) break
      end <- end_of(position)
      bindings[[length(bindings) + 1L]] <- list(
        name = name, start = if (recursive) position else end + 1L,
        end = scope_end - 1L
      )
      position <- end + 1L
      if (position >= scope_end || tokens[[position]] != "," ||
            quoted[[position]]) break
      position <- position + 1L
    }
    query_starts <- c(query_starts, position)
  }
  structure(bindings, query_starts = query_starts,
            column_declarations = column_declarations)
}

# Decode a captured reader relation spelling without asking DuckDB to bind it.
# Escape strings permit hexadecimal and octal bytes within a filename suffix.
duckdb_reader_relation_path <- function(token) {
  if (grepl("^\\$[a-z_0-9]*\\$", token)) {
    return(sub(
      "(?s)^\\$([a-z_0-9]*)\\$(.*)\\$\\1\\$$", "\\2", token, perl = TRUE
    ))
  }
  if (!startsWith(token, "e'")) {
    return(if (startsWith(token, "'")) {
      gsub("''", "'", substring(token, 2L, nchar(token) - 1L), fixed = TRUE)
    } else {
      token
    })
  }
  path <- substring(token, 3L, nchar(token) - 1L)
  path <- gsub("''", "'", path, fixed = TRUE)
  positions <- gregexpr(
    "\\\\(?:[0-7]{1,3}|x[0-9a-f]{1,2}|.)", path, perl = TRUE
  )
  escapes <- regmatches(path, positions)[[1L]]
  regmatches(path, positions) <- list(vapply(escapes, function(escape) {
    value <- substring(escape, 2L)
    if (grepl("^x[0-9a-f]+$", value)) {
      intToUtf8(strtoi(substring(value, 2L), base = 16L))
    } else if (grepl("^[0-7]", value)) {
      intToUtf8(strtoi(value, base = 8L))
    } else {
      switch(value, n = "\n", r = "\r", t = "\t", b = "\b", f = "\f", value)
    }
  }, character(1)))
  sql_ascii_lower(path)
}

# Decode the qualified name beginning at a captured relation position.
# Function heads are optionally marked for view admission rather than excluded.
duckdb_relation_parts <- function(tokens, position, functions = FALSE) {
  quoted <- attr(tokens, "quoted")
  last <- position
  while (last + 2L <= length(tokens) && tokens[[last + 1L]] == "." &&
           !quoted[[last + 1L]]) {
    part <- tokens[[last + 2L]]
    if (!quoted[[last + 2L]] && !grepl(
      "^[[:alpha:]_][[:alnum:]_$]*$|^e?'|^\\$[a-z_0-9]*\\$", part
    )) break
    last <- last + 2L
  }
  function_head <- last < length(tokens) && tokens[[last + 1L]] == "(" &&
    !quoted[[last + 1L]]
  if (function_head && !functions) {
    return(character())
  }
  parts <- vapply(seq.int(position, last, by = 2L), function(i) {
    if (quoted[[i]]) tokens[[i]] else duckdb_reader_relation_path(tokens[[i]])
  }, character(1))
  if (functions) attr(parts, "function") <- function_head
  parts
}

# Capture catalog names and the configured search path without binding any
# caller relation. This resolves filename fallback, not a second typed snapshot.
duckdb_metadata_catalog <- function(con) {
  # System qualification keeps caller macros outside this metadata query.
  sql <- dbplyr::sql(paste(
    "SELECT database_name, schema_name, table_name AS relation_name,",
    "'table' AS kind, NULL::VARCHAR AS definition,",
    "system.main.current_database() AS default_database,",
    "system.main.current_setting('search_path') AS search_path",
    "FROM system.main.duckdb_tables() UNION ALL",
    "SELECT database_name, schema_name, view_name, 'view', sql,",
    "system.main.current_database(),",
    "system.main.current_setting('search_path')",
    "FROM system.main.duckdb_views() UNION ALL",
    "SELECT database_name, schema_name, function_name, function_type, NULL,",
    "system.main.current_database(),",
    "system.main.current_setting('search_path')",
    "FROM system.main.duckdb_functions()",
    "WHERE function_type IN ('macro', 'table_macro') UNION ALL",
    "SELECT NULL, NULL, keyword_name, 'keyword', keyword_category,",
    "system.main.current_database(),",
    "system.main.current_setting('search_path')",
    "FROM system.main.duckdb_keywords()"
  ))
  catalog <- with_sent_query(
    "relation_catalog", sql, DBI::dbGetQuery(con, sql)
  )
  # duckdb_register() keeps each frame alive as a `_registered_df_<name>`
  # attribute of the connection pointer; its temp.main view has no SQL.
  registered <- sub("^_registered_df_", "", grep(
    "^_registered_df_", names(attributes(con@conn_ref)), value = TRUE
  ))
  catalog$registered <- catalog$kind %in% "view" &
    catalog$database_name %in% "temp" & catalog$schema_name %in% "main" &
    catalog$relation_name %in% registered &
    (is.na(catalog$definition) | !nzchar(catalog$definition))
  catalog
}

# Resolve one captured multipart name against catalog-only metadata. DuckDB's
# search path includes temp.main, its configured entries, then default main.
# The caller supplies this operation's duckdb_metadata_catalog() result.
duckdb_metadata_catalog_match <- function(parts, catalog, namespace = NULL,
                                          kinds = c("table", "view")) {
  if (!nrow(catalog) || length(parts) > 3L) return(integer())
  identifiers <- c("database_name", "schema_name", "relation_name",
                   "default_database", "search_path")
  catalog[identifiers] <- lapply(catalog[identifiers], sql_ascii_lower)
  default <- catalog$default_database[[1L]]
  tokens <- duckdb_metadata_tokens(catalog$search_path[[1L]])
  separators <- which(tokens == "," & !attr(tokens, "quoted"))
  starts <- c(1L, separators + 1L)
  ends <- c(separators - 1L, length(tokens))
  configured <- if (length(tokens)) lapply(seq_along(starts), function(i) {
    path <- tokens[seq.int(starts[[i]], ends[[i]])]
    path <- path[seq.int(1L, length(path), by = 2L)]
    if (length(path) == 1L) c(default, path) else path
  }) else list()
  view_paths <- if (!is.null(namespace) &&
                      !namespace[[1L]] %in% c("temp", "system")) {
    list(sql_ascii_lower(namespace), c(sql_ascii_lower(namespace[[1L]]), ""))
  } else {
    list()
  }
  paths <- c(list(c("temp", "main")), view_paths, configured,
             list(c(default, "main"), c("system", "main"),
                  c("system", "pg_catalog")))
  if (length(parts) == 3L) {
    paths <- list(parts[1:2])
  } else if (length(parts) == 2L) {
    prefix <- parts[[1L]]
    # Two-part names can qualify a schema or an attached database.
    if (prefix %in% catalog$database_name) {
      paths <- Filter(function(path) {
        path[[1L]] == prefix && nzchar(path[[2L]])
      }, paths)
      if (!length(paths)) paths <- list(c(prefix, "main"))
    } else {
      paths <- Filter(function(path) path[[2L]] %in% c(prefix, ""), paths)
      paths <- lapply(paths, function(path) c(path[[1L]], prefix))
      if (!length(paths)) paths <- list(c(default, prefix))
    }
  }
  for (path in paths) {
    matched <- which(catalog$database_name == path[[1L]] &
                       catalog$schema_name == path[[2L]] &
                       catalog$relation_name == utils::tail(parts, 1L) &
                       catalog$kind %in% kinds)
    if (length(matched)) return(matched[[1L]])
  }
  integer()
}

# Which rows of a duckdb_metadata_catalog() snapshot are macros or table macros
# a caller defined, rather than DuckDB's own in database `system`.
duckdb_caller_macros <- function(catalog) {
  catalog$kind %in% c("macro", "table_macro") &
    !catalog$database_name %in% "system"
}

# Whether catalog row `index`, a call's first macro match, expands without
# reaching source work: one of DuckDB's own scalar macros while the snapshot
# holds no caller macro its body could resolve to (ADR 0020). Its arguments
# are authored text the caller's scan still checks.
duckdb_inert_macro <- function(index, catalog) {
  catalog$kind[[index]] == "macro" &&
    catalog$database_name[[index]] %in% "system" &&
    !any(duckdb_caller_macros(catalog))
}

# Refuse view work whose safety cannot be established from captured definitions.
abort_duckdb_view_metadata <- function() {
  abort_marginplyr(c(
    "Can't safely determine column metadata for this DuckDB view.",
    i = "Its definition could hide source work while binding a zero-row query.",
    i = paste0(
      "Supply established reader metadata directly, or collect the input ",
      "explicitly with {.code dplyr::collect()} ",
      "and pass the resulting data frame."
    )
  ))
}

# Refuse a client-created view whose definition DuckDB does not store, other
# than a registered data frame, since only binding it would reveal its source.
abort_duckdb_unstored_view <- function() {
  abort_marginplyr(c(
    "Can't safely determine column metadata for this DuckDB view.",
    i = paste0(
      "It has no stored SQL definition, so its source can't be checked ",
      "without binding it."
    ),
    i = paste0(
      "Create the view with SQL {.code CREATE VIEW ... AS}, register an R ",
      "data frame with {.code duckdb::duckdb_register()}, or collect the ",
      "input explicitly with {.code dplyr::collect()} ",
      "and pass the resulting data frame."
    )
  ))
}

# Refuse authored calls whose source dependencies are opaque before binding.
abort_duckdb_call_metadata <- function() {
  abort_marginplyr(c(
    "Can't safely determine column metadata for this DuckDB call.",
    i = paste0(
      "A macro, query() or query_table() call could hide source work ",
      "during binding."
    ),
    i = paste0(
      "Supply established reader metadata directly, or collect the input ",
      "explicitly with {.code dplyr::collect()} ",
      "and pass the resulting data frame."
    )
  ))
}

# Identify authored function calls without treating SQL declarations as calls.
# The caller supplies this operation's catalog-only snapshot, never a binding.
validate_duckdb_authored_calls <- function(tokens, catalog) {
  quoted <- attr(tokens, "quoted")
  heads <- which(c(tokens[-1L], "") == "(" & !c(quoted[-1L], FALSE))
  identifier <- grepl(paste0(
    "(*UTF)^[a-z_\\x{80}-\\x{10ffff}]",
    "[a-z_0-9$\\x{80}-\\x{10ffff}]*$"
  ), tokens[heads], perl = TRUE)
  syntax <- c("select", "from", "where", "on", "having", "qualify", "in",
              "exists", "as", "with", "values", "using",
              "group", "by", "order", "distinct", "not", "and", "or")
  qualified <- heads > 1L & tokens[pmax(1L, heads - 1L)] == "."
  heads <- heads[quoted[heads] | (identifier &
                                    (!tokens[heads] %in% syntax | qualified))]
  declarations <- attr(duckdb_metadata_ctes(tokens), "column_declarations")
  relations <- duckdb_metadata_relations(tokens, functions = TRUE)
  for (position in attr(relations, "positions")) {
    parts <- duckdb_relation_parts(tokens, position, functions = TRUE)
    if (!isTRUE(attr(parts, "function"))) {
      declarations <- c(declarations, position + 2L * length(parts) - 1L)
    }
  }
  previous <- pmax(1L, heads - 1L)
  alias <- heads > 1L & tokens[previous] == "as" & !quoted[previous]
  materialized <- tokens[heads] == "materialized" & !quoted[heads] &
    tokens[previous] == "not" & heads > 2L &
    tokens[pmax(1L, heads - 2L)] == "as"
  modifier <- tokens[heads] %in% c("exclude", "replace", "rename") &
    !quoted[heads] & tokens[previous] == "*" & !quoted[previous]
  # Alias column lists and OVER/FILTER clauses follow closing expressions.
  implicit_alias <- heads > 1L & tokens[previous] == ")" & !quoted[previous]
  heads <- heads[!alias & !materialized & !modifier &
                   !implicit_alias & !heads %in% declarations]
  for (head in heads) {
    while (head > 2L && tokens[[head - 1L]] == "." &&
             !quoted[[head - 1L]]) head <- head - 2L
    parts <- duckdb_relation_parts(tokens, head, functions = TRUE)
    macro <- duckdb_metadata_catalog_match(
      parts, catalog(), kinds = c("macro", "table_macro")
    )
    if ((length(macro) && !duckdb_inert_macro(macro, catalog())) ||
          utils::tail(parts, 1L) %in% c("query", "query_table")) {
      abort_duckdb_call_metadata()
    }
  }
  invisible(NULL)
}

# Inspect a stored view without binding it. Strip CREATE VIEW's header, keep
# its own catalog/schema search context, and stop cycles or hidden macro work.
# `seen` holds catalog row positions on the current view path.
validate_duckdb_view_metadata <- function(index, catalog, seen) {
  if (index %in% seen) abort_duckdb_view_metadata()
  view <- catalog()[index, , drop = FALSE]
  # A registered R data frame binds from the frame alone, reading no source.
  if (isTRUE(view$registered[1L])) return(invisible(NULL))
  sql <- view$definition[[1L]]
  if (is.na(sql) || !nzchar(sql)) abort_duckdb_unstored_view()
  tokens <- duckdb_metadata_tokens(sql)
  quoted <- attr(tokens, "quoted")
  depth <- cumsum((tokens == "(" & !quoted) - (tokens == ")" & !quoted))
  start <- which(tokens == "as" & !quoted & depth == 0L)
  if (!length(start)) abort_duckdb_view_metadata()
  range <- seq.int(start[[1L]] + 1L, length(tokens))
  body <- structure(tokens[range], quoted = quoted[range])
  namespace <- c(view$database_name[[1L]], view$schema_name[[1L]])
  heads <- which(c(body[-1L], "") == "(" &
                   !c(attr(body, "quoted")[-1L], FALSE))
  grammar <- catalog()
  noncall <- grammar$relation_name[grammar$kind == "keyword"]
  identifier <- grepl(paste0(
    "(*UTF)^[a-z_\\x{80}-\\x{10ffff}]",
    "[a-z_0-9$\\x{80}-\\x{10ffff}]*$"
  ), body[heads], perl = TRUE)
  # Stored FunctionExpression names quote every keyword (ADR 0020).
  heads <- heads[attr(body, "quoted")[heads] |
                   (identifier & !body[heads] %in% noncall)]
  declarations <- attr(duckdb_metadata_ctes(body), "column_declarations")
  previous <- pmax(1L, heads - 1L)
  # Stored aliases follow AS; CTE column lists have their own declaration heads.
  syntax <- heads > 1L & body[previous] == "as" &
    !attr(body, "quoted")[previous]
  heads <- heads[!syntax & !heads %in% declarations]
  for (head in heads) {
    while (head > 2L && body[[head - 1L]] == "." &&
             !attr(body, "quoted")[[head - 1L]]) head <- head - 2L
    parts <- duckdb_relation_parts(body, head, functions = TRUE)
    macro <- duckdb_metadata_catalog_match(
      parts, catalog(), namespace, kinds = c("macro", "table_macro")
    )
    if (length(macro) && !duckdb_inert_macro(macro, catalog())) {
      abort_duckdb_view_metadata()
    }
  }
  validate_duckdb_csv_metadata(
    body, catalog, namespace = namespace, seen = c(seen, index)
  )
}

# Refuse reader inference before DuckDB can bind the captured input (ADR 0020).
# The caller supplies the CSV or JSON family established from captured SQL.
abort_duckdb_reader_metadata <- function(family) {
  if (family == "JSON") {
    abort_marginplyr(c(
      "Can't safely determine column metadata for this DuckDB JSON input.",
      i = paste0(
        "JSON auto-detection could sample source rows ",
        "even with a zero-row query."
      ),
      i = paste0(
        "Supply a JSON source with an explicit schema and ",
        "{.code auto_detect = false} and explicit ",
        "{.code records = true} or {.code records = false}, ",
        "or collect the input explicitly with {.code dplyr::collect()} ",
        "and pass the resulting data frame."
      )
    ))
  }
  abort_marginplyr(c(
    "Can't safely determine column metadata for this DuckDB CSV input.",
    i = paste0(
      "CSV auto-detection could sample source rows ",
      "even with a zero-row query."
    ),
    i = paste0(
      "Supply a CSV source with an explicit schema and ",
      "{.code auto_detect = false}, or collect the input explicitly with ",
      "{.code dplyr::collect()} and pass the resulting data frame."
    )
  ))
}

# CSV and JSON binders can sample values even under LIMIT 0. Admit readers
# whose literal arguments establish inference is disabled at their own depth.
# The caller supplies captured SQL, without binding the input (ADR 0020).
# `catalog` lazily returns this operation's catalog-only name snapshot.
# Resolve every relation name before following stored reader dependencies.
validate_duckdb_csv_metadata <- function(
  sql, catalog, namespace = NULL, seen = integer()) {
  tokens <- if (is.null(namespace)) {
    duckdb_metadata_tokens(as.character(sql))
  } else {
    sql
  }
  relations <- duckdb_metadata_relations(
    tokens, functions = !is.null(namespace)
  )
  relation_paths <- vapply(relations, paste, character(1), collapse = ".")
  readers <- c("read_csv", "read_csv_auto", "read_json", "read_json_auto",
               "read_ndjson", "read_ndjson_auto")
  for (i in seq_along(relations)) {
    if (isTRUE(attr(relations[[i]], "function"))) {
      if (!utils::tail(relations[[i]], 1L) %in% readers) {
        abort_duckdb_view_metadata()
      }
      relation_paths[[i]] <- ""
      next
    }
    index <- duckdb_metadata_catalog_match(relations[[i]], catalog(), namespace)
    if (length(index)) {
      if (catalog()$kind[[index]] == "view") {
        validate_duckdb_view_metadata(index, catalog, seen)
      }
      relation_paths[[i]] <- ""
    }
  }
  if (any(grepl(
    "[.](csv|tsv)([.](gz|zst))?$|[.](csv|tsv)[?]", relation_paths
  ))) {
    abort_duckdb_reader_metadata("CSV")
  }
  if (any(grepl(
    "[.](json|jsonl|ndjson)([.](gz|zst))?$|[.](json|jsonl|ndjson)[?]",
    relation_paths
  ))) {
    abort_duckdb_reader_metadata("JSON")
  }
  json <- c("read_json", "read_json_auto", "read_ndjson", "read_ndjson_auto")
  readers <- which(tokens %in% c("read_csv", "read_csv_auto", json) &
                     c(tokens[-1L], "") == "(")
  for (reader in readers) {
    family <- if (tokens[[reader]] %in% json) "JSON" else "CSV"
    rest <- tokens[seq.int(reader + 1L, length(tokens))]
    depth <- cumsum((rest %in% c("(", "[", "{")) -
                      (rest %in% c(")", "]", "}")))
    end <- which(depth == 0L)
    if (length(end) > 0L) {
      rest <- rest[seq_len(end[[1L]])]
      depth <- depth[seq_along(rest)]
      separators <- which(depth == 1L & rest == ",")
      starts <- c(2L, separators + 1L)
      ends <- c(separators - 1L, length(rest) - 1L)
      arguments <- lapply(seq_along(starts), function(i) {
        rest[seq.int(starts[[i]], ends[[i]])]
      })
      if (!is.null(namespace)) {
        arguments <- lapply(arguments, duckdb_stored_reader_arg)
      }
      disabled <- vapply(arguments, function(arg) {
        identical(arg, c("auto_detect", "=", "false")) ||
          identical(arg, c("auto_detect", ":=", "false"))
      }, logical(1))
      records <- vapply(arguments, function(arg) {
        length(arg) == 3L && identical(arg[[1L]], "records") &&
          arg[[2L]] %in% c("=", ":=") &&
          arg[[3L]] %in% c("true", "false", "'true'", "'false'")
      }, logical(1))
      schema_size <- max(vapply(arguments, duckdb_json_schema_size, integer(1)))
      # JSON record inference samples objects even with one established type.
      safe <- if (family == "JSON") {
        (any(disabled) || schema_size > 0L) &&
          (any(records) || schema_size > 1L)
      } else {
        any(disabled)
      }
      if (safe) {
        next
      }
    }
    abort_duckdb_reader_metadata(family)
  }
  if (is.null(namespace)) validate_duckdb_authored_calls(tokens, catalog)
  invisible(NULL)
}

# Check SQL fragments and table paths throughout a captured lazy query,
# including both join inputs. Leave translation to the owned audit/collect
# paths so preflight cannot duplicate translation conditions or change their
# Sent-query outcomes.
validate_duckdb_csv_query <- function(query, catalog) {
  if (rlang::is_missing(query)) {
    return(invisible(NULL))
  }
  if (inherits(query, "dbplyr_table_path")) {
    # dbplyr captures table paths as already quoted multipart SQL identifiers.
    validate_duckdb_csv_metadata(paste("TABLE", query), catalog)
  } else if (inherits(query, "sql")) {
    for (sql in as.character(query)) {
      validate_duckdb_csv_metadata(sql, catalog)
    }
  } else if (rlang::is_quosure(query)) {
    validate_duckdb_csv_query(rlang::quo_get_expr(query), catalog)
  } else if (is.list(query) || is.call(query)) {
    parts <- unclass(query)
    if (is.call(parts)) {
      parts <- as.list(parts)
    }
    for (part in parts) {
      validate_duckdb_csv_query(part, catalog)
    }
  }
  invisible(NULL)
}
