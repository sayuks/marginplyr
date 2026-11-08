# SQL lexical tokens for reader metadata guards. Quoted values stay opaque,
# quoted identifiers keep their spelling, and nested comments are discarded.
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
  structure(tolower(tokens), quoted = quoted)
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
duckdb_metadata_relations <- function(tokens) {
  frames <- list(list(
    query = TRUE, table = FALSE, expect = FALSE, start = TRUE
  ))
  relations <- integer()
  quoted <- attr(tokens, "quoted")
  for (i in seq_along(tokens)) {
    token <- tokens[[i]]
    n <- length(frames)
    frame <- frames[[n]]
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
    # ONLY prefixes a table name without supplying the relation itself.
    if (frame$expect && token == "only") next
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
    if (token == "select") {
      frame$query <- TRUE
      frame$table <- frame$expect <- FALSE
    } else if ((token %in% c("from", "pivot", "unpivot") &&
                  (frame$query || frame$start)) ||
                 (token == "table" && frame$start) ||
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
  bindings <- duckdb_metadata_ctes(tokens)
  relations <- relations[!vapply(relations, function(position) {
    parts <- duckdb_relation_parts(tokens, position)
    length(parts) == 1L && any(vapply(bindings, function(binding) {
      parts[[1L]] == binding$name && position >= binding$start &&
        position <= binding$end
    }, logical(1)))
  }, logical(1))]
  paths <- lapply(relations, function(position) {
    duckdb_relation_parts(tokens, position)
  })
  paths[lengths(paths) > 0L]
}

# Find each CTE's visible interval within its enclosing SQL scope. A normal
# CTE becomes visible after its definition; RECURSIVE admits self-reference.
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
  }
  bindings
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
  tolower(path)
}

# Decode the qualified name beginning at a captured relation position.
# Function heads are excluded; their reader arguments have separate admission.
duckdb_relation_parts <- function(tokens, position) {
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
  if (last < length(tokens) && tokens[[last + 1L]] == "(" &&
        !quoted[[last + 1L]]) {
    return(character())
  }
  parts <- vapply(seq.int(position, last, by = 2L), function(i) {
    if (quoted[[i]]) tokens[[i]] else duckdb_reader_relation_path(tokens[[i]])
  }, character(1))
  parts
}

# Capture catalog names and the configured search path without binding any
# caller relation. This resolves filename fallback, not a second typed snapshot.
duckdb_metadata_catalog <- function(con) {
  # System qualification keeps caller macros outside this metadata query.
  sql <- dbplyr::sql(paste(
    "SELECT database_name, schema_name, table_name AS relation_name,",
    "system.main.current_database() AS default_database,",
    "system.main.current_setting('search_path') AS search_path",
    "FROM system.main.duckdb_tables() UNION ALL",
    "SELECT database_name, schema_name, view_name AS relation_name,",
    "system.main.current_database(),",
    "system.main.current_setting('search_path')",
    "FROM system.main.duckdb_views()"
  ))
  with_sent_query("relation_catalog", sql, DBI::dbGetQuery(con, sql))
}

# Resolve one captured multipart name against catalog-only metadata. DuckDB's
# search path includes temp.main, its configured entries, then default main.
# The caller supplies this operation's duckdb_metadata_catalog() result.
duckdb_metadata_catalog_has <- function(parts, catalog) {
  if (!nrow(catalog) || length(parts) > 3L) return(FALSE)
  catalog[] <- lapply(catalog, tolower)
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
  paths <- c(list(c("temp", "main")), configured,
             list(c(default, "main"), c("system", "main"),
                  c("system", "pg_catalog")))
  if (length(parts) == 3L) {
    paths <- list(parts[1:2])
  } else if (length(parts) == 2L) {
    prefix <- parts[[1L]]
    # Two-part names can qualify a schema or an attached database.
    if (prefix %in% catalog$database_name) {
      paths <- Filter(function(path) path[[1L]] == prefix, paths)
      if (!length(paths)) paths <- list(c(prefix, "main"))
    } else {
      paths <- Filter(function(path) path[[2L]] == prefix, paths)
      if (!length(paths)) paths <- list(c(default, prefix))
    }
  }
  any(vapply(paths, function(path) {
    any(catalog$database_name == path[[1L]] &
          catalog$schema_name == path[[2L]] &
          catalog$relation_name == utils::tail(parts, 1L))
  }, logical(1)))
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
validate_duckdb_csv_metadata <- function(sql, catalog) {
  tokens <- duckdb_metadata_tokens(as.character(sql))
  relations <- duckdb_metadata_relations(tokens)
  relation_paths <- vapply(relations, paste, character(1), collapse = ".")
  candidates <- grepl(paste0(
    "[.](csv|tsv|json|jsonl|ndjson)([.](gz|zst))?$|",
    "[.](csv|tsv|json|jsonl|ndjson)[?]"
  ), relation_paths)
  for (i in which(candidates)) {
    if (duckdb_metadata_catalog_has(relations[[i]], catalog())) {
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
  invisible(NULL)
}

# Check SQL fragments throughout a captured lazy query, including both join
# inputs. Leave translation to the owned audit/collect paths so preflight
# cannot duplicate translation conditions or change their Sent-query outcomes.
validate_duckdb_csv_query <- function(query, catalog) {
  if (rlang::is_missing(query)) {
    return(invisible(NULL))
  }
  if (inherits(query, "sql")) {
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
