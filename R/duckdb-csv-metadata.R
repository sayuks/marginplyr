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
        tail(arg, 1L) != "}") {
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

# Captured relation tokens, including comma joins and parenthesized sources.
# Scalar function arguments have their own scope and do not name relations.
duckdb_metadata_relations <- function(tokens) {
  frames <- list(list(
    query = TRUE, table = FALSE, expect = FALSE, start = TRUE
  ))
  relations <- character()
  quoted <- attr(tokens, "quoted")
  for (i in seq_along(tokens)) {
    token <- tokens[[i]]
    n <- length(frames)
    frame <- frames[[n]]
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
                 (token == "table" && frame$start && !quoted[[i]]) ||
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
      relations <- c(relations, token)
      frame$expect <- FALSE
    }
    frame$start <- token %in% c("union", "intersect", "except") ||
      (frame$start && token %in% c("all", "distinct"))
    frames[[n]] <- frame
  }
  relations
}

# Decode a captured CSV relation spelling without asking DuckDB to bind it.
# Escape strings permit hexadecimal and octal bytes within a filename suffix.
duckdb_csv_relation_path <- function(token) {
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

# Refuse reader inference before DuckDB can bind the captured input (ADR 0020).
abort_duckdb_reader_metadata <- function(family) {
  abort_marginplyr(c(
    paste0(
      "Can't safely determine column metadata for this DuckDB ",
      family, " input."
    ),
    i = paste0(
      family, " auto-detection could sample source rows ",
      "even with a zero-row query."
    ),
    i = paste0(
      "Supply a ", family, " source with an explicit schema and ",
      "{.code auto_detect = false}",
      if (family == "JSON") " and explicit {.code records = true} or false",
      ", or collect the input explicitly with ",
      "{.code dplyr::collect()} and pass the resulting data frame."
    )
  ))
}

# CSV and JSON binders can sample values even under LIMIT 0. Admit readers
# whose literal arguments establish inference is disabled at their own depth.
# The caller supplies captured SQL, without binding the input (ADR 0020).
validate_duckdb_csv_metadata <- function(sql) {
  tokens <- duckdb_metadata_tokens(as.character(sql))
  relations <- duckdb_metadata_relations(tokens)
  csv_relations <- vapply(relations, duckdb_csv_relation_path, character(1))
  if (any(grepl(
    "[.](csv|tsv)([.](gz|zst))?$|[.](csv|tsv)[?]", csv_relations
  ))) {
    abort_duckdb_reader_metadata("CSV")
  }
  if (any(grepl("[.](json|jsonl|ndjson)([.](gz|zst|zstd))?'?$", relations))) {
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
validate_duckdb_csv_query <- function(query) {
  if (rlang::is_missing(query)) {
    return(invisible(NULL))
  }
  if (inherits(query, "sql")) {
    for (sql in as.character(query)) {
      validate_duckdb_csv_metadata(sql)
    }
  } else if (rlang::is_quosure(query)) {
    validate_duckdb_csv_query(rlang::quo_get_expr(query))
  } else if (is.list(query) || is.call(query)) {
    parts <- unclass(query)
    if (is.call(parts)) {
      parts <- as.list(parts)
    }
    for (part in parts) {
      validate_duckdb_csv_query(part)
    }
  }
  invisible(NULL)
}
