# SQL lexical tokens for the CSV metadata guard. Quoted values stay opaque,
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
  tolower(tokens)
}

# A CSV binder can sample values even under LIMIT 0. Admit only an explicit
# literal opt-out of auto-detection, at that reader's own argument depth.
# The caller supplies captured SQL, without binding the input (ADR 0020).
validate_duckdb_csv_metadata <- function(sql) {
  tokens <- duckdb_metadata_tokens(as.character(sql))
  readers <- which(tokens %in% c("read_csv", "read_csv_auto") &
                     c(tokens[-1L], "") == "(")
  for (reader in readers) {
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
      if (any(disabled)) {
        next
      }
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
