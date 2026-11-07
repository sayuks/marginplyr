pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
socket <- commandArgs(trailingOnly = TRUE)[[1L]]
main <- function() {
  con <- DBI::dbConnect(RPostgres::Postgres(), host = socket,
                       port = 55378L, dbname = "postgres")
  on.exit(DBI::dbDisconnect(con))
  cat("PostgreSQL", DBI::dbGetQuery(con, "SHOW server_version")[[1L]], "\n")
  DBI::dbExecute(con, "CREATE TYPE hue AS ENUM ('blue', 'red')")
  DBI::dbExecute(con, "CREATE DOMAIN tag AS varchar(4) CHECK (VALUE <> 'bad')")
  DBI::dbExecute(con, "CREATE DOMAIN nn_tag AS text NOT NULL")
  DBI::dbExecute(con, paste(
    "CREATE TEMP TABLE fixture (k integer, token text COLLATE \"C\",",
    "positive tag, strict nn_tag, hue hue, amount numeric(20,4),",
    "day date, instant timestamp with time zone, tick time, point point,",
    "flags boolean, bytes bytea, j jsonb, whole bigint, bounded varchar(3),",
    "letter char(3), things int[], \".env\" text, \".data\" text, \"MiX\" text)"
  ))
  DBI::dbExecute(con, paste(
    "INSERT INTO fixture VALUES",
    "(1,'b','ok','qq','red',1.25,'2026-01-01','2026-01-01 01:00Z',",
    "'01:02','(1,2)',true,decode('11','hex'),'{\"x\":1}',9007199254740993,'lon','q',",
    "ARRAY[1,2],'bee','one','B'),",
    "(2,'A','top','a','blue',-2,'2026-01-02','2026-01-02 01:00Z',",
    "'03:00','(3,4)',false,decode('ff','hex'),'[]',5,'lit','r',ARRAY[3],'aye','two','a')"
  ))
  cols <- setdiff(DBI::dbListFields(con, "fixture"), "k")
  sort_modes <- c("none", "last", "first")
  graph_sql <- c(
    base = "SELECT * FROM fixture",
    empty = "SELECT * FROM fixture WHERE FALSE",
    derived = "SELECT * FROM fixture WHERE k > 0 ORDER BY k DESC LIMIT 1"
  )
  patterns <- c("included", "omitted_first", "included_first")
  quote_name <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  cap <- function(fn) {
    tryCatch(suppressWarnings(fn()), error = identity)
  }
  for (graph in names(graph_sql)) for (col in cols) {
    key <- quote_name(col)
    projection <- paste0("SELECT k, ", key, " FROM (", graph_sql[[graph]],
                         ") AS projected")
    input <- dplyr::tbl(con, dbplyr::sql(projection), vars = c("k", col))
    for (pattern in patterns) for (order in sort_modes) {
      dims <- grouping_set(dplyr::all_of(col))
      plan <- switch(pattern, included = dims,
                     omitted_first = grouping_sets(grouping_set(), dims),
                     included_first = grouping_sets(dims, grouping_set()))
      kept <- if (pattern == "omitted_first") 2L else 1L
      omitted <- if (kept == 2L) 1L else 2L
      args <- c(graph, col, pattern, order)
      got <- cap(function() {
        expand_with_margins(input, .grouping = plan, .id = "occ",
                            .margin_label = NULL, .sort = order) |>
          dplyr::collect()
      })
      select_from <- paste0("SELECT ", key, ", ", kept, " AS occ, k FROM src")
      order_sql <- if (order == "none") {
        "ORDER BY occ,k"
      } else {
        first <- if (order == "first") omitted else kept
        paste0("ORDER BY (occ <> ", first, "), ", key, " NULLS LAST")
      }
      if (pattern != "included") {
        select_from <- paste0(
          select_from, " UNION ALL SELECT (SELECT ", key,
          " FROM src WHERE FALSE) AS ", key, ", ", omitted, " AS occ, k FROM src"
        )
      }
      expected <- cap(function() DBI::dbGetQuery(con, paste0(
        "WITH src AS (", graph_sql[[graph]], ") SELECT * FROM (",
        select_from, ") as result ", order_sql
      )))
      if (inherits(got, "error") || inherits(expected, "error")) {
        state <- if (inherits(got, "error") && inherits(expected, "error")) {
          "both_error"
        } else if (inherits(got, "error")) {
          "package_error"
        } else {
          "reference_error"
        }
        diagnostic <- function(x) {
          if (inherits(x, "error")) strsplit(conditionMessage(x), "\n")[[1L]][1L] else ""
        }
        cat(paste(c(args, state, diagnostic(got), diagnostic(expected)), collapse = "\t"), "\n", sep = "")
        next
      }
      expected <- tibble::as_tibble(expected)
      normalize <- function(frame) {
        if (order == "none") return(frame[order(frame$occ, frame$k), ])
        tied <- which(is.na(frame[[col]]))
        frame[tied, ] <- frame[tied[order(frame$k[tied])], ]
        frame
      }
      got <- normalize(got)
      expected <- normalize(expected)
      state <- if (identical(got, expected)) "match" else "mismatch"
      cat(paste(c(args, state, if (state == "mismatch") {
        paste(capture.output(all.equal(got, expected)), collapse = " ")
      }), collapse = "\t"), "\n", sep = "")
    }
  }
}
main()
