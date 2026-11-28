# `duckdb_implicit_functions` is a written list of the names DuckDB binds from
# syntax that does not spell them, so it is held here from DuckDB's side:
# `helper-duckdb-reach.R` asks the DuckDB under test which names it reaches.

# The typed plan of a lazy input, which needs its column metadata.
numeric_plan <- function(input) {
  inspect_grouping(
    input, .grouping = grouping_set(tidyselect::where(is.numeric))
  )$included
}

test_that("every caller macro DuckDB reaches unwritten is refused (#1843)", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  # With no caller macro, none of DuckDB's own views is refused.
  catalog_now <- duckdb_metadata_catalog(con)
  own_views <- DBI::dbGetQuery(con, paste(
    "SELECT schema_name || '.' || view_name AS name FROM duckdb_views()",
    "WHERE internal"
  ))$name
  expect_gt(length(own_views), 0L)
  for (view in own_views) {
    expect_no_error(validate_duckdb_csv_metadata(
      paste("SELECT * FROM", view), function(read = TRUE) catalog_now
    ))
  }
  sweep <- duckdb_reach_sweep(con, withr::local_tempdir())
  # A form that stopped binding measures nothing.
  stable <- sweep$probes$stable
  expect_identical(stable$probe[!stable$live], character())
  # A PIVOT form, and on DuckDB 1.5 a subscript or field access, stops under
  # both bodies at an error that names no macro; any other form that stops is
  # a reach the sweep cannot see.
  unbound <- if (utils::packageVersion("duckdb") >= "2.0.0") {
    "PIVOT"
  } else {
    "PIVOT|\\[|[.]a AS y|s[.][*]"
  }
  expect_identical(grep(unbound, sweep$blocked, value = TRUE, invert = TRUE),
                   character())
  pairs <- duckdb_reach_verdicts(con, sweep$pairs)
  # The whole sweep ran: each kind of statement reached some macro.
  expect_setequal(pairs$kind, c("corpus", "bare", "call"))
  stored <- pairs[pairs$stored_confirmed %in% TRUE, ]
  expect_gt(nrow(stored), 0L)
  expect_identical(unique(sprintf(
    "%s from a view of %s", stored$reached, stored$sql
  )[!stored$stored_refused]), character())
  pairs <- pairs[pairs$confirmed, ]
  # ADR 0020 admits a statement naming no relation and no call head, for which
  # the scan reads no catalog; every other reach is refused.
  expect_gt(sum(pairs$asked), sum(!pairs$asked))
  missed <- pairs[pairs$asked & !pairs$refused, ]
  expect_identical(
    unique(sprintf("%s from %s", missed$reached, missed$sql)), character()
  )
  # The list holds no name left over: each is reached by a form that does not
  # write it, bar the names only the other supported version or an unloaded
  # extension reaches.
  elsewhere <- c("timezone", if (utils::packageVersion("duckdb") >= "2.0.0") {
    c("!__postfix", "not_like_escape", "not_ilike_escape")
  } else {
    c("factorial", "regexp_matches", "array_agg", "row_number")
  })
  unwritten <- pairs$reached[pairs$kind != "call" & !pairs$written]
  expect_identical(
    setdiff(duckdb_implicit_functions, c(unwritten, elsewhere)), character()
  )
})

test_that("a listed name refuses the syntax that reaches it (#1843)", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dir <- withr::local_tempdir()
  path <- file.path(dir, "p.csv")
  writeLines(c("k", "101"), path)
  reader <- paste0("read_csv_auto(", DBI::dbQuoteString(con, path), ")")
  parquet <- DBI::dbQuoteString(con, file.path(dir, "t.parquet"))
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = 1:2, w = 3:4))
  DBI::dbExecute(con, paste("COPY t TO", parquet, "(FORMAT parquet)"))
  scalar <- paste0("(a := NULL, b := NULL) AS (SELECT first(k) FROM ", reader,
                   ")")
  tabular <- paste0("(a := NULL) AS TABLE SELECT k AS v FROM ", reader)
  cases <- list(
    list(name = "unpivot_list", body = scalar, refusal = "DuckDB call",
         sql = "UNPIVOT t ON v, w INTO NAME n VALUE e"),
    list(name = "list_filter", body = scalar, refusal = "DuckDB call",
         sql = "SELECT g, v, [e FOR e IN [v] IF e > 0] AS y FROM t"),
    list(name = "parquet_scan", body = tabular, refusal = "DuckDB call",
         sql = paste("SELECT v FROM", parquet)),
    # DuckDB's own view, named by a schema only its catalog holds.
    list(name = "duckdb_tables", body = tabular, refusal = "DuckDB view",
         sql = "SELECT table_name FROM information_schema.tables")
  )
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  # dbplyr binds a statement to learn its columns, so each input is captured
  # before the macro its statement reaches exists.
  inputs <- lapply(cases, function(case) {
    dplyr::tbl(con, dbplyr::sql(case$sql))
  })
  # With no such macro each statement keeps a typed plan.
  plans <- lapply(inputs, numeric_plan)
  for (i in seq_along(cases)) {
    case <- cases[[i]]
    writeLines(c("k", "101"), path)
    DBI::dbExecute(con, paste0("CREATE MACRO ", case$name, case$body))
    for (value in c("101", "source_tail", "absent")) {
      if (value == "absent") unlink(path) else writeLines(c("k", value), path)
      expect_error(numeric_plan(inputs[[i]]), case$refusal, fixed = TRUE,
                   class = "marginplyr_error", info = case$name)
      expect_duckdb_catalog_only()
    }
    DBI::dbExecute(con, paste(
      "DROP MACRO", if (identical(case$body, tabular)) "TABLE", case$name
    ))
  }
  expect_identical(lapply(inputs, numeric_plan), plans)
})

test_that("a statement DuckDB rewrites refuses beside a caller macro (#1843)", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("k", "101"), path)
  reader <- paste0("read_csv_auto(", DBI::dbQuoteString(con, path), ")")
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = 1:2))
  statements <- c(
    "SUMMARIZE t", "SHOW TABLES", "DESCRIBE t", "DESC t", "show tables",
    "SELECT column_name FROM (DESC SELECT v FROM t)",
    "SELECT column_name FROM (DESCRIBE SELECT g, v FROM t)",
    # DuckDB 2.0 accepts one after a set operator too.
    if (utils::packageVersion("duckdb") >= "2.0.0") {
      c("SELECT 'x' AS name UNION ALL SHOW TABLES",
        "SELECT 'x' AS column_name UNION ALL BY NAME DESC t")
    }
  )
  # DuckDB 1.5 stores such a statement in a view only as a subquery.
  for (i in seq_along(statements)) {
    DBI::dbExecute(con, paste0(
      "CREATE VIEW rewritten_", i, " AS SELECT * FROM (", statements[[i]], ")"
    ))
  }
  # dbplyr binds a statement to learn its columns, so the inputs are captured
  # before any macro exists.
  authored <- lapply(statements, function(sql) {
    dplyr::tbl(con, dbplyr::sql(sql))
  })
  stored <- lapply(seq_along(statements), function(i) {
    dplyr::tbl(con, paste0("rewritten_", i))
  })
  # Elsewhere than at a statement's start each word is an alias, a qualified
  # name or a sort direction.
  words <- lapply(c(
    "SELECT g, v AS show FROM t", "SELECT g, v AS describe FROM t",
    "SELECT g, v AS summarize FROM t", "SELECT g, v AS \"desc\" FROM t",
    "SELECT q.show AS v FROM (SELECT v AS show FROM t) q",
    "SELECT g, v FROM t ORDER BY (v) DESC",
    "SELECT g, v FROM t ORDER BY ALL DESC",
    paste("SELECT g AS name, v FROM t UNION ALL SELECT g, v FROM t",
          "ORDER BY name DESC"),
    paste("SELECT q.union AS v FROM (SELECT v AS \"union\" FROM t) q",
          "ORDER BY q.union DESC"),
    paste("SELECT v AS name, w AS union FROM (SELECT v, v AS w FROM t)",
          "ORDER BY name DESC")
  ), function(sql) dplyr::tbl(con, dbplyr::sql(sql)))
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  plans <- lapply(c(authored, stored, words), numeric_plan)
  # DuckDB chooses the functions these statements call, so a macro they reach
  # need not be one a list could name: `min` here, and one named for nothing.
  macros <- c(
    min = paste0("(a) AS (SELECT first(k) FROM ", reader, ")"),
    unrelated = "(a) AS a"
  )
  for (name in names(macros)) {
    DBI::dbExecute(con, paste0("CREATE MACRO ", name, macros[[name]]))
    for (value in c("101", "source_tail")) {
      writeLines(c("k", value), path)
      for (input in authored) {
        expect_error(numeric_plan(input), "DuckDB call", fixed = TRUE,
                     class = "marginplyr_error", info = name)
        expect_duckdb_catalog_only()
      }
      for (input in stored) {
        expect_error(numeric_plan(input), "DuckDB view", fixed = TRUE,
                     class = "marginplyr_error", info = name)
        expect_duckdb_catalog_only()
      }
    }
    DBI::dbExecute(con, paste("DROP MACRO", name))
  }
  # A statement that starts none of them is not refused for the macro.
  DBI::dbExecute(con, "CREATE MACRO unrelated(a) AS a")
  expect_identical(lapply(words, numeric_plan),
                   utils::tail(plans, length(words)))
  DBI::dbExecute(con, "DROP MACRO unrelated")
  expect_identical(lapply(c(authored, stored, words), numeric_plan), plans)
})
