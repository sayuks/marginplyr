# The measurement behind test-duckdb-implicit-reach.R: which function names the
# DuckDB under test binds from SQL that does not write them as a call head.
#
# `duckdb_implicit_functions` and `duckdb_value_functions` in
# `R/duckdb-csv-metadata.R` are written lists of such names, and a test that
# walks a list proves only the names already on it. This asks DuckDB instead.
# Every function name of the system catalog is shadowed by a caller macro whose
# body cannot bind, so binding a statement fails naming the first macro it
# reaches; dropping that macro and binding again names the next.

# Syntax that names no function it binds, over the tables
# `duckdb_reach_tables()` creates: `stable` for the forms every supported
# DuckDB binds and `varying` for the rest. An entry starting with a statement
# keyword is a whole statement; any other is one select-list expression.
# `parquet` is the quoted file path `duckdb_reach_tables()` returns. A form
# added here is measured without another edit.
duckdb_reach_probes <- function(parquet) {
  stable <- c(
    # Operators.
    "v + 1", "v - 1", "v * 2", "v / 2", "v % 2", "v // 2", "v ^ 2", "v ** 2",
    "g || 'b'", "v << 1", "v >> 1", "v & 1", "v | 1", "~ v", "@ v", "- v",
    "+ v", "g ^@ 'a'", "l && l", "l @> l", "l <@ l", "[1.0] <-> [1.0]",
    "[1.0] <=> [1.0]",
    "v !", "v!", "v = 1", "v == 1", "v <> 1", "v != 1", "v < 1", "v <= 1",
    "v > 1", "v >= 1", "v IS DISTINCT FROM 1", "v IS NOT DISTINCT FROM 1",
    "v IS NULL", "v IS NOT NULL", "v ISNULL", "v NOTNULL", "(v = 1) IS TRUE",
    "(v = 1) IS NOT FALSE", "(v = 1) IS UNKNOWN", "v BETWEEN 0 AND 2",
    "v NOT BETWEEN 0 AND 2", "v IN (1, 2)", "v NOT IN (1, 2)", "v IN [1, 2]",
    "v NOT IN [1, 2]", "v IN l", "v IN (SELECT v FROM u)", "v = ANY ([1, 2])",
    "v = ALL ([1, 2])", "v > ANY (SELECT v FROM u)",
    "v = SOME (SELECT v FROM u)", "EXISTS (SELECT 1 FROM u)",
    "NOT EXISTS (SELECT 1 FROM u)", "v = 1 AND w = 2", "v = 1 OR w = 2",
    "NOT (v = 1)", "(SELECT max(z) FROM u)",
    "'a' IN g", "l = [1]", "d + v", "(v, w) = (1, 2)", "(v, w) < (1, 2)",
    "{'a': 1} = s", "s IS NULL", "v IS NOT TRUE", "EXISTS (FROM u)",
    "v > ALL (SELECT v FROM u)",
    "d BETWEEN DATE '2020-01-01' AND DATE '2020-12-31'",
    # Pattern matching.
    "g LIKE 'a'", "g NOT LIKE 'a'", "g ILIKE 'a'", "g NOT ILIKE 'a'",
    "g GLOB 'a'", "g SIMILAR TO 'a'",
    "g NOT SIMILAR TO 'a'", "g LIKE 'a' ESCAPE '$'",
    "g NOT LIKE 'a' ESCAPE '$'", "g ILIKE 'a' ESCAPE '$'",
    "g NOT ILIKE 'a' ESCAPE '$'", "g ~ 'a'", "g !~ 'a'",
    "g ~~ 'a'", "g !~~ 'a'", "g ~~* 'a'", "g !~~* 'a'",
    "g ~~~ 'a'",
    # Casts and conditionals.
    "v::VARCHAR", "CAST(v AS VARCHAR)", "TRY_CAST(g AS INTEGER)",
    "l::VARCHAR[]", "v::DECIMAL(5, 2)", "ts::DATE", "g::BLOB", "v::BOOLEAN",
    "x::INTEGER", "CASE WHEN v = 1 THEN 'a' ELSE 'b' END",
    "CASE v WHEN 1 THEN 'a' END", "COALESCE(v, 0)", "NULLIF(v, 0)",
    "g COLLATE NOCASE", "g COLLATE NOCASE = 'A'", "g COLLATE NOACCENT",
    "g COLLATE C",
    "NULL::INTEGER", "'1 day'::INTERVAL", "ts::TIMESTAMPTZ",
    # Constructors and literals.
    "[v]", "[v, w]", "ARRAY[v, w]", "[]", "{'a': v}", "{a: v}",
    "{'a': v, 'b': w}", "(v, w)", "ROW(v, w)", "MAP {'a': v}", "MAP {}",
    "{'a': [v]}", "[e + 1 FOR e IN l]", "[e FOR e IN l IF e > 1]",
    "[e + i FOR e, i IN l]", "DATE '2020-01-02'",
    "TIMESTAMP '2020-01-02 03:04:05'", "TIME '03:04:05'",
    "TIMESTAMPTZ '2020-01-02 03:04:05+00'", "INTEGER '1'", "'a'::VARCHAR",
    "INTERVAL 1 DAY", "INTERVAL '1' DAY", "INTERVAL (v) DAY",
    "INTERVAL '1 day'", "INTERVAL (x) DAY", "INTERVAL 1 YEAR",
    "INTERVAL (v) YEAR", "INTERVAL (v) MONTH", "INTERVAL (v) WEEK",
    "INTERVAL (v) HOUR", "INTERVAL (v) MINUTE", "INTERVAL (v) SECOND",
    "INTERVAL (v) MILLISECOND", "INTERVAL (v) MICROSECOND",
    "INTERVAL (v) QUARTER", "INTERVAL (v) DECADE", "INTERVAL (v) CENTURY",
    "INTERVAL (v) MILLENNIUM", "INTERVAL (v) DAYS", "INTERVAL (v) YEARS",
    "INTERVAL 1 DAYS", "d + INTERVAL 1 DAY", "d - INTERVAL (v) MONTH",
    "ts - ts", "d - d", "E'a\\nb'", "$$a$$", "1e3",
    "1_000", "'\\x41'::BLOB", "TRUE", "NULL", "-1", "1.5e-3", ".5",
    # Subscripts and field access.
    "l[1]", "l[1:2]", "l[1:2:1]", "l[:2]", "l[2:]", "l[-1]", "l[-1:]", "s.a",
    "s['a']", "(s).a", "m['k']", "g[1]", "g[1:2]", "s.*", "t.s.a",
    "[1, 2][1]", "{'a': 1}.a", "l[v]", "m[g]",
    # Functions written with their own syntax.
    paste0("EXTRACT(", c(
      "year", "month", "day", "decade", "century", "millennium", "quarter",
      "week", "dow", "doy", "isodow", "isoyear", "era", "julian", "'year'"
    ), " FROM d)"),
    paste0("EXTRACT(", c(
      "epoch", "hour", "minute", "second", "millisecond", "microsecond"
    ), " FROM ts)"),
    "POSITION('a' IN g)", "SUBSTRING(g FROM 1 FOR 2)", "SUBSTRING(g FROM 1)",
    "SUBSTRING(g, 1, 2)",
    "TRIM(g)", "TRIM(BOTH 'x' FROM g)",
    "TRIM(LEADING 'x' FROM g)", "TRIM(TRAILING 'x' FROM g)",
    "TRIM(LEADING FROM g)", "TRIM(TRAILING FROM g)", "TRIM(BOTH FROM g)",
    "TRIM('x' FROM g)", "TRIM(FROM g)", "GREATEST(v, w)", "LEAST(v, w)",
    "COLUMNS(*)", "COLUMNS('v')",
    "COLUMNS(lambda c: c LIKE 'v')", "COLUMNS(* EXCLUDE (g))", "UNNEST(l)",
    "UNNEST(s)", "UNNEST(l, recursive := true)",
    "list_transform(l, lambda e: e + 1)",
    "list_filter(l, lambda e: e > 1)",
    "list_reduce(l, lambda a, b: a + b)",
    "TRY(v + 1)", "#1", "#2 + 1",
    # Windows.
    "sum(v) OVER ()", "COUNT(*) OVER ()", "sum(v) OVER (PARTITION BY g)",
    "sum(v) OVER (ORDER BY w)",
    "sum(v) OVER (ORDER BY w ROWS BETWEEN 1 PRECEDING AND CURRENT ROW)",
    "sum(v) OVER (ORDER BY w RANGE BETWEEN 1 PRECEDING AND 1 FOLLOWING)",
    paste("sum(v) OVER (ORDER BY d RANGE BETWEEN INTERVAL 1 DAY PRECEDING",
          "AND CURRENT ROW)"),
    "sum(v) OVER (ORDER BY w GROUPS BETWEEN 1 PRECEDING AND CURRENT ROW)",
    "sum(v) OVER (ORDER BY w ROWS UNBOUNDED PRECEDING EXCLUDE TIES)",
    "sum(v) OVER (ORDER BY w ROWS BETWEEN v PRECEDING AND w FOLLOWING)",
    "lag(v) OVER (ORDER BY w)", "lag(v IGNORE NULLS) OVER (ORDER BY w)",
    "first_value(v RESPECT NULLS) OVER (ORDER BY w)",
    "row_number() OVER (ORDER BY w)", "rank() OVER (ORDER BY w NULLS FIRST)",
    "nth_value(v, 1) OVER (ORDER BY w)", "ntile(2) OVER (ORDER BY w)",
    "sum(DISTINCT v) OVER (PARTITION BY g)",
    "sum(v ORDER BY w) OVER (PARTITION BY g)",
    "sum(v) FILTER (WHERE w > 0) OVER (PARTITION BY g)",
    "fill(v) OVER (ORDER BY w)", "cume_dist() OVER (ORDER BY w)",
    # Aggregates.
    paste("SELECT g,", c(
      "COUNT(*)", "COUNT()", "count(DISTINCT v)",
      "count(v) FILTER (WHERE v > 0)", "COUNT(*) FILTER (WHERE v > 0)",
      "max(v) FILTER (v > 0)", "sum(v ORDER BY w)",
      "string_agg(j, ',' ORDER BY j)", "first(v ORDER BY w DESC)",
      "percentile_cont(0.5) WITHIN GROUP (ORDER BY v)",
      "percentile_disc(0.5) WITHIN GROUP (ORDER BY v)",
      "mode() WITHIN GROUP (ORDER BY v)", "list(v ORDER BY w)",
      "ARRAY_AGG(v)", "GROUPING(g)"
    ), "AS y FROM t GROUP BY g"),
    # Grouping, ordering and limits.
    "SELECT g, sum(v) AS y FROM t GROUP BY ALL",
    "SELECT g, sum(v) AS y FROM t GROUP BY ROLLUP (g)",
    "SELECT g, sum(v) AS y FROM t GROUP BY CUBE (g, w)",
    "SELECT g, sum(v) AS y FROM t GROUP BY GROUPING SETS ((g), ())",
    "SELECT g, sum(v) AS y FROM t GROUP BY g HAVING sum(v) > 0",
    "SELECT g, v FROM t GROUP BY g, v ORDER BY sum(w)",
    "SELECT g AS k, v FROM t GROUP BY k, v", "SELECT g, v FROM t ORDER BY ALL",
    "SELECT g, v FROM t ORDER BY v DESC NULLS LAST",
    "SELECT g, v FROM t ORDER BY g COLLATE NOCASE",
    "SELECT g, v FROM t ORDER BY 1", "SELECT g, v FROM t LIMIT 1",
    "SELECT g, v FROM t LIMIT 1 OFFSET 1", "SELECT g, v FROM t LIMIT 10%",
    "SELECT g, v FROM t LIMIT 10 PERCENT",
    "SELECT g, v FROM t LIMIT (SELECT 1)",
    "SELECT g, v FROM t FETCH FIRST 1 ROWS ONLY",
    "SELECT g, v FROM t USING SAMPLE 10%",
    "SELECT g, v FROM t USING SAMPLE 1 ROWS",
    "SELECT g, v FROM t USING SAMPLE reservoir(1 ROWS) REPEATABLE (1)",
    "SELECT g, v FROM t TABLESAMPLE bernoulli(10%)",
    "SELECT g, v FROM t TABLESAMPLE system(10 PERCENT)",
    "SELECT DISTINCT g, v FROM t", "SELECT DISTINCT ON (g) g, v FROM t",
    "SELECT g, v FROM t WHERE v = 1", "SELECT 1 AS y WHERE 1 = 1",
    paste("SELECT g, v FROM t",
          "QUALIFY row_number() OVER (PARTITION BY g ORDER BY v) = 1"),
    paste("SELECT g, sum(v) OVER win AS y FROM t",
          "WINDOW win AS (PARTITION BY g ORDER BY v)"),
    paste("SELECT g, v, row_number() OVER",
          "(PARTITION BY (g) ORDER BY (v)) AS y FROM t"),
    # Set operations and row sources.
    paste("SELECT g, v FROM t", c(
      "UNION", "UNION ALL", "UNION BY NAME", "UNION ALL BY NAME", "INTERSECT",
      "EXCEPT", "INTERSECT ALL", "EXCEPT ALL"
    ), "SELECT g, v FROM u"),
    "VALUES ('a', 1), ('b', 2)", "FROM t", "FROM t SELECT g, v", "TABLE t",
    "SELECT g, v FROM (FROM t)", "SELECT g, v FROM main.t",
    "SELECT g, v FROM memory.main.t", "SELECT * FROM (SELECT 1) q",
    "SELECT * FROM (VALUES (1), (2)) AS q(e)", "SELECT * FROM t AS q(a, b)",
    "SELECT * FROM range(3)", "SELECT * FROM generate_series(1, 3)",
    "SELECT * FROM unnest([1, 2])", "SELECT UNNEST([1, 2]) AS y",
    "SELECT #1, #2 FROM t", "SELECT a: g, b: v FROM t",
    "SELECT g, v, 'x' AS \"y z\" FROM t",
    # Stars.
    "SELECT * FROM t", "SELECT t.* FROM t", "SELECT * EXCLUDE (g) FROM t",
    "SELECT * REPLACE (v + 1 AS v) FROM t", "SELECT * RENAME (v AS vv) FROM t",
    "SELECT * LIKE 'v%' FROM t", "SELECT * ILIKE 'v%' FROM t",
    "SELECT * NOT LIKE 'g%' FROM t", "SELECT * GLOB 'v*' FROM t",
    "SELECT * SIMILAR TO 'v' FROM t", "SELECT t.* EXCLUDE (g) FROM t",
    "SELECT COLUMNS(*) + 1 FROM (SELECT v, w FROM t)",
    "SELECT min(COLUMNS(*)) FROM t",
    "SELECT g, v FROM t WHERE COLUMNS(*) IS NOT NULL",
    # Joins and subqueries.
    "SELECT t.g, t.v, u.z FROM t JOIN u ON t.g = u.g",
    "SELECT g, v, z FROM t JOIN u USING (g, v)",
    "SELECT * FROM t NATURAL JOIN u",
    "SELECT t.g, u.z FROM t LEFT JOIN u ON t.v = u.v",
    "SELECT t.g, u.z FROM t FULL OUTER JOIN u ON t.v = u.v",
    "SELECT t.g, u.z FROM t CROSS JOIN u", "SELECT t.g, u.z FROM t, u",
    "SELECT t.g FROM t SEMI JOIN u ON t.g = u.g",
    "SELECT t.g FROM t ANTI JOIN u ON t.g = u.g",
    "SELECT t.g, u.z FROM t ASOF JOIN u ON t.g = u.g AND t.v >= u.v",
    "SELECT t.g, u.z FROM t ASOF LEFT JOIN u USING (g, v)",
    "SELECT * FROM t POSITIONAL JOIN u",
    "SELECT t.g, q.z FROM t, LATERAL (SELECT z FROM u WHERE u.v = t.v) q",
    paste("SELECT t.g, q.z FROM t LEFT JOIN LATERAL",
          "(SELECT z FROM u WHERE u.v = t.v) q ON TRUE"),
    "SELECT t.g, q.e FROM t, UNNEST(t.l) AS q(e)",
    "SELECT g, e FROM t CROSS JOIN UNNEST(l) AS q(e)",
    "SELECT g, e FROM t, (SELECT UNNEST(l) AS e) q",
    "SELECT g, v FROM t WHERE v IN (SELECT v FROM u)",
    "SELECT g, v FROM t WHERE EXISTS (SELECT 1 FROM u WHERE u.v = t.v)",
    "SELECT g, (SELECT max(z) FROM u WHERE u.g = t.g) AS y FROM t",
    "SELECT g, v FROM t WHERE (g, v) IN (SELECT g, v FROM u)",
    "SELECT g, ARRAY(SELECT z FROM u) AS y FROM t",
    "SELECT * FROM t t1, t t2 WHERE t1.v = t2.v",
    "SELECT g, v FROM t WHERE v IN (FROM u SELECT v)",
    "SELECT g, v FROM t WHERE g = ANY (SELECT g FROM u)",
    "SELECT g, v FROM t WHERE v = (SELECT min(v) FROM u)",
    "SELECT g, (SELECT list(z) FROM u) AS y FROM t",
    "SELECT * FROM unnest([1, 2]) WITH ORDINALITY",
    "SELECT g, v FROM t LIMIT ALL",
    "SELECT g, v FROM t USING SAMPLE 50 PERCENT (bernoulli)",
    "SELECT COLUMNS('v|w')::VARCHAR FROM t",
    "SELECT g, count(*) AS y FROM t GROUP BY g ORDER BY count(*) DESC",
    "SELECT g, v, v::VARCHAR COLLATE NOCASE AS y FROM t ORDER BY y",
    "SELECT g, v FROM t ORDER BY g COLLATE NOCASE.NOACCENT",
    "SELECT DISTINCT g COLLATE NOCASE AS y FROM t",
    "SELECT g, v FROM t WHERE g COLLATE NOACCENT LIKE 'a'",
    "SELECT min(g COLLATE NOCASE) AS y FROM t",
    # Common table expressions.
    "WITH c AS (SELECT g, v FROM t) SELECT * FROM c",
    "WITH c(a, b) AS (SELECT g, v FROM t) SELECT * FROM c",
    "WITH c AS MATERIALIZED (SELECT g, v FROM t) SELECT * FROM c",
    "WITH c AS NOT MATERIALIZED (SELECT g, v FROM t) SELECT * FROM c",
    paste("WITH RECURSIVE c(n) AS (SELECT 1 UNION ALL SELECT n + 1 FROM c",
          "WHERE n < 3) SELECT * FROM c"),
    paste("WITH RECURSIVE c(n) USING KEY (n) AS (SELECT 1 UNION SELECT n + 1",
          "FROM c WHERE n < 3) SELECT * FROM c"),
    # Statements DuckDB rewrites into queries of its own.
    "PIVOT t ON g USING sum(v)", "PIVOT t ON g USING sum(v) GROUP BY w",
    "PIVOT t ON g USING sum(v) AS total, count(*) AS n", "PIVOT t ON g",
    "PIVOT t ON g IN ('a') USING first(v)", "PIVOT t ON g, w USING sum(v)",
    "PIVOT t ON g || '_' || w USING sum(v)", "PIVOT t USING sum(v) GROUP BY g",
    "SELECT * FROM t PIVOT (sum(v) FOR g IN ('a', 'b'))",
    paste("SELECT * FROM t PIVOT",
          "(sum(v) AS total FOR g IN ('a' AS first) GROUP BY w)"),
    "UNPIVOT t ON v, w INTO NAME n VALUE e",
    "UNPIVOT t ON COLUMNS('^[vw]$') INTO NAME n VALUE e",
    "UNPIVOT t ON (v, w) AS first INTO NAME n VALUE e1, e2",
    "SELECT * FROM t UNPIVOT (e FOR n IN (v, w))",
    "SELECT * FROM t UNPIVOT INCLUDE NULLS (e FOR n IN (v, w))",
    "SUMMARIZE t", "SUMMARIZE SELECT g, v FROM t", "SUMMARIZE u",
    "DESCRIBE t", "DESCRIBE SELECT g, v FROM t", "DESCRIBE TABLE t",
    "SHOW t", "SHOW TABLES", "SHOW ALL TABLES", "SHOW DATABASES",
    "SHOW TABLES FROM memory", "SHOW TABLES FROM memory.main", "SHOW SCHEMAS",
    "SHOW VARIABLES", "SHOW ALL", "DESCRIBE",
    "SELECT * FROM (SUMMARIZE t)", "SELECT * FROM (DESCRIBE t)",
    "SELECT * FROM (SHOW TABLES)", "FROM (PIVOT t ON g USING sum(v))",
    "SELECT * FROM (SELECT g, v FROM t) PIVOT (count(*) FOR g IN ('a'))",
    "DESCRIBE DATABASES", "SHOW TABLE t", "DESCRIBE SELECT * FROM t",
    "SUMMARIZE TABLE t", "SHOW memory.main.t", "DESCRIBE main.t", "DESC t",
    "DESC TABLES", "DESC", "DESC SELECT g, v FROM t"
  )
  # Forms one supported DuckDB version, or one with an extension loaded,
  # binds and another refuses.
  varying <- c(
    "j -> 'a'", "j ->> 'a'", "v <<= 1", "v >>= 1", "g NOT GLOB 'a'",
    "g ~* 'a'", "g !~* 'a'", "ts AT TIME ZONE 'UTC'",
    "OVERLAY(g PLACING 'x' FROM 1)", "OVERLAY(g PLACING 'x' FROM 1 FOR 1)",
    "COLUMNS(c -> c LIKE 'v')", "list_transform(l, e -> e + 1)",
    "list_filter(l, e -> e > 1)", "list_reduce(l, (a, b) -> a + b)",
    "SHOW search_path", "SELECT 'x' AS name UNION ALL SHOW TABLES",
    "SELECT 'x' AS column_name UNION ALL BY NAME DESC t",
    # A replacement scan of the Parquet file `duckdb_reach_tables()` wrote.
    paste0("SELECT * FROM ", parquet)
  )
  list(stable = stable, varying = varying)
}

# Create the tables every probe reads and a Parquet copy of one of them in
# `dir`. Returns the quoted path the Parquet probe scans.
duckdb_reach_tables <- function(con, dir) {
  DBI::dbExecute(con, paste(
    "CREATE TABLE t AS SELECT 'a' AS g, 1 AS v, 2 AS w,",
    "DATE '2020-01-02' AS d,",
    "TIMESTAMP '2020-01-02 03:04:05' AS ts, [1, 2, 3] AS l, {'a': 1} AS s,",
    "MAP {'k': 1} AS m, '{\"a\":1}' AS j, 1.5 AS x"
  ))
  DBI::dbExecute(con, "CREATE TABLE u AS SELECT 'a' AS g, 1 AS v, 5 AS z")
  parquet <- DBI::dbQuoteString(con, file.path(dir, "u.parquet"))
  # A build without the Parquet extension leaves the probe unbound.
  try(DBI::dbExecute(con, paste("COPY u TO", parquet, "(FORMAT parquet)")),
      silent = TRUE)
  as.character(parquet)
}

# The statement a probe stands for.
duckdb_reach_statement <- function(probe) {
  whole <- grepl(paste0(
    "^(SELECT|FROM|WITH|VALUES|PIVOT|UNPIVOT|SUMMARIZE|DESCRIBE|DESC|SHOW|",
    "TABLE)( |$)"
  ), probe)
  ifelse(whole, probe, paste("SELECT g, v,", probe, "AS y FROM t"))
}

# The zero-row statement dbplyr renders over authored SQL, which is the text a
# Margin verb's typed proxy binds.
duckdb_reach_rendered <- function(sql) {
  paste0("SELECT q01.* FROM (", sql, ") q01 LIMIT 0")
}

# The condition message of binding `sql` as that statement, or `NULL` where it
# binds. Converting the empty result to R can warn about a column type, which
# is not what is asked.
duckdb_reach_bind <- function(con, sql) {
  tryCatch({
    suppressWarnings(DBI::dbGetQuery(con, duckdb_reach_rendered(sql)))
    NULL
  }, error = function(cnd) conditionMessage(cnd))
}

# Define a caller macro `name`, scalar or table, whose body names something
# dropped at once: DuckDB checks the body when the macro is created and again
# when a statement binds it, so binding it afterwards fails naming `marker`.
# A table macro selects from a dropped table. A scalar macro calls a dropped
# macro, or with `aggregate` one aggregate over that call, which is what
# DuckDB 2.0 requires of a macro standing where a window function does; each
# binds in places the other does not. FALSE where DuckDB refuses the
# definition.
duckdb_reach_shadow <- function(con, name, table, marker, aggregate = FALSE) {
  if (table) {
    DBI::dbExecute(con, paste0("CREATE TABLE ", marker, "(r INTEGER)"))
    body <- paste0("TABLE SELECT r FROM ", marker)
  } else {
    DBI::dbExecute(con, paste0("CREATE MACRO ", marker, "() AS 1"))
    body <- paste0(marker, "()")
    if (aggregate) body <- paste0("system.main.max(", body, ")")
  }
  created <- tryCatch({
    DBI::dbExecute(con, paste0(
      "CREATE MACRO ", DBI::dbQuoteIdentifier(con, name),
      "(a := NULL, b := NULL, c := NULL, d := NULL) AS ", body
    ))
    TRUE
  }, error = function(cnd) FALSE)
  DBI::dbExecute(con, paste("DROP", if (table) "TABLE" else "MACRO", marker))
  created
}

duckdb_reach_unshadow <- function(con, name, table) {
  DBI::dbExecute(con, paste(
    "DROP MACRO", if (table) "TABLE", DBI::dbQuoteIdentifier(con, name)
  ))
  invisible(NULL)
}

# Shadow every system function name, then bind each probe, each system view,
# every function name and keyword as a bare word, and each of those as a call
# head. The probes and views are bound under both macro bodies. Returns the
# probes with whether each binds while no caller macro exists; the probe
# statements whose binding stopped, under both bodies, at an error that names
# no macro and is not DuckDB declining to expand one (`blocked`); and one row
# per statement and macro it reached: `written` says whether the statement
# spells that name as a call head, which a call-head statement's own name
# never is, its macro having stepped aside. `dir` is a directory the caller
# removes. Leaves no macro behind.
duckdb_reach_sweep <- function(con, dir) {
  probes <- duckdb_reach_probes(duckdb_reach_tables(con, dir))
  functions <- DBI::dbGetQuery(con, paste(
    "SELECT function_name AS name,",
    "function_type IN ('table', 'table_macro') AS is_table,",
    "list_sort(list_distinct(list(len(parameters)) FILTER (",
    "WHERE function_type IN ('macro', 'table_macro')))) AS arities",
    "FROM duckdb_functions() WHERE database_name = 'system'",
    "AND function_type <> 'pragma' GROUP BY ALL ORDER BY ALL"
  ))
  keywords <- DBI::dbGetQuery(
    con, "SELECT keyword_name FROM duckdb_keywords()"
  )$keyword_name
  # DuckDB's own views are stored queries over its functions, named with and
  # without their schema.
  views <- DBI::dbGetQuery(con, paste(
    "SELECT schema_name, view_name FROM duckdb_views()",
    "WHERE internal ORDER BY ALL"
  ))
  views <- paste("SELECT * FROM", unique(c(
    paste(views$schema_name, views$view_name, sep = "."), views$view_name
  )))
  probes <- lapply(probes, function(forms) {
    statements <- duckdb_reach_statement(forms)
    data.frame(probe = forms, sql = statements, live = vapply(
      statements, function(sql) is.null(duckdb_reach_bind(con, sql)),
      logical(1), USE.NAMES = FALSE
    ))
  })
  views <- views[vapply(views, function(sql) {
    is.null(duckdb_reach_bind(con, sql))
  }, logical(1))]
  corpus <- c(unlist(lapply(probes, function(forms) forms$sql[forms$live]),
                     use.names = FALSE), views)

  functions$marker <- paste0("marginplyr_reach_", seq_len(nrow(functions)))
  shadowed <- logical(nrow(functions))
  aggregate <- FALSE
  shadow <- function(row, on) {
    if (on) {
      shadowed[[row]] <<- duckdb_reach_shadow(
        con, functions$name[[row]], functions$is_table[[row]],
        functions$marker[[row]], aggregate
      )
    } else if (shadowed[[row]]) {
      duckdb_reach_unshadow(con, functions$name[[row]],
                            functions$is_table[[row]])
      shadowed[[row]] <<- FALSE
    }
  }
  shadow_all <- function(on) {
    for (row in if (on) seq_len(nrow(functions)) else which(shadowed)) {
      shadow(row, on)
    }
  }
  on.exit(shadow_all(FALSE), add = TRUE)

  # The rows of `functions` binding `sql` reaches, outermost first, with
  # `stopped` set where the binding ended in an error that names no macro.
  # Where DuckDB names a macro it met and would not expand -- one standing
  # where only a function may, or given arguments it does not take -- that
  # macro steps aside unrecorded and the binding goes on.
  declined <- c(
    "Macro (.+)\\(\\) ", "Error: (.+) is not an? [a-z]+ function",
    "Function \"(.+)\" is a Macro Function"
  )
  declined_rows <- function(message) {
    names <- unlist(lapply(declined, function(pattern) {
      hit <- regmatches(message, regexec(pattern, message))[[1L]]
      if (length(hit)) hit[[2L]]
    }))
    which(functions$name %in% names & shadowed)
  }
  reach <- function(sql) {
    found <- aside <- integer()
    stopped <- FALSE
    repeat {
      message <- duckdb_reach_bind(con, sql)
      if (is.null(message)) break
      marker <- regmatches(message, regexpr("marginplyr_reach_[0-9]+", message))
      if (!length(marker)) {
        rows <- declined_rows(message)
        stopped <- !length(rows)
        if (stopped) break
        aside <- c(aside, rows)
        for (row in rows) shadow(row, FALSE)
        next
      }
      row <- match(marker, functions$marker)
      found <- c(found, row)
      shadow(row, FALSE)
    }
    for (row in c(found, aside)) shadow(row, TRUE)
    structure(found, stopped = stopped, message = message)
  }
  pairs <- list()
  stops <- list()
  record <- function(kind, sql) {
    rows <- reach(sql)
    if (attr(rows, "stopped")) {
      stops[[length(stops) + 1L]] <<- data.frame(
        sql = sql, aggregate = aggregate, message = attr(rows, "message")
      )
    }
    if (length(rows)) {
      pairs[[length(pairs) + 1L]] <<- data.frame(
        kind = kind, sql = sql, reached = functions$name[rows],
        table = functions$is_table[rows],
        written = vapply(functions$name[rows], function(name) {
          grepl("^[a-z0-9_]+$", name) &&
            grepl(paste0("(^|[^a-z0-9_])", name, "[(]"), tolower(sql))
        }, logical(1), USE.NAMES = FALSE)
      )
    }
  }
  aggregate <- TRUE
  shadow_all(TRUE)
  for (sql in corpus) record("corpus", sql)
  shadow_all(FALSE)
  aggregate <- FALSE
  shadow_all(TRUE)
  for (sql in corpus) record("corpus", sql)
  words <- unique(c(functions$name, keywords))
  words <- sort(words[grepl("^[a-z_][a-z0-9_]*$", words)])
  arguments <- function(count) {
    paste0("(", paste(seq_len(count), collapse = ", "), ")")
  }
  for (word in words) {
    record("bare", paste("SELECT", word, "AS y"))
    # A head reaches its own name first, so that macro steps aside. A system
    # macro is also called with each number of arguments it declares, which
    # is what expands its body.
    own <- which(functions$name == word)
    shadows <- own[shadowed[own]]
    for (row in shadows) shadow(row, FALSE)
    declared <- function(table) {
      unlist(functions$arities[own[functions$is_table[own] == table]])
    }
    scalar <- unique(c("()", "(1)", "(1, 2)", "(*)",
                       vapply(declared(FALSE), arguments, character(1))))
    tabular <- unique(c("(1)", vapply(declared(TRUE), arguments,
                                      character(1))))
    for (sql in c(paste0("SELECT ", word, scalar),
                  paste0("SELECT * FROM ", word, tabular))) {
      record("call", sql)
    }
    for (row in shadows) shadow(row, TRUE)
  }
  stops <- do.call(rbind, stops)
  list(probes = probes, stops = stops, blocked = intersect(
    stops$sql[stops$aggregate], stops$sql[!stops$aggregate]
  ), pairs = unique(do.call(rbind, pairs)))
}

# For each row of a sweep's `pairs`, with that macro as the only caller macro:
# whether the statement reaches it under either macro body (`confirmed`),
# whether the preflight read the catalog for the statement (`asked`), and
# whether it refused (`refused`).
# The preflight is asked as an operation asks it: of the authored text, then
# of the rendered statement. A probe DuckDB stores as a view is asked again
# through the view (`stored_confirmed`, `stored_refused`), whose definition
# DuckDB rewrites.
duckdb_reach_verdicts <- function(con, pairs) {
  pairs$confirmed <- pairs$asked <- pairs$refused <- NA
  pairs$stored_confirmed <- pairs$stored_refused <- NA
  statements <- unique(pairs$sql[pairs$kind == "corpus"])
  # DuckDB 1.5 stores some statements in a view only as a subquery.
  views <- vapply(seq_along(statements), function(i) {
    view <- paste0("marginplyr_reach_view_", i)
    tryCatch({
      DBI::dbExecute(con, paste0(
        "CREATE VIEW ", view, " AS SELECT * FROM (", statements[[i]], ")"
      ))
      view
    }, error = function(cnd) NA_character_)
  }, character(1))
  macros <- unique(pairs[c("reached", "table")])
  for (i in seq_len(nrow(macros))) {
    name <- macros$reached[[i]]
    table <- macros$table[[i]]
    rows <- which(pairs$reached == name & pairs$table == table)
    reached <- function(sql) {
      message <- duckdb_reach_bind(con, sql)
      !is.null(message) &&
        grepl("marginplyr_reach_alone", message, fixed = TRUE)
    }
    # The preflight reads the same catalog under either body, so it is asked
    # under the second alone.
    windowed <- logical(length(rows))
    if (!table) {
      duckdb_reach_shadow(con, name, table, "marginplyr_reach_alone", TRUE)
      windowed <- vapply(pairs$sql[rows], reached, logical(1))
      duckdb_reach_unshadow(con, name, table)
    }
    duckdb_reach_shadow(con, name, table, "marginplyr_reach_alone")
    snapshot <- duckdb_metadata_catalog(con)
    for (row in rows) {
      # As an operation holds it: nothing until something asks to read it.
      asked <- new.env(parent = emptyenv())
      asked$read <- FALSE
      catalog <- function(read = TRUE) {
        if (read) asked$read <- TRUE
        if (asked$read) snapshot
      }
      refused <- function(sql) {
        tryCatch({
          validate_duckdb_csv_metadata(sql, catalog)
          validate_duckdb_csv_metadata(duckdb_reach_rendered(sql), catalog)
          FALSE
        }, marginplyr_error = function(cnd) TRUE)
      }
      pairs$confirmed[[row]] <- reached(pairs$sql[[row]]) ||
        windowed[[match(row, rows)]]
      pairs$refused[[row]] <- refused(pairs$sql[[row]])
      pairs$asked[[row]] <- asked$read
      view <- views[match(pairs$sql[[row]], statements)]
      if (is.na(view)) next
      pairs$stored_confirmed[[row]] <- reached(paste("SELECT * FROM", view))
      # As validate_duckdb_csv_query() spells a captured table path.
      pairs$stored_refused[[row]] <- refused(paste("TABLE", view))
    }
    duckdb_reach_unshadow(con, name, table)
  }
  pairs
}
