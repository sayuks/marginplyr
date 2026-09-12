#!/usr/bin/env Rscript

# Controlled measurements for investigation/2026-09-12-performance-scalability-testing.md.
# This is an investigation harness, not a benchmark gate. It deliberately
# records structural metrics beside noisy elapsed/allocation observations.

pkgload::load_all(".", quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
section <- if (length(args) == 0L) "all" else args[[1L]]

make_data <- function(n, dimensions) {
  rows <- seq_len(n) - 1L
  data <- as.data.frame(lapply(
    seq_len(dimensions),
    function(i) ifelse((rows %/% (2^((i - 1L) %% 16L))) %% 2L == 0L, "a", "b")
  ))
  names(data) <- paste0("d", seq_len(dimensions))
  data$value <- (rows %% 97L) + 1L
  data
}

make_spec <- function(kind, dimensions) {
  eval(rlang::call2(kind, !!!rlang::syms(paste0("d", seq_len(dimensions)))))
}

make_repeated_spec <- function(occurrences) {
  do.call(grouping_sets, rep(list(grouping_set(d1)), occurrences))
}

count_fixed <- function(text, pattern) {
  starts <- gregexpr(pattern, text, fixed = TRUE)[[1L]]
  sum(starts > 0L)
}

elapsed_summary <- function(run, repetitions = 5L) {
  invisible(run())
  times <- numeric(repetitions)
  for (i in seq_len(repetitions)) {
    gc(FALSE)
    started <- Sys.time()
    invisible(run())
    times[[i]] <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  }
  c(
    elapsed_median = stats::median(times),
    elapsed_min = min(times),
    elapsed_max = max(times)
  )
}

allocated_bytes <- function(run) {
  path <- tempfile("marginplyr-rprofmem-")
  on.exit(unlink(path), add = TRUE)
  gc(FALSE)
  utils::Rprofmem(path)
  invisible(run())
  utils::Rprofmem(NULL)
  lines <- readLines(path, warn = FALSE)
  sizes <- suppressWarnings(as.numeric(sub(" .*", "", lines)))
  sum(sizes, na.rm = TRUE)
}

print_result <- function(name, rows) {
  cat("\n## ", name, "\n", sep = "")
  print(do.call(rbind, rows), row.names = FALSE, digits = 6)
}

run_local_rows <- function() {
  rows <- list()
  for (n in c(1000L, 4000L, 16000L, 64000L)) {
    data <- make_data(n, 3L)
    spec <- make_spec("rollup", 3L)
    operations <- list(
      summary = function() summarize_with_margins(
        data,
        total = sum(value),
        .grouping = spec,
        .margin_label = NULL
      ),
      expand = function() expand_with_margins(
        data,
        .grouping = spec,
        .margin_label = NULL
      ),
      nest = function() nest_with_margins(
        data,
        .grouping = spec,
        .margin_label = NULL
      ),
      nest_by = function() nest_by_with_margins(
        data,
        .grouping = spec,
        .margin_label = NULL
      )
    )
    for (operation in names(operations)) {
      run <- operations[[operation]]
      timing <- elapsed_summary(run)
      result <- run()
      payload_rows <- if ("data" %in% names(result)) {
        sum(vapply(result$data, nrow, integer(1)))
      } else {
        NA_integer_
      }
      rows[[length(rows) + 1L]] <- data.frame(
        operation = operation,
        source_rows = n,
        sets = 4L,
        result_rows = nrow(result),
        payload_rows = payload_rows,
        result_bytes = as.numeric(object.size(result)),
        allocated_bytes = allocated_bytes(run),
        t(timing),
        check.names = FALSE
      )
    }
  }
  print_result("local_rows", rows)
}

run_plan_cube <- function() {
  rows <- list()
  for (dimensions in 1:10) {
    data <- make_data(1L, dimensions)
    spec <- make_spec("cube", dimensions)
    run <- function() inspect_grouping(data, .grouping = spec, .format = "list")
    timing <- elapsed_summary(run)
    result <- run()
    rows[[dimensions]] <- data.frame(
      dimensions = dimensions,
      sets = nrow(result),
      result_bytes = as.numeric(object.size(result)),
      allocated_bytes = allocated_bytes(run),
      t(timing),
      check.names = FALSE
    )
  }
  print_result("plan_cube", rows)

  row_counts <- c(1L, 1000L, 100000L, 1000000L)
  invariant <- lapply(row_counts, function(n) {
    data <- make_data(n, 4L)
    spec <- make_spec("cube", 4L)
    run <- function() inspect_grouping(data, .grouping = spec, .format = "list")
    timing <- elapsed_summary(run)
    data.frame(source_rows = n, sets = nrow(run()), t(timing), check.names = FALSE)
  })
  print_result("inspect_row_invariance", invariant)
}

run_local_cube <- function() {
  rows <- list()
  for (dimensions in 1:8) {
    data <- make_data(256L, dimensions)
    spec <- make_spec("cube", dimensions)
    operations <- list(
      summary = function() summarize_with_margins(
        data,
        total = sum(value),
        .grouping = spec,
        .margin_label = NULL
      ),
      expand = function() expand_with_margins(
        data,
        .grouping = spec,
        .margin_label = NULL
      )
    )
    for (operation in names(operations)) {
      run <- operations[[operation]]
      timing <- elapsed_summary(run, repetitions = 3L)
      result <- run()
      rows[[length(rows) + 1L]] <- data.frame(
        operation = operation,
        dimensions = dimensions,
        sets = 2^dimensions,
        result_rows = nrow(result),
        result_bytes = as.numeric(object.size(result)),
        allocated_bytes = allocated_bytes(run),
        t(timing),
        check.names = FALSE
      )
    }
  }
  print_result("local_cube", rows)
}

run_occurrences <- function() {
  data <- make_data(256L, 1L)
  rows <- list()
  for (occurrences in c(1L, 4L, 16L, 64L, 128L)) {
    spec <- make_repeated_spec(occurrences)
    operations <- list(
      summary = function() summarize_with_margins(
        data,
        total = sum(value),
        .grouping = spec,
        .duplicates = "keep",
        .id = "set_id",
        .margin_label = NULL
      ),
      expand = function() expand_with_margins(
        data,
        .grouping = spec,
        .duplicates = "keep",
        .id = "set_id",
        .margin_label = NULL
      )
    )
    for (operation in names(operations)) {
      run <- operations[[operation]]
      timing <- elapsed_summary(run, repetitions = 3L)
      result <- run()
      rows[[length(rows) + 1L]] <- data.frame(
        operation = operation,
        occurrences = occurrences,
        result_rows = nrow(result),
        result_bytes = as.numeric(object.size(result)),
        allocated_bytes = allocated_bytes(run),
        t(timing),
        check.names = FALSE
      )
    }
  }
  print_result("duplicate_occurrences", rows)
}

sqlite_plan_metrics <- function(con, sql, table) {
  plan <- DBI::dbGetQuery(con, paste("EXPLAIN QUERY PLAN", sql))
  detail <- paste(plan$detail, collapse = "\n")
  c(
    base_scans = count_fixed(detail, paste("SCAN", table)),
    cte_scans = 0L,
    plan_nodes = nrow(plan)
  )
}

duckdb_plan_metrics <- function(con, sql, table) {
  # DuckDB's text tree elides wide UNIONs after 16 branches. JSON keeps every
  # node, so scan counts remain exact at the upper end of the series.
  plan <- DBI::dbGetQuery(con, paste("EXPLAIN (FORMAT JSON)", sql))
  detail <- paste(plan$explain_value, collapse = "\n")
  c(
    base_scans = count_fixed(detail, "SEQ_SCAN"),
    cte_scans = count_fixed(detail, "CTE_SCAN"),
    plan_nodes = count_fixed(detail, "│")
  )
}

run_sql_cube <- function() {
  rows <- list()
  backends <- list(
    sqlite = list(
      connect = function() DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
      disconnect = function(con) DBI::dbDisconnect(con),
      explain = sqlite_plan_metrics
    ),
    duckdb = list(
      connect = function() DBI::dbConnect(duckdb::duckdb(shared_home = FALSE)),
      disconnect = function(con) DBI::dbDisconnect(con, shutdown = TRUE),
      explain = duckdb_plan_metrics
    )
  )
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (backend_name in names(backends)) {
    backend <- backends[[backend_name]]
    con <- backend$connect()
    data <- make_data(256L, 6L)
    table <- paste0("perf_", backend_name)
    dplyr::copy_to(con, data, table, overwrite = TRUE, temporary = TRUE)
    remote_all <- dplyr::tbl(con, table)
    invisible(dplyr::collect(utils::head(remote_all, n = 1L)))
    for (dimensions in 1:6) {
      remote <- dplyr::select(
        remote_all,
        dplyr::all_of(c(paste0("d", seq_len(dimensions)), "value"))
      )
      spec <- make_spec("cube", dimensions)
      operations <- list(
        summary = function() summarize_with_margins(
          remote,
          total = sum(value, na.rm = TRUE),
          .grouping = spec,
          .margin_label = NULL
        ),
        expand = function() expand_with_margins(
          remote,
          .grouping = spec,
          .margin_label = NULL
        )
      )
      for (operation in names(operations)) {
        build <- operations[[operation]]
        build_timing <- elapsed_summary(build, repetitions = 3L)
        query <- build()
        sent <- last_sent_queries()
        render_started <- Sys.time()
        sql <- as.character(dbplyr::sql_render(query))
        render_elapsed <- as.numeric(difftime(
          Sys.time(), render_started, units = "secs"
        ))
        plan <- backend$explain(con, sql, table)
        collect_started <- Sys.time()
        result <- dplyr::collect(query)
        collect_elapsed <- as.numeric(difftime(
          Sys.time(), collect_started, units = "secs"
        ))
        rows[[length(rows) + 1L]] <- data.frame(
          backend = backend_name,
          operation = operation,
          dimensions = dimensions,
          sets = 2^dimensions,
          sql_bytes = nchar(sql, type = "bytes"),
          unions = count_fixed(sql, "UNION ALL"),
          grouping_sets = count_fixed(sql, "GROUPING SETS"),
          source_references = count_fixed(sql, table),
          selection_proxy_queries = sum(sent$purpose == "selection_proxy"),
          result_records = sum(sent$purpose == "result"),
          base_scans = plan[["base_scans"]],
          cte_scans = plan[["cte_scans"]],
          result_rows = nrow(result),
          query_bytes = as.numeric(object.size(query)),
          build_elapsed = build_timing[["elapsed_median"]],
          render_elapsed = render_elapsed,
          collect_elapsed = collect_elapsed
        )
      }
    }
    backend$disconnect(con)
  }
  print_result("sql_cube", rows)
}

run_arrow_cube <- function() {
  rows <- list()
  for (dimensions in 1:6) {
    input <- arrow::as_arrow_table(make_data(256L, dimensions))
    spec <- make_spec("cube", dimensions)
    operations <- list(
      summary = function() summarize_with_margins(
        input,
        total = sum(value),
        .grouping = spec,
        .margin_label = NULL
      ),
      expand = function() expand_with_margins(
        input,
        .grouping = spec,
        .margin_label = NULL
      )
    )
    for (operation in names(operations)) {
      build <- operations[[operation]]
      timing <- elapsed_summary(build, repetitions = 3L)
      query <- build()
      plan <- paste(capture.output(dplyr::show_query(query)), collapse = "\n")
      collect_started <- Sys.time()
      result <- dplyr::collect(query)
      collect_elapsed <- as.numeric(difftime(
        Sys.time(), collect_started, units = "secs"
      ))
      rows[[length(rows) + 1L]] <- data.frame(
        operation = operation,
        dimensions = dimensions,
        sets = 2^dimensions,
        query_bytes = as.numeric(object.size(query)),
        plan_bytes = nchar(plan, type = "bytes"),
        result_rows = nrow(result),
        build_elapsed = timing[["elapsed_median"]],
        collect_elapsed = collect_elapsed
      )
    }
  }
  print_result("arrow_cube", rows)
}

run_fixed_keys <- function() {
  rows <- list()
  n <- 32768L
  for (cardinality in c(1L, 8L, 64L, 512L, 2048L)) {
    data <- data.frame(
      fixed = as.character((seq_len(n) - 1L) %% cardinality),
      d1 = rep(c("a", "b"), length.out = n),
      value = 1L
    )
    operations <- list(
      plain = function() summarize_with_margins(
        data,
        total = sum(value),
        .by = fixed,
        .grouping = rollup(d1),
        .margin_label = NULL
      ),
      total_share = function() summarize_with_margins(
        data,
        total = sum(value),
        share = share_of_total(total),
        .by = fixed,
        .grouping = rollup(d1),
        .margin_label = NULL
      )
    )
    for (operation in names(operations)) {
      run <- operations[[operation]]
      timing <- elapsed_summary(run)
      result <- run()
      rows[[length(rows) + 1L]] <- data.frame(
        operation = operation,
        cardinality = cardinality,
        source_rows = n,
        result_rows = nrow(result),
        result_bytes = as.numeric(object.size(result)),
        elapsed_median = timing[["elapsed_median"]]
      )
    }
  }
  print_result("fixed_key_cardinality", rows)
}

run_parent_id_scaling <- function() {
  rows <- list()
  for (dimensions in c(16L, 32L, 64L, 128L, 256L)) {
    data <- make_data(1L, dimensions)
    plan <- compile_grouping_spec(
      make_spec("rollup", dimensions),
      names(data),
      duplicates_choices = margin_duplicates_choices
    )
    run <- function() parent_set_ids(plan)
    timing <- elapsed_summary(run, repetitions = 9L)
    rows[[length(rows) + 1L]] <- data.frame(
      dimensions = dimensions,
      sets = length(plan$sets),
      elapsed_median = timing[["elapsed_median"]],
      elapsed_min = timing[["elapsed_min"]],
      elapsed_max = timing[["elapsed_max"]]
    )
  }
  print_result("parent_set_ids", rows)
}

run_order_rollup <- function() {
  rows <- list()
  for (dimensions in c(8L, 16L, 32L, 64L, 128L)) {
    data <- make_data(1L, dimensions)
    spec <- make_spec("rollup", dimensions)
    for (sort in c("none", "last")) {
      run <- function() summarize_with_margins(
        data,
        total = sum(value),
        .grouping = spec,
        .margin_label = NULL,
        .sort = sort
      )
      timing <- elapsed_summary(run, repetitions = 3L)
      result <- run()
      rows[[length(rows) + 1L]] <- data.frame(
        sort = sort,
        dimensions = dimensions,
        sets = dimensions + 1L,
        result_rows = nrow(result),
        result_bytes = as.numeric(object.size(result)),
        elapsed_median = timing[["elapsed_median"]]
      )
    }
  }
  print_result("order_rollup", rows)
}

dtplyr_union_depth <- function(step) {
  if (!inherits(step, "dtplyr_step_set")) {
    return(0L)
  }
  1L + max(dtplyr_union_depth(step$parent), dtplyr_union_depth(step$parent2))
}

dtplyr_union_nodes <- function(step) {
  if (!inherits(step, "dtplyr_step_set")) {
    return(0L)
  }
  1L + dtplyr_union_nodes(step$parent) + dtplyr_union_nodes(step$parent2)
}

run_dtplyr_cube <- function() {
  data <- make_data(256L, 9L)
  rows <- list()
  for (dimensions in 1:9) {
    input <- dtplyr::lazy_dt(dplyr::select(
      data,
      dplyr::all_of(c(paste0("d", seq_len(dimensions)), "value"))
    ))
    spec <- make_spec("cube", dimensions)
    build <- function() expand_with_margins(
      input,
      .grouping = spec,
      .margin_label = NULL
    )
    build_timing <- elapsed_summary(build, repetitions = 3L)
    query <- build()
    show <- paste(capture.output(dplyr::show_query(query)), collapse = "\n")
    collect_started <- Sys.time()
    result <- dplyr::collect(query)
    collect_elapsed <- as.numeric(difftime(
      Sys.time(), collect_started, units = "secs"
    ))
    rows[[dimensions]] <- data.frame(
      dimensions = dimensions,
      sets = 2^dimensions,
      union_nodes = dtplyr_union_nodes(query),
      union_depth = dtplyr_union_depth(query),
      query_bytes = as.numeric(object.size(query)),
      expression_bytes = nchar(show, type = "bytes"),
      result_rows = nrow(result),
      build_elapsed = build_timing[["elapsed_median"]],
      collect_elapsed = collect_elapsed
    )
  }
  print_result("dtplyr_cube", rows)
}

make_summary_dots <- function(count) {
  stats::setNames(
    lapply(seq_len(count), function(i) rlang::quo(sum(value, na.rm = TRUE))),
    paste0("summary_", seq_len(count))
  )
}

make_share_dots <- function(count, kind = "total", unique_sources = FALSE) {
  summaries <- if (unique_sources) make_summary_dots(count) else make_summary_dots(1L)
  sources <- if (unique_sources) names(summaries) else rep("summary_1", count)
  helper <- paste0("share_of_", kind)
  shares <- stats::setNames(
    lapply(
      sources,
      function(source) rlang::new_quosure(
        rlang::call2(helper, rlang::sym(source)),
        env = rlang::global_env()
      )
    ),
    paste0("share_", seq_len(count))
  )
  c(summaries, shares)
}

run_shares <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- make_data(256L, 32L)
  dplyr::copy_to(con, data, "perf_share", overwrite = TRUE, temporary = TRUE)
  remote_all <- dplyr::tbl(con, "perf_share")
  expression_rows <- list()

  for (count in c(1L, 4L, 16L, 64L)) {
    for (kind in c("plain", "total_reused", "total_unique")) {
      dots <- switch(
        kind,
        plain = make_summary_dots(count),
        total_reused = make_share_dots(count, unique_sources = FALSE),
        total_unique = make_share_dots(count, unique_sources = TRUE)
      )
      build <- function() summarize_with_margins(
        remote_all,
        !!!dots,
        .grouping = rollup(d1),
        .margin_label = NULL,
        .check_share_source = FALSE
      )
      timing <- elapsed_summary(build, repetitions = 3L)
      query <- build()
      render_started <- Sys.time()
      sql <- as.character(dbplyr::sql_render(query))
      render_elapsed <- as.numeric(difftime(
        Sys.time(), render_started, units = "secs"
      ))
      expression_rows[[length(expression_rows) + 1L]] <- data.frame(
        series = kind,
        count = count,
        sql_bytes = nchar(sql, type = "bytes"),
        left_joins = count_fixed(sql, "LEFT JOIN"),
        query_bytes = as.numeric(object.size(query)),
        build_elapsed = timing[["elapsed_median"]],
        render_elapsed = render_elapsed
      )
    }
  }

  print_result("share_expression_count", expression_rows)

  rollup_rows <- list()
  for (dimensions in c(1L, 2L, 4L, 8L, 12L, 16L, 24L, 32L)) {
    remote <- dplyr::select(
      remote_all,
      dplyr::all_of(c(paste0("d", seq_len(dimensions)), "value"))
    )
    spec <- make_spec("rollup", dimensions)
    for (kind in c("plain", "total", "parent")) {
      build <- function() switch(
        kind,
        plain = summarize_with_margins(
          remote,
          total = sum(value, na.rm = TRUE),
          .grouping = spec,
          .margin_label = NULL
        ),
        total = summarize_with_margins(
          remote,
          total = sum(value, na.rm = TRUE),
          share = share_of_total(total),
          .grouping = spec,
          .margin_label = NULL,
          .check_share_source = FALSE
        ),
        parent = summarize_with_margins(
          remote,
          total = sum(value, na.rm = TRUE),
          share = share_of_parent(total),
          .grouping = spec,
          .margin_label = NULL,
          .check_share_source = FALSE
        )
      )
      timing <- elapsed_summary(build, repetitions = 3L)
      query <- build()
      render_started <- Sys.time()
      sql <- as.character(dbplyr::sql_render(query))
      render_elapsed <- as.numeric(difftime(
        Sys.time(), render_started, units = "secs"
      ))
      plan <- duckdb_plan_metrics(con, sql, "perf_share")
      rollup_rows[[length(rollup_rows) + 1L]] <- data.frame(
        series = paste0("rollup_", kind),
        count = dimensions,
        sql_bytes = nchar(sql, type = "bytes"),
        left_joins = count_fixed(sql, "LEFT JOIN"),
        query_bytes = as.numeric(object.size(query)),
        build_elapsed = timing[["elapsed_median"]],
        render_elapsed = render_elapsed,
        grouping_sets = count_fixed(sql, "GROUPING SETS"),
        union_branches = count_fixed(sql, "UNION ALL") + 1L,
        base_scans = plan[["base_scans"]],
        cte_scans = plan[["cte_scans"]]
      )
    }
  }
  print_result("share_rollup_dimensions", rollup_rows)
}

run_metadata <- function() {
  counter <- new.env(parent = emptyenv())
  counter$proxy <- 0L
  counter$collect <- 0L
  assign(".marginplyr_perf_counter", counter, envir = .GlobalEnv)
  on.exit(rm(".marginplyr_perf_counter", envir = .GlobalEnv), add = TRUE)
  trace(
    "grouping_selection_proxy",
    tracer = quote(.GlobalEnv$.marginplyr_perf_counter$proxy <-
      .GlobalEnv$.marginplyr_perf_counter$proxy + 1L),
    where = asNamespace("marginplyr"),
    print = FALSE
  )
  on.exit(untrace("grouping_selection_proxy", where = asNamespace("marginplyr")), add = TRUE)
  trace(
    "collect",
    tracer = quote(.GlobalEnv$.marginplyr_perf_counter$collect <-
      .GlobalEnv$.marginplyr_perf_counter$collect + 1L),
    where = asNamespace("dplyr"),
    print = FALSE
  )
  on.exit(untrace("collect", where = asNamespace("dplyr")), add = TRUE)

  data <- make_data(64L, 8L)
  sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(sqlite), add = TRUE)
  dplyr::copy_to(sqlite, data, "perf_metadata", overwrite = TRUE, temporary = TRUE)
  duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)
  dplyr::copy_to(duck, data, "perf_metadata", overwrite = TRUE, temporary = TRUE)
  inputs <- list(
    local = data,
    dtplyr = dtplyr::lazy_dt(data),
    sqlite = dplyr::tbl(sqlite, "perf_metadata"),
    duckdb = dplyr::tbl(duck, "perf_metadata"),
    arrow = arrow::as_arrow_table(data)
  )
  rows <- list()
  for (backend in names(inputs)) {
    input <- inputs[[backend]]
    for (dimensions in c(1L, 8L)) {
      spec <- make_spec("rollup", dimensions)
      calls <- list(
        inspect = function() inspect_grouping(input, .grouping = spec),
        summary = function() summarize_with_margins(
          input,
          total = sum(value, na.rm = TRUE),
          .grouping = spec,
          .margin_label = NULL
        ),
        expand = function() expand_with_margins(
          input,
          .grouping = spec,
          .margin_label = NULL
        )
      )
      if (backend %in% c("local", "dtplyr")) {
        calls$nest <- function() nest_with_margins(
          input,
          .grouping = spec,
          .margin_label = NULL
        )
        calls$nest_by <- function() nest_by_with_margins(
          input,
          .grouping = spec,
          .margin_label = NULL
        )
      }
      for (operation in names(calls)) {
        counter$proxy <- 0L
        counter$collect <- 0L
        status <- tryCatch({
          invisible(calls[[operation]]())
          "ok"
        }, error = function(cnd) paste0("error: ", conditionMessage(cnd)))
        rows[[length(rows) + 1L]] <- data.frame(
          backend = backend,
          operation = operation,
          dimensions = dimensions,
          proxy_calls = counter$proxy,
          collect_calls = counter$collect,
          status = status
        )
      }
    }
  }
  print_result("metadata", rows)
}

sections <- list(
  local_rows = run_local_rows,
  plan_cube = run_plan_cube,
  local_cube = run_local_cube,
  occurrences = run_occurrences,
  sql_cube = run_sql_cube,
  dtplyr_cube = run_dtplyr_cube,
  arrow_cube = run_arrow_cube,
  shares = run_shares,
  fixed_keys = run_fixed_keys,
  parent_ids = run_parent_id_scaling,
  order_rollup = run_order_rollup,
  metadata = run_metadata
)

cat("R:", R.version.string, "\n")
cat(
  "Packages:",
  paste(
    paste0(
      c("dplyr", "dbplyr", "dtplyr", "RSQLite", "duckdb", "arrow"),
      "=",
      vapply(
        c("dplyr", "dbplyr", "dtplyr", "RSQLite", "duckdb", "arrow"),
        function(package) as.character(utils::packageVersion(package)),
        character(1)
      )
    ),
    collapse = ", "
  ),
  "\n"
)

if (identical(section, "all")) {
  for (run in sections) run()
} else if (section %in% names(sections)) {
  sections[[section]]()
} else {
  stop("Unknown section: ", section)
}
