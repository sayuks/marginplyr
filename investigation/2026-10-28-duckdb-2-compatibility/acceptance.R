# Run from the repository root with one DuckDB version on R_LIBS_USER:
# Rscript investigation/2026-10-28-duckdb-2-compatibility/acceptance.R VERSION OUTPUT
# Each case gets a fresh R process and a 60-second timeout.
args <- commandArgs(TRUE)
stopifnot(length(args) %in% 2:3)
version <- args[[1L]]
output <- args[[2L]]
dir.create(output, recursive = TRUE, showWarnings = FALSE)

with_connection <- function(f) {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  f(con)
}

cases <- list(
  registration = function() with_connection(function(con) {
    data <- data.frame(g = factor(c('a', 'b', 'a', NA),
      levels = c('b', 'a', 'unused')), value = c(2L, 6L, 4L, 8L))
    table <- arrow::Table$create(data)
    shapes <- list(table, arrow::record_batch(data),
      arrow::InMemoryDataset$create(table),
      dplyr::select(table, dplyr::everything()),
      dplyr::filter(dplyr::select(table, dplyr::everything()), .data$value > 2L))
    for (i in seq_along(shapes)) {
      name <- paste0('registered_', i)
      duckdb::duckdb_register_arrow(con, name, shapes[[i]])
      source <- dplyr::tbl(con, name)
      query <- summarize_with_margins(source,
        total = sum(value, na.rm = TRUE), .grouping = rollup(g),
        .margin_label = NULL, .id = 'sid')
      result <- dplyr::collect(query)
      result$g <- as.character(result$g)
      result <- result[order(result$sid, result$g, na.last = TRUE), ]
      stopifnot(identical(result$g, c('a', 'b', NA, NA)),
        identical(result$sid, c(1L, 1L, 1L, 2L)),
        identical(as.numeric(result$total),
          if (i == 5L) c(4, 6, 8, 18) else c(6, 6, 8, 20)))
      expansion <- expand_with_margins(source, .grouping = rollup(g),
        .margin_label = NULL, .id = 'sid')
      result <- dplyr::collect(expansion)
      stopifnot(nrow(result) == if (i == 5L) 6L else 8L,
        sum(result$value) == if (i == 5L) 36 else 40,
        sum(is.na(result$g[result$sid == 2L])) == if (i == 5L) 3L else 4L)
    }
  }),
  registration_boundaries = function() with_connection(function(con) {
    data <- tibble::tibble(
      g = factor(c('a', 'b', 'a', NA), levels = c('b', 'a', 'unused')),
      value = c(2L, 6L, 4L, 8L), day = as.Date('2026-10-01') + 0:3,
      stamp = as.POSIXct('2026-10-01 12:00:00', tz = 'UTC') + 0:3,
      items = list(1:2, integer(), NULL, 3L),
      info = tibble::tibble(flag = c(TRUE, FALSE, NA, TRUE),
        name = c('u', 'v', 'w', 'z'))
    )
    batch <- arrow::record_batch(data)
    empty <- arrow::RecordBatch$create(vctrs::vec_slice(data, integer()),
      schema = batch$schema)
    table <- arrow::Table$create(empty, batch, empty)
    stopifnot(table$GetColumnByName('value')$num_chunks == 3L)
    empty_table <- arrow::Table$create(empty)
    shapes <- list(table, empty_table, empty,
      arrow::InMemoryDataset$create(empty_table),
      dplyr::filter(dplyr::select(table, dplyr::everything()), .data$value < 0L))
    for (i in seq_along(shapes)) {
      name <- paste0('boundaries_', i)
      duckdb::duckdb_register_arrow(con, name, shapes[[i]])
      source <- dplyr::tbl(con, name)
      control <- dplyr::collect(source)
      rows <- nrow(control)
      stopifnot(rows == if (i == 1L) 4L else 0L)
      summary <- dplyr::collect(summarize_with_margins(source,
        total = sum(value, na.rm = TRUE), .grouping = rollup(g),
        .margin_label = NULL, .id = 'sid'))
      stopifnot(nrow(summary) == if (rows) 4L else 1L,
        if (rows) summary$total[summary$sid == 2L] == 20 else is.na(summary$total))
      query <- expand_with_margins(source, .grouping = rollup(g),
        .margin_label = NULL, .id = 'sid')
      indices <- rep(order(control$value), 2L)
      for (result in list(query, dplyr::compute(query))) {
        result <- dplyr::collect(dplyr::arrange(result, .data$sid, .data$value))
        stopifnot(nrow(result) == rows * 2L,
          identical(result$sid, rep(1:2, each = rows)))
        for (column in c('value', 'day', 'stamp', 'items', 'info')) {
          stopifnot(identical(result[[column]],
            vctrs::vec_slice(control[[column]], indices)))
        }
      }
    }
  }),
  export_null = function() with_connection(function(con) {
    for (empty in c(FALSE, TRUE)) {
      source <- dplyr::tbl(con, dbplyr::sql(paste(
        'SELECT NULL AS g, 7 AS value', if (empty) 'WHERE FALSE' else '')))
      query <- expand_with_margins(source, .grouping = rollup(g),
        .margin_label = NULL, .id = 'sid')
      expected <- dplyr::collect(query)
      reader <- arrow::to_arrow(query)
      stopifnot(inherits(reader, 'RecordBatchReader'))
      table <- arrow::as_arrow_table(reader)
      result <- dplyr::collect(table)
      stopifnot(nrow(result) == if (empty) 0L else 2L,
        identical(result$sid, expected$sid),
        identical(result$value, expected$value), all(is.na(result$g)))
      if (utils::packageVersion('duckdb') >= '1.5.6') {
        stream <- duckdb::to_arrow_stream(query)
        streamed <- arrow::as_arrow_table(stream)
        stopifnot(streamed$schema$Equals(table$schema),
          identical(dplyr::collect(streamed), result))
      }
      # Explicit materialization supplies the reusable Arrow input.
      summary <- summarize_with_margins(table, rows = dplyr::n(),
        .grouping = grouping_set(), .margin_label = NULL)
      stopifnot(as.numeric(dplyr::collect(summary)$rows) == if (empty) 0 else 2)
    }
  }),
  reader_contract = function() with_connection(function(con) {
    source <- dplyr::copy_to(con,
      data.frame(g = c('a', 'b'), value = c(2L, 6L)), 'reader_input')
    routes <- list(materialized = arrow::to_arrow)
    if (utils::packageVersion('duckdb') >= '1.5.6') {
      routes$stream <- duckdb::to_arrow_stream
    }
    for (route in routes) {
      for (shape in c('direct', 'query', 'grouped', 'union', 'right_join')) {
        reader <- route(source)
        data <- switch(shape,
          direct = reader,
          query = dplyr::select(reader, dplyr::everything()),
          grouped = dplyr::group_by(reader, .data$value),
          union = dplyr::union_all(reader,
            arrow::Table$create(data.frame(g = 'c', value = 9L))),
          right_join = dplyr::left_join(
            arrow::Table$create(data.frame(g = c('a', 'b'))), reader, by = 'g')
        )
        for (verb in list(summarize_with_margins, expand_with_margins)) {
          error <- tryCatch(verb(data, .grouping = rollup(g)), error = identity)
          stopifnot(inherits(error, 'marginplyr_error'),
            grepl('RecordBatchReader', conditionMessage(error), fixed = TRUE))
        }
        plan <- inspect_grouping(data, .grouping = rollup(g))
        stopifnot(nrow(plan) == 2L)
        remaining <- dplyr::collect(arrow::as_arrow_table(reader))
        stopifnot(identical(remaining$g, c('a', 'b')),
          identical(remaining$value, c(2L, 6L)))
      }
    }
  }),
  stream_batches = function() with_connection(function(con) {
    source <- dplyr::tbl(con, dbplyr::sql(
      'SELECT i::INTEGER AS value FROM range(1000005) t(i)'))
    reader <- duckdb::to_arrow_stream(source)
    rows <- 0L
    total <- 0
    batches <- 0L
    repeat {
      batch <- reader$read_next_batch()
      if (is.null(batch)) break
      data <- as.data.frame(batch)
      rows <- rows + nrow(data)
      total <- total + sum(as.double(data$value))
      batches <- batches + 1L
    }
    stopifnot(rows == 1000005L, total == 500004500010, batches > 1L)
  }),
  stream_invalidation = function() with_connection(function(con) {
    source <- dplyr::tbl(con, dbplyr::sql(
      'SELECT i::INTEGER AS value FROM range(1000005) t(i)'))
    reader <- duckdb::to_arrow_stream(source)
    first <- reader$read_next_batch()
    stopifnot(first$num_rows > 0L, first$num_rows < 1000005L)
    DBI::dbGetQuery(con, 'SELECT 1 AS ok')
    error <- tryCatch(reader$read_next_batch(), error = identity)
    stopifnot(inherits(error, 'error'),
      grepl('invalidat', conditionMessage(error), ignore.case = TRUE))
    reader <- duckdb::to_arrow_stream(source)
    with_connection(function(other) DBI::dbGetQuery(other, 'SELECT 1 AS ok'))
    result <- dplyr::collect(arrow::as_arrow_table(reader))
    stopifnot(nrow(result) == 1000005L,
      sum(as.double(result$value)) == 500004500010)
  }),
  stream_close = function() {
    for (shutdown in c(FALSE, TRUE)) {
      drv <- duckdb::duckdb(shared_home = FALSE)
      on.exit(if (DBI::dbIsValid(drv)) duckdb::duckdb_shutdown(drv), add = TRUE)
      con <- DBI::dbConnect(drv)
      source <- dplyr::tbl(con, dbplyr::sql(
        'SELECT i::INTEGER AS value FROM range(1000005) t(i)'))
      reader <- duckdb::to_arrow_stream(source)
      first <- reader$read_next_batch()
      rows <- first$num_rows
      stopifnot(rows > 0L, rows < 1000005L)
      DBI::dbDisconnect(con, shutdown = shutdown)
      stopifnot(!DBI::dbIsValid(con))
      # The exported stream retains its client context after DBI disconnects.
      remaining <- dplyr::collect(arrow::as_arrow_table(reader))
      stopifnot(identical(remaining$value, seq.int(rows, 1000004L)),
        rows + nrow(remaining) == 1000005L)
      if (DBI::dbIsValid(drv)) duckdb::duckdb_shutdown(drv)
    }
  },
  parser_controls = function() with_connection(function(con) {
    for (literal in c("X'AB'", "B'1010'")) {
      if (literal == "B'1010'" && utils::packageVersion('duckdb') >= '2.0.0') {
        error <- tryCatch(DBI::dbGetQuery(con, paste('SELECT', literal, 'AS g')),
          error = identity)
        stopifnot(inherits(error, 'duckdb_error'),
          grepl('Unknown type for column', conditionMessage(error), fixed = TRUE),
          grepl('BIT', conditionMessage(error), fixed = TRUE))
        cat('Upstream R vector conversion rejects BIT before a Margin verb.\n')
        next
      }
      source <- dplyr::tbl(con, dbplyr::sql(paste(
        'SELECT', literal, 'AS g, 7 AS value')))
      input <- dplyr::collect(source)
      query <- expand_with_margins(source, .grouping = rollup(g),
        .margin_label = NULL, .id = 'sid')
      result <- dplyr::collect(query)
      stopifnot(nrow(result) == 2L,
        identical(result$g[result$sid == 1L], input$g),
        identical(result$value, c(7L, 7L)))
    }
    source <- dplyr::tbl(con, dbplyr::sql(
      "SELECT 'abc' AS g, ('abc' ~ 'b')::INTEGER AS value"))
    result <- dplyr::collect(summarize_with_margins(source,
      total = sum(value, na.rm = TRUE), .grouping = rollup(g)))
    expected <- if (utils::packageVersion('duckdb') >= '2.0.0') 1 else 0
    stopifnot(identical(as.numeric(result$total), rep(expected, 2L)))
    source <- dplyr::tbl(con, dbplyr::sql("SELECT 'a' AS g, 12 AS value"))
    expressions <- list(quote(sum(value, na.rm = TRUE) %/% 0),
      quote(sum(value, na.rm = TRUE) %% 0))
    for (expression in expressions) {
      ordinary <- dplyr::summarize(source, outcome = !!expression)
      margin <- summarize_with_margins(source, outcome = !!expression,
        .grouping = rollup(g))
      control <- tryCatch(dplyr::collect(ordinary), error = identity)
      result <- tryCatch(dplyr::collect(margin), error = identity)
      if (inherits(control, 'error')) {
        stopifnot(inherits(result, 'error'), identical(class(result), class(control)))
      } else {
        stopifnot(identical(result$outcome, rep(control$outcome, 2L)))
      }
    }
    for (operator in c('//', '%')) {
      query <- paste('SELECT 12', operator, '0 AS value')
      result <- tryCatch(DBI::dbGetQuery(con, query), error = identity)
      if (utils::packageVersion('duckdb') >= '2.0.0') {
        stopifnot(inherits(result, 'error'))
      } else {
        stopifnot(is.na(result$value))
      }
    }
  })
)

if (length(args) == 3L) {
  pkgload::load_all('.', quiet = TRUE)
  source('inst/suggests/guard.R')
  stopifnot(marginplyr_suggest_available('duckdb'),
    marginplyr_suggest_available('arrow'),
    as.character(utils::packageVersion('duckdb')) == version)
  cases[[args[[3L]]]]()
  cat('PASS', args[[3L]], '\n')
} else {
  stopifnot(as.character(utils::packageVersion('duckdb')) == version)
  metadata <- with_connection(function(con) list(
    client = version, engine = DBI::dbGetQuery(con, 'PRAGMA version'),
    library = find.package('duckdb'), session = utils::sessionInfo()))
  writeLines(capture.output(dput(metadata)), file.path(output, 'environment.txt'))
  selected <- names(cases)
  if (utils::packageVersion('duckdb') < '1.5.6') {
    selected <- setdiff(selected, c('stream_batches', 'stream_invalidation', 'stream_close'))
  }
  script <- normalizePath(sub('^--file=', '',
    grep('^--file=', commandArgs(FALSE), value = TRUE)))
  statuses <- vapply(selected, function(name) {
    log <- file.path(output, paste0(name, '.log'))
    status <- system2(file.path(R.home('bin'), 'Rscript'),
      c(shQuote(script), shQuote(version), shQuote(output), name),
      stdout = log, stderr = log, timeout = 60)
    cat(name, status, '\n')
    as.integer(status)
  }, integer(1))
  write.table(data.frame(case = names(statuses), status = statuses),
    file.path(output, 'cases.tsv'), sep = '\t', quote = FALSE, row.names = FALSE)
  if (any(statuses != 0L)) quit(status = 1L)
}
