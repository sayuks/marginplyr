# Measurement script for
# investigation/2026-11-21-unbounded-grouping-set-count.md.
#
# One cell is one (backend, verb, D, N): a `cube()` over D two-valued character
# columns of an N-row input. Each cell runs in its own R process so that a cell
# which does not finish can be killed without losing the others, and each
# phase's result is written as soon as that phase ends so that a killed cell
# still reports the phases it completed.
#
#   Rscript investigation/2026-11-21-unbounded-grouping-set-count.R \
#     grid <out.tsv> [timeout-seconds] [backend,...] [D,...] [N] [verb,...]
#   Rscript investigation/2026-11-21-unbounded-grouping-set-count.R \
#     cell <backend> <verb> <D> <N> <out.tsv>
#
# The grid's default backends leave out `postgres`, which needs a live server.
#
# Three further modes print one line each. They restate the one-off code that
# produced the note's inspection, time-limit and DuckDB thread figures:
#
#   ... inspect <D> <list|text>
#   ... limit <backend> <summarize|expand|inspect> <D> <N> <seconds>
#   ... duckdb <D> <threads> <temp-directory>
#
# MP_LIB, when set, is prepended to the library path; it names a library
# holding the marginplyr build being measured. MP_PG_HOST and MP_PG_PORT name
# the live PostgreSQL socket directory and port for the `postgres` backend.

args <- commandArgs(trailingOnly = TRUE)
script <- sub(
  "^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]]
)

phases <- c(
  "sets", "inspect_s", "build_s", "render_s", "sql_bytes", "union_all",
  "exec_s", "collect_s", "rows", "r_peak_mb", "status"
)

backends <- c(
  "local", "dtplyr", "arrow", "duckdb", "sqlite", "postgres_sim", "mssql_sim",
  "postgres"
)

# Median elapsed seconds of up to three evaluations; one when the first takes
# five seconds or more. Returns the last value alongside.
timed <- function(f) {
  elapsed <- numeric()
  repeat {
    t0 <- proc.time()[["elapsed"]]
    value <- f()
    elapsed <- c(elapsed, proc.time()[["elapsed"]] - t0)
    if (elapsed[[1L]] >= 5 || length(elapsed) == 3L) {
      break
    }
  }
  list(seconds = stats::median(elapsed), value = value)
}

attach_build <- function() {
  if (nzchar(Sys.getenv("MP_LIB"))) {
    .libPaths(c(Sys.getenv("MP_LIB"), .libPaths()))
  }
  suppressPackageStartupMessages({
    library(marginplyr)
    library(dplyr, warn.conflicts = FALSE)
  })
}

# D two-valued character columns `k1`..`kD` and a numeric `v`, N rows.
cube_frame <- function(d, n) {
  set.seed(20261121L)
  keys <- paste0("k", seq_len(d))
  df <- as.data.frame(
    lapply(
      stats::setNames(keys, keys),
      function(key) sample(c("a", "b"), n, replace = TRUE)
    )
  )
  df$v <- as.numeric(seq_len(n))
  df
}

cube_grouping <- function(d) {
  rlang::expr(cube(!!!rlang::syms(paste0("k", seq_len(d)))))
}

# The frame as one backend's input, with the connection to close afterwards
# when there is one.
open_input <- function(backend, df) {
  con <- NULL
  input <- switch(
    backend,
    local = df,
    dtplyr = dtplyr::lazy_dt(data.table::as.data.table(df)),
    arrow = arrow::arrow_table(df),
    duckdb = {
      con <- DBI::dbConnect(duckdb::duckdb())
      DBI::dbWriteTable(con, "df", df)
      dplyr::tbl(con, "df")
    },
    sqlite = {
      con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
      DBI::dbWriteTable(con, "df", df)
      dplyr::tbl(con, "df")
    },
    postgres = {
      con <- DBI::dbConnect(
        RPostgres::Postgres(),
        host = Sys.getenv("MP_PG_HOST"), port = Sys.getenv("MP_PG_PORT"),
        dbname = "postgres"
      )
      DBI::dbWriteTable(con, "df", df, temporary = TRUE)
      dplyr::tbl(con, "df")
    },
    postgres_sim = dbplyr::tbl_lazy(df, con = dbplyr::simulate_postgres()),
    mssql_sim = dbplyr::tbl_lazy(df, con = dbplyr::simulate_mssql())
  )
  list(input = input, con = con)
}

call_verb <- function(verb, input, grouping, format = "list") {
  switch(
    verb,
    summarize = rlang::inject(summarize_with_margins(
      input, n = dplyr::n(), s = sum(v, na.rm = TRUE), .grouping = !!grouping
    )),
    expand = rlang::inject(expand_with_margins(input, .grouping = !!grouping)),
    inspect = rlang::inject(
      inspect_grouping(input, .grouping = !!grouping, .format = format)
    )
  )
}

r_peak_mb <- function() {
  usage <- gc()
  sum(usage[, ncol(usage)])
}

run_cell <- function(backend, verb, d, n, out) {
  attach_build()
  record <- stats::setNames(as.list(rep(NA_character_, length(phases))), phases)
  record$status <- "started"
  put <- function(name, value) {
    record[[name]] <<- format(value, scientific = FALSE, digits = 4)
    utils::write.table(
      as.data.frame(c(
        list(backend = backend, verb = verb, d = d, n = n), record
      )),
      out, sep = "\t", quote = FALSE, row.names = FALSE
    )
  }

  grouping <- cube_grouping(d)
  opened <- open_input(backend, cube_frame(d, n))
  input <- opened$input
  con <- opened$con
  on.exit(if (!is.null(con)) DBI::dbDisconnect(con), add = TRUE)

  invisible(gc(reset = TRUE))
  inspected <- timed(function() call_verb("inspect", input, grouping))
  put("sets", nrow(inspected$value))
  put("inspect_s", inspected$seconds)

  built <- timed(function() call_verb(verb, input, grouping))
  put("build_s", built$seconds)
  result <- built$value

  if (inherits(result, "tbl_lazy")) {
    rendered <- timed(function() as.character(dbplyr::sql_render(result)))
    sql <- rendered$value
    put("render_s", rendered$seconds)
    put("sql_bytes", nchar(sql, type = "bytes"))
    put("union_all", lengths(regmatches(sql, gregexpr("UNION ALL", sql))))
    if (!is.null(con)) {
      # The database's share alone: the rendered statement sent through DBI.
      put("exec_s", timed(function() DBI::dbGetQuery(con, sql))$seconds)
    }
  }

  simulated <- backend %in% c("postgres_sim", "mssql_sim")
  if (is.data.frame(result)) {
    put("rows", nrow(result))
  } else if (!simulated) {
    collected <- timed(function() dplyr::collect(result))
    put("collect_s", collected$seconds)
    put("rows", nrow(collected$value))
  }
  put("r_peak_mb", r_peak_mb())
  put("status", "done")
}

run_grid <- function(out, timeout, chosen, dims, n, verbs) {
  rows <- list()
  for (backend in chosen) {
    for (verb in verbs) {
      for (d in dims) {
        cell <- tempfile(fileext = ".tsv")
        log <- tempfile(fileext = ".log")
        t0 <- proc.time()[["elapsed"]]
        code <- suppressWarnings(system2(
          file.path(R.home("bin"), "Rscript"),
          c(shQuote(script), "cell", backend, verb, d, n, shQuote(cell)),
          stdout = log, stderr = log, timeout = timeout
        ))
        wall <- proc.time()[["elapsed"]] - t0
        row <- if (file.exists(cell)) {
          utils::read.delim(cell, colClasses = "character")
        } else {
          as.data.frame(c(
            list(backend = backend, verb = verb, d = d, n = n),
            stats::setNames(
              as.list(rep(NA_character_, length(phases))), phases
            )
          ))
        }
        if (code == 124L) {
          row$status <- paste0("killed at ", timeout, " s")
        } else if (code != 0L) {
          tail <- utils::tail(readLines(log, warn = FALSE), 4L)
          row$status <- paste("error:", paste(trimws(tail), collapse = " "))
        }
        row$wall_s <- format(wall, digits = 4)
        rows[[length(rows) + 1L]] <- row
        utils::write.table(
          do.call(rbind, rows), out, sep = "\t", quote = FALSE,
          row.names = FALSE
        )
        message(paste(row, collapse = " | "))
        if (code != 0L) {
          break
        }
      }
    }
  }
}

# `inspect_grouping()` of a D-column cube on a two-row local data frame.
run_inspect <- function(d, format) {
  attach_build()
  df <- cube_frame(d, 2L)
  invisible(gc(reset = TRUE))
  t0 <- proc.time()[["elapsed"]]
  sets <- nrow(call_verb("inspect", df, cube_grouping(d), format))
  cat(sprintf(
    "inspect %s d=%d: %d sets in %.2f s, R peak %.0f MB\n",
    format, d, sets, proc.time()[["elapsed"]] - t0, r_peak_mb()
  ))
}

# One call under `setTimeLimit()`: when it ended, with what, holding how much.
run_limit <- function(backend, verb, d, n, seconds) {
  attach_build()
  opened <- open_input(backend, cube_frame(d, n))
  on.exit(if (!is.null(opened$con)) DBI::dbDisconnect(opened$con), add = TRUE)
  grouping <- cube_grouping(d)
  invisible(gc(reset = TRUE))
  t0 <- proc.time()[["elapsed"]]
  outcome <- tryCatch(
    {
      setTimeLimit(elapsed = seconds, transient = TRUE)
      call_verb(verb, opened$input, grouping)
      "finished"
    },
    error = function(cnd) conditionMessage(cnd)
  )
  setTimeLimit()
  cat(sprintf(
    "limit %s %s d=%d: ended after %.2f s, R peak %.0f MB: %s\n",
    backend, verb, d, proc.time()[["elapsed"]] - t0, r_peak_mb(),
    substr(gsub("\\s+", " ", outcome), 1L, 120L)
  ))
}

# The native DuckDB summary's statement, executed at a chosen thread count
# with DuckDB's temporary directory pointed at a writable path.
run_duckdb <- function(d, threads, temp_directory) {
  attach_build()
  opened <- open_input("duckdb", cube_frame(d, 256L))
  con <- opened$con
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, sprintf("SET temp_directory = '%s'", temp_directory))
  DBI::dbExecute(con, sprintf("SET threads = %d", threads))
  sql <- as.character(dbplyr::sql_render(
    call_verb("summarize", opened$input, cube_grouping(d))
  ))
  t0 <- proc.time()[["elapsed"]]
  outcome <- tryCatch(
    paste(nrow(DBI::dbGetQuery(con, sql)), "rows"),
    error = function(cnd) conditionMessage(cnd)
  )
  cat(sprintf(
    "duckdb d=%d threads=%d: %.2f s: %s\n",
    d, threads, proc.time()[["elapsed"]] - t0,
    substr(gsub("\\s+", " ", outcome), 1L, 160L)
  ))
}

pick <- function(i, default) if (length(args) >= i) args[[i]] else default

switch(
  args[[1L]],
  cell = {
    stopifnot(args[[2L]] %in% backends)
    run_cell(
      args[[2L]], args[[3L]], as.integer(args[[4L]]), as.integer(args[[5L]]),
      args[[6L]]
    )
  },
  grid = run_grid(
    out = args[[2L]],
    timeout = as.numeric(pick(3L, "300")),
    chosen = strsplit(
      pick(4L, paste(setdiff(backends, "postgres"), collapse = ",")), ","
    )[[1L]],
    dims = as.integer(strsplit(pick(5L, "2,4,6,8,10,12,14,16"), ",")[[1L]]),
    n = as.integer(pick(6L, "256")),
    verbs = strsplit(pick(7L, "summarize,expand"), ",")[[1L]]
  ),
  inspect = run_inspect(as.integer(args[[2L]]), args[[3L]]),
  limit = run_limit(
    args[[2L]], args[[3L]], as.integer(args[[4L]]), as.integer(args[[5L]]),
    as.numeric(args[[6L]])
  ),
  duckdb = run_duckdb(as.integer(args[[2L]]), as.integer(args[[3L]]), args[[4L]]),
  stop("Unknown mode: ", args[[1L]], call. = FALSE)
)
