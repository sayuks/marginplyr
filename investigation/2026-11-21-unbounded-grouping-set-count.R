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

run_cell <- function(backend, verb, d, n, out) {
  if (nzchar(Sys.getenv("MP_LIB"))) {
    .libPaths(c(Sys.getenv("MP_LIB"), .libPaths()))
  }
  suppressPackageStartupMessages({
    library(marginplyr)
    library(dplyr, warn.conflicts = FALSE)
  })
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

  set.seed(20261121L)
  keys <- paste0("k", seq_len(d))
  df <- as.data.frame(
    lapply(
      stats::setNames(keys, keys),
      function(key) sample(c("a", "b"), n, replace = TRUE)
    )
  )
  df$v <- as.numeric(seq_len(n))
  grouping <- rlang::expr(cube(!!!rlang::syms(keys)))

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
  on.exit(if (!is.null(con)) DBI::dbDisconnect(con), add = TRUE)

  invisible(gc(reset = TRUE))
  inspected <- timed(function() {
    rlang::inject(
      inspect_grouping(input, .grouping = !!grouping, .format = "list")
    )
  })
  put("sets", nrow(inspected$value))
  put("inspect_s", inspected$seconds)

  built <- timed(function() {
    if (identical(verb, "summarize")) {
      rlang::inject(summarize_with_margins(
        input, n = dplyr::n(), s = sum(v, na.rm = TRUE), .grouping = !!grouping
      ))
    } else {
      rlang::inject(expand_with_margins(input, .grouping = !!grouping))
    }
  })
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
  usage <- gc()
  put("r_peak_mb", sum(usage[, ncol(usage)]))
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

if (identical(args[[1L]], "cell")) {
  stopifnot(args[[2L]] %in% backends)
  run_cell(
    args[[2L]], args[[3L]], as.integer(args[[4L]]), as.integer(args[[5L]]),
    args[[6L]]
  )
} else {
  pick <- function(i, default) if (length(args) >= i) args[[i]] else default
  run_grid(
    out = args[[2L]],
    timeout = as.numeric(pick(3L, "300")),
    chosen = strsplit(pick(4L, paste(backends[-8L], collapse = ",")), ",")[[1L]],
    dims = as.integer(strsplit(pick(5L, "2,4,6,8,10,12,14,16"), ",")[[1L]]),
    n = as.integer(pick(6L, "256")),
    verbs = strsplit(pick(7L, "summarize,expand"), ",")[[1L]]
  )
}
