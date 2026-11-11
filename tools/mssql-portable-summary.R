#!/usr/bin/env Rscript

# Live execution of portable Margin summaries on a disposable SQL Server,
# selected through MSSQL_SERVER, MSSQL_UID and MSSQL_PWD. Install the working
# tree first. SQL Server takes the portable `UNION ALL` path, which the
# simulators establish as rendered text only.
#
# By default this records what the server answers and fails only when the
# server cannot be reached or the one-set control differs from the local
# result. With `--assert-contract` every case must equal the local data frame;
# #1586 holds why that mode does not pass yet.
library(marginplyr)

args <- commandArgs(trailingOnly = TRUE)
assert_contract <- "--assert-contract" %in% args
evidence_dir <- setdiff(args, "--assert-contract")
evidence_dir <- if (length(evidence_dir)) evidence_dir[[1L]] else "."
dir.create(evidence_dir, recursive = TRUE, showWarnings = FALSE)
evidence <- file.path(
  evidence_dir,
  if (assert_contract) "mssql-contract.txt" else "mssql-observed.txt"
)
record <- function(...) {
  cat(..., "\n", sep = "")
  cat(..., "\n", sep = "", file = evidence, append = TRUE)
}
unlink(evidence)

# A service container accepts connections some seconds after it starts.
connect <- function(attempts = 30L, wait = 5) {
  for (attempt in seq_len(attempts)) {
    con <- tryCatch(
      DBI::dbConnect(
        odbc::odbc(),
        driver = "ODBC Driver 18 for SQL Server",
        server = Sys.getenv("MSSQL_SERVER"),
        uid = Sys.getenv("MSSQL_UID"),
        pwd = Sys.getenv("MSSQL_PWD"),
        TrustServerCertificate = "yes"
      ),
      error = function(cnd) cnd
    )
    if (!inherits(con, "error")) {
      return(con)
    }
    if (attempt == attempts) {
      stop("SQL Server did not accept a connection: ", conditionMessage(con))
    }
    Sys.sleep(wait)
  }
}

# The whole cause chain: dbplyr wraps the server's message in its own.
condition_text <- function(cnd) {
  text <- character()
  while (!is.null(cnd)) {
    text <- c(text, conditionMessage(cnd))
    cnd <- cnd$parent
  }
  paste(gsub("\\s+", " ", text), collapse = " | ")
}

# Unordered rows with dimensions as text, so a backend's own spelling of a
# number's storage type does not decide the comparison.
canonical_rows <- function(data, measures) {
  data <- as.data.frame(data)
  for (name in setdiff(names(data), measures)) {
    data[[name]] <- as.character(data[[name]])
  }
  for (name in measures) {
    data[[name]] <- as.numeric(data[[name]])
  }
  keys <- lapply(unname(data), function(column) {
    ifelse(is.na(column), "\001", as.character(column))
  })
  data <- data[do.call(order, c(keys, list(method = "radix"))), , drop = FALSE]
  row.names(data) <- NULL
  data
}

observe <- function(expr) {
  tryCatch(
    list(value = as.data.frame(expr), error = NULL),
    error = function(cnd) list(value = NULL, error = condition_text(cnd))
  )
}

data <- data.frame(
  year = c(2025L, 2025L, 2026L, 2026L),
  region = c("n", "s", "n", "s"),
  v = c(1, 2, 4, 8)
)

cases <- list(
  list(
    name = "one set, integer dimension (control: no type anchor)",
    measures = "s", control = TRUE,
    call = function(x) {
      summarize_with_margins(
        x, s = sum(v, na.rm = TRUE), .grouping = grouping_set(year)
      )
    }
  ),
  list(
    name = "rollup, character dimension, text label",
    measures = "s", control = FALSE,
    call = function(x) {
      summarize_with_margins(
        x, s = sum(v, na.rm = TRUE), .grouping = rollup(region)
      )
    }
  ),
  list(
    name = "rollup, integer dimension, text label",
    measures = "s", control = FALSE,
    call = function(x) {
      summarize_with_margins(
        x, s = sum(v, na.rm = TRUE), .grouping = rollup(year)
      )
    }
  ),
  list(
    name = "rollup, integer dimension, typed-missing label",
    measures = "s", control = FALSE,
    call = function(x) {
      summarize_with_margins(
        x, s = sum(v, na.rm = TRUE), .grouping = rollup(year),
        .margin_label = NULL
      )
    }
  ),
  list(
    name = "rollup with a fixed key, Total share and Margin order",
    measures = c("s", "sh"), control = FALSE,
    call = function(x) {
      summarize_with_margins(
        x, s = sum(v, na.rm = TRUE), sh = share_of_total(s),
        .by = region, .grouping = rollup(year), .sort = "last",
        .check_share_source = FALSE
      )
    }
  )
)

con <- connect()
failures <- character()
tryCatch({
  version <- DBI::dbGetQuery(con, "SELECT @@VERSION AS version")$version
  record("Server: ", gsub("\\s+", " ", version))
  record("Connection class: ", class(con)[[1L]])
  record("Mode: ", if (assert_contract) "assert contract" else "observe")
  remote <- dplyr::copy_to(
    con, data, "portable_summary", temporary = TRUE, overwrite = TRUE
  )

  # The anchor's predicate on its own, as dbplyr spells `filter(x, FALSE)`
  # for this dialect, so its acceptance does not rest on a larger query.
  record("")
  record("== constant-false predicate alone")
  for (predicate in c("WHERE (0)", "WHERE (1 = 0)")) {
    probe <- observe(DBI::dbGetQuery(
      con, paste("SELECT [year] FROM [#portable_summary]", predicate)
    ))
    record(
      predicate, ": ",
      if (is.null(probe$error)) {
        paste(nrow(probe$value), "rows")
      } else {
        paste("error:", probe$error)
      }
    )
  }

  for (case in cases) {
    record("")
    record("== ", case$name)
    expected <- canonical_rows(case$call(data), case$measures)
    query <- case$call(remote)
    sql <- as.character(dbplyr::sql_render(query))
    record("SQL: ", gsub("\\s+", " ", sql))
    live <- observe(dplyr::collect(query))
    if (is.null(live$error)) {
      actual <- canonical_rows(live$value, case$measures)[names(expected)]
      same <- isTRUE(all.equal(expected, actual, check.attributes = FALSE))
      record("Outcome: ", nrow(actual), " rows; equal to local: ", same)
    } else {
      same <- FALSE
      record("Outcome: error: ", live$error)
    }
    if (!same && (case$control || assert_contract)) {
      failures <- c(failures, case$name)
    }

    # Which of the two anchor symptoms the server objects to: the same text
    # with the constant-false predicate spelled as a comparison.
    if (!is.null(live$error) && grepl("WHERE (0)", sql, fixed = TRUE)) {
      patched <- sub("WHERE (0)", "WHERE (1 = 0)", sql, fixed = TRUE)
      retry <- observe(DBI::dbGetQuery(con, patched))
      record(
        "With `WHERE (1 = 0)` in the anchor: ",
        if (is.null(retry$error)) {
          paste(nrow(retry$value), "rows")
        } else {
          paste("error:", retry$error)
        }
      )
    }
  }
}, finally = DBI::dbDisconnect(con))

record("")
if (length(failures)) {
  record("Differs from the local result: ", paste(failures, collapse = "; "))
  quit(status = 1L)
}
record(
  if (assert_contract) {
    "Every portable summary equals the local result on SQL Server."
  } else {
    "The control equals the local result; the other outcomes are recorded."
  }
)
