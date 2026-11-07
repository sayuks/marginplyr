Sys.setenv(TESTTHAT_PARALLEL = "false")
suppressPackageStartupMessages({
  library(dplyr)
  pkgload::load_all(".", quiet = TRUE)
})

normalize_key <- function(x) tolower(trimws(x))

reference <- function(data, plan, fixed, mode) {
  data$p <- normalize_key(data$p)
  data$g <- normalize_key(data$g)
  keys <- c(if (fixed) "p", "g", "h")
  sets <- if (plan == "root-first") {
    list(character(), c("g", "h"), "g", character())
  } else {
    list(c("g", "h"), "g", character())
  }
  rows <- lapply(seq_along(sets), function(i) {
    include <- c(if (fixed) "p", sets[[i]])
    part <- data |>
      group_by(!!!rlang::syms(include)) |>
      summarise(value = sum(x), .groups = "drop")
    for (key in setdiff(keys, include)) {
      part[[key]] <- if (key == "h") NA_integer_ else NA_character_
    }
    part$sid <- i
    part
  })
  value <- bind_rows(rows)
  root <- which(lengths(sets) == 0L)[[1L]]
  eq <- function(x, y) (is.na(x) & is.na(y)) |
    (!is.na(x) & !is.na(y) & x == y)
  ratio <- function(i, target) {
    if (length(sets[[i]]) == 0L) return(rep(1, nrow(rows[[i]])))
    vapply(seq_len(nrow(rows[[i]])), function(j) {
      a <- rows[[i]][j, ]
      b <- rows[[target]]
      match_keys <- c(if (fixed) "p", sets[[target]])
      ok <- rep(TRUE, nrow(b))
      for (k in match_keys) ok <- ok & eq(b[[k]], a[[k]])
      stopifnot(sum(ok) == 1L)
      if (b$value[ok] == 0) NA_real_ else a$value / b$value[ok]
    }, numeric(1))
  }
  value$t <- unlist(lapply(seq_along(sets), ratio, target = root))
  if (plan != "root-first") {
    value$s <- unlist(lapply(seq_along(sets), function(i) {
      ratio(i, i + 1L)
    }))
  }
  order_value <- function(value, mode) {
    if (mode == "none") return(order(value$sid, value$value, if (fixed) value$p else value$sid, value$g, value$h))
    ordering <- list()
    if (fixed) ordering <- c(ordering, list(is.na(value$p), value$p))
    for (k in c("g", "h")) {
      bit <- !vapply(sets, function(set) k %in% set, logical(1))[value$sid]
      if (mode == "first") bit <- !bit
      ordering <- c(ordering, list(bit, is.na(value[[k]]), value[[k]]))
    }
    do.call(order, ordering)
  }
  value <- value[order_value(value, mode), c(keys, "sid", "value", "t",
                                           if (plan != "root-first") "s")]
  value
}

main <- function(engine) {
  con <- switch(engine,
    postgres = DBI::dbConnect(RPostgres::Postgres(),
      host = Sys.getenv("MARGIN_INVARIANTS_PG_SOCKET"), port = 55737L,
      dbname = "postgres"),
    sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  )
  on.exit(DBI::dbDisconnect(con))
  if (engine == "postgres") {
    DBI::dbExecute(con, paste(
      "CREATE COLLATION case_insensitive",
      "(provider = icu, locale = 'und-u-ks-level2', deterministic = false)"
    ))
  }
  collation <- if (engine == "postgres") "case_insensitive" else "NOCASE"
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE input (p TEXT COLLATE ", collation,
    ", g TEXT COLLATE ", collation, ", h INTEGER, x INTEGER)"
  ))
  data <- data.frame(
    p = c("C", "c", "C", "a", "A", "b", "B", NA, NA, NA),
    g = c("B", "b", "a", "B", "b", "A", "a", "a", "B", NA),
    h = c(1L, 2L, NA, 1L, 2L, 1L, 2L, 1L, NA, NA),
    x = c(2L, 7L, 11L, -3L, 3L, 13L, 19L, 29L, 31L, 37L)
  )
  DBI::dbAppendTable(con, "input", data)
  src <- tbl(con, "input")
  forms <- list(
    table = src,
    derived = src |> mutate(g = !!dbplyr::sql(paste(
      "(CASE WHEN x IS NOT NULL THEN g ELSE NULL END) COLLATE", collation
    )))
  )
  checks <- 0L
  failures <- 0L
  for (form in names(forms)) for (fixed in c(FALSE, TRUE)) {
    for (plan in c("native", "portable", "root-first")) {
      for (mode in c("none", "first", "last")) {
        grouping <- if (plan == "root-first") {
          grouping_sets(grouping_set(), grouping_set(g, h),
                        grouping_set(g), grouping_set())
        } else {
          rollup(g, h)
        }
        share <- if (plan == "root-first") list() else
          rlang::exprs(s = share_of_parent(value))
        query <- summarize_with_margins(
          forms[[form]], value = sum(x, na.rm = TRUE),
          t = share_of_total(value), !!!share,
          .by = all_of(if (fixed) "p" else character()),
          .grouping = grouping, .margin_label = NULL, .id = "sid",
          .sort = mode, .duplicates = if (plan == "native") "error" else "keep",
          .check_share_source = FALSE
        )
        sql <- as.character(dbplyr::sql_render(query))
        is_native <- grepl("GROUPING SETS", sql, fixed = TRUE)
        stopifnot(identical(is_native, engine == "postgres" && plan == "native"))
        want <- reference(data, plan, fixed, mode)
        for (cte in c(FALSE, TRUE)) {
          opts <- dbplyr::sql_options(cte = cte)
          for (endpoint in c("collect", "compute")) {
            checks <- checks + 1L
            observed <- tryCatch({
              input <- if (endpoint == "compute") {
                compute(query, name = paste0("materialized_", checks),
                        sql_options = opts)
              } else {
                query
              }
              if (endpoint == "compute") collect(input) else collect(input, sql_options = opts)
            }, error = identity)
            if (inherits(observed, "error")) {
              result <- conditionMessage(observed)
            } else {
              for (name in intersect(c("p", "g"), names(observed))) {
                observed[[name]] <- normalize_key(observed[[name]])
              }
              observed$value <- as.numeric(observed$value)
              if (mode == "none") {
                observed <- observed[order(observed$sid, observed$value, if (fixed) observed$p else observed$sid, observed$g, observed$h), ]
              }
              result <- all.equal(as.list(observed), as.list(want),
                                  check.attributes = FALSE, tolerance = 1e-12)
            }
            if (!isTRUE(result)) {
              failures <- failures + 1L
              cat(paste(engine, form, fixed, plan, mode, cte, endpoint,
                        ":", paste(result, collapse = "; ")), "\n")
              if (failures <= 3L && !inherits(observed, "error")) {
                print(observed)
                print(want)
              }
            }
          }
        }
      }
    }
  }
  cat(engine, "checks:", checks, "failures:", failures, "\n")
  invisible(failures)
}
quit(status = as.integer(main(commandArgs(trailingOnly = TRUE)[[1L]]) > 0L))
