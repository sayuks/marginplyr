# Expected-behavior replay for Boolean aliases across binary SQL query graphs.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
out_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempdir())
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
capture <- function(expr) tryCatch(suppressWarnings(force(expr)), error = identity)
source <- copy_to(con, tibble(g = "a", `TRUE` = 7L), "source", temporary = TRUE)
keys <- select(source, g)
expanded <- expand_with_margins(source, .grouping = rollup(g))
materialized <- compute(expanded, name = "materialized", temporary = TRUE)
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  ordinary <- collect(left_join(keys, source, by = "g"))
  computed <- collect(left_join(keys, materialized, by = "g"))
  stopifnot(
    identical(ordinary, computed), identical(names(computed), c("g", "TRUE")),
    identical(computed$`TRUE`, 7L)
  )
  minimal <- capture(collect(left_join(keys, expanded, by = "g")))
  print(minimal)
  DBI::dbDisconnect(con)
  stopifnot(!inherits(minimal, "error"), identical(minimal, computed))
  quit(status = 0L)
}
other <- copy_to(con, tibble(g = "a", `TRUE` = 7L), "other", temporary = TRUE)
if ("--cause" %in% commandArgs(trailingOnly = TRUE)) {
  candidates <- list(
    union_lhs = union_all(expanded, other),
    union_rhs = union_all(other, expanded),
    join_lhs = left_join(expanded, keys, by = "g"),
    join_rhs = left_join(keys, expanded, by = "g")
  )
  # Direct helper invocation distinguishes dispatch loss from an unhandled query node.
  repair_query <- getFromNamespace("sqlite_boolean_derived_query", "marginplyr")
  for (name in names(candidates)) {
    x <- candidates[[name]]
    repair <- repair_query(x)
    cat("\nCAUSE", name, "\n")
    cat("OUTER", paste(class(x), collapse = "/"), "\n")
    cat("ROOT", paste(class(x$lazy_query), collapse = "/"), "\n")
    before <- capture(DBI::dbGetQuery(con, as.character(dbplyr::sql_render(x))))
    after <- capture(DBI::dbGetQuery(con, as.character(dbplyr::sql_render(repair))))
    cat("BEFORE", if (inherits(before, "error")) conditionMessage(before) else "success", "\n")
    cat("AFTER", if (inherits(after, "error")) conditionMessage(after) else "success", "\n")
  }
  DBI::dbDisconnect(con)
  quit(status = 0L)
}
operations <- list(
  direct = function(x) x,
  head = function(x) head(x, 1L),
  collapse = function(x) collapse(x),
  collapse_head = function(x) head(collapse(x), 1L),
  left_join_lhs = function(x) left_join(x, keys, by = "g"),
  left_join_rhs = function(x) left_join(keys, x, by = "g"),
  inner_join_lhs = function(x) inner_join(x, keys, by = "g"),
  inner_join_rhs = function(x) inner_join(keys, x, by = "g"),
  union_all_lhs = function(x) union_all(x, other),
  union_all_rhs = function(x) union_all(other, x),
  union_lhs = function(x) union(x, other),
  union_rhs = function(x) union(other, x),
  intersect_lhs = function(x) intersect(x, other),
  intersect_rhs = function(x) intersect(other, x),
  setdiff_lhs = function(x) setdiff(x, other),
  setdiff_rhs = function(x) setdiff(other, x)
)
rows <- list()
for (name in names(operations)) {
  operation <- operations[[name]]
  for (input in c("source", "materialized", "expanded")) {
    x <- get(input)
    query <- capture(operation(x))
    got <- capture(collect(query))
    cat("\nCASE", name, input, "\n")
    if (!inherits(query, "error")) {
      cat("CLASS", paste(class(query), collapse = "/"), "\n")
      sql <- capture(dbplyr::sql_render(query))
      if (!inherits(sql, "error")) cat(as.character(sql), "\n")
    }
    if (inherits(got, "error")) {
      cat("ERROR", class(got)[[1L]], conditionMessage(got), "\n")
    } else {
      print(got)
    }
    rows[[length(rows) + 1L]] <- data.frame(
      operation = name, input,
      status = if (inherits(got, "error")) "error" else "success",
      class = if (inherits(query, "error")) "construction_error" else paste(class(query), collapse = "/"),
      names = if (inherits(got, "error")) "" else paste(names(got), collapse = "/"),
      n = if (inherits(got, "error")) NA_integer_ else nrow(got),
      detail = if (inherits(got, "error")) conditionMessage(got) else ""
    )
  }
}
results <- bind_rows(rows)
write.csv(results, file.path(out_dir, "sql-alias-graph.csv"), row.names = FALSE)
cat("\nSUMMARY\n")
print(results[c("operation", "input", "status", "names", "n")], row.names = FALSE)
minimal <- capture(collect(left_join(keys, expanded, by = "g")))
ordinary <- collect(left_join(keys, source, by = "g"))
computed <- collect(left_join(keys, materialized, by = "g"))
stopifnot(identical(ordinary, computed), identical(names(computed), c("g", "TRUE")), identical(computed$`TRUE`, 7L))
DBI::dbDisconnect(con)
