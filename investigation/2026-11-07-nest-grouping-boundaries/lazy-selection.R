suppressPackageStartupMessages({
  pkgload::load_all(".", quiet = TRUE)
  library(dplyr)
})
df <- tibble(id = 1:3, a = c("one", "two", "three"), b = c(1L, NA, 3L), f = factor(c("x","y","x")), d = as.Date("2020-01-01") + 1:3, l = c(TRUE, NA, FALSE), z = c(1, NaN, 3))
other <- tibble(id = c(2L, 3L, 4L), a = c("x", "y", "z"), other = c(1L, 1L, 3L))
local <- list(arrow = list(arrow::arrow_table(df), arrow::arrow_table(other)))
for (db in c("duckdb", "sqlite")) {
  conn <- DBI::dbConnect(switch(db, duckdb = duckdb::duckdb(shared_home = FALSE), sqlite = RSQLite::SQLite()), ":memory:")
  local[[db]] <- list(copy_to(conn, df, "df", temporary = FALSE), copy_to(conn, other, "other", temporary = FALSE))
}
derive <- list(
  root = function(x,y) x,
  empty = function(x,y) filter(x, FALSE),
  reorder = function(x,y) relocate(x, b),
  rename = function(x,y) rename(x, test = a),
  select = function(x,y) select(x, a, b),
  logical = function(x,y) mutate(x, value = b > 1L),
  integer = function(x,y) mutate(x, value = b + 1L),
  double = function(x,y) mutate(x, value = b + 1),
  grouped = function(x,y) group_by(x, id),
  cast_int = function(x,y) mutate(x, value = as.integer(z)),
  cast_dbl = function(x,y) mutate(x, value = as.double(b)),
  cast_chr = function(x,y) mutate(x, value = as.character(b)),
  date = function(x,y) mutate(x, value = d + 1L),
  case_int = function(x,y) mutate(x, value = if_else(l, b, 1L)),
  case_chr = function(x,y) mutate(x, value = if_else(l, a, "literal")),
  group_summ = function(x,y) summarize(group_by(x, a), value = sum(b, na.rm = TRUE)),
  inner = function(x,y) inner_join(x, y, by = "id"),
  left = function(x,y) left_join(x, y, by = "id"),
  right = function(x,y) right_join(x, y, by = "id"),
  full = function(x,y) full_join(x, y, by = "id"),
  semi = function(x,y) semi_join(x, y, by = "id"),
  anti = function(x,y) anti_join(x, y, by = "id"),
  distinct = function(x,y) distinct(x, a, .keep_all = TRUE),
  union = function(x,y) union_all(x, filter(x, FALSE)),
  mixed_union = function(x,y) union_all(select(x, id), transmute(y, id = as.double(id))),
  setdiff = function(x,y) setdiff(x, filter(x, FALSE)),
  order = function(x,y) arrange(x, a),
  compute = function(x,y) compute(select(x, a, b)),
  unpack = function(x,y) rename(select(x, starts_with("a")), renamed = a)
)
result <- function(x) tryCatch(x, error = identity)
msg <- function(x) if (inherits(x, "error")) conditionMessage(x) else paste(x, collapse = ",")
test <- function(x, y, names, backend) {
  current <- result(collect(x))
  if (inherits(current, "error")) {
    cat(backend,names,"native execution error:", gsub("\n"," / ",msg(current)),"\n")
    return(c(0L,0L,0L))
  }
  counts <- c(0L, 0L, 0L)
  y <- dplyr::ungroup(current)
  queries <- list(
    character = rlang::expr(where(is.character)),
    integer = rlang::expr(where(is.integer)),
    double = rlang::expr(where(is.double)),
    logical = rlang::expr(where(is.logical)),
    date = rlang::expr(where(function(v) inherits(v, "Date"))),
    factor = rlang::expr(where(is.factor)),
    id = rlang::expr(any_of(c("id", "renamed", "other"))),
    names = rlang::expr(starts_with("a")),
    all = rlang::expr(everything())
  )
  for (position in c("fixed", "included")) for (item in names(queries)) {
    if ((position == "fixed") && length(group_vars(x))) next
    query <- queries[[item]]
    expect <- if (position == "fixed") names(tidyselect::eval_select(query, y[0,])) else setdiff(names(tidyselect::eval_select(query, y[0,])), group_vars(x))
    actual <- if (position == "fixed") result(inspect_grouping(x, .by = !!query, .format = "list")$fixed[[1L]]) else result(inspect_grouping(x, .grouping = grouping_set(all_of(setdiff(names(y), group_vars(x))) & !!query), .format = "list")$included[[1L]])
    cls <- if (identical(expect, actual)) 1L else if (backend == "sqlite" && inherits(actual,"marginplyr_error") && item %in% c("character","integer","double","logical","date","factor") && grepl("predicate", msg(actual), fixed = TRUE)) 2L else 3L
    counts[[cls]] <- counts[[cls]] + 1L
    if (cls == 3L) cat(backend,names,position,item,"expected=", msg(expect)," actual=", gsub("\n"," / ",msg(actual)),"\n")
  }
  counts
}
for (backend in names(local)) {
  inputs <- local[[backend]]
  counts <- c(0L,0L,0L)
  for (shape in names(derive)) {
    x <- suppressWarnings(result(derive[[shape]](inputs[[1L]], inputs[[2L]])))
    if (inherits(x, "error")) {
      cat(backend,shape,"native derivation error:",gsub("\n"," / ",msg(x)),"\n")
      next
    }
    counts <- counts + suppressWarnings(test(x, inputs[[2L]], shape, backend))
  }
  cat("COUNTS",backend,paste(counts,collapse=" / "),"\n")
}
