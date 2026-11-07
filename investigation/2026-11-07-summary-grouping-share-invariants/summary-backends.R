Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
library(dplyr)
a <- tibble::tibble(f = c("q", "q", "q", "r", "r", NA),
                    g = c("a", "a", NA, "b", "b", "b"),
                    h = c(1L, 2L, 1L, 2L, 2L, NA),
                    x = c(2, 7, 4, 9, 3, 1))
cs <- list(sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
           duckdb = DBI::dbConnect(duckdb::duckdb(), ":memory:"))
for (con in cs) DBI::dbWriteTable(con, "data", a)
inputs <- c(list(local = a, dtplyr = dtplyr::lazy_dt(a), arrow = arrow::Table$create(a)),
            lapply(cs, dplyr::tbl, "data"))
specs <- list(rollup = rollup(g, h), reverse = grouping_sets(grouping_set(), grouping_set(g)),
              composite = rollup(c(g, h)), duplicated = rollup(g, g),
              nonhier = grouping_sets(grouping_set(h), grouping_set(g), grouping_set()))
expressions <- list(sum = rlang::exprs(val = sum(x)), count = rlang::exprs(n = n()),
                    constant = rlang::exprs(val = 2L),
                    bit = rlang::exprs(bit = grouping_bit(g)),
                    flag = rlang::exprs(id = grouping_id()),
                    none = rlang::exprs(),
                    dimension = rlang::exprs(vals = sum(h, na.rm = TRUE)),
                    select = rlang::exprs(across(x, sum)),
                    source = rlang::exprs(val = sum(.data[["x"]])),
                    bool = rlang::exprs(v = sum(x) > 5),
                    special = rlang::exprs(`.ROW` = sum(x), `.BY` = n()))
try <- function(expr) suppressWarnings(tryCatch(expr, error = identity))
errors <- list()
rows <- 0L
fix <- function(dat, sort) {
  dat <- tibble::as_tibble(dat)
  if (sort == "none") dat <- dplyr::arrange(dat, dplyr::across(dplyr::everything()))
  dat
}
for (spec in names(specs)) for (expr in names(expressions))
for (empty in c(FALSE, TRUE)) for (fixed in c(FALSE, TRUE))
for (sort in c("none", "last", "first")) {
  call <- function(src) {
    if (empty) src <- dplyr::filter(src, x < 0)
    summarize_with_margins(src, !!!expressions[[expr]],
       .grouping = specs[[spec]],
       .by = !!if (fixed) rlang::sym("f") else NULL,
       .duplicates = "keep", .id = "plan", .margin_label = NULL, .sort = sort)
  }
  expected <- try(fix(call(a), sort))
  for (backend in setdiff(names(inputs), "local")) {
    rows <- rows + 1L
    actual <- try(fix(dplyr::collect(call(inputs[[backend]])), sort))
    id <- paste(spec, expr, empty, fixed, sort, backend, sep = "/")
    if (inherits(expected, "error") || inherits(actual, "error")) {
      errors[[id]] <- paste(if (inherits(actual, "error")) conditionMessage(actual) else "accepted",
                             " | local ",
                             if (inherits(expected, "error")) conditionMessage(expected) else "accepted")
    } else if (!isTRUE(all.equal(actual, expected, check.attributes = FALSE))) {
      errors[[id]] <- all.equal(actual, expected, check.attributes = FALSE)
    }
  }
}
cat("count", rows, "mismatches", length(errors), "\n")
for (x in names(errors)) cat(x, ":", paste(errors[[x]], collapse = " / "), "\n")
for (con in cs) DBI::dbDisconnect(con)
