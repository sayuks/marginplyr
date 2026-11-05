Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
library(dplyr)
library(marginplyr)
set.seed(753621)
conns <- list(sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
              duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:"))
on.exit <- function() invisible(lapply(conns, DBI::dbDisconnect))
special <- c("a", ".data", ".env", "TRUE", "FALSE", "NA", "NaN", "Inf", "NULL", "...1", "..1", "..0", "..01", "..marginplyr_order_1", "marginplyr_expand_source_1", "a b", "a`b", "a\"b", "a\nb", "日本語", "é", "É", "Rowid", "oid", "_ROWID_")
failures <- list(); count <- 0L; source_failures <- 0L
for (engine in names(conns)) {
  con <- conns[[engine]]
  for (nm in special) for (role in c("g", "f", "v", "id")) {
    src <- tibble(g = c("b", NA, "a"), f = c(2L, 1L, NA_integer_), v = c(3.5, 9.5, NA_real_))
    cols <- names(src)
    if (role != "id") names(src)[cols == role] <- nm
    g <- names(src)[cols == "g"]; f <- names(src)[cols == "f"]
    id <- if (role == "id") nm else "sid"
    # 'a' can coexist with all defaults; reserved syntax is selected by name.
    DBI::dbWriteTable(con, "source", src, overwrite = TRUE)
    input <- tbl(con, "source")
    for (label in list(NULL, "Total")) for (sort in c("none", "last", "first")) {
      err <- function(e) e
      actual <- tryCatch(expand_with_margins(input, .by = all_of(f),
        .grouping = rollup(all_of(g)), .margin_label = label, .sort = sort, .id = id), error = err)
      eager <- tryCatch(expand_with_margins(src, .by = all_of(f),
        .grouping = rollup(all_of(g)), .margin_label = label, .sort = sort, .id = id), error = err)
      count <- count + 1L
      if (!inherits(actual,"error")) actual <- tryCatch(collect(actual), error=err)
      if (inherits(actual,"error") || inherits(eager,"error")) {
        if (inherits(actual,"error") && inherits(eager,"error")) next
        failures[[length(failures)+1L]] <- list(engine=engine,name=nm,role=role,label=label,sort=sort,actual=if(inherits(actual,"error"))conditionMessage(actual) else actual,eager=if(inherits(eager,"error"))conditionMessage(eager) else eager)
      } else {
        if (sort=="none") {
          safe_order <- function(x) {
            old <- names(x)
            names(x) <- paste0("col",seq_along(x))
            x <- arrange(x, across(everything()))
            names(x) <- old
            x
          }
          actual <- safe_order(actual)
          eager <- safe_order(eager)
        }
        if (!isTRUE(all.equal(as.data.frame(actual),as.data.frame(eager),check.attributes=FALSE))) failures[[length(failures)+1L]] <- list(engine=engine,name=nm,role=role,label=label,sort=sort,actual=actual,eager=eager)
      }
    }
  }
  cat(engine, "completed", count, "comparisons; failures", length(failures), "\n")
}
saveRDS(failures, tempfile(pattern = "expand-sql-failures-", fileext = ".rds"))
print(utils::head(failures,10))
invisible(lapply(conns, DBI::dbDisconnect))
