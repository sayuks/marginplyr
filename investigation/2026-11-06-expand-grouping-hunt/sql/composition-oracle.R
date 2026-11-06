Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
library(dplyr)

out_dir <- "investigation/2026-11-06-expand-grouping-hunt/sql"
conns <- list(
  sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
  duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
)
src <- tibble(f = c(2L, 1L, NA_integer_, 1L),
              g = c("b", NA, "a", "b"), h = c("q", "r", NA, "q"),
              v = c(8.5, NA_real_, -2.5, 0), rid = seq_len(4L))
forms <- list(
  table = identity,
  filtered = function(x) filter(x, rid > 0),
  arranged = function(x) arrange(x, desc(rid)),
  collapsed = function(x) collapse(filter(x, rid > 0)),
  selected = function(x) select(x, rid, v, h, g, f),
  renamed = function(x) rename(rename(x, temporary = g), g = temporary),
  mutated = function(x) mutate(x, v = v + 0),
  window = function(x) mutate(x, v = v + 0 * row_number(rid)),
  limited = function(x) head(arrange(x, rid), 3L),
  distinct = function(x) distinct(x),
  unioned = function(x) union_all(filter(x, rid <= 2), filter(x, rid > 2)),
  joined = function(x) left_join(x, select(x, rid), by = "rid"),
  cte = function(x) tbl(dbplyr::remote_con(x), dbplyr::sql(paste0(
    "WITH marginplyr_expand_source_1 AS (SELECT * FROM facts) ",
    "SELECT * FROM marginplyr_expand_source_1 WHERE rid > 0")), vars = names(src)),
  expression = function(x) tbl(dbplyr::remote_con(x), dbplyr::sql(
    "SELECT f, g, h, v, rid FROM facts WHERE rid > 0"), vars = names(src))
)
specs <- list(
  rollup = list(spec = rollup(g, h), sets = list(c("g", "h"), "g", character())),
  cube = list(spec = cube(g, h), sets = list(c("g", "h"), "g", "h", character())),
  reverse = list(spec = grouping_sets(grouping_set(), grouping_set(h), grouping_set(g, h)),
                 sets = list(character(), "h", c("g", "h")))
)
canonical <- function(x) {
  x <- as.data.frame(x)
  x <- x[do.call(order, c(unname(x), list(na.last = TRUE))), , drop = FALSE]
  rownames(x) <- NULL
  x
}
oracle <- function(x, sets, label, sorting, identifier) {
  dimensions <- unique(unlist(sets))
  copies <- lapply(seq_along(sets), function(i) {
    y <- as.data.frame(x)
    for (col in setdiff(dimensions, sets[[i]])) y[[col]] <- if (is.null(label)) NA_character_ else label
    for (col in dimensions) y[[paste0("bit_", col)]] <- as.integer(!col %in% sets[[i]])
    y$set_id <- i
    y
  })
  y <- bind_rows(copies)
  if (sorting != "none") {
    terms <- list(is.na(y$f), y$f)
    for (col in dimensions) {
      bit <- y[[paste0("bit_", col)]]
      terms <- c(terms, list(if (sorting == "first") -bit else bit, is.na(y[[col]]), y[[col]]))
    }
    terms <- c(terms, list(y$set_id))
    y <- y[do.call(order, c(terms, list(na.last = TRUE))), , drop = FALSE]
  }
  public <- c("f", dimensions, setdiff(names(x), c("f", dimensions)))
  if (!is.null(identifier)) {
    y[[identifier]] <- y$set_id
    public <- c("f", dimensions, identifier, setdiff(names(x), c("f", dimensions)))
  }
  y <- as.data.frame(y[public])
  rownames(y) <- NULL
  y
}
results <- list()
failures <- list()
for (engine in names(conns)) {
  con <- conns[[engine]]
  DBI::dbWriteTable(con, "facts", src, overwrite = TRUE)
  for (form in names(forms)) {
    input <- forms[[form]](tbl(con, "facts"))
    base <- collect(input)
    for (spec_name in names(specs)) for (label in list(NULL, "All")) {
      for (sorting in c("none", "last", "first")) for (identifier in list(NULL, "sid")) {
        spec <- specs[[spec_name]]
        expected <- oracle(base, spec$sets, label, sorting, identifier)
        query <- expand_with_margins(input, .by = f, .grouping = spec$spec,
                                    .margin_label = label, .sort = sorting, .id = identifier)
        for (cte in c(FALSE, TRUE)) {
          actual <- tryCatch(collect(query, sql_options = dbplyr::sql_options(cte = cte)), error = identity)
          ok <- !inherits(actual, "error") && identical(names(actual), names(expected)) &&
            isTRUE(all.equal(canonical(actual), canonical(expected), check.attributes = FALSE))
          if (ok && sorting != "none") {
            keys <- c("f", unique(unlist(spec$sets)), identifier)
            a <- as.data.frame(actual[keys]); rownames(a) <- NULL
            e <- as.data.frame(expected[keys]); rownames(e) <- NULL
            ok <- isTRUE(all.equal(a, e, check.attributes = FALSE))
          }
          entry <- data.frame(engine, form, spec_name, label = if (is.null(label)) "NULL" else label,
                              sorting, identifier = if (is.null(identifier)) "NULL" else identifier, cte, ok)
          results[[length(results) + 1L]] <- entry
          if (!ok) failures[[length(failures) + 1L]] <- list(case = entry, expected = expected,
              actual = if (inherits(actual, "error")) conditionMessage(actual) else actual,
              sql = tryCatch(as.character(dbplyr::sql_render(query, sql_options = dbplyr::sql_options(cte = cte))),
                             error = conditionMessage))
        }
      }
    }
    cat(engine, form, length(results), "observations; failures", length(failures), "\n")
    write.csv(bind_rows(results), file.path(out_dir, "composition-results.csv"), row.names = FALSE)
    saveRDS(failures, file.path(out_dir, "composition-failures.rds"))
  }
}
write.csv(bind_rows(results), file.path(out_dir, "composition-results.csv"), row.names = FALSE)
saveRDS(failures, file.path(out_dir, "composition-failures.rds"))
print(head(failures, 6L))
invisible(lapply(conns, DBI::dbDisconnect))
