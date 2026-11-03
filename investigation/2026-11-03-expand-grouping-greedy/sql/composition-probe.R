Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
outdir <- "investigation/2026-11-03-expand-grouping-greedy/sql"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
capture <- function(code) {
  tryCatch(list(ok = TRUE, value = force(code)), error = function(e) {
    list(ok = FALSE, error = conditionMessage(e), class = class(e)[1L])
  })
}
sources <- list(
  identity = identity,
  collapse = function(x) collapse(x),
  cte_collapse = function(x) collapse(x, sql_options = dbplyr::sql_options(cte = TRUE)),
  head = function(x) head(x, 2L),
  distinct = function(x) distinct(x),
  transmute = function(x) transmute(x, across(everything())),
  row_number = function(x) mutate(x, rn = row_number()),
  row_filter = function(x) filter(x, row_number() <= 2L),
  group = function(x) group_by(x, g),
  union_self = function(x) union_all(x, x),
  semi_self = function(x) semi_join(x, x, by = "g"),
  anti_self = function(x) anti_join(x, x, by = "g"),
  full_self = function(x) full_join(x, x, by = "g", suffix = c(".a", ".b")),
  cross_self = function(x) cross_join(x, x, suffix = c(".a", ".b")),
  nest_join = function(x) nest_join(x, x, by = "g"),
  rename = function(x) rename(x, renamed = g)
)
opts <- list(default = NULL, cte = dbplyr::sql_options(cte = TRUE),
             no_star = dbplyr::sql_options(use_star = FALSE))
rows <- list()
for (backend in c("sqlite", "duckdb")) {
  con <- if (backend == "sqlite") DBI::dbConnect(RSQLite::SQLite()) else
    DBI::dbConnect(duckdb::duckdb())
  for (nm in c("TRUE", "FALSE", ".env", "normal")) {
    dat <- tibble(g = c("b", "a", NA_character_), rid = 1:3, val = c(7L, 11L, 19L))
    names(dat)[3L] <- nm
    DBI::dbWriteTable(con, "src", as.data.frame(dat), overwrite = TRUE)
    src <- tbl(con, "src")
    for (sort in c("none", "first", "last")) {
      expanded <- expand_with_margins(src, .grouping = rollup(g), .id = "sid", .sort = sort)
      mat <- compute(expanded, name = "ordinary_control", overwrite = TRUE)
      for (shape in names(sources)) for (option in names(opts)) {
        control <- capture(collect(sources[[shape]](mat), sql_options = opts[[option]]))
        result <- capture(collect(sources[[shape]](expanded), sql_options = opts[[option]]))
        status <- if (!control$ok) "ordinary_error" else if (!result$ok) "package_error" else {
          a <- ungroup(result$value)
          b <- ungroup(control$value)
          # Row order of derived queries is delegated; compare complete row bags.
          sig <- function(x) sort(vapply(seq_len(nrow(x)), function(i) {
            paste(vapply(x, function(y) paste(y[i], collapse = ","), ""), collapse = "|#|" )
          }, ""))
          if (identical(names(a), names(b)) && identical(sig(a), sig(b))) "match" else "difference"
        }
        rows[[length(rows) + 1L]] <- data.frame(backend, name = nm, sort, shape, option,
          status, detail = if (!result$ok) result$error else if (!control$ok) control$error else "")
      }
    }
  }
  DBI::dbDisconnect(con, shutdown = backend == "duckdb")
}
res <- do.call(rbind, rows)
write.csv(res, file.path(outdir, "composition-probe.csv"), row.names = FALSE)
print(with(res, table(backend, status)))
print(unique(res[res$status %in% c("package_error", "difference"),
                 c("backend", "name", "shape", "option", "status", "detail")]), row.names = FALSE)
