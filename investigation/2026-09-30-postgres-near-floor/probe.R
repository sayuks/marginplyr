args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
case <- args[[1L]]
out <- args[[2L]]
socket <- args[[3L]]
dir.create(out, recursive = TRUE, showWarnings = FALSE)

library(marginplyr)
library(RPostgres)
stopifnot(length(.libPaths()) == 3L)
target <- if (case == "near") {
  c(dplyr = "1.2.0", dbplyr = "2.6.0", cli = "3.6.2", glue = "1.6.2",
    rlang = "1.1.7", tidyselect = "1.2.1", vctrs = "0.7.1")
} else {
  c(dplyr = "1.2.1", dbplyr = "2.6.0", cli = "3.6.6", glue = "1.8.1",
    rlang = "1.3.0", tidyselect = "1.2.1", vctrs = "0.7.3")
}
stopifnot(identical(vapply(names(target), function(package) {
  as.character(packageVersion(package))
}, character(1)), target))
stopifnot(identical(as.character(packageVersion("RPostgres")), "1.4.10"))
con <- DBI::dbConnect(
  RPostgres::Postgres(), dbname = "marginprobe", host = socket,
  port = 55475L, user = "marginprobe"
)
server <- DBI::dbGetQuery(con, "SELECT version() AS version")$version[[1L]]
data <- data.frame(
  period = c("P1", "P1", "P1", "P2", "P2"),
  region = c("A", "A", "B", "A", "B"),
  store = c("x", "y", "x", "x", NA_character_),
  value = c(2L, 3L, 5L, 7L, 11L)
)
DBI::dbWriteTable(con, "source_rows", data, temporary = TRUE)
DBI::dbExecute(con, "CREATE TEMP SEQUENCE read_counter START 1")
DBI::dbExecute(con, paste(
  "CREATE FUNCTION pg_temp.probe_value(v integer) RETURNS integer",
  "LANGUAGE plpgsql VOLATILE AS $$ BEGIN",
  "PERFORM nextval('pg_temp.read_counter'); RETURN v; END $$"
))
DBI::dbExecute(con, paste(
  "CREATE TEMP VIEW source_probe AS SELECT period, region, store,",
  "pg_temp.probe_value(value) AS value FROM source_rows",
  "WHERE pg_temp.probe_value(1) = 1"
))
source <- dplyr::tbl(con, "source_probe")
read_count <- function() {
  counter <- DBI::dbGetQuery(con, "SELECT last_value, is_called FROM read_counter")
  if (counter$is_called[[1L]]) as.numeric(as.character(counter$last_value[[1L]])) else 0
}
construction_reads <- c(source = read_count())
stopifnot(identical(construction_reads[["source"]], 0))
options(marginplyr.audit_sql = TRUE)

plan <- inspect_grouping(
  source, .by = period, .grouping = rollup(region, store), .format = "list"
)
plan_audit <- last_sent_queries()
construction_reads <- c(construction_reads, plan = read_count())
stopifnot(identical(construction_reads[["plan"]], 0))

summary <- summarize_with_margins(
  source, total = sum(value, na.rm = TRUE), .by = period,
  .grouping = rollup(region, store), .id = "set_id", .sort = "last"
)
summary_audit <- last_sent_queries()
summary_sql <- as.character(dbplyr::sql_render(summary))
stopifnot(grepl("GROUPING SETS", summary_sql, fixed = TRUE))
stopifnot(!grepl("UNION ALL", summary_sql, fixed = TRUE))
construction_reads <- c(construction_reads, summary = read_count())
stopifnot(identical(construction_reads[["summary"]], 0))

shares <- summarize_with_margins(
  source, total = sum(value, na.rm = TRUE), parent = share_of_parent(total),
  grand = share_of_total(total), .by = period,
  .grouping = rollup(region, store), .id = "set_id", .sort = "last"
)
shares_audit <- last_sent_queries()
shares_sql <- as.character(dbplyr::sql_render(shares))
stopifnot(grepl("GROUPING SETS", shares_sql, fixed = TRUE))
construction_reads <- c(construction_reads, shares = read_count())
stopifnot(identical(construction_reads[["shares"]], 0))

expanded <- expand_with_margins(
  source, .by = period, .grouping = rollup(region, store),
  .id = "set_id", .sort = "last"
)
expanded_audit <- last_sent_queries()
expanded_sql <- as.character(dbplyr::sql_render(expanded))
stopifnot(grepl("UNION ALL", expanded_sql, fixed = TRUE))
construction_reads <- c(construction_reads, expansion = read_count())
stopifnot(identical(construction_reads[["expansion"]], 0))

values <- list(
  plan = plan,
  summary_collect = dplyr::collect(summary),
  shares_collect = dplyr::collect(shares),
  expansion_collect = dplyr::collect(expanded),
  summary_compute = dplyr::collect(dplyr::compute(summary)),
  shares_compute = dplyr::collect(dplyr::compute(shares)),
  expansion_compute = dplyr::collect(dplyr::compute(expanded))
)
stopifnot(as.numeric(read_count()) > 0)
stopifnot(nrow(values$plan) == 3L)
stopifnot(nrow(values$summary_collect) == 11L)
stopifnot(nrow(values$shares_collect) == 11L)
stopifnot(nrow(values$expansion_collect) == 15L)
stopifnot(identical(table(values$expansion_collect$set_id),
                    table(factor(rep(1:3, each = 5), levels = 1:3))))
for (kind in c("summary", "shares", "expansion")) {
  stopifnot(identical(values[[paste0(kind, "_collect")]],
                      values[[paste0(kind, "_compute")]]))
}
print(values$summary_collect)
stopifnot(identical(values$summary_collect$period,
                    c(rep("P1", 6L), rep("P2", 5L))))
stopifnot(identical(values$summary_collect$region,
                    c("A", "A", "A", "B", "B", "Total",
                      "A", "A", "B", "B", "Total")))
stopifnot(identical(values$summary_collect$store,
                    c("x", "y", "Total", "x", "Total", "Total",
                      "x", "Total", NA_character_, "Total", "Total")))
stopifnot(identical(values$summary_collect$set_id,
                    c(1L, 1L, 2L, 1L, 2L, 3L, 1L, 2L, 1L, 2L, 3L)))
stopifnot(identical(as.numeric(values$summary_collect$total),
                    c(2, 3, 5, 5, 5, 10, 7, 7, 11, 11, 18)))
stopifnot(isTRUE(all.equal(values$shares_collect$parent,
                           c(2 / 5, 3 / 5, 5 / 10, 1, 5 / 10, 1,
                             1, 7 / 18, 1, 11 / 18, 1))))
stopifnot(isTRUE(all.equal(values$shares_collect$grand,
                           c(2 / 10, 3 / 10, 5 / 10, 5 / 10, 5 / 10, 1,
                             7 / 18, 7 / 18, 11 / 18, 11 / 18, 1))))

dput(values, file.path(out, "values.dput"),
     control = c("keepNA", "keepInteger", "niceNames", "showAttributes", "digits17"))
writeLines(trimws(readLines(file.path(out, "values.dput")), which = "right"),
           file.path(out, "values.dput"))
stopifnot(identical(values, dget(file.path(out, "values.dput")), num.eq = FALSE))
utils::write.csv(data.frame(stage = names(construction_reads),
                            source_row_reads = unname(construction_reads)),
                 file.path(out, "construction-reads.csv"), row.names = FALSE)
for (name in names(values)) {
  if (name != "plan") {
    utils::write.csv(values[[name]], file.path(out, paste0(name, ".csv")),
                     row.names = FALSE, na = "")
  }
}
writeLines(summary_sql, file.path(out, "summary.sql"))
writeLines(shares_sql, file.path(out, "shares.sql"))
writeLines(expanded_sql, file.path(out, "expansion.sql"))
for (name in c("plan", "summary", "shares", "expanded")) {
  utils::write.csv(get(paste0(name, "_audit")),
                   file.path(out, paste0(name, "-sent-queries.csv")),
                   row.names = FALSE, na = "")
}

loaded <- loadedNamespaces()
paths <- vapply(loaded, function(package) normalizePath(find.package(package)),
                character(1))
external <- !startsWith(paths, paste0(normalizePath(.Library), "/"))
allowed <- normalizePath(.libPaths()[1:2])
stopifnot(all(vapply(paths[external], function(path) {
  any(startsWith(path, paste0(allowed, "/")))
}, logical(1))))
loaded <- loaded[external]
manifest <- data.frame(
  package = loaded,
  version = vapply(loaded, function(package) as.character(packageVersion(package)),
                   character(1)),
  path = unname(paths[external])
)
utils::write.csv(manifest[order(manifest$package), ],
                 file.path(out, "loaded-manifest.csv"), row.names = FALSE)
writeLines(c(
  paste("case:", case), R.version.string, paste("R.home:", R.home()),
  paste("PostgreSQL:", server),
  paste(".libPaths:", paste(.libPaths(), collapse = " | ")),
  paste("source reads before collection:", 0),
  paste("source reads after collection and computation:", read_count())
), file.path(out, "environment.txt"))
cat("case:", case, "\n", "PostgreSQL:", server, "\n")
for (name in names(values)) {
  value <- values[[name]]
  cat(name, "rows:", nrow(value), "columns:",
      paste(names(value), collapse = ","), "types:",
      paste(vapply(value, typeof, character(1)), collapse = ","), "\n")
  print(value)
}
cat("source-row reads after collection and computation:", read_count(), "\n")
DBI::dbDisconnect(con)
