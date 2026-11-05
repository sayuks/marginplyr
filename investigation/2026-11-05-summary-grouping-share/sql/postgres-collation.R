suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(".", quiet = TRUE)
local({
  con <- DBI::dbConnect(RPostgres::Postgres(), host = Sys.getenv("MP_HUNT_SOCKET"),
                        port = 55689L, dbname = "postgres")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, paste(
    "CREATE COLLATION pg_temp.hunt_casefold",
    "(provider = icu, locale = 'und-u-ks-level2', deterministic = false)"
  ))
  DBI::dbExecute(con, paste(
    "CREATE TEMP TABLE src (p text COLLATE pg_temp.hunt_casefold,",
    "g text COLLATE pg_temp.hunt_casefold, h text, v integer)"
  ))
  DBI::dbExecute(con, paste(
    "INSERT INTO src VALUES ('A','X','k',1),('a','x','k',2),('a','X','l',3),",
    "('A','Y','k',6),(NULL,'x','m',7),(NULL,'X','n',8),('B',NULL,'k',4)"
  ))
  input <- tbl(con, "src")
  count <- 0L
  for (mode in c("error", "keep")) for (sort in c("none", "last", "first")) {
    for (label in list(NULL, "total")) {
      query <- summarize_with_margins(
        input, value = sum(v, na.rm = TRUE), parent = share_of_parent(value),
        total = share_of_total(value), .by = p, .grouping = rollup(g, h),
        .id = "set", .duplicates = mode, .sort = sort, .margin_label = label
      )
      ans <- collect(query)
      key <- function(x) ifelse(is.na(x), "<missing>", tolower(x))
      grand <- match(key(ans$p), key(ans$p[ans$set == 3L]))
      middle <- match(paste(key(ans$p), key(ans$g)), paste(
        key(ans$p[ans$set == 2L]), key(ans$g[ans$set == 2L])
      ))
      want_total <- ifelse(ans$set == 3L, 1, as.double(ans$value) /
        as.double(ans$value[ans$set == 3L][grand]))
      want_parent <- ifelse(ans$set != 1L, want_total, as.double(ans$value) /
        as.double(ans$value[ans$set == 2L][middle]))
      stopifnot(nrow(ans) == 13L, isTRUE(all.equal(ans$parent, want_parent)),
                isTRUE(all.equal(ans$total, want_total)))
      count <- count + 1L
    }
  }
  cat("PostgreSQL case-insensitive collation comparisons:", count, "\n")
})
