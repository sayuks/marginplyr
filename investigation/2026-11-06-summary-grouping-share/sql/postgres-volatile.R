Sys.setenv(TESTTHAT_PARALLEL = "false")
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(".", quiet = TRUE)
main <- function() {
  area <- tempfile("mp-postgres-", tmpdir = "/private/tmp")
  dir.create(area)
  socket <- file.path(area, "socket")
  dir.create(socket)
  data_dir <- file.path(area, "data")
  processx::run("initdb", c("-D", data_dir, "-A", "trust", "-U", "postgres"))
  processx::run("pg_ctl", c("-D", data_dir, "-l", file.path(area, "server.log"),
                            "-o", paste0("-k ", socket, " -p 55691 -h ''"), "-w", "start"))
  on.exit(processx::run("pg_ctl", c("-D", data_dir, "-m", "fast", "-w", "stop")), add = TRUE)
  con <- DBI::dbConnect(RPostgres::Postgres(), host = socket, port = 55691L,
                       user = "postgres", dbname = "postgres")
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  cat("PostgreSQL", DBI::dbGetQuery(con, "SHOW server_version")[[1L]], "\n")
  source <- copy_to(con, tibble(p = rep(c("east", "west"), each = 4L),
                                a = rep(c("a", "a", "b", "b"), 2L),
                                b = rep(c("x", "y"), 4L), v = 1:8), "facts")
  count <- 0L
  for (fixed in c(FALSE, TRUE)) for (aggregate in c("auto", "portable")) {
    for (sort in c("none", "first", "last")) for (request in c("parent", "total", "both")) {
      DBI::dbExecute(con, "DROP SEQUENCE IF EXISTS stamp")
      DBI::dbExecute(con, "CREATE SEQUENCE stamp")
      shares <- rlang::exprs(p_share = share_of_parent(value), t_share = share_of_total(value))
      if (request == "parent") shares <- shares[1L]
      if (request == "total") shares <- shares[2L]
      query <- summarize_with_margins(source, value = sum(v) + dbplyr::sql("nextval('stamp')"),
                                      !!!shares, .by = all_of(if (fixed) "p" else character()),
                                      .grouping = rollup(a, b), .aggregate = aggregate,
                                      .id = "set", .sort = sort, .margin_label = NULL)
      result <- collect(query)
      for (i in seq_len(nrow(result))) {
        target <- result$set[[i]] + 1L
        if (target > 3L) target <- 3L
        same <- rep(TRUE, nrow(result))
        if (fixed) same <- result$p == result$p[[i]]
        for (kind in intersect(c("p_share", "t_share"), names(result))) {
          denominator_set <- if (kind == "t_share") 3L else target
          take <- same & result$set == denominator_set
          if (denominator_set == 2L) take <- take & result$a == result$a[[i]]
          stopifnot(sum(take) == 1L)
          want <- if (result$set[[i]] == 3L) 1 else result$value[[i]] / result$value[take]
          if (!isTRUE(all.equal(as.double(result[[kind]][[i]]), as.double(want)))) {
            print(result)
            stop(paste("visible denominator mismatch", fixed, aggregate, sort, request, i, kind))
          }
        }
      }
      count <- count + 1L
    }
  }
  cat(count, "PostgreSQL stateful-share result comparisons matched\n")
}
main()
