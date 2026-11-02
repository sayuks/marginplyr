Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
out <- "investigation/2026-11-02-summary-grouping-share-bug-hunt/backend"
run <- function() {
  cons <- list(sqlite = DBI::dbConnect(RSQLite::SQLite()),
    duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE)))
  on.exit(lapply(cons, DBI::dbDisconnect), add = TRUE)
  rows <- list()
  for (bk in names(cons)) {
    src <- copy_to(cons[[bk]], tibble(k = c(1L, NA_integer_, 1L, NA_integer_),
      g = c("a", "a", "b", "b"), h = c("x", "y", "x", "y")), "volatile_input")
    measure <- if (bk == "sqlite") dbplyr::sql("(abs(random()) / 9223372036854775808.0) + 0.1") else dbplyr::sql("random() + 0.1")
    for (adapter in c("native", "portable")) for (kind in c("parent", "total")) for (sorting in c("none", "first", "last")) for (repeat_id in 1:3) {
      dots <- list(v = measure, p = rlang::call2(if (kind == "parent") "share_of_parent" else "share_of_total", quote(v)))
      q <- summarize_with_margins(src, !!!dots, .by = k, .grouping = rollup(g,h),
        .check_share_source = FALSE, .id = "set", .sort = sorting,
        .duplicates = if (adapter == "portable") "keep" else "error")
      v <- collect(q)
      stopifnot(is.double(v$p))
      expected <- vapply(seq_len(nrow(v)), function(i) {
        if (v$set[[i]] == 3L) return(1)
        target_set <- if (kind == "total") 3L else v$set[[i]]+1L
        same_partition <- if (is.na(v$k[[i]])) is.na(v$k) else !is.na(v$k) & v$k==v$k[[i]]
        candidates <- which(same_partition & v$set == target_set &
          (target_set == 3L | v$g == v$g[[i]]))
        stopifnot(length(candidates) == 1L)
        v$v[[i]]/v$v[[candidates]]
      }, numeric(1))
      if (!isTRUE(all.equal(v$p, expected))) {
        print(list(backend=bk,adapter=adapter,kind=kind,sort=sorting,replicate=repeat_id))
        print(v)
        print(expected)
        print(dbplyr::sql_render(q))
        stop("Displayed share differs from displayed numerator/denominator")
      }
      rows[[length(rows)+1L]] <- data.frame(backend=bk,adapter=adapter,kind=kind,
        sort=sorting,replicate=repeat_id,rows=nrow(v),matched=TRUE)
    }
  }
  records <- bind_rows(rows)
  write.csv(records,file.path(out,"volatile-shares.csv"),row.names=FALSE)
  cat("Displayed-value oracle matched:",nrow(records),"calls;",sum(records$rows),"share rows\n")
}
run()
