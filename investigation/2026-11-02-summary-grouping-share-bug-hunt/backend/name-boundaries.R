options(warn = 1)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
library(dplyr)
out <- "investigation/2026-11-02-summary-grouping-share-bug-hunt/backend"
cons <- list(sqlite = DBI::dbConnect(RSQLite::SQLite()),
             duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE)))
capture <- function(f) tryCatch(list(ok = TRUE, value = f(), message = ""),
                               error = function(e) list(ok = FALSE, message = conditionMessage(e)))
nms <- c("value", "Inf", "NaN", "NA", "NULL", "TRUE", "FALSE", "T", "F",
         ".data", ".env", ".", "..", "...", "..0", "..00", "..-0", "..+0",
         ".. 1", "..\t1", "..1.0", "..1e0", "..01", "..+01", "..-01",
         "..2147483648", "..-2147483648", "_rowid_", "rowid", "oid", ".BY", ".N", ".SD")
rows <- list()
for (bk in names(cons)) {
  con <- cons[[bk]]
  tbl <- copy_to(con, tibble(g = c("a", "b"), x = c(1, 3)), "name_probe", overwrite = TRUE)
  for (nm in nms) for (role in c("output", "share", "id")) {
    dot <- setNames(list(rlang::quo(sum(x, na.rm = TRUE))), if (role == "output") nm else "v")
    if (role == "share") dot[[nm]] <- rlang::quo(share_of_total(v))
    ctrl <- capture(function() collect(summarise(tbl, !!!setNames(list(rlang::quo(sum(x, na.rm = TRUE))), nm))))
    r <- capture(function() {
      q <- summarize_with_margins(tbl, !!!dot, .grouping = rollup(g),
                                  .id = if (role == "id") nm else NULL,
                                  .check_share_source = FALSE, .sort = "last")
      v <- collect(q)
      z <- collect(compute(q, name = paste0("out_", role), overwrite = TRUE))
      target <- v[[nm]]
      expected <- switch(role, output = c(1, 3, 4), share = c(0.25, 0.75, 1), id = c(1L, 1L, 2L))
      stopifnot(identical(names(v), names(z)), isTRUE(all.equal(as_tibble(v), as_tibble(z))),
                isTRUE(all.equal(target, expected, check.attributes = FALSE)))
      TRUE
    })
    rows[[length(rows) + 1L]] <- data.frame(backend = bk, name = nm, role = role,
                                           control = ctrl$ok, margin = r$ok,
                                           message = r$message)
  }
  DBI::dbDisconnect(con)
}
ans <- bind_rows(rows)
write.csv(ans, file.path(out, "name-boundaries.csv"), row.names = FALSE)
print(with(ans, table(backend, margin)))
print(ans[!ans$margin, ], row.names = FALSE)
