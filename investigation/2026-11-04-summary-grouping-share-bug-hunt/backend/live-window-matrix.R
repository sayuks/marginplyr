pkgload::load_all(quiet = TRUE)
library(dplyr)
out_dir <- 'investigation/2026-11-04-summary-grouping-share-bug-hunt/backend'
set.seed(20261104)
rows <- list()
for (backend in c('sqlite', 'duckdb')) {
  con <- if (backend == 'sqlite') DBI::dbConnect(RSQLite::SQLite(), ':memory:') else DBI::dbConnect(duckdb::duckdb(), dbdir = ':memory:', shared_home = FALSE)
  for (fixture in c('finite', 'zero', 'missing', 'empty')) {
    dat <- data.frame(f = rep(c('z', 'a', NA_character_), each = 6L),
      g = rep(c('x', 'x', 'y', 'y', NA, NA), 3L), h = rep(c(1L, 2L), 9L),
      x = sample(c(-5, -1, 0, 2, 7), 18L, replace = TRUE))
    if (fixture == 'zero') dat$x <- 0
    if (fixture == 'missing') dat$x <- NA_real_
    if (fixture == 'empty') dat <- dat[FALSE, ]
    src <- copy_to(con, dat, paste0('facts_', fixture), temporary = TRUE)
    for (fixed in c(FALSE, TRUE)) for (portable in c(FALSE, TRUE)) for (sorted in c(FALSE, TRUE)) {
      query <- summarize_with_margins(src, n = sum(x),
        p = share_of_parent(n), t = share_of_total(n),
        .grouping = rollup(g, h), .by = all_of(if (fixed) "f" else character()),
        .id = 'set', .margin_label = NULL, .sort = if (sorted) 'last' else 'none',
        .duplicates = if (portable) 'keep' else 'error', .check_share_source = FALSE)
      for (verb in c('direct', 'select', 'mutate', 'collapse', 'compute')) {
        result <- tryCatch({
          candidate <- switch(verb, direct = query, select = select(query, everything()),
            mutate = mutate(query, k = n + 1L), collapse = collapse(query),
            compute = compute(query, name = paste0('tmp_', fixed, portable, sorted)))
          collect(candidate)
        }, error = identity)
        pass <- !inherits(result, 'error')
        message <- if (pass) '' else conditionMessage(result)
        if (pass) {
          expected_t <- expected_p <- numeric(nrow(result))
          for (i in seq_len(nrow(result))) {
            if (result$set[[i]] == 3L) {expected_t[[i]] <- expected_p[[i]] <- 1; next}
            same <- rep(TRUE, nrow(result))
            if (fixed) same <- (is.na(result$f) & is.na(result$f[[i]])) | (!is.na(result$f) & !is.na(result$f[[i]]) & result$f == result$f[[i]])
            total <- result$n[which(same & result$set == 3L)]
            parent_id <- result$set[[i]] + 1L
            if (parent_id == 2L) same <- same & ((is.na(result$g) & is.na(result$g[[i]])) | (!is.na(result$g) & !is.na(result$g[[i]]) & result$g == result$g[[i]]))
            parent <- result$n[which(same & result$set == parent_id)]
            ratio <- function(denom) if (length(denom) != 1L || is.na(result$n[[i]]) || is.na(denom) || denom == 0) NA_real_ else result$n[[i]] / denom
            expected_t[[i]] <- ratio(total); expected_p[[i]] <- ratio(parent)
          }
          pass <- isTRUE(all.equal(as.numeric(result$t), expected_t)) && isTRUE(all.equal(as.numeric(result$p), expected_p))
          if (!pass) message <- paste('ratio mismatch', paste(result$t, collapse = ','), paste(expected_t, collapse = ','))
        }
        rows[[length(rows) + 1L]] <- data.frame(backend, fixture, fixed, portable, sorted, verb, pass, message)
        if (verb == 'compute' && !inherits(result, 'error')) DBI::dbRemoveTable(con, paste0('tmp_', fixed, portable, sorted))
      }
    }
  }
  if (backend == 'duckdb') DBI::dbDisconnect(con, shutdown = TRUE) else DBI::dbDisconnect(con)
}
results <- do.call(rbind, rows)
write.csv(results, file.path(out_dir, 'live-window-matrix.csv'), row.names = FALSE)
print(results[!results$pass, ], row.names = FALSE)
cat('cases:', nrow(results), 'failures:', sum(!results$pass), '\n')
stopifnot(all(results$pass))
