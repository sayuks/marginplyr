Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "/private/tmp/marginplyr-exhaustive-b-root/source"), quiet = TRUE)
out_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", "/private/tmp/marginplyr-expand-grouping-exhaustive-20261107-b/investigation/2026-11-07-expand-grouping-exhaustive-b/coordinator")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
special <- c("normal", ".env", ".data", "...", "..1", "..0", "..00", "..01", "..-1", "..+1", ".. 1", ".. -1", ".BY", ".I", ".N", ".SD", ".GRP", "a b", "a`b", "a\nb", "a\tb", "é", "NA", "TRUE", ".marginplyr_set_id", "__marginplyr_set_id")
results <- list()
idx <- 0L
for (backend in c("local", "dtplyr", "sqlite", "duckdb")) {
  con <- switch(backend, sqlite = DBI::dbConnect(RSQLite::SQLite()), duckdb = DBI::dbConnect(duckdb::duckdb()), NULL)
  for (role in c("dimension", "fixed", "payload", "id")) {
    for (name in special) {
      for (sort in c("none", "first", "last")) {
        idx <- idx + 1L
        dat <- tibble::tibble(fixed = c(2L, 1L), dimension = c("B", "A"), payload = c(13L, 29L))
        names(dat)[match(role, c("fixed", "dimension", "payload"), nomatch = 0L)] <- name
        fixed <- if (role == "fixed") name else "fixed"
        dimension <- if (role == "dimension") name else "dimension"
        payload <- if (role == "payload") name else "payload"
        id <- if (role == "id") name else "set_id"
        input <- tryCatch(switch(backend, local = dat, dtplyr = dtplyr::lazy_dt(dat), dplyr::copy_to(con, dat, overwrite = TRUE, name = "source")), error = identity)
        if (inherits(input, "error")) {
          results[[idx]] <- data.frame(backend, role, name, sort, ok = NA, symptom = paste("Input constructor:", conditionMessage(input)))
          next
        }
        control <- tryCatch({
          dplyr::select(input, dplyr::everything())
          tidyselect::eval_select(rlang::expr(tidyselect::all_of(c(!!fixed, !!dimension))), dat)
          created <- dplyr::mutate(input, !!id := 1L)
          combined <- dplyr::union_all(created, created)
          combined <- dplyr::relocate(combined, tidyselect::all_of(c(fixed, dimension, id)))
          if (backend != "local") dplyr::collect(combined) else combined
        }, error = identity)
        if (inherits(control, "error")) {
          results[[idx]] <- data.frame(backend, role, name, sort, ok = NA, symptom = paste("Ordinary operation:", conditionMessage(control)))
          next
        }
        expected <- dat[rep(seq_len(nrow(dat)), 2L), ]
        expected[[dimension]][3:4] <- "Total"
        expected[[id]] <- rep(1:2, each = 2L)
        expected <- expected[c(fixed, dimension, id, payload)]
        actual <- tryCatch(expand_with_margins(input, .by = tidyselect::all_of(fixed), .grouping = rollup(tidyselect::all_of(dimension)), .id = id, .sort = sort), error = identity)
        if (!inherits(actual, "error")) actual <- tryCatch(if (backend == "local") actual else dplyr::collect(actual), error = identity)
        normalize <- function(x) x[order(x[[id]], x[[fixed]]), ]
        ok <- !inherits(actual, "error") && identical(normalize(tibble::as_tibble(actual, .name_repair = "minimal")), normalize(expected))
        results[[idx]] <- data.frame(backend, role, name, sort, ok, symptom = if (inherits(actual, "error")) conditionMessage(actual) else if (!ok) paste(capture.output(str(actual)), collapse = " ") else "")
      }
    }
  }
  if (!is.null(con)) DBI::dbDisconnect(con)
}
results <- do.call(rbind, results)
write.csv(results, file.path(out_dir, "names.csv"), row.names = FALSE)
cat("Observations:", nrow(results), "; matches:", sum(results$ok, na.rm = TRUE), "; mismatches:", sum(!results$ok, na.rm = TRUE), "; ordinary-input exclusions:", sum(is.na(results$ok)), "\n")
print(results[which(!results$ok), ], row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(out_dir, "session-info.txt"))
