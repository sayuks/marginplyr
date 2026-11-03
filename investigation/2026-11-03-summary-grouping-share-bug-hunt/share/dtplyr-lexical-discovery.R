Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
dir <- "investigation/2026-11-03-summary-grouping-share-bug-hunt/share"
cat("SHA:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE), "\n")
input <- data.frame(g = c("a", "b"), .BY = c(1, 3), x = c(2, 6))
expressions <- alist(
  direct = local({ .BY <- c(5, 7); sum(.BY) }),
  literal_list2env = local({base::list2env(list(.BY = c(5, 7)), envir = environment()); sum(.BY)}),
  dynamic_list2env = local({base::list2env(setNames(list(c(5, 7)), ".BY"), envir = environment()); sum(.BY)}),
  env_bind = local({rlang::env_bind(environment(), .BY = c(5, 7)); sum(.BY)}),
  env_poke = local({rlang::env_poke(environment(), ".BY", c(5, 7)); sum(.BY)}),
  reflected_get = sum(get(".BY")),
  lambda_list2env = (function() {base::list2env(list(.BY = c(5, 7)), envir = environment()); sum(.BY)})(),
  nested_eval = local({eval(quote(base::list2env(list(.BY = c(5, 7)), envir = environment()))); sum(.BY)})
)
rows <- list()
for (column in c(".BY", ".SD", ".GRP", ".NGRP", ".SDall", ".xSD", ".iSD")) {
  data <- input
  names(data)[names(data) == ".BY"] <- column
  safe <- data
  names(safe)[names(safe) == column] <- "measure"
  for (name in names(expressions)) {
    body <- rlang::parse_expr(gsub(".BY", column,
                                  paste(deparse(expressions[[name]]), collapse = "\n"),
                                  fixed = TRUE))
    for (mode in c("local", "dt_no_share", "dt_share", "dt_renamed")) {
      source <- switch(mode, local = data, dt_renamed = dtplyr::lazy_dt(safe), dtplyr::lazy_dt(data))
      args <- list(value = quote(sum(x)), independent = body)
      if (mode %in% c("local", "dt_share", "dt_renamed")) args$fraction <- quote(share_of_total(value))
      call <- rlang::call2("summarize_with_margins", quote(source), !!!args, .grouping = quote(rollup(g)))
      out <- tryCatch({q <- eval(call); if (inherits(q, "dtplyr_step")) collect(q) else q}, error = identity)
      row <- data.frame(column, name, mode, error = inherits(out, "error"),
                        package_error = inherits(out, "marginplyr_error"),
                        values = if (inherits(out, "error")) "" else paste(out$independent, collapse = ","),
                        message = if (inherits(out, "error")) conditionMessage(out) else "")
      rows[[length(rows) + 1L]] <- row
      cat(column, name, mode, if (row$error) class(out)[[1L]] else row$values, "\n")
    }
  }
}
write.csv(do.call(rbind, rows), file.path(dir, "dtplyr-lexical-discovery.csv"), row.names = FALSE)
