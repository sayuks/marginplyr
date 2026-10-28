args <- commandArgs(trailingOnly = TRUE)
devtools::load_all(if (length(args)) args[[1L]] else getwd(), quiet = TRUE)
script_path <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
output_dir <- dirname(normalizePath(script_path))
suppressPackageStartupMessages(library(dplyr))
data <- data.frame(g = c("a", "b"), x = c(1, 3))
sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
on.exit(DBI::dbDisconnect(sqlite))
on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)
inputs <- list(local = data, data.table = data.table::as.data.table(data),
  dtplyr = dtplyr::lazy_dt(data), sqlite = copy_to(sqlite, data, "input"),
  duckdb = copy_to(duck, data, "input"))
fns <- alist(symbol = sum, formula = ~sum(.x),
  lambda = function(z) sum(z), composed_lambda = function(z) sum(z) + mean(z) - mean(z))
rows <- list()
wrap <- function(x, depth) {
  for (i in seq_len(depth)) x <- call("(", x)
  x
}
for (backend in names(inputs)) for (fn_name in names(fns)) {
  for (depth in 0:2) for (qualified in c(FALSE, TRUE)) {
    for (template in c("s", "{.col}")) {
      fn <- wrap(fns[[fn_name]], depth)
      head <- if (qualified) quote(dplyr::across) else quote(across)
      expr <- rlang::call2(head, quote(x), fn, .names = template)
      source <- if (template == "s") "s" else "x"
      run <- function(share) tryCatch(rlang::inject(summarize_with_margins(
        inputs[[backend]], !!expr, !!!share, .grouping = rollup(g),
        .check_share_source = FALSE
      )) |> collect(), error = identity)
      control <- run(list())
      for (helper in c("share_of_total", "share_of_parent")) {
        share <- list(share = rlang::call2(helper, rlang::sym(source)))
        observed <- run(share)
        control_ok <- !inherits(control, "error") &&
          identical(sort(control[[source]]), c(1, 3, 4))
        share_ok <- !inherits(observed, "error") &&
          isTRUE(all.equal(sort(observed$share), c(0.25,0.75,1)))
        msg <- if (inherits(observed, "error")) conditionMessage(observed) else ""
        rows[[length(rows) + 1L]] <- data.frame(
          backend, fn_name, depth, qualified, template, helper,
          control_ok, share_ok,
          error_class = if (inherits(observed, "error")) class(observed)[[1L]] else "",
          message = msg,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}
results <- tibble::as_tibble(bind_rows(rows))
write.csv(results, file.path(output_dir, "parenthesized-fns-matrix.csv"), row.names = FALSE)
print(count(results, backend, depth, control_ok, share_ok), n = Inf)
failures <- filter(results, control_ok & !share_ok)
cat("CASES", nrow(results), "VALID CONTROL BUT SHARE REFUSED", nrow(failures), "\n")
print(count(failures, backend, fn_name), n = Inf)
stopifnot(nrow(failures) > 0L)
