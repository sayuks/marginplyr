Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
dir <- "investigation/2026-11-03-summary-grouping-share-bug-hunt/share"
run_case <- function(column, helper, style, role, shape, mode) {
  input <- data.table::data.table(f = c("F", "F", NA, NA),
                                  g = c("a", "b", "a", "b"),
                                  x = c(2, 6, 4, 12), unused = c(1, 3, 2, 6))
  data.table::setnames(input, "unused", column)
  before <- data.table::copy(input)
  body <- if (shape == "local") {
    "local({base::list2env(list(SPECIAL = c(5, 7)), envir = environment()); sum(SPECIAL)})"
  } else {
    "(function() {base::list2env(list(SPECIAL = c(5, 7)), envir = environment()); sum(SPECIAL)})()"
  }
  body <- rlang::parse_expr(gsub("SPECIAL", column, body, fixed = TRUE))
  data <- if (mode == "local") tibble::as_tibble(input) else {
    value <- data.table::copy(input)
    if (mode == "renamed") data.table::setnames(value, column, "unused")
    dtplyr::lazy_dt(value)
  }
  source <- if (role == "source") body else quote(sum(x))
  args <- if (style == "direct") list(value = source) else {
    list(rlang::expr(dplyr::across(x, ~ !!source, .names = "value")))
  }
  if (role == "independent") args$independent <- body
  args$fraction <- rlang::call2(helper, quote(value))
  args$.by <- quote(f)
  args$.grouping <- quote(rollup(g))
  out <- tryCatch({
    q <- eval(rlang::call2("summarize_with_margins", quote(data), !!!args))
    if (inherits(q, "dtplyr_step")) dplyr::collect(q) else q
  }, error = identity)
  values <- if (inherits(out, "error")) numeric() else out[[if (role == "source") "value" else "independent"]]
  data.frame(column, helper, style, role, shape, mode,
             error = inherits(out, "error"),
             values = paste(values, collapse = ","), lexical = length(values) == 6L && all(values == 12),
             untouched = identical(input, before),
             message = if (inherits(out, "error")) conditionMessage(out) else "")
}
rows <- list()
for (column in c(".BY", ".SD", ".GRP", ".NGRP", ".SDall", ".xSD", ".iSD")) {
  for (helper in c("share_of_total", "share_of_parent")) {
    for (style in c("direct", "across")) {
      for (role in c("source", "independent")) {
        for (shape in c("local", "lambda")) {
          for (mode in c("local", "lazy", "renamed")) {
            rows[[length(rows) + 1L]] <- run_case(column, helper, style, role, shape, mode)
          }
        }
      }
    }
  }
}
out <- do.call(rbind, rows)
stopifnot(!any(out$error), all(out$untouched),
          all(out$lexical[out$mode != "lazy"]),
          !any(out$lexical[out$mode == "lazy"]))
write.csv(out, file.path(dir, "dtplyr-list2env-matrix.csv"), row.names = FALSE)
cat("SHA:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE), "\n")
cat("Wrong lexical values:", sum(out$mode == "lazy"), "Controls:", sum(out$mode != "lazy"), "\n")
print(with(out, table(mode, lexical)))
