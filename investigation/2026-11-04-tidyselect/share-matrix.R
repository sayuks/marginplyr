# From the repository root: Rscript share-matrix.R <backend> <prefix> <kind>
# backend: local, data.table, dtplyr, DuckDB, or SQLite.
# prefix: simple (named sums) or across (first ordinary across).
# kind: parent, total, or both.
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(quiet = TRUE)
options(lifecycle_verbosity = "quiet")
d <- data.frame(g = c("a", "a", "b", "b"),
                A01 = c(1, 2, 4, 5), A02 = c(2, 1, 3, 4),
                B01 = c(3, 2, 3, 2), beta = c(7, 8, 9, 10),
                spare = c(11, 12, 13, 14))
sources <- names(d)[-1]
external_names <- c("spare", "A02")
helper_args <- c(starts_with = '"A"', ends_with = '"01"', contains = '"eta"',
                 matches = '"^([AB][0-9]+|spare)$"', num_range = '"A", 1:2, width = 2',
                 one_of = '"beta", "B01"', everything = '', last_col = 'offset = 1',
                 all_of = 'external_names', any_of = 'c("B01", "absent", "A02")')
helper_calls <- function(namespace) sprintf("%s%s(%s)", namespace, names(helper_args), helper_args)
xs <- helper_calls("")
pair_forms <- c("c(%s, %s)", "(%s | %s)", "(%s & %s)", "(%s / %s)",
                "c(%s, -%s)", "(%s & !%s)")
forms <- c(
  xs, helper_calls("tidyselect::"), helper_calls("dplyr::"),
  sprintf("!%s", xs), sprintf("-%s", xs), sprintf("-(!%s)", xs), sprintf("!(-%s)", xs),
  "A01", "spare:B01", "-(A01:B01)", "!(A01:B01)", "c(-A02, -B01, A02)",
  "c(-A02, -B01, !beta, -A01)", "c(A02, B01, -A02, A01)",
  "c(-A02, A02, -B01)", "c(2L, -5L)", "c(-2L, 5L)", "c('beta', 'spare')",
  ".data$beta", ".data[['beta']]", ".env$external_names",
  "c(everything(), -any_of(c('beta','absent')))",
  "-any_of(c('beta','absent'))", "!any_of(c('beta','absent'))", "-any_of('absent')",
  "all_of(character())", "any_of('absent')", "c()", "NULL", "!everything()",
  "all_of('absent')", "-all_of('absent')", "!all_of('absent')",
  "c(out = beta)", "all_of(c(out = 'beta'))", "-unknown", "!unknown",
  "(spare:B01) & !(starts_with('A') | -any_of('beta'))",
  "c(starts_with('A'), -last_col(), -ends_with('01'), !contains('eta'))",
  "c(-starts_with('A'), (last_col() & !ends_with('e')), -any_of('B01'))",
  "tidyselect::all_of(tidyselect::peek_vars()[c(5, 2)])"
)
paired <- as.vector(outer(xs, xs, Vectorize(function(x, y)
  paste(vapply(pair_forms, sprintf, character(1), x, y), collapse = "\n")
)))
forms <- unique(c(forms, unlist(strsplit(paired, "\n", fixed = TRUE))))
backend <- commandArgs(trailingOnly = TRUE)[[1]]
prefix <- commandArgs(trailingOnly = TRUE)[[2]]
kind <- commandArgs(trailingOnly = TRUE)[[3]]
if (backend == "dtplyr") input <- dtplyr::lazy_dt(d)
if (backend == "data.table") input <- data.table::as.data.table(d)
if (backend == "local") input <- d
if (backend %in% c("DuckDB", "SQLite")) {
  conn <- if (backend == "DuckDB") DBI::dbConnect(duckdb::duckdb(shared_home = FALSE)) else
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  input <- dplyr::copy_to(conn, d, "share_matrix_d", temporary = TRUE)
}
if (prefix == "simple") {
  summaries <- lapply(sources, function(nm) rlang::call2("sum", rlang::sym(nm)))
  names(summaries) <- sources
} else {
  summaries <- list(quote(dplyr::across(!any_of("g"), sum)))
}
capture <- function(expr) tryCatch(suppressWarnings(force(expr)), error = identity)
run <- function(form, kind) {
  syntax <- parse(text = form, keep.source = FALSE)[[1]]
  quo <- rlang::new_quosure(syntax, globalenv())
  expected <- capture(names(tidyselect::eval_select(quo, d[sources], allow_rename = FALSE,
                                                    allow_predicates = FALSE)))
  share_call <- rlang::expr(dplyr::across(!!quo, !!rlang::sym(kind), .names = "{.col}_share"))
  actual <- capture(dplyr::collect(rlang::inject(summarize_with_margins(input, !!!summaries,
              !!share_call, .grouping = rollup(g), .check_share_source = FALSE))))
  if (inherits(expected, "error")) {
    if (!inherits(actual, "error")) {
      return(c(kind, form, conditionMessage(expected), paste(names(actual), collapse = "|")))
    }
    return(NULL)
  }
  if (inherits(actual, "error")) {
    return(c(kind, form, paste(expected, collapse = "|"), conditionMessage(actual)))
  }
  actual_names <- names(actual)[!names(actual) %in% c("g", sources)]
  expected_names <- paste0(expected, "_share", recycle0 = TRUE)
  if (!identical(actual_names, expected_names)) {
    return(c(kind, form, paste(expected_names, collapse = "|"), paste(actual_names, collapse = "|")))
  }
  for (name in expected) {
    expected_ratios <- c(sum(d[d$g == "a", name]), sum(d[d$g == "b", name]),
                         sum(d[, name])) / sum(d[, name])
    actual_ratios <- actual[[paste0(name, "_share")]][match(c("a", "b", "Total"), actual$g)]
    if (!isTRUE(all.equal(expected_ratios, actual_ratios))) {
      return(c(kind, form, paste(expected_ratios, collapse = "|"),
                          paste(actual_ratios, collapse = "|")))
    }
  }
  NULL
}
if (backend %in% c("DuckDB", "SQLite", "data.table") || prefix == "across") {
  # Stratified combination samples preserve all operators, helper pairs
  # with the same operand, and all atom/single/nested forms.
  head_forms <- unique(c(xs, helper_calls("tidyselect::"), helper_calls("dplyr::"),
                          forms[seq_len(95L)]))
  forms <- unique(c(head_forms, forms[seq(96L, length(forms), by = 13L)],
    unlist(lapply(xs, function(x) vapply(pair_forms, sprintf, character(1), x, x)))))
}
kinds <- if (kind == "both") c("share_of_parent", "share_of_total") else paste0("share_of_", kind)
out <- stdout()
cat("backend", backend, "prefix", prefix, "kinds", kinds, "n.forms", length(forms), "\n", file = out)
for (kind in kinds) {
  failures <- 0L
  timing <- system.time(for (form in forms) {
    result <- run(form, kind)
    if (!is.null(result)) {
      failures <- failures + 1L
      dput(result, out)
    }
  })
  cat(kind, "failures:", failures, "elapsed:", timing[["elapsed"]], "\n", file = out)
  flush(out)
}
if (backend %in% c("DuckDB", "SQLite")) DBI::dbDisconnect(conn)
