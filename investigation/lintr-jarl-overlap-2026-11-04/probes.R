args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1L]] else "/private/tmp/lintr-jarl-overlap-probes.tsv"
cases <- data.frame(
  linter = c(rep("assignment_linter", 4), rep("equals_na_linter", 4),
             rep("seq_linter", 7), "return_linter", rep("vector_logic_linter", 4),
             rep("T_and_F_symbol_linter", 12)),
  code = c("x = 1", "1 -> x", "x <<- 1", "x %<>% f()",
           "x == NA", "NA == NA", "NA_character_ != NA_real_", "x %in% NA",
           "1:length(x)", "seq(length(x))", "length(x):1", "1:n()", "1:.N",
           "seq_len(length(x))", "unlist(lapply(x, seq_len))",
           "f <- function(x) { return(x) }",
           "if (x & y) z", "expect_true(x & y)", "subset(x, a && b)", "filter(x, a && b)",
           "x <- T", "T <- 1", "x[T]", "T[x]", "x ~ T", "x ~ I(T)", "foo(x = T) ~ y", "function(T) T",
           "f <- function(T) T", "x$T", "foo(T = 1)", "f <- function(x = F) x"),
  stringsAsFactors = FALSE
)
cases$lintr <- ""
cases$jarl <- ""
for (i in seq_len(nrow(cases))) {
  path <- tempfile(fileext = ".R")
  writeLines(cases$code[[i]], path)
  selected <- setNames(list(getExportedValue("lintr", cases$linter[[i]])()), cases$linter[[i]])
  findings <- lintr::lint(path, linters = selected)
  cases$lintr[[i]] <- paste(unique(vapply(findings, `[[`, character(1), "linter")), collapse = ",")
  res <- processx::run("jarl", c("check", "--extend-select=DPLYR,expect_not", "--output-format=json", path),
                      error_on_status = FALSE)
  stopifnot(res$status %in% c(0L, 1L))
  parsed <- jsonlite::fromJSON(res$stdout, simplifyVector = FALSE)
  stopifnot(length(parsed$errors) == 0L)
  cases$jarl[[i]] <- paste(unique(vapply(parsed$diagnostics, function(x) x$message$name, character(1))), collapse = ",")
  unlink(path)
}
write.table(cases, out, sep = "\t", row.names = FALSE, quote = TRUE)
print(cases, row.names = FALSE)
cat("lintr", as.character(packageVersion("lintr")), "\n")
cat(processx::run("jarl", "--version")$stdout)

# Confirm that the other 22 default linters do not recover these findings.
reduced <- lintr::linters_with_defaults(
  equals_na_linter = NULL,
  seq_linter = NULL,
  T_and_F_symbol_linter = NULL,
  vector_logic_linter = NULL
)
for (code in c("NA == NA", "length(x):1", "filter(x, a && b)")) {
  baseline <- lintr::lint(text = code, linters = lintr::default_linters,
                         parse_settings = FALSE)
  result <- lintr::lint(text = code, linters = reduced,
                       parse_settings = FALSE)
  stopifnot(length(baseline) == 1L, length(result) == 0L)
}
