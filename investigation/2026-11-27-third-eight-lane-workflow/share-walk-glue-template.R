# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Second round: the components of a literal glue template are not read by the share dependency walk (#1824).
# String-template evaluators (glue, str_glue, englue, cli inline) read a share or an earlier alias unseen
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressPackageStartupMessages(library(dplyr))
hidden <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 5), sh = c(100, 200, 300))
plain <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 5))
d <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 4))
msg <- function(e) gsub("\\s+", " ", conditionMessage(e))
run <- function(label, expr) {
  out <- tryCatch({
    x <- eval(expr, parent.frame())
    keep <- intersect(c("g", "sh", "z", "zz", "src", "p", "lbl"), names(x))
    paste("value:", paste(capture.output(print(as.data.frame(x)[keep], row.names = FALSE)), collapse = " | "))
  }, error = function(e) paste0("error [", paste(class(e)[1:2], collapse = "/"), "]: ", substr(msg(e), 1, 160)))
  cat(sprintf("  %-10s %s\n", label, out))
}
forward <- function(code, data = "hidden") {
  cat("forward z =", code, "on", data, "\n")
  run("marginplyr", str2lang(sprintf("summarize_with_margins(%s, s = sum(v), sh = share_of_total(s), z = %s, .grouping = rollup(g))", data, code)))
  run("native", str2lang(sprintf("summarise(group_by(%s, g), s = sum(v), sh = s / sum(%s$v), z = %s)", data, data, code)))
}
reverse <- function(code) {
  cat("reverse src =", code, "\n")
  run("marginplyr", str2lang(sprintf("summarize_with_margins(d, zz = sum(v) * 100, src = %s, p = share_of_parent(src), .grouping = rollup(g), .margin_label = NULL)", code)))
  run("native", str2lang(sprintf("summarise(group_by(d, g), zz = sum(v) * 100, src = %s)", code)))
}
cat("marginplyr", as.character(packageVersion("marginplyr")), "glue", as.character(packageVersion("glue")), "rlang", as.character(packageVersion("rlang")), "cli", as.character(packageVersion("cli")), "\n")
cat("Expected (ADR 0010, ?share_of_parent): every forward probe refused with 'Ordinary summaries cannot use an earlier Total share (`sh`)', every reverse probe refused with '... depends on earlier summary alias `zz`', as the get() twins are.\n\n")
cat("-- twins the walk already reads --\n")
forward('sum(get("sh"))')
reverse('get("zz")')
cat("\n-- template evaluators --\n")
for (code in c('as.numeric(glue::glue("{sum(sh)}"))',
               'as.numeric(glue::glue_data(list(), "{sum(sh)}"))',
               'as.numeric(stringr::str_glue("{sum(sh)}"))',
               'as.numeric(rlang::englue("{sum(sh)}"))',
               'as.numeric(cli::format_inline("{sum(sh)}"))',
               'as.numeric(cli::pluralize("{sum(sh)}"))')) forward(code)
cat("\n-- the label shape, no same-named input column --\n")
forward('glue::glue("{round(100 * sh)}%")', data = "plain")
cat("\n-- reverse --\n")
for (code in c('as.numeric(glue::glue("{zz}"))', 'as.numeric(stringr::str_glue("{zz}"))', 'as.numeric(rlang::englue("{zz}"))'))
  reverse(code)
cat("\n-- data.table input --\n")
if (requireNamespace("data.table", quietly = TRUE)) {
  hidden_dt <- data.table::as.data.table(hidden)
  run("marginplyr", quote(summarize_with_margins(hidden_dt, s = sum(v), sh = share_of_total(s), z = as.numeric(glue::glue("{sum(sh)}")), .grouping = rollup(g))))
}
cat("\n-- legal reads stay legal --\n")
reverse('as.numeric(glue::glue("{sum(v)}"))')
reverse('{ zz <- 2; as.numeric(glue::glue("{zz}")) }')
