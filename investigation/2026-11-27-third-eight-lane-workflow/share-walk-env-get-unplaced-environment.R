# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Second round: rlang::env_get() on an environment the walk cannot place is read as reaching no mask frame (#1829).
# rlang::env_get()/env_get_list() on an environment the walk cannot place is read as reaching no mask frame, where get() on the same environment is refused
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
cat("Expected: each env_get()/env_get_list() probe refused exactly as its get()/mget() twin on the same environment expression.\n\n")
cat("-- get()/mget() twins (refused today) --\n")
forward('sum(get("sh", envir = new.env()))')
forward('sum(get("sh", envir = rlang::env()))')
forward('sum(mget("sh", envir = environment(NULL), inherits = TRUE)[[1]])')
cat("\n-- env_get()/env_get_list() on the same environments --\n")
for (code in c('sum(rlang::env_get(new.env(), "sh", inherit = TRUE))',
               'sum(rlang::env_get(rlang::env(), "sh", inherit = TRUE))',
               'sum(rlang::env_get(rlang::child_env(environment()), "sh", inherit = TRUE))',
               'sum(rlang::env_get(rlang::new_environment(parent = environment()), "sh", inherit = TRUE))',
               'sum(rlang::env_get(environment(NULL), "sh", inherit = TRUE))',
               'sum(rlang::env_get(rlang::caller_env(0), "sh", inherit = TRUE))',
               'sum(unlist(rlang::env_get_list(rlang::env(), "sh", inherit = TRUE)))',
               '{ e <- new.env(); sum(rlang::env_get(e, "sh", inherit = TRUE)) }',
               'sum(rlang::env_get(rlang::env_clone(parent.env(environment())), "sh"))'))
  forward(code)
cat("\n-- placed twin (refused today) --\n")
forward('{ e <- environment(); sum(rlang::env_get(e, "sh", inherit = TRUE)) }')
cat("\n-- reverse --\n")
reverse('rlang::env_get(rlang::env(), "zz", inherit = TRUE)')
reverse('{ e <- new.env(); rlang::env_get(e, "zz", inherit = TRUE) }')
reverse('rlang::env_get(rlang::env_clone(parent.env(environment())), "zz")')
reverse('get("zz", envir = new.env())')
cat("\n-- legal: a local environment holding its own binding --\n")
reverse('{ e <- new.env(); e$zz <- 2; rlang::env_get(e, "zz") }')
reverse('{ e <- new.env(); e$zz <- 2; get("zz", envir = e) }')
cat("\n-- the same unplaceable environments read by `$`, `[[` and as.list() after parent steps --\n")
forward('sum(parent.env(parent.env(new.env()))$sh)')
forward('sum(rlang::env_parent(rlang::env(), 2)$sh)')
forward('sum(parent.env(parent.env(new.env()))[["sh"]])')
forward('sum(as.list(parent.env(parent.env(new.env())))$sh)')
forward('sum(get("sh", envir = parent.env(parent.env(new.env())), inherits = FALSE))')

cat("\n-- data.table input --\n")
if (requireNamespace("data.table", quietly = TRUE)) { hidden_dt <- data.table::as.data.table(hidden); forward('sum(rlang::env_get(rlang::env(), "sh", inherit = TRUE))', data = "hidden_dt") }
