# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# The default env of rlang::env_parent() and env_get() is not modelled by the share dependency walk (#1821).
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
d <- tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
run <- function(src) {
  call <- parse(text = sprintf(paste0(
    "summarize_with_margins(d, zz = sum(v) * 100, src = %s, p = share_of_parent(src), ",
    ".grouping = rollup(g), .margin_label = NULL)"), src))[[1]]
  tryCatch({ x <- eval(call); sprintf("ACCEPTED src = %s ; p = %s", paste(x$src, collapse = ","), paste(round(x$p, 3), collapse = ",")) },
           marginplyr_error = function(e) paste("Package refusal:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 95)),
           error = function(e) paste("External error:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 70)))
}
cat("Contract (?share_of_parent, Eligible source summaries): a source summary \"cannot refer to an earlier summary\".\n")
cat("Expected: each source below reads the earlier summary zz, so each is a Package refusal.\n\n")
for (src in c('rlang::env_get(rlang::env_parent(), "zz")',
              'rlang::env_get(rlang::env_parent(rlang::current_env()), "zz")',
              'rlang::env_get(rlang::caller_env(), "zz", inherit = TRUE)',
              'rlang::env_get(rlang::env_parent(), "zz", inherit = TRUE)',
              'rlang::env_get_list(rlang::env_parent(), "zz")[[1]]',
              'get("zz")')) cat(sprintf("%-70s %s\n", src, run(src)))
cat("\nNative control (plain dplyr, no marginplyr): the first source reads the earlier alias zz and evaluates:\n")
x <- summarise(group_by(d, g), zz = sum(v) * 100, src = rlang::env_get(rlang::env_parent(), "zz"))
cat("  src =", paste(x$src, collapse = ","), "(= zz)\n")

# Added in adjudication: the forward direction, where the result lacks a summary the caller named.
show <- function(label, expr) cat(label, ":", tryCatch(paste(capture.output(print(as.data.frame(expr))), collapse = " | "), error = function(e) paste("ERROR", class(e)[1], gsub("\n", " ", conditionMessage(e)))), "\n")
d2 <- dplyr::tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
show("obs = rlang::env_parent()$zz after a Parent share zz    ", summarize_with_margins(d2, s = sum(v), zz = share_of_parent(s), obs = rlang::env_parent()$zz, .grouping = rollup(g)))
show("the rlang::current_env() twin                           ", summarize_with_margins(d2, s = sum(v), zz = share_of_parent(s), obs = rlang::env_parent(rlang::current_env())$zz, .grouping = rollup(g)))
show("plain summarise(), zz an ordinary earlier summary       ", dplyr::summarise(d2, s = sum(v), zz = s / 7, obs = rlang::env_parent()$zz, .by = g))
