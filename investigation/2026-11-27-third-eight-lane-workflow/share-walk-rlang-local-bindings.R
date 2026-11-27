# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# rlang::local_bindings() and with_bindings() given zap() are not counted as removals (#1822).
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
d <- tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
# A literal captures the local zz; removing that local makes the literal read the earlier summary zz (ADR 0010, #1676).
block <- function(removal, call_f = "f()") sprintf("(function() { zz <- 1; f <- function() zz; %s })()", gsub("CALL", call_f, removal, fixed = TRUE))
sources <- c(
  `no removal (control)`              = block("CALL"),
  `rm(zz) (control)`                  = block("rm(zz); CALL"),
  `rlang::env_unbind (control)`       = block('rlang::env_unbind(environment(), "zz"); CALL'),
  `rlang::env_bind(zz = zap()) (control)` = block("rlang::env_bind(environment(), zz = rlang::zap()); CALL"),
  `rlang::local_bindings(zz = zap())` = block("rlang::local_bindings(zz = rlang::zap()); CALL"),
  `rlang::with_bindings(f(), zz = zap())` = block("rlang::with_bindings(CALL, zz = rlang::zap())")
)
cat("Contract (ADR 0010): removing a captured name from its scope reads the earlier alias at the point of removal, so a source containing a removal is refused.\n")
cat("Expected: Package refusal for every source except the first.\n\n")
for (nm in names(sources)) {
  src <- sources[[nm]]
  nat <- tryCatch({ x <- summarise(group_by(d, g), zz = sum(v) * 100, src = eval(parse(text = src))); paste(x$src, collapse = ",") }, error = function(e) "ERR")
  call <- parse(text = sprintf("summarize_with_margins(d, zz = sum(v) * 100, src = %s, p = share_of_parent(src), .grouping = rollup(g), .margin_label = NULL)", src))[[1]]
  res <- tryCatch({ x <- eval(call); paste("ACCEPTED src =", paste(x$src, collapse = ",")) },
                  marginplyr_error = function(e) "Package refusal", error = function(e) paste("External error:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 50)))
  cat(sprintf("%-42s marginplyr: %-34s native dplyr src (per group): %s\n", nm, res, nat))
}
cat("\nsrc = 300,400 natively means the literal read the earlier summary zz (sum(v) * 100); src = 1 means it read the local.\n")
