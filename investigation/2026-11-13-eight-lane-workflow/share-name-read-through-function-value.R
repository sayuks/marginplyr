# Investigated SHA: 6165f4768400a28520cc8977e056da0db49a32ed
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A later summary that reaches a share's name through a function held as a value reads the hidden input column of that name, with no condition.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
d <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 5), sh = c(100, 200, 300))
ext_get <- get
try_it <- function(expr) tryCatch(
  paste("z =", paste(expr$z, collapse = " ")),
  marginplyr_error = function(e) paste("Package refusal:", strsplit(conditionMessage(e), "\n")[[1]][1]),
  error = function(e) paste0("<", class(e)[1], "> ", gsub("\\s+", " ", conditionMessage(e)))
)
margin <- function(z, data = d) {
  z <- substitute(z)
  eval(bquote(try_it(summarize_with_margins(data, s = sum(v), sh = share_of_total(s), z = .(z), .grouping = rollup(g)))))
}
cat("the input column sh summed by hand: a = 300, b = 300, Total = 600; the share sh is 0.375, 0.625, 1\n")
cat("sum(sh)                              :", margin(sum(sh)), "\n")
cat("{ sh <- 1; rm(sh); sum(sh) }         :", margin({ sh <- 1; rm(sh); sum(sh) }), "\n")
cat("sum(get('sh'))                       :", margin(sum(get("sh"))), "\n")
cat("get('rm')(sh) after a local sh       :", margin({ sh <- 1; get("rm")(sh); sum(sh) }), "\n")
cat("{ sh <- 1; f <- rm; f(sh); sum(sh) } :", margin({ sh <- 1; f <- rm; f(sh); sum(sh) }), "\n")
cat("{ f <- get; sum(f('sh')) }           :", margin({ f <- get; sum(f("sh")) }), "\n")
cat("sum(identity(get)('sh'))             :", margin(sum(identity(get)("sh"))), "\n")
cat("sum(list(get)[[1]]('sh'))            :", margin(sum(list(get)[[1]]("sh"))), "\n")
cat("sum(ext_get('sh')), a caller alias   :", margin(sum(ext_get("sh"))), "\n")
cat("no input column sh, aliased get      :", margin({ f <- get; sum(f("sh")) }, data = d[c("g", "v")]), "\n")
stage <- dplyr::summarise(d, s = sum(v), z = { f <- get; sum(f("sh")) }, .by = g)
cat("native summarise() holding no sh summary: z =", paste(stage$z, collapse = " "), "\n")
alias <- dplyr::summarise(d, s = sum(v), sh = s / 8, z = { f <- get; sum(f("sh")) }, .by = g)
cat("native summarise() with an ordinary sh  : z =", paste(alias$z, collapse = " "), "\n")
