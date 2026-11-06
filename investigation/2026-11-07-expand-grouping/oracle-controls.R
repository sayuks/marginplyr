# Exercise the actual parsed predicates with differences the first oracles hid.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
root <- args[[1L]]
assignment <- function(expr, name) {
  if (missing(expr)) return(NULL)
  if (!is.call(expr) && !is.expression(expr)) return(NULL)
  if (is.call(expr) && identical(expr[[1L]], quote(`<-`)) &&
      identical(expr[[2L]], as.name(name))) return(expr[[3L]])
  for (child in as.list(expr)) {
    found <- assignment(child, name)
    if (!is.null(found)) return(found)
  }
  NULL
}
predicate <- function(file, target) {
  code <- parse(file.path(root, file))
  env <- new.env(parent = globalenv())
  env$canonical <- eval(assignment(code, "canonical"), env)
  expression <- assignment(code, target)
  stopifnot(!is.null(expression))
  function(actual, expected) {
    env$actual <- actual
    env$expected <- expected
    eval(expression, env)
  }
}
sql <- predicate("sql/types.R", "comparison")
reference <- data.frame(g = "a", sid = 1L, rid = 2L)
double <- reference
double$rid <- as.double(double$rid)
stopifnot(sql(reference, reference) == "match")
stopifnot(sql(double, reference) == "mismatch")
arrow <- predicate("lazy/arrow-hidden-partitions.R", "equal")
stopifnot(arrow(reference, reference))
stopifnot(!arrow(reference[c("sid", "g", "rid")], reference))
extra <- reference
extra$unexpected <- 7L
stopifnot(!arrow(extra, reference))
cat("SQL predicate accepts identical input and rejects integer/double storage drift.\n")
cat("Arrow predicate accepts identical input and rejects reordered/extra public columns.\n")
