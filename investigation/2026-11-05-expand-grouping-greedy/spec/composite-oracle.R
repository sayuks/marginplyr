Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
set.seed(51796)
data <- data.frame(a = 1L, b = "b", c = FALSE, d = 1.5, e = 3L)
limit <- if (length(commandArgs(TRUE))) as.integer(commandArgs(TRUE)[[1L]]) else 1000L
cross <- function(fs) {
  out <- list(character())
  for (f in fs) out <- do.call(c, lapply(out, function(l) lapply(f, function(r) unique(c(l, r)))))
  out
}
unit <- function(composite = TRUE) {
  columns <- sample(names(data), sample(1:4, 1L), replace = TRUE)
  expr <- if (composite && sample(c(TRUE, FALSE), 1L)) {
    as.call(c(list(quote(grouping_set)), lapply(columns, as.name)))
  } else {
    as.call(list(quote(dplyr::all_of), columns))
  }
  list(expr = expr, family = list(unique(columns)), composite = identical(expr[[1]], quote(grouping_set)))
}
node <- function(depth) {
  kind <- if (depth < 1L) "unit" else sample(c("unit", "sets", "product", "rollup", "cube"), 1L)
  if (kind == "unit") return(unit())
  n <- if (kind == "product") sample(0:3, 1L) else sample(1:3, 1L)
  ch <- replicate(n, if (kind %in% c("rollup", "cube")) unit() else node(depth - 1L), simplify = FALSE)
  fs <- if (kind %in% c("rollup", "cube")) {
    unlist(lapply(ch, function(x) if (x$composite) list(x$family[[1]]) else lapply(x$family[[1]], function(y)y)), recursive=FALSE)
  } else lapply(ch, `[[`, "family")
  fun <- c(sets="grouping_sets",product="grouping_spec",rollup="rollup",cube="cube")[[kind]]
  expr <- as.call(c(list(as.name(fun)), lapply(ch, `[[`, "expr")))
  family <- switch(kind,
    sets = do.call(c, fs),
    product = cross(fs),
    rollup = lapply(length(fs):0L, function(i) unique(as.character(unlist(fs[seq_len(i)])))),
    cube = {
      if (length(fs)>10L) return(list(expr=expr,family=rep(list(character()),501L)))
      subsets <- do.call(c,lapply(length(fs):0L,function(size)combn(seq_along(fs),size,simplify=FALSE)))
      lapply(subsets,function(ix)unique(as.character(unlist(fs[ix]))))
    })
  list(expr=expr,family=family)
}
counts <- c(compared=0L, large=0L, leaf=0L)
for (i in seq_len(limit)) {
  candidate <- node(4L)
  if (length(candidate$family)>500L) {counts[["large"]]<-counts[["large"]]+1L; next}
  if (identical(candidate$expr[[1]],quote(dplyr::all_of))) {counts[["leaf"]]<-counts[["leaf"]]+1L; next}
  expected <- candidate$family
  keys <- vapply(expected,function(x)paste(sort(x),collapse=""),"")
  dims <- unique(as.character(unlist(expected)))
  expected <- lapply(expected,function(x)dims[dims %in% x])
  spec <- eval(candidate$expr)
  good <- tryCatch({
    kept <- inspect_grouping(data,.grouping=spec,.duplicates="keep",.format="list")
    dropped <- inspect_grouping(data,.grouping=spec,.duplicates="drop",.format="list")
    stopifnot(identical(kept$included,expected),identical(dropped$included,expected[!duplicated(keys)]))
    TRUE
  },error=function(e)e)
  if (!isTRUE(good)) {print(candidate$expr); print(expected); stop(good)}
  counts[["compared"]]<-counts[["compared"]]+1L
}
print(counts)
