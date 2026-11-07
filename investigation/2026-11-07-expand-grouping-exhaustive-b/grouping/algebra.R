args <- commandArgs(trailingOnly = TRUE)
source_dir <- if (length(args)) args[[1L]] else "."
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(source_dir, quiet = TRUE)
set.seed(71107)
vars <- c("a", "b", "c", "d", "e")
data <- tibble::as_tibble(setNames(rep(list(1L), length(vars)), vars))
leaf <- function() list(kind = "leaf", cols = sample(vars, sample(0:4, 1)))
gen <- function(depth = 0L) {
  kind <- if (depth >= 3L) sample(c("set", "rollup", "cube"), 1) else sample(c("set", "sets", "rollup", "cube", "product"), 1)
  n <- sample(if (kind %in% c("set", "product")) 0:3 else 1:3, 1)
  children <- lapply(seq_len(n), function(i) {
    if (kind %in% c("sets", "product") && runif(1) < 0.6) return(gen(depth + 1L))
    if (kind %in% c("rollup", "cube") && runif(1) < 0.5) return(list(kind="set", children=list(list(kind="leaf", cols=sample(vars, sample(1:3, 1))))))
    x <- leaf()
    if (kind %in% c("rollup", "cube") && !length(x$cols)) x$cols <- sample(vars, 1)
    x
  })
  list(kind = kind, children = children)
}
family <- function(x) {
  if (x$kind == "leaf") return(list(x$cols))
  fs <- lapply(x$children, family)
  if (x$kind == "set") return(list(unique(unlist(fs, use.names=FALSE))))
  if (x$kind == "sets") return(unlist(fs, recursive=FALSE))
  if (x$kind == "product") return(Reduce(function(xs, ys) unlist(lapply(xs, function(a) lapply(ys, function(b) union(a,b))), recursive=FALSE), fs, init=list(character())))
  units <- unlist(Map(function(ch, f) if(ch$kind == "leaf") as.list(ch$cols) else f, x$children, fs), recursive=FALSE)
  if (x$kind == "rollup") return(lapply(rev(seq.int(0L, length(units))), function(n) unique(as.character(unlist(units[seq_len(n)], use.names=FALSE)))))
  unlist(lapply(rev(seq.int(0L,length(units))), function(n) lapply(utils::combn(seq_along(units), n, simplify=FALSE), function(i) unique(as.character(unlist(units[i], use.names=FALSE))))), recursive=FALSE)
}
expr <- function(x) {
  if (x$kind == "leaf") return(rlang::expr(dplyr::all_of(!!x$cols)))
  name <- c(set="grouping_set", sets="grouping_sets", rollup="rollup", cube="cube", product="grouping_spec")[[x$kind]]
  rlang::call2(name, !!!lapply(x$children, expr))
}
key <- function(s) paste(vars %in% s, collapse="")
counts <- c(generated=0L, skipped=0L, checked=0L)
for (i in seq_len(5000)) {
  x <- gen(); f <- family(x); counts[["generated"]] <- counts[["generated"]]+1L
  if (length(f) > 2000L) {counts[["skipped"]] <- counts[["skipped"]]+1L; next}
  f <- lapply(f, function(z) if(is.null(z)) character() else z)
  dimensions <- unique(as.character(unlist(f, use.names=FALSE)))
  normalized <- lapply(f, function(s) dimensions[dimensions %in% s])
  for (duplicates in c("keep", "drop")) {
    expected <- if (duplicates=="keep") normalized else normalized[!duplicated(vapply(normalized,key,character(1)))]
    actual <- tryCatch(inspect_grouping(data, .grouping=eval(expr(x)), .duplicates=duplicates, .format="list"), error=identity)
    if (inherits(actual,"condition") || !identical(actual$included, expected)) {
      print(list(case=i, expr=expr(x), duplicates=duplicates, expected=expected, actual=actual)); stop("algebra mismatch")
    }
    counts[["checked"]] <- counts[["checked"]]+1L
  }
}
print(counts)
print(sessionInfo())
