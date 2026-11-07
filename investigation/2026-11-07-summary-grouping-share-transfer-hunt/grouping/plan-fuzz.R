Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
set.seed(20261107)
data <- tibble::tibble(a = c("a", "b"), b = c("x", "y"), c = "z", d = "w", value = 1:2)
gen <- function(depth = 0L) {
  kind <- if (depth > 2L) "grouping_set" else sample(c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec"), 1L)
  if (kind == "grouping_set") return(as.call(c(list(as.name(kind)), lapply(sample(letters[1:4], sample(0:4, 1L)), as.name))))
  n <- if (kind %in% c("rollup", "cube")) sample(1:4, 1L) else sample(1:3, 1L)
  args <- if (kind %in% c("rollup", "cube")) lapply(seq_len(n), function(i) {
    if (runif(1) < .4) as.call(c(list(as.name("grouping_set")), lapply(sample(letters[1:4], sample(1:3, 1L)), as.name))) else as.name(sample(letters[1:4], 1L))
  }) else lapply(seq_len(n), function(i) gen(depth + 1L))
  as.call(c(list(as.name(kind)), args))
}
reference <- function(expr) {
  if (is.symbol(expr)) return(list(as.character(expr)))
  kind <- as.character(expr[[1L]])
  args <- as.list(expr)[-1L]
  children <- lapply(args, reference)
  if (kind == "grouping_set") return(list(unique(unlist(children, use.names=FALSE))))
  if (kind == "grouping_sets") return(unlist(children, recursive=FALSE))
  if (kind == "grouping_spec") {
    out <- list(character())
    for (family in children) out <- unlist(lapply(out, function(x) lapply(family, function(y) union(x,y))),recursive=FALSE)
    return(out)
  }
  units <- lapply(children, function(x) x[[1L]])
  if (kind == "rollup") return(lapply(rev(seq.int(0L,length(units))), function(n) unique(unlist(units[seq_len(n)], use.names=FALSE))))
  combos <- unlist(lapply(rev(seq.int(0L,length(units))), function(size) utils::combn(seq_along(units),size,simplify=FALSE)),recursive=FALSE)
  lapply(combos, function(index) unique(unlist(units[index],use.names=FALSE)))
}
for (i in seq_len(1500L)) {
  expr <- gen()
  expected <- reference(expr)
  dims <- as.character(unique(unlist(expected, use.names=FALSE)))
  expected <- lapply(expected, function(x) dims[dims %in% x])
  expected <- unique(expected)
  actual <- tryCatch(inspect_grouping(data, .grouping = eval(expr), .duplicates="drop", .format="list"), error=identity)
  if (inherits(actual,"error") || !identical(actual$included,expected)) {
    cat("FAIL",i,"\n"); dput(expr); dput(expected); print(actual)
    output <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", unset=tempdir())
    saveRDS(list(expr=expr,expected=expected,actual=actual),file.path(output,"fuzz-failure.rds"))
    quit(status=1L)
  }
}
cat("PASS 1500 random Grouping specifications matched naive set expansion under duplicate drop.\n")
