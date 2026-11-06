Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
set.seed(11062026)
columns <- c("a", "b", "c", "x,y", "(z)", "q`r")
data <- as.data.frame(stats::setNames(rep(list(1L), length(columns)), columns),
                      check.names = FALSE)
sel <- function(x) rlang::call2("all_of", x)
node <- function(kind, children) list(kind = kind, children = children)
leaf <- function() sample(columns, sample(0:3, 1), replace = FALSE)
generate <- function(depth = 0L) {
  kind <- sample(c("set", "sets", "rollup", "cube", "product"), 1)
  n <- sample(if (kind %in% c("set", "product")) 0:3 else 1:3, 1)
  children <- lapply(seq_len(n), function(i) {
    if (depth < 2L && kind %in% c("sets", "product") && runif(1) < 0.4) {
      generate(depth + 1L)
    } else if (kind %in% c("rollup", "cube") && runif(1) < 0.3) {
      node("set", list(sample(columns, sample(1:3, 1))))
    } else {
      leaf()
    }
  })
  node(kind, children)
}
constructor <- c(set = "grouping_set", sets = "grouping_sets", rollup = "rollup",
                 cube = "cube", product = "grouping_spec")
expression <- function(x) {
  args <- lapply(x$children, function(y) if (is.list(y)) expression(y) else sel(y))
  rlang::call2(constructor[[x$kind]], !!!args)
}
oracle <- function(x) {
  k <- x$kind
  if (k == "set") return(list(unique(as.character(unlist(x$children, use.names = FALSE)))))
  if (k %in% c("rollup", "cube")) {
    units <- unlist(lapply(x$children, function(y) {
      if (is.list(y)) oracle(y) else lapply(y, function(z) z)
    }), recursive = FALSE)
    if (!length(units)) return(NULL)
    if (k == "rollup") return(lapply(rev(seq.int(0L, length(units))), function(i) {
      unique(as.character(unlist(units[seq_len(i)], use.names = FALSE)))
    }))
    masks <- rev(seq.int(0L, 2^length(units) - 1L))
    sizes <- vapply(masks, function(mask) sum(as.integer(intToBits(mask))), integer(1))
    masks <- masks[order(sizes, decreasing = TRUE)]
    return(lapply(masks, function(mask) {
      present <- as.logical(bitwAnd(mask, 2^rev(seq.int(0L, length(units)-1L))))
      unique(as.character(unlist(units[present], use.names = FALSE)))
    }))
  }
  families <- lapply(x$children, function(y) if (is.list(y)) oracle(y) else list(y))
  if (any(vapply(families, is.null, logical(1)))) return(NULL)
  if (k == "sets") return(unlist(families, recursive = FALSE))
  answer <- list(character())
  for (family in families) {
    answer <- unlist(lapply(answer, function(a) lapply(family, function(b) {
      unique(c(a, b))
    })), recursive = FALSE)
  }
  answer
}
normalize <- function(family, dims) lapply(family, function(x) dims[dims %in% x])
count <- 0L
refusals <- 0L
for (i in seq_len(1500L)) {
  x <- generate()
  expected <- oracle(x)
  expr <- expression(x)
  spec <- eval(expr)
  for (policy in c("keep", "drop")) {
    actual <- tryCatch(inspect_grouping(data, .grouping = spec,
                                       .duplicates = policy, .format = "list"),
                       error = identity)
    if (is.null(expected)) {
      stopifnot(inherits(actual, "marginplyr_error"))
      refusals <- refusals + 1L
      next
    }
    if (inherits(actual, "error")) {
      print(expr); stop(conditionMessage(actual))
    }
    dims <- unique(unlist(expected, use.names = FALSE))
    wanted <- normalize(expected, dims)
    if (policy == "drop") wanted <- unique(wanted)
    if (!identical(actual$included, wanted)) {
      print(expr); print(actual$included); print(wanted)
      stop("ordered family mismatch")
    }
    bits <- lapply(wanted, function(y) stats::setNames(as.integer(!dims %in% y), dims))
    if (!length(dims)) bits <- rep(list(integer()), length(wanted))
    stopifnot(identical(actual$grouping_bits, bits))
    stopifnot(identical(actual$set_id, seq_along(wanted)))
    count <- count + 1L
  }
}
cat("exact family/bit/set-id matches:", count, "required empty-unit refusals:", refusals, "\n")
print(sessionInfo())
