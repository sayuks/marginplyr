pkgload::load_all(".", quiet = TRUE)
set.seed(74182L)
data <- data.frame(a = "a", b = 1L, c = TRUE, d = 1.5, e = as.Date("2026-11-07"))
columns <- names(data)
pick_columns <- function() sample(columns, sample.int(3L, 1L) - 1L)
leaf <- function() list(
  kind = if (runif(1L) < 0.35) "typed" else "bare",
  columns = pick_columns()
)
node <- function(depth = 0L, only_set = FALSE) {
  kind <- if (only_set) "grouping_set" else sample(
    c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec"), 1L
  )
  n <- switch(kind, grouping_set = sample(0:3, 1L),
              grouping_spec = sample(0:3, 1L), sample.int(3L, 1L))
  children <- lapply(seq_len(n), function(i) {
    if (kind == "grouping_set" || depth > 3L || runif(1L) < 0.55) {
      out <- leaf()
      if (kind %in% c("rollup", "cube") && length(out$columns) == 0L) {
        out$columns <- sample(columns, 1L)
      }
      return(out)
    }
    if (kind %in% c("rollup", "cube")) {
      out <- node(depth + 1L, only_set = TRUE)
      out$children <- list(list(kind = sample(c("typed", "bare"), 1L),
                                columns = sample(columns, 2L)))
      return(out)
    }
    node(depth + 1L)
  })
  list(kind = kind, children = children)
}
to_expr <- function(x) {
  if (x$kind == "bare") return(rlang::call2("c", !!!rlang::syms(x$columns)))
  if (x$kind == "typed") return(rlang::call2("identity", x$columns))
  rlang::call2(x$kind, !!!lapply(x$children, to_expr))
}
oracle <- function(x) {
  if (x$kind %in% c("bare", "typed")) return(list(x$columns))
  pieces <- lapply(x$children, oracle)
  if (x$kind == "grouping_set") {
    return(list(unique(as.character(unlist(pieces, use.names = FALSE)))))
  }
  if (x$kind == "grouping_sets") return(do.call(c, pieces))
  if (x$kind == "grouping_spec") {
    return(Reduce(function(left, right) {
      unlist(lapply(left, function(l) lapply(right, function(r) union(l, r))),
             recursive = FALSE)
    }, pieces, init = list(character())))
  }
  dims <- do.call(c, Map(function(child, part) {
    if (child$kind == "grouping_set") part else as.list(part[[1L]])
  }, x$children, pieces))
  n <- length(dims)
  powers <- if (x$kind == "rollup") lapply(n:0L, seq_len) else
    do.call(c, lapply(n:0L, function(k) combn(seq_len(n), k, simplify = FALSE)))
  lapply(powers, function(ind) unique(as.character(unlist(dims[ind], use.names = FALSE))))
}
n_gen <- 1800L
observations <- integer(5L)
failure <- list()
for (i in seq_len(n_gen)) {
  ast <- node()
  sets <- oracle(ast)
  if (length(sets) > 128L) {
    observations[[5L]] <- observations[[5L]] + 1L
    next
  }
  dims <- unique(as.character(unlist(sets, use.names = FALSE)))
  canonical <- lapply(sets, function(set) dims[dims %in% set])
  keys <- vapply(canonical, paste, character(1), collapse = "|")
  duplicates <- duplicated(keys)
  expr <- to_expr(ast)
  spec <- eval(expr)
  keep <- inspect_grouping(data, .grouping = spec, .duplicates = "keep",
                           .format = "list")
  drop <- inspect_grouping(data, .grouping = spec, .duplicates = "drop",
                           .format = "list")
  stopifnot(identical(keep$included, canonical),
            identical(drop$included, canonical[!duplicates]),
            identical(keep$set_id, seq_along(canonical)),
            identical(drop$set_id, seq_along(canonical[!duplicates])))
  observations[[1L]] <- observations[[1L]] + 2L
  verdict <- tryCatch(inspect_grouping(data, .grouping = spec, .format = "list"),
                      error = identity)
  if (!any(duplicates)) {
    stopifnot(identical(verdict, keep))
    observations[[2L]] <- observations[[2L]] + 1L
    next
  }
  stopifnot(inherits(verdict, "marginplyr_error"),
            grepl("Duplicate grouping sets", conditionMessage(verdict), fixed = TRUE))
  line <- strsplit(conditionMessage(verdict), "\n")[[1L]][[2L]]
  shown <- strsplit(sub("^.*?([0-9][0-9,; ]*)\\.$", "\\1", line), "; ", fixed = TRUE)[[1L]]
  groups <- lapply(strsplit(shown, ", ", fixed = TRUE), as.integer)
  valid <- all(vapply(groups, function(g) {
    length(g) > 1L && !anyNA(g) && all(g >= 1L & g <= length(keys)) &&
      length(unique(keys[g])) == 1L && length(unique(g)) == length(g)
  }, logical(1)))
  if (valid) {
    observations[[3L]] <- observations[[3L]] + 1L
  } else {
    observations[[4L]] <- observations[[4L]] + 1L
    failure[[length(failure) + 1L]] <- list(
      index = i, expr = deparse1(expr), diagnostic = conditionMessage(verdict),
      actual_duplicates = split(seq_along(keys), keys)[
        lengths(split(seq_along(keys), keys)) > 1L
      ]
    )
  }
}
names(observations) <- c("keep/drop", "unique error-policy", "true duplicate diagnostics",
                        "wrong duplicate positions", "over 128 sets")
print(observations)
for (one in head(failure, 15L)) print(one)
