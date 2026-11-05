Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
set.seed(20261105)
root <- "investigation/2026-11-05-summary-grouping-share-greedy/spec"
vars <- c("a", "b|c", "a,b", "x=y", "..0")
constructors <- c(set = "grouping_set", sets = "grouping_sets", rollup = "rollup", cube = "cube", product = "grouping_spec")
data <- tibble::as_tibble(setNames(rep(list(1L), length(vars)), vars))
data[["fixed key"]] <- "partition"
node <- function(depth = 0L, kind = sample(names(constructors), 1L)) {
  n <- sample(if (kind %in% c("sets", "rollup", "cube")) 1:2 else 0:2, 1L)
  children <- lapply(seq_len(n), function(i) {
    if (kind != "set" && depth < 3L && runif(1) < 0.65) {
      if (kind %in% c("rollup", "cube")) {
        return(list(kind = "set", children = list(sample(vars, sample(1:4, 1L)))))
      }
      return(node(depth + 1L))
    }
    sample(vars, sample(if (kind %in% c("rollup", "cube")) 1:4 else 0:4, 1L))
  })
  list(kind = kind, children = children)
}
written <- function(x) {
  if (is.character(x)) return(rlang::call2("all_of", x))
  value <- rlang::call2(constructors[[x$kind]], !!!lapply(x$children, written))
  switch(sample(1:3, 1L), value, rlang::call2("(", value), rlang::call2("(", value, .ns = "base"))
}
family <- function(x) {
  if (is.character(x)) return(list(x))
  fs <- lapply(x$children, family)
  if (any(vapply(fs, is.null, logical(1)))) return(NULL)
  if (x$kind == "set") return(list(as.character(unique(unlist(fs, use.names = FALSE)))))
  if (x$kind == "sets") {
    if (sum(lengths(fs)) > 256L) return(NULL)
    return(unlist(fs, recursive = FALSE))
  }
  if (x$kind == "product") {
    if (!length(fs)) return(list(character()))
    if (prod(lengths(fs)) > 256L) return(NULL)
    grid <- do.call(expand.grid, c(rev(lapply(lengths(fs), seq_len)), KEEP.OUT.ATTRS = FALSE))
    grid <- grid[, rev(seq_along(fs)), drop = FALSE]
    return(lapply(seq_len(nrow(grid)), function(i) as.character(unique(unlist(lapply(seq_along(fs), function(j) fs[[j]][[grid[i, j]]]), use.names = FALSE)))))
  }
  units <- unlist(lapply(seq_along(fs), function(i) {
    if (is.character(x$children[[i]])) as.list(x$children[[i]]) else fs[[i]]
  }), recursive = FALSE)
  if (x$kind == "rollup") return(lapply(length(units):0L, function(n) as.character(unique(unlist(units[seq_len(n)], use.names = FALSE)))))
  if (length(units) > 8L) return(NULL)
  grid <- as.matrix(do.call(expand.grid, c(rep(list(c(1L, 0L)), length(units)), KEEP.OUT.ATTRS = FALSE)))
  indexes <- order(-rowSums(grid), apply(grid, 1L, function(z) paste0(1L-z, collapse = "")))
  lapply(indexes, function(i) as.character(unique(unlist(units[as.logical(grid[i, ])], use.names = FALSE))))
}
records <- list()
skipped <- 0L
for (trial in seq_len(900L)) {
  x <- node()
  expected <- family(x)
  if (is.null(expected)) {
    skipped <- skipped + 1L
    next
  }
  syntax <- written(x)
  spec <- eval(syntax)
  dims <- as.character(unique(unlist(expected, use.names = FALSE)))
  expected <- lapply(expected, function(s) dims[dims %in% s])
  keys <- vapply(expected, function(s) paste0(as.integer(dims %in% s), collapse = ""), character(1))
  for (policy in c("error", "drop", "keep")) {
    actual <- tryCatch(inspect_grouping(data, .by = all_of("fixed key"), .grouping = spec, .duplicates = policy, .format = "list"), error = identity)
    duplicate <- policy == "error" && anyDuplicated(keys) > 0L
    if (duplicate) {
      stopifnot(inherits(actual, "marginplyr_error"), grepl("Duplicate grouping sets", conditionMessage(actual), fixed = TRUE))
      verdict <- "required-duplicate-refusal"
    } else {
      target <- if (policy == "drop") expected[!duplicated(keys)] else expected
      if (inherits(actual, "error") || !identical(actual$included, target)) {
        print(list(trial = trial, syntax = syntax, policy = policy, expected = target, actual = actual))
        stop("Independent composition oracle mismatch")
      }
      stopifnot(identical(actual$set_id, seq_along(target)), all(vapply(actual$fixed, identical, logical(1), "fixed key")))
      for (i in seq_along(target)) {
        bits <- as.integer(!dims %in% target[[i]])
        if (length(dims)) names(bits) <- dims
        stopifnot(identical(actual$omitted[[i]], setdiff(dims, target[[i]])), identical(actual$grouping_bits[[i]], bits), identical(actual$grouping_id[[i]], as.integer(sum(bits * 2^(length(bits)-seq_along(bits))))))
      }
      verdict <- "exact-plan-match"
    }
    records[[length(records) + 1L]] <- data.frame(trial, policy, occurrences = length(expected), verdict)
  }
}
records <- bind_rows(records)
write.csv(records, file.path(root, "composition-oracle-results.csv"), row.names = FALSE)
print(table(records$verdict))
cat("Skipped over explicit 256-occurrence bound:", skipped, "\n")
cat("All retained plans, fixed keys, occurrence order, dimensions, bits and identifiers matched.\n")
