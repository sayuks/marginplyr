args <- commandArgs(trailingOnly = TRUE)
source_dir <- Sys.getenv("MARGINPLYR_HUNT_SOURCE", unset = getwd())
pkgload::load_all(source_dir, quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
library(dplyr)
set.seed(20261104)
out <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", unset = file.path(
  getwd(), "investigation/2026-11-04-summary-grouping-share-bug-hunt/grouping"
))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
vars <- c("a", "b", "c", "d")
leaf <- function() sample(vars, sample(0:3, 1), replace = FALSE)
constructors <- c(set = "grouping_set", sets = "grouping_sets",
                  rollup = "rollup", cube = "cube", product = "grouping_spec")
node <- function(depth = 0L, kind = NULL) {
  if (is.null(kind)) kind <- sample(names(constructors), 1)
  n <- sample(if (kind %in% c("rollup", "cube", "sets")) 1:3 else 0:3, 1)
  children <- vector("list", n)
  for (i in seq_len(n)) {
    nested <- depth < 2L && kind != "set" && runif(1) < 0.45
    if (nested) {
      child_kind <- if (kind %in% c("rollup", "cube")) "set" else NULL
      child <- node(depth + 1L, child_kind)
      if (kind %in% c("rollup", "cube") && !length(unlist(child$children))) {
        child$children <- list(sample(vars, 1))
      }
      children[[i]] <- child
    } else {
      children[[i]] <- if (kind %in% c("rollup", "cube")) sample(vars, sample(1:3, 1)) else leaf()
    }
  }
  list(kind = kind, children = children)
}
syntax <- function(x) {
  if (is.character(x)) return(rlang::call2("all_of", x))
  rlang::call2(constructors[[x$kind]], !!!lapply(x$children, syntax))
}
family <- function(x) {
  if (is.character(x)) return(list(x))
  fs <- lapply(x$children, family)
  kind <- x$kind
  if (kind == "set") return(list(unique(unlist(fs, use.names = FALSE))))
  if (kind == "sets") return(unlist(fs, recursive = FALSE))
  if (kind == "product") {
    if (!length(fs)) return(list(character()))
    sizes <- lengths(fs)
    grid <- do.call(expand.grid, c(rev(lapply(sizes, seq_len)), KEEP.OUT.ATTRS = FALSE))
    grid <- grid[, rev(seq_along(fs)), drop = FALSE]
    return(lapply(seq_len(nrow(grid)), function(i) unique(unlist(
      lapply(seq_along(fs), function(j) fs[[j]][[grid[i, j]]]), use.names = FALSE
    ))))
  }
  units <- unlist(lapply(seq_along(x$children), function(i) {
    if (is.character(x$children[[i]])) as.list(x$children[[i]]) else fs[[i]]
  }), recursive = FALSE)
  if (kind == "rollup") return(lapply(length(units):0, function(n) unique(unlist(
    units[seq_len(n)], use.names = FALSE
  ))))
  bits <- as.matrix(do.call(expand.grid, c(rep(list(c(1L, 0L)), length(units)),
                                        KEEP.OUT.ATTRS = FALSE)))
  bit_order <- order(-rowSums(bits), apply(bits, 1, function(z) paste0(1L - z, collapse = "")))
  lapply(bit_order, function(i) unique(unlist(units[as.logical(bits[i, ])], use.names = FALSE)))
}
data <- tibble(a = c(1, 1, NA, 2), b = c("u", "v", "u", NA),
               c = factor(c("x", "x", "y", NA)), d = c(TRUE, FALSE, TRUE, NA),
               value = c(2, 3, 5, 7))
records <- list()
summary_calls <- 0L
for (trial in seq_len(600L)) {
  x <- node()
  written <- syntax(x)
  spec <- eval(written)
  raw <- family(x)
  dimensions <- as.character(unique(unlist(raw, use.names = FALSE)))
  raw <- lapply(raw, function(s) dimensions[dimensions %in% s])
  keys <- vapply(raw, function(s) paste(as.integer(dimensions %in% s), collapse = ""), character(1))
  if (length(raw) > 100L) next
  for (policy in c("keep", "drop", "error")) {
    expected_error <- policy == "error" && anyDuplicated(keys) > 0L
    actual <- tryCatch(inspect_grouping(data, .grouping = spec,
                                       .duplicates = policy, .format = "list"), error = identity)
    if (expected_error) {
      stopifnot(inherits(actual, "marginplyr_error"))
      verdict <- "required-duplicate-refusal"
    } else {
      stopifnot(!inherits(actual, "error"))
      expected <- if (policy == "drop") raw[!duplicated(keys)] else raw
      if (!identical(actual$included, expected)) {
        print(list(trial = trial, expression = written, policy = policy, actual = actual$included, expected = expected))
        stop("Family oracle mismatch")
      }
      stopifnot(identical(actual$set_id, seq_along(expected)))
      for (i in seq_along(expected)) {
        omitted <- setdiff(dimensions, expected[[i]])
        bits <- as.integer(dimensions %in% omitted)
        if (length(dimensions)) names(bits) <- dimensions
        id <- as.integer(sum(bits * 2^(length(bits) - seq_along(bits))))
        stopifnot(identical(actual$omitted[[i]], omitted),
                  identical(actual$grouping_bits[[i]], bits),
                  identical(actual$grouping_id[[i]], id))
      }
      verdict <- "exact-family-bits-id-match"
    }
    records[[length(records) + 1L]] <- data.frame(
      trial, policy, sets = length(raw), verdict,
      expression = paste(deparse(written, width.cutoff = 500L), collapse = " ")
    )
  }
  if (trial <= 80L && length(raw) <= 30L) {
    result <- summarize_with_margins(data, n = n(), total = sum(value),
                                     .grouping = spec, .duplicates = "keep", .id = "occurrence",
                                     .margin_label = NULL, .sort = "none")
    for (i in seq_along(raw)) {
      expected <- summarize(data, n = n(), total = sum(value), .by = all_of(raw[[i]]))
      actual <- result[result$occurrence == i, c(raw[[i]], "n", "total"), drop = FALSE]
      cmp <- if (length(raw[[i]])) {
        list(arrange(actual, across(all_of(raw[[i]]))), arrange(expected, across(all_of(raw[[i]]))))
      } else list(actual, expected)
      equal <- isTRUE(all.equal(as.data.frame(cmp[[1]]), as.data.frame(cmp[[2]]), check.attributes = FALSE))
      if (!equal) {
        print(list(trial = trial, expression = written, occurrence = i, actual = cmp[[1]], expected = cmp[[2]]))
        stop("Summary oracle mismatch")
      }
    }
    summary_calls <- summary_calls + 1L
  }
}
records <- bind_rows(records)
write.csv(records, file.path(out, "algebra-results.csv"), row.names = FALSE)
print(table(records$verdict))
cat("Summary calls:", summary_calls, "\n")
cat("All retained family, bit, identifier, duplicate-policy and summary comparisons passed.\n")
print(sessionInfo())
