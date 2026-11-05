# Run from the tested package root.
# R_MAX_VSIZE=96M Rscript product-scope-repro.R 18 nested inspect first --assert
pkgload::load_all(quiet = TRUE)
args <- commandArgs(TRUE)
n <- as.integer(args[[1L]])
shape <- args[[2L]]
consumer <- args[[3L]]
orientation <- args[[4L]]
keys <- sprintf("d%02d", seq_len(n))
input <- as.data.frame(stats::setNames(rep(list(1L), n), keys))
syms <- rlang::syms(keys)
family <- cube(!!!syms)
required <- grouping_set(!!!syms)
wrapped <- switch(shape,
  direct = family,
  union = grouping_sets(!!family),
  nested = grouping_sets(grouping_spec(!!family))
)
spec <- if (orientation == "first") {
  grouping_spec(!!wrapped, !!required)
} else {
  grouping_spec(!!required, !!wrapped)
}
observed <- tryCatch({
  if (consumer == "inspect") {
    inspect_grouping(input, .grouping = spec, .duplicates = "drop", .format = "list")
  } else {
    expand_with_margins(input, .grouping = spec, .duplicates = "drop",
                        .margin_label = NULL, .id = "occurrence")
  }
}, error = identity)
label <- paste(n, shape, consumer, orientation)
if (inherits(observed, "error")) {
  stopifnot(grepl("vector memory limit", conditionMessage(observed), fixed = TRUE))
  cat(label, conditionMessage(observed), "\n")
  if ("--assert" %in% args) stop(observed)
} else {
  if (consumer == "inspect") {
    stopifnot(identical(observed$included, list(keys)), identical(observed$set_id, 1L),
              identical(observed$grouping_id, 0L),
              identical(observed$grouping_bits[[1L]], stats::setNames(rep(0L, n), keys)))
  } else {
    stopifnot(nrow(observed) == 1L, identical(observed$occurrence, 1L),
              identical(as.data.frame(observed[keys]), input))
  }
  cat(label, "one retained set\n")
}
