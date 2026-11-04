output_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempdir())
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
library(marginplyr)
set.seed(20261104)
cat("SHA: 78ebbc36200bdc5cef70838dd4e772c5c4a1401c\n")
columns <- c("a", "b", "c", "d")
bit_for <- stats::setNames(2L ^ (0:3), columns)
join_mask <- function(x) Reduce(bitwOr, x, init = 0L)
leaf <- function(nonempty = FALSE) {
  chosen <- sample(columns, sample.int(4L + !nonempty, 1L) - !nonempty)
  list(expr = as.call(c(list(as.name("grouping_set")), lapply(chosen, as.name))),
       masks = join_mask(bit_for[chosen]))
}
generate <- function(depth) {
  kind <- sample(c("set", "sets", "product", "rollup", "cube"), 1L)
  if (depth == 0L || kind == "set") return(leaf())
  count <- sample.int(3L, 1L)
  if (kind %in% c("rollup", "cube")) {
    units <- replicate(count, leaf(TRUE), simplify = FALSE)
    candidates <- expand.grid(rep(list(c(FALSE, TRUE)), count))
    if (kind == "rollup") {
      candidates <- candidates[apply(candidates, 1L, function(row) {
        identical(as.logical(row), seq_len(count) <= sum(row))
      }), , drop = FALSE]
    }
    selections <- lapply(seq_len(nrow(candidates)), function(i) which(as.logical(candidates[i, ])))
    selections <- selections[order(-lengths(selections), vapply(selections, paste, collapse = "", character(1)))]
    masks <- vapply(selections, function(selected) {
      join_mask(vapply(units[selected], function(unit) unit$masks, integer(1)))
    }, integer(1))
    children <- units
  } else {
    if (kind == "product" && sample(c(FALSE, TRUE), 1L)) count <- 0L
    children <- replicate(count, generate(depth - 1L), simplify = FALSE)
    if (kind == "sets") {
      masks <- unlist(lapply(children, function(child) child$masks), use.names = FALSE)
    } else {
      masks <- 0L
      for (child in children) {
        masks <- unlist(lapply(masks, function(left) bitwOr(left, child$masks)), use.names = FALSE)
      }
    }
  }
  constructor <- c(sets = "grouping_sets", product = "grouping_spec", rollup = "rollup", cube = "cube")[[kind]]
  list(expr = as.call(c(list(as.name(constructor)), lapply(children, function(child) child$expr))),
       masks = as.integer(masks))
}
x <- tibble::tibble(a = c("x", "y"), b = 1:2, c = c(TRUE, FALSE), d = c("u", "v"))
observations <- list()
for (i in 1:500) {
  tree <- generate(2L)
  specification <- eval(tree$expr)
  for (policy in c("keep", "drop", "error")) {
    expected <- if (policy == "drop") unique(tree$masks) else tree$masks
    result <- tryCatch(inspect_grouping(x, .grouping = specification,
                                       .duplicates = policy, .format = "list"), error = identity)
    duplicate_refusal <- policy == "error" && anyDuplicated(tree$masks) > 0L
    if (inherits(result, "error")) {
      ok <- duplicate_refusal && inherits(result, "marginplyr_error")
      status <- if (ok) "expected_duplicate_refusal" else "unexpected_error"
      detail <- conditionMessage(result)
    } else {
      observed_masks <- vapply(result$included, function(included) join_mask(bit_for[included]), integer(1))
      dimensions <- columns[bitwAnd(join_mask(tree$masks), bit_for) != 0L]
      expected_omitted <- lapply(expected, function(mask) sort(dimensions[bitwAnd(mask, bit_for[dimensions]) == 0L]))
      actual_omitted <- lapply(result$omitted, sort)
      bits_ok <- all(vapply(seq_len(nrow(result)), function(row) {
        bits <- result$grouping_bits[[row]]
        expected_bits <- as.integer(bitwAnd(expected[[row]], bit_for[names(bits)]) == 0L)
        expected_id <- sum(expected_bits * 2L ^ rev(seq_along(bits) - 1L))
        identical(unname(bits), expected_bits) && identical(result$grouping_id[[row]], as.integer(expected_id))
      }, logical(1)))
      ok <- !duplicate_refusal && identical(observed_masks, expected) &&
        identical(actual_omitted, expected_omitted) && bits_ok &&
        identical(result$set_id, seq_along(expected))
      status <- if (ok) "match" else "mismatch"
      detail <- paste("expected", paste(expected, collapse = "/"), "actual", paste(observed_masks, collapse = "/"))
    }
    observations[[length(observations) + 1L]] <- data.frame(case = i, expression = deparse1(tree$expr), policy, status, detail)
  }
}
observations <- do.call(rbind, observations)
write.csv(observations, file.path(output_dir, "plan-algebra.csv"), row.names = FALSE)
print(table(observations$status))
print(observations[!observations$status %in% c("match", "expected_duplicate_refusal"), ])
stopifnot(all(observations$status %in% c("match", "expected_duplicate_refusal")))
