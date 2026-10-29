# Dated investigation evidence; investigated 2026-10-29 on main 4c63bb9.
# Run from the repository root, or pass its path as the first argument.
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1L]] else "."
pkgload::load_all(repo, quiet = TRUE)
options(lifecycle_verbosity = "quiet")
set.seed(20261029L)

# The independent oracle operates on character-vector families, never on a
# compiled package plan. Leaves use both symbols and explicit selections.
variable_names <- c("a", ".data", ".env", "a,b", "a=b", "a`b", "\u65e5\u672c\u8a9e", "\u00e9")
data <- as.data.frame(stats::setNames(rep(list(1:2), length(variable_names)),
                                   variable_names), check.names = FALSE)
data[["fixed key"]] <- c("x", "y")
wrap <- function(expr, depth) {
  for (i in seq_len(depth)) {
    expr <- rlang::call2("(", expr)
  }
  expr
}
leaf <- function(nonempty = FALSE, composite = FALSE) {
  n <- sample(if (nonempty) 1:4 else 0:4, 1L)
  columns <- sample(variable_names, n)
  form <- sample(if (any(columns %in% c(".data", ".env"))) 2:3 else 1:3, 1L)
  expr <- switch(form,
    rlang::call2("c", !!!rlang::syms(columns)),
    rlang::call2("all_of", columns, .ns = "tidyselect"),
    rlang::call2("c", !!!as.list(match(columns, names(data))))
  )
  expr <- wrap(expr, sample(0:3, 1L))
  if (composite) {
    expr <- rlang::call2("grouping_set", expr, .ns = "marginplyr")
  }
  list(expr = expr, family = list(columns))
}
combine_product <- function(left, right) {
  unlist(lapply(left, function(a) {
    lapply(right, function(b) unique(c(a, b)))
  }), recursive = FALSE)
}
generate <- function(depth = 0L) {
  kind <- sample(if (depth >= 2L) c("set", "leaf") else
                   c("set", "sets", "rollup", "cube", "product"), 1L)
  if (kind %in% c("set", "leaf")) {
    return(leaf(composite = identical(kind, "set")))
  }
  count <- sample(if (identical(kind, "product")) 0:3 else 1:3, 1L)
  if (kind %in% c("rollup", "cube")) {
    children <- lapply(seq_len(count), function(i) leaf(TRUE, TRUE))
    units <- lapply(children, function(child) child$family[[1L]])
    indices <- if (identical(kind, "rollup")) {
      lapply(count:0, seq_len)
    } else {
      unlist(lapply(count:0, function(size) {
        utils::combn(seq_len(count), size, simplify = FALSE)
      }), recursive = FALSE)
    }
    family <- lapply(indices, function(index) {
      result <- unique(unlist(units[index], use.names = FALSE))
      if (is.null(result)) character() else result
    })
  } else {
    children <- lapply(seq_len(count), function(i) generate(depth + 1L))
    families <- lapply(children, `[[`, "family")
    family <- if (identical(kind, "sets")) {
      unlist(families, recursive = FALSE)
    } else {
      Reduce(combine_product, families, init = list(character()))
    }
  }
  constructor <- switch(kind, sets = "grouping_sets", product = "grouping_spec", kind)
  expr <- rlang::call2(constructor, !!!lapply(children, `[[`, "expr"),
                      .ns = "marginplyr")
  list(expr = wrap(expr, sample(0:3, 1L)), family = family)
}
inputs <- list(local = data, dtplyr = dtplyr::lazy_dt(data))
checks <- 0L
for (i in seq_len(600L)) {
  generated <- generate()
  # A top-level selection is represented by the public one-set constructor.
  call <- rlang::call2("grouping_spec", generated$expr, .ns = "marginplyr")
  spec <- eval(call)
  dimensions <- unique(unlist(generated$family, use.names = FALSE))
  if (is.null(dimensions)) dimensions <- character()
  family <- lapply(generated$family, function(columns) {
    dimensions[dimensions %in% columns]
  })
  keys <- vapply(family, function(columns) {
    paste(as.integer(dimensions %in% columns), collapse = "")
  }, character(1))
  for (backend in names(inputs)) {
    for (policy in c("keep", "drop")) {
      expected <- if (identical(policy, "drop")) family[!duplicated(keys)] else family
      result <- inspect_grouping(inputs[[backend]], .by = all_of("fixed key"),
                                 .grouping = spec, .duplicates = policy,
                                 .format = "list")
      masks <- lapply(expected, function(columns) {
        if (!length(dimensions)) integer() else
          stats::setNames(as.integer(!dimensions %in% columns), dimensions)
      })
      ids <- vapply(masks, function(mask) {
        if (!length(mask)) return(0L)
        as.integer(sum(mask * 2^rev(seq.int(0L, length(mask) - 1L))))
      }, integer(1))
      if (!identical(result$included, expected) ||
          !identical(result$omitted, lapply(expected, function(columns) {
            dimensions[!dimensions %in% columns]
          })) || !identical(result$grouping_bits, masks) ||
          !identical(result$grouping_id, ids) ||
          !identical(result$set_id, seq_along(expected)) ||
          !identical(result$fixed, rep(list("fixed key"), length(expected)))) {
        print(call)
        stop(paste("Oracle mismatch", i, backend, policy))
      }
      checks <- checks + 1L
    }
    observed <- tryCatch(inspect_grouping(inputs[[backend]], .grouping = spec),
                         error = identity)
    if (anyDuplicated(keys)) {
      stopifnot(inherits(observed, "marginplyr_error"),
                grepl("Duplicate grouping sets", conditionMessage(observed), fixed = TRUE))
    } else {
      stopifnot(!inherits(observed, "error"), nrow(observed) == length(family))
    }
    checks <- checks + 1L
  }
}
cat("PASS:", checks, "independent inspections over 600 generated specification ASTs; seed 20261029\n")
print(utils::sessionInfo())
