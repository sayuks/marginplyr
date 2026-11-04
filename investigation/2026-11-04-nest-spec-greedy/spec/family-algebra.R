source_dir <- Sys.getenv("MARGINPLYR_HUNT_SOURCE", ".")
output_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempfile("family-algebra-"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(source_dir, quiet = TRUE)
set.seed(20261104)
columns <- c("a", "b", "c", "d")
dat <- tibble::tibble(a = "x", b = 1L, c = TRUE, d = as.Date("2026-11-04"), fixed = "partition")
leaf <- function(nonempty = FALSE) {
  size <- sample(seq.int(as.integer(nonempty), 4L), 1L)
  selected <- if (size) sample(columns, size) else character()
  list(kind = "selection", columns = selected,
       spelling = sample(c("names", "all_of", "positions", "c"), 1L))
}
node <- function(depth = 0L, forced = NULL) {
  kind <- if (!is.null(forced)) forced else {
    sample(c("set", "sets", "rollup", "cube", "product"), 1L)
  }
  count <- sample(if (kind %in% c("set", "product")) 0:3 else 1:3, 1L)
  children <- lapply(seq_len(count), function(i) {
    if (kind %in% c("rollup", "cube")) {
      if (sample(c(TRUE, FALSE), 1L)) {
        return(list(kind = "set", children = list(leaf(TRUE))))
      }
      return(leaf(TRUE))
    }
    if (kind == "set" || depth >= 2L || sample(c(TRUE, FALSE), 1L)) {
      return(leaf())
    }
    node(depth + 1L)
  })
  list(kind = kind, children = children)
}
expression_for <- function(tree) {
  if (tree$kind == "selection") {
    selected <- tree$columns
    return(switch(tree$spelling,
      names = selected,
      all_of = rlang::call2("all_of", selected, .ns = "tidyselect"),
      positions = match(selected, columns),
      c = if (length(selected)) rlang::call2("c", !!!lapply(selected, as.name)) else integer()
    ))
  }
  constructor <- c(set = "grouping_set", sets = "grouping_sets", rollup = "rollup",
                   cube = "cube", product = "grouping_spec")[[tree$kind]]
  rlang::call2(constructor, !!!lapply(tree$children, expression_for))
}
family_for <- function(tree) {
  if (tree$kind == "selection") return(list(tree$columns))
  children <- tree$children
  if (tree$kind == "set") {
    return(list(unique(unlist(lapply(children, function(x) x$columns), use.names = FALSE))))
  }
  if (tree$kind == "sets") {
    return(unlist(lapply(children, family_for), recursive = FALSE))
  }
  if (tree$kind == "product") {
    answer <- list(character())
    for (child in children) {
      next_family <- family_for(child)
      answer <- unlist(lapply(answer, function(left) {
        lapply(next_family, function(right) unique(c(left, right)))
      }), recursive = FALSE)
    }
    return(answer)
  }
  units <- unlist(lapply(children, function(child) {
    if (child$kind == "set") return(family_for(child))
    lapply(child$columns, function(column) column)
  }), recursive = FALSE)
  count <- length(units)
  indices <- if (tree$kind == "rollup") {
    lapply(count:0L, seq_len)
  } else {
    unlist(lapply(count:0L, function(size) {
      utils::combn(seq_len(count), size, simplify = FALSE)
    }), recursive = FALSE)
  }
  lapply(indices, function(index) unique(unlist(units[index], use.names = FALSE)))
}
expected_plan <- function(family, fixed, policy) {
  dimensions <- unique(unlist(family, use.names = FALSE))
  if (is.null(dimensions)) dimensions <- character()
  family <- lapply(family, function(x) dimensions[dimensions %in% x])
  duplicate <- duplicated(family)
  if (policy == "error" && any(duplicate)) return(NULL)
  if (policy == "drop") family <- family[!duplicate]
  omitted <- lapply(family, function(x) dimensions[!dimensions %in% x])
  bits <- lapply(family, function(x) {
    if (!length(dimensions)) return(integer())
    stats::setNames(as.integer(!dimensions %in% x), dimensions)
  })
  ids <- vapply(bits, function(bit) {
    if (!length(bit)) return(0L)
    as.integer(sum(bit * 2 ^ rev(seq.int(0L, length(bit) - 1L))))
  }, integer(1))
  tibble::tibble(set_id = seq_along(family), fixed = rep(list(fixed), length(family)),
                 included = family, omitted = omitted, grouping_bits = bits,
                 grouping_id = ids)
}

trees <- lapply(seq_len(180L), function(i) {
  kind <- c("set", "sets", "rollup", "cube", "product")[(i - 1L) %% 5L + 1L]
  repeat {
    candidate <- node(forced = kind)
    family <- family_for(candidate)
    if (length(family) <= 64L) return(candidate)
  }
})
records <- list()
for (i in seq_along(trees)) {
  tree <- trees[[i]]
  expression <- expression_for(tree)
  specification <- eval(expression)
  family <- family_for(tree)
  for (fixed in c(FALSE, TRUE)) {
    for (policy in c("error", "drop", "keep")) {
      expected <- expected_plan(family, if (fixed) "fixed" else character(), policy)
      actual <- tryCatch({
        if (fixed) inspect_grouping(dat, .by = fixed, .grouping = specification,
                                     .duplicates = policy, .format = "list") else {
          inspect_grouping(dat, .grouping = specification,
                           .duplicates = policy, .format = "list")
        }
      }, error = identity)
      matches <- if (is.null(expected)) {
        inherits(actual, "marginplyr_error") &&
          grepl("Duplicate grouping sets", conditionMessage(actual), fixed = TRUE)
      } else identical(actual, expected)
      records[[length(records) + 1L]] <- data.frame(
        case = i, kind = tree$kind, fixed = fixed, policy = policy,
        matches = matches, family_size = length(family),
        expression = paste(deparse(expression, width.cutoff = 500L), collapse = " "),
        observation = if (inherits(actual, "condition")) conditionMessage(actual) else "plan"
      )
    }
  }
}
results <- do.call(rbind, records)
write.csv(results, file.path(output_dir, "family-algebra.csv"), row.names = FALSE)
print(with(results, table(kind, policy, matches)))
cat("Trees:", length(trees), "Comparisons:", nrow(results),
    "Matches:", sum(results$matches), "\n")
if (!all(results$matches)) print(results[!results$matches, ], row.names = FALSE)
print(sessionInfo())
stopifnot(nrow(results) == 1080L, all(results$matches))
