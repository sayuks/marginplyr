source_dir <- Sys.getenv("MARGINPLYR_HUNT_SOURCE", ".")
output_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", "investigation/2026-11-06-nest-grouping-greedy/spec")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(source_dir, quiet = TRUE)
set.seed(20261106)
columns <- c("a b", "a,b", "a=b", "a`b", "é", "e\u0301", "Tab\tColumn", "a\nb", ".rows", "..0")
dat <- tibble::as_tibble(stats::setNames(rep(list("x"), length(columns)), columns))
fixed_name <- "fixed key\n"
dat[[fixed_name]] <- "partition"
leaf <- function(nonempty = FALSE) {
  size <- sample(seq.int(as.integer(nonempty), 4L), 1L)
  selected <- if (size) sample(columns, size) else character()
  list(kind = "selection", columns = selected,
       spelling = sample(c("names", "all_of", "positions"), 1L))
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
    if (kind == "set" || depth >= 5L || sample(c(TRUE, FALSE), 1L)) {
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
    if (length(bit) > 31L) NA_integer_ else as.integer(sum(bit * 2 ^ rev(seq.int(0L, length(bit) - 1L))))
  }, integer(1))
  tibble::tibble(set_id = seq_along(family), fixed = rep(list(fixed), length(family)),
                 included = family, omitted = omitted, grouping_bits = bits,
                 grouping_id = ids)
}

trees <- lapply(seq_len(250L), function(i) {
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
      expected <- expected_plan(family, if (fixed) fixed_name else character(), policy)
      actual <- tryCatch({
        if (fixed) inspect_grouping(dat, .by = tidyselect::all_of(fixed_name), .grouping = specification,
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
stopifnot(nrow(results) == 1500L, all(results$matches))

boundary_records <- list()
for (width in c(0L, 1L, 30L, 31L, 32L, 33L, 64L)) {
  wide_names <- if (width) paste0("dimension ", seq_len(width)) else character()
  wide <- tibble::as_tibble(stats::setNames(rep(list("x"), width), wide_names))
  for (kind in c("sets", "product", "rollup_composite", "cube_composite")) {
    if (!width && kind %in% c("rollup_composite", "cube_composite")) next
    specification <- switch(kind,
      sets = grouping_sets(grouping_set(tidyselect::all_of(wide_names)), grouping_set()),
      product = grouping_spec(grouping_sets(grouping_set(tidyselect::all_of(wide_names)), grouping_set()), grouping_spec()),
      rollup_composite = rollup(grouping_set(tidyselect::all_of(wide_names))),
      cube_composite = cube(grouping_set(tidyselect::all_of(wide_names)))
    )
    family <- list(wide_names, character())
    for (policy in c("error", "drop", "keep")) {
      expected <- expected_plan(family, character(), policy)
      actual <- tryCatch(inspect_grouping(wide, .grouping = specification,
                                         .duplicates = policy, .format = "list"), error = identity)
      matches <- if (is.null(expected)) {
        inherits(actual, "marginplyr_error") &&
          grepl("Duplicate grouping sets", conditionMessage(actual), fixed = TRUE)
      } else identical(actual, expected)
      boundary_records[[length(boundary_records) + 1L]] <- data.frame(width, kind, policy, matches)
    }
  }
}
boundaries <- do.call(rbind, boundary_records)
write.csv(boundaries, file.path(output_dir, "dimension-boundaries.csv"), row.names = FALSE)
cat("Dimension boundary comparisons:", nrow(boundaries), "Matches:", sum(boundaries$matches), "\n")
stopifnot(all(boundaries$matches))

empty_cases <- list(
  empty_set = list(grouping_set(tidyselect::all_of(character())), list(character())),
  empty_union = list(grouping_sets(tidyselect::all_of(character())), list(character())),
  identity_product = list(grouping_spec(), list(character())),
  empty_product = list(grouping_spec(tidyselect::all_of(character())), list(character())),
  empty_leading_rollup = list(rollup(tidyselect::all_of(character()), tidyselect::all_of(columns[[1L]])), list(columns[[1L]], character())),
  empty_trailing_cube = list(cube(tidyselect::all_of(columns[[1L]]), tidyselect::all_of(character())), list(columns[[1L]], character()))
)
for (case in names(empty_cases)) {
  expected <- expected_plan(empty_cases[[case]][[2L]], character(), "error")
  actual <- inspect_grouping(dat, .grouping = empty_cases[[case]][[1L]], .format = "list")
  stopifnot(identical(actual, expected))
}
for (specification in list(rollup(tidyselect::all_of(character())),
                          cube(tidyselect::all_of(character())),
                          rollup(grouping_set(tidyselect::all_of(character()))),
                          cube(grouping_set(tidyselect::all_of(character())), tidyselect::all_of(columns[[1L]])))) {
  actual <- tryCatch(inspect_grouping(dat, .grouping = specification), error = identity)
  stopifnot(inherits(actual, "marginplyr_error"))
}
cat("Empty-selection comparisons: 10 Matches: 10\n")
