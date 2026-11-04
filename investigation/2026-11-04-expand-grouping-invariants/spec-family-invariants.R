# Run from the repository root. Expected families are modeled independently.
pkgload::load_all(quiet = TRUE)

dimensions <- letters[1:5]
input <- as.data.frame(stats::setNames(
  lapply(seq_along(dimensions), function(i) as.integer(c(i, i + 10L))),
  dimensions
))
input$fixed <- c("east", "west")
input$source <- seq_len(nrow(input))

set_node <- function(cols) {
  list(expr = as.call(c(list(as.name("grouping_set")), lapply(cols, as.name))),
       family = list(unique(cols)))
}

make_node <- function(depth) {
  kinds <- c("grouping_set", "rollup", "cube", "grouping_sets", "grouping_spec")
  kind <- if (depth == 0L) "grouping_set" else sample(kinds, 1L)
  if (kind == "grouping_set") {
    return(set_node(sample(dimensions, sample(0:3, 1L), replace = TRUE)))
  }
  if (kind %in% c("rollup", "cube")) {
    units <- replicate(
      sample(1:4, 1L),
      sample(dimensions, sample(1:3, 1L), replace = TRUE),
      simplify = FALSE
    )
    args <- lapply(units, function(cols) {
      if (length(cols) == 1L) as.name(cols) else set_node(cols)$expr
    })
    n <- length(units)
    if (kind == "rollup") {
      choices <- lapply(n:0L, seq_len)
    } else {
      bits <- as.matrix(expand.grid(rep(list(c(TRUE, FALSE)), n)))
      ordering <- do.call(order, c(list(-rowSums(bits)),
                                   as.list(as.data.frame(!bits))))
      choices <- lapply(ordering, function(i) which(bits[i, ]))
    }
    family <- lapply(choices, function(i) {
      unique(as.character(unlist(units[i], use.names = FALSE)))
    })
    return(list(expr = as.call(c(list(as.name(kind)), args)), family = family))
  }

  arity <- if (kind == "grouping_spec") 0:3 else 1:3
  children <- replicate(sample(arity, 1L), make_node(depth - 1L),
                        simplify = FALSE)
  families <- lapply(children, `[[`, "family")
  family <- list(character())
  if (kind == "grouping_sets") {
    family <- do.call(c, families)
  } else if (length(families)) {
    choices <- expand.grid(rev(lapply(families, seq_along)))
    family <- lapply(seq_len(nrow(choices)), function(i) {
      indexes <- rev(as.integer(choices[i, ]))
      sets <- Map(function(factor, index) factor[[index]], families, indexes)
      unique(as.character(unlist(sets, use.names = FALSE)))
    })
  }
  list(
    expr = as.call(c(list(as.name(kind)), lapply(children, `[[`, "expr"))),
    family = family
  )
}

check_plan <- function(spec, family, policy, fixed) {
  keys <- vapply(family, function(set) paste(sort(set), collapse = ""),
                 character(1))
  if (policy == "error" && anyDuplicated(keys)) {
    err <- tryCatch(inspect_grouping(input, .by = tidyselect::all_of(fixed),
                                    .grouping = spec),
                    error = identity)
    stopifnot(inherits(err, "marginplyr_error"),
              grepl("Duplicate grouping sets", conditionMessage(err),
                    fixed = TRUE))
    return(invisible(NULL))
  }
  ordered_dimensions <- unique(as.character(unlist(family, use.names = FALSE)))
  if (policy == "drop") {
    family <- family[!duplicated(keys)]
  }
  included <- lapply(family, function(set) {
    ordered_dimensions[ordered_dimensions %in% set]
  })
  omitted <- lapply(included, function(set) setdiff(ordered_dimensions, set))
  expected_bits <- lapply(omitted, function(set) {
    if (!length(ordered_dimensions)) return(integer())
    stats::setNames(as.integer(ordered_dimensions %in% set), ordered_dimensions)
  })
  expected_ids <- vapply(expected_bits, function(bits) {
    as.integer(sum(bits * 2^rev(seq_along(bits) - 1L)))
  }, integer(1))
  plan <- inspect_grouping(input, .by = tidyselect::all_of(fixed),
                           .grouping = spec, .duplicates = policy,
                           .format = "list")
  stopifnot(identical(plan$set_id, seq_along(family)),
            identical(plan$fixed, rep(list(fixed), length(family))),
            identical(plan$included, included),
            identical(plan$omitted, omitted),
            identical(plan$grouping_bits, expected_bits),
            identical(plan$grouping_id, expected_ids))
  if (policy == "error") {
    return(invisible(NULL))
  }

  output <- expand_with_margins(input, .by = tidyselect::all_of(fixed),
                                .grouping = spec, .duplicates = policy,
                                .margin_label = NULL, .id = "set_id")
  stopifnot(nrow(output) == nrow(input) * length(family))
  output <- output[order(output$set_id, output$source), ]
  stopifnot(identical(output$set_id, rep(seq_along(family), each = nrow(input))),
            identical(output$source, rep(input$source, length(family))),
            identical(output$fixed, rep(input$fixed, length(family))))
  for (i in seq_along(family)) {
    rows <- output[output$set_id == i, ]
    for (col in dimensions) {
      expected <- if (col %in% omitted[[i]]) rep(NA_integer_, nrow(input)) else
        input[[col]]
      stopifnot(identical(rows[[col]], expected))
    }
  }
}

set.seed(40641)
families <- replicate(100L, make_node(3L), simplify = FALSE)
sizes <- lengths(lapply(families, `[[`, "family"))
families <- families[sizes <= 250L]
for (i in seq_along(families)) {
  node <- families[[i]]
  spec <- eval(node$expr)
  if (i %% 3L == 0L) {
    spec <- unserialize(serialize(spec, NULL))
  }
  fixed <- if (i %% 2L == 0L) "fixed" else character()
  for (policy in c("keep", "drop", "error")) {
    check_plan(spec, node$family, policy, fixed)
  }
}
duplicate_families <- sum(vapply(families, function(node) {
  anyDuplicated(lapply(node$family, sort)) > 0L
}, logical(1)))
cat(length(families), "families;", duplicate_families, "have duplicates;",
    sum(sizes > 250L), "over the occurrence cap; all comparisons passed.\n")
