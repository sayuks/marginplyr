# Direct typed tidyselect supplies the selector oracle. Family expansion below
# is independent of marginplyr's compiler and bit helpers.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
set.seed(202611061L)
column_names <- c("a", "b", "c", "a,b", "a|b", "1", ".env")
input <- stats::setNames(as.data.frame(rep(list(c("x", "y")), 7L)),
                         column_names)
cols <- c("a,b", "a")
position <- 2L
lookup <- tidyselect::peek_vars
extract_names <- base::names
helper <- function(prefix = "a") paste0(prefix)
selection_atoms <- list(
  quote(everything()), quote(starts_with("a")), quote(ends_with("b")),
  quote(contains("|")), quote(matches("^[abc]$")), quote(last_col()),
  quote(all_of(cols)), quote(any_of(c("a", "absent"))),
  quote(all_of(.env$cols)), quote(all_of(.env[["cols"]])),
  quote(lookup()[1:3]), quote(names(tidyselect::peek_data())[2:4]),
  quote(extract_names(tidyselect::peek_data())[c(2, 1)]),
  quote(all_of(tolower(c("A", "B")))),
  quote(all_of(helper())), quote(all_of(paste0("a", ",b"))),
  quote(c("a", "b")), quote(1:3), quote(c(2L, 1L)),
  quote(.data$a), quote(.data[["a,b"]]), quote(.env$cols),
  quote(.env[["cols"]]), quote(a)
)
# Include explicit qualified identities and both function-position spellings.
base_paren <- rlang::call2("::", quote(base), quote(`(`))
selection_atoms <- c(selection_atoms, lapply(selection_atoms, function(expr) {
  as.call(list(base_paren, expr))
}))
expressions <- selection_atoms
for (i in seq_len(252L)) {
  left <- selection_atoms[[sample.int(length(selection_atoms), 1L)]]
  right <- selection_atoms[[sample.int(length(selection_atoms), 1L)]]
  operation <- sample(c("c", "|", "&", "/", "!", "-"), 1L)
  expressions[[length(expressions) + 1L]] <- if (operation %in% c("!", "-"))
    rlang::call2(operation, left) else rlang::call2(operation, left, right)
}
expected_family <- function(kind, selected) {
  if (kind %in% c("grouping_set", "grouping_sets", "grouping_spec")) {
    return(list(selected))
  }
  if (kind == "rollup") {
    return(lapply(length(selected):0L, function(n) head(selected, n)))
  }
  result <- list()
  for (size in length(selected):0L) {
    subsets <- if (size == 0L) list(character()) else
      utils::combn(selected, size, simplify = FALSE)
    result <- c(result, subsets)
  }
  result
}
successes <- rejected <- empty_units <- 0L
for (expr in expressions) {
  selection <- rlang::new_quosure(expr, environment())
  direct <- suppressWarnings(tryCatch(tidyselect::eval_select(selection, input),
                                     error = identity))
  if (inherits(direct, "error")) {
    rejected <- rejected + 1L
    next
  }
  selected <- column_names[unname(direct)]
  if (!identical(names(direct), selected)) stop("Unexpected renamed fixture")
  for (kind in c("grouping_set", "grouping_sets", "grouping_spec", "rollup",
                 "cube")) {
    spec_expr <- rlang::call2(kind, selection)
    spec <- rlang::eval_bare(spec_expr, environment())
    actual <- suppressWarnings(tryCatch(inspect_grouping(input,
      .grouping = spec, .duplicates = "keep", .format = "list"),
      error = identity))
    if (length(selected) == 0L && kind %in% c("rollup", "cube")) {
      stopifnot(inherits(actual, "marginplyr_error"),
                grepl("at least one", conditionMessage(actual), fixed = TRUE))
      empty_units <- empty_units + 1L
      next
    }
    if (inherits(actual, "error")) {
      print(expr)
      stop(actual)
    }
    expected <- expected_family(kind, selected)
    if (!identical(actual$included, expected)) {
      print(list(expression = expr, kind = kind, actual = actual$included,
                 expected = expected))
    }
    stopifnot(identical(actual$included, expected),
              identical(actual$set_id, seq_along(expected)))
    successes <- successes + 1L
  }
  fixed <- suppressWarnings(inspect_grouping(input, .by = !!selection,
    .grouping = grouping_set(), .format = "list"))
  stopifnot(identical(fixed$fixed, list(selected)))
  successes <- successes + 1L
}
cat("Generated selectors:", length(expressions), "\n",
    "Exact successful plan/fixed-key comparisons:", successes, "\n",
    "Required empty-unit refusals:", empty_units, "\n",
    "Direct tidyselect-rejected selectors excluded:", rejected, "\n")
print(sessionInfo())
