pkgload::load_all(quiet = TRUE)

desired <- "--assert" %in% commandArgs(trailingOnly = TRUE)
capture_error <- function(expr) tryCatch(expr, error = identity)

source <- tibble::tibble(g = 1:2, row = 1:2)
label <- matrix("Total", nrow = 1L, ncol = 1L)
stopifnot(rlang::is_string(label))
for (verb in list(nest_with_margins, nest_by_with_margins)) {
  with_check <- capture_error(verb(
    source, .grouping = rollup(g), .margin_label = list(g = label)
  ))
  cat(
    if (inherits(with_check, "error")) conditionMessage(with_check) else "success",
    "\n"
  )
  unchecked <- verb(
    source, .grouping = rollup(g), .margin_label = list(g = label),
    .check_margin_label = FALSE
  )
  direct <- verb(source, .grouping = rollup(g), .margin_label = label)
  plain <- verb(source, .grouping = rollup(g), .margin_label = list(g = "Total"))
  stopifnot(identical(direct, plain))
  cat("Unchecked display dimensions:", dim(unchecked$g), "\n")
}

operations <- list(
  nest = nest_with_margins,
  rowwise = nest_by_with_margins,
  expand = expand_with_margins,
  summary = function(source, ...) summarize_with_margins(
    source, size = dplyr::n(), ...
  )
)
label_forms <- list(
  matrix = function(value) matrix(value, nrow = 1L, ncol = 1L),
  array = function(value) array(value, dim = rep(1L, 3L)),
  asis = I,
  noquote = noquote,
  named_scalar = function(value) c(label = value)
)
cases <- expand.grid(
  rows = 0:3,
  type = c("integer", "character", "factor"),
  check = c(FALSE, TRUE),
  label = c("Total", "1", NA_character_),
  form = names(label_forms),
  operation = names(operations),
  stringsAsFactors = FALSE
)

compare <- function(case) {
  values <- seq_len(case$rows)
  key <- switch(
    case$type,
    integer = values,
    character = as.character(values),
    factor = factor(values, levels = c("2", "1", "3"))
  )
  source <- tibble::tibble(g = key, row = values)
  label <- label_forms[[case$form]](case$label)
  stopifnot(is.character(label), length(label) == 1L)
  verb <- operations[[case$operation]]
  run <- function(margin_label) capture_error(verb(
    source, .grouping = rollup(g), .margin_label = margin_label,
    .check_margin_label = case$check, .id = "set", .sort = "last"
  ))
  control <- run(list(g = case$label))
  direct <- run(if (is.null(names(label))) label else unname(label))
  if (inherits(control, "error")) {
    stopifnot(inherits(control, "marginplyr_error"))
    stopifnot(inherits(direct, "marginplyr_error"))
    stopifnot(identical(conditionMessage(control), conditionMessage(direct)))
  } else {
    stopifnot(identical(control, direct))
  }

  result <- run(list(g = label))
  if (inherits(result, "error")) {
    if (inherits(control, "error") &&
          identical(conditionMessage(control), conditionMessage(result))) {
      return("match")
    }
    if (grepl("dims [product 1]", conditionMessage(result), fixed = TRUE)) {
      return("dimension_error")
    }
    if (inherits(result, "vctrs_error_ptype2")) {
      return("branch_type_error")
    }
    stop("Unexpected error in named-list variant: ", conditionMessage(result))
  }
  if (inherits(control, "error")) {
    return("missed_collision")
  }
  stopifnot(
    identical(result$set, control$set),
    identical(as.character(result$g), as.character(control$g))
  )
  if (identical(result, control)) "match" else "shape_or_class"
}

results <- vapply(seq_len(nrow(cases)), function(i) {
  compare(cases[i, , drop = FALSE])
}, "")
print(with(cases, table(form, results)))
cat(R.version.string, "\n")
if (desired) {
  stopifnot(all(results == "match"))
}
