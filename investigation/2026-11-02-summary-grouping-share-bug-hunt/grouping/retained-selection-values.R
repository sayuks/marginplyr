# Dated investigation harness; run from the repository root.
pkgload::load_all(quiet = TRUE)
options(lifecycle_verbosity = "quiet")
frame <- tibble::tibble(a = 1:2, b = c("x", "y"), c = c(TRUE, FALSE))
values <- list(null = NULL, empty_integer = integer(), empty_character = character(),
  first = 1L, negative = -1L, all = 1:3, truth = TRUE, falsehood = FALSE,
  missing = NA_integer_, character = "a", characters = c("b", "a"),
  language = quote(a), call = quote(c(a, b)), expression = expression(a),
  quosure = rlang::quo(a), list = list("a"), factor = factor("a"),
  complex = 1 + 0i, raw = as.raw(1), matrix = matrix(1L),
  predicate = is.numeric, local_predicate = function(x) is.character(x),
  bad_predicate = function(x) c(TRUE, FALSE))
capture <- function(expr) tryCatch(expr, error = identity)
record <- list()
index <- 0L
for (value_name in names(values)) {
  value <- values[[value_name]]
  for (shape in c("binding", "constructor_call")) for (parens in 0:2) {
    reads <- 0L
    read <- function(...) {
      reads <<- reads + 1L
      value
    }
    env <- rlang::env(grouping_set = read)
    makeActiveBinding("selected", read, env)
    expr <- if (shape == "binding") quote(selected) else quote(grouping_set(unforced_argument))
    if (parens > 0L) {
      for (i in seq_len(parens)) {
        expr <- rlang::call2("(", expr)
      }
    }
    selection <- rlang::new_quosure(expr, env)
    oracle_env <- rlang::env(selected = value, grouping_set = function(...) value)
    direct <- capture(tidyselect::eval_select(rlang::new_quosure(expr, oracle_env), frame))
    for (parent in c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")) {
      reads <- 0L
      constructor <- getExportedValue("marginplyr", parent)
      specification <- rlang::inject(constructor(!!selection))
      answer <- capture(inspect_grouping(frame, .grouping = specification, .format = "list"))
      index <- index + 1L
      if (inherits(direct, "error")) {
        match <- inherits(answer, "error") && identical(class(answer), class(direct))
      } else if (length(direct) == 0L && parent %in% c("rollup", "cube")) {
        match <- inherits(answer, "marginplyr_error") && grepl("requires at least one dimension", conditionMessage(answer), fixed = TRUE)
      } else {
        expected <- names(frame)[unname(direct)]
        match <- !inherits(answer, "error") && identical(answer$included[[1L]], expected)
      }
      record[[index]] <- data.frame(value = value_name, shape = shape,
        parens = parens, parent = parent, reads = reads, match = match,
        observed = if (inherits(answer, "error")) conditionMessage(answer) else paste(answer$included[[1L]], collapse = ","))
    }
  }
}
record <- do.call(rbind, record)
print(with(record, table(match, reads)))
if (!all(record$match)) print(record[!record$match, ])
cat("retained-value comparisons:", nrow(record), "\n")
utils::write.csv(record, "investigation/2026-11-02-summary-grouping-share-bug-hunt/grouping/retained-selection-values.csv", row.names = FALSE)
stopifnot(all(record$match), all(record$reads == 1L))
