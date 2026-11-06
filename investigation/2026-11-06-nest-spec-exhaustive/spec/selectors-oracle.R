source_dir <- Sys.getenv("MARGINPLYR_HUNT_SOURCE", ".")
output_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempfile("selector-oracle-"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(source_dir, quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
selected_names <- c("a", "b")
chooser <- function(x = "a") x
forward <- function(...) c(...)
exprs <- alist(
  a, c(a, b), a:b, a / b, !b, -b, a | b,
  all_of(selected_names), any_of(c("a", "absent")), starts_with("a"),
  ends_with("b"), matches("^[ab]$"), num_range("x", 1:2),
  where(is.character), where(is.integer), where(is.logical), where(is.numeric),
  where(function(x) inherits(x, "Date")), where(function(x) anyNA(x)),
  where(function(x) length(x) > 0L && is.character(x)),
  names(tidyselect::peek_data()), names(tidyselect::peek_data())[1:2],
  tidyselect::peek_vars(), tidyselect::peek_vars()[c(1L, 2L)],
  all_of(names(tidyselect::peek_data())),
  all_of(base::names(tidyselect::peek_data())[1L]),
  all_of((base::names)(tidyselect::peek_data())[1L]),
  all_of(chooser()), all_of(chooser("b")), all_of(forward("a", "b")),
  all_of((function(x = "a") x)()),
  all_of((function(x) x)("b")),
  all_of((function(x = names(tidyselect::peek_data())) x)()),
  all_of(.env$selected_names), all_of(.env[["selected_names"]]),
  c(.env$selected_names, "p"), c(a = a), all_of(c(a = "a")),
  base::`(`(all_of(selected_names)), (all_of(selected_names)),
  all_of(base::`(`(selected_names)), all_of((base::`(`)(selected_names)),
  base::`c`("a", "b"), (c)("a", "b"),
  all_of(as.character(c("a", "b"))), all_of(paste0(c("a", "b"))),
  all_of(unlist(list("a", "b"))), all_of(rep("a", 1L)),
  all_of(names(tidyselect::peek_data())[is.character(tidyselect::peek_data()[[1L]])]),
  where(~is.character(.x)), all_of(NULL), all_of(character())
)
family_for <- function(columns, kind) {
  if (kind == "set") return(list(columns))
  if (kind == "sets") return(list(columns, character()))
  if (kind == "product") return(lapply(list("a", character()), function(right) unique(c(columns, right))))
  if (!length(columns)) return(NULL)
  indices <- if (kind == "rollup") lapply(length(columns):0L, seq_len) else {
    unlist(lapply(length(columns):0L, function(n) utils::combn(seq_along(columns), n, simplify = FALSE)), recursive = FALSE)
  }
  lapply(indices, function(i) columns[i])
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
  tibble::tibble(set_id = seq_along(family), fixed = rep(list(fixed), length(family)), included = family, omitted = omitted, grouping_bits = bits, grouping_id = ids)
}
records <- list()
for (empty in c(FALSE, TRUE)) {
  data <- tibble::tibble(a = c("x", NA_character_), b = c(1L, 2L), c = c(TRUE, FALSE), p = as.Date(c("2026-01-01", "2026-01-02")))
  if (empty) data <- data[0L, ]
  for (i in seq_along(exprs)) {
    q <- rlang::new_quosure(exprs[[i]], environment())
    direct <- tryCatch(suppressWarnings(tidyselect::eval_select(q, data)), error = identity)
    if (inherits(direct, "error")) stop("Oracle selector failed: ", i, ": ", conditionMessage(direct))
    columns <- names(direct)
    actual <- tryCatch(suppressWarnings(inspect_grouping(data, .by = !!q, .format = "list")), error = identity)
    expected <- expected_plan(list(character()), columns, "error")
    records[[length(records) + 1L]] <- data.frame(empty, selector = i, kind = "fixed", policy = "error", matches = identical(actual, expected), observation = if (inherits(actual, "condition")) conditionMessage(actual) else "plan")
    for (kind in c("set", "sets", "rollup", "cube", "product")) {
      spec <- switch(kind,
        set = grouping_set(!!q), sets = grouping_sets(!!q, grouping_set()),
        rollup = rollup(!!q), cube = cube(!!q),
        product = grouping_spec(grouping_set(!!q), grouping_sets(grouping_set(a), grouping_set()))
      )
      family <- family_for(columns, kind)
      for (policy in c("error", "drop", "keep")) {
        expected <- if (is.null(family)) NULL else expected_plan(family, character(), policy)
        actual <- tryCatch(suppressWarnings(inspect_grouping(data, .grouping = spec, .duplicates = policy, .format = "list")), error = identity)
        matches <- if (is.null(expected)) inherits(actual, "marginplyr_error") && grepl(if (is.null(family)) "requires at least one dimension" else "Duplicate grouping sets", conditionMessage(actual), fixed = TRUE) else identical(actual, expected)
        records[[length(records) + 1L]] <- data.frame(empty, selector = i, kind, policy, matches, observation = if (inherits(actual, "condition")) conditionMessage(actual) else "plan")
      }
    }
  }
}
results <- do.call(rbind, records)
write.csv(results, file.path(output_dir, "selectors-oracle.csv"), row.names = FALSE)
writeLines(vapply(exprs, function(e) paste(deparse(e, width.cutoff = 500L), collapse = " "), character(1)), file.path(output_dir, "selectors.txt"))
cat("Selectors:", length(exprs), "Comparisons:", nrow(results), "Matches:", sum(results$matches), "\n")
if (!all(results$matches)) print(results[!results$matches, ], row.names = FALSE)
print(sessionInfo())
stopifnot(all(results$matches))
