file_arg <- grep("^--file=", commandArgs(), value = TRUE)
here <- dirname(normalizePath(sub("^--file=", "", file_arg[[1L]])))
source(file.path(here, "constructor-selection-matrix.R"))
dt_data <- dtplyr::lazy_dt(dataset)
dt_proxy <- dplyr::collect(utils::head(dt_data, 0L))
backends$dtplyr <- list(input = dt_data, oracle = dt_proxy)
verbs <- list(
  inspect_grouping = function(d, by, spec) inspect_grouping(
    d, .by = !!by, .grouping = spec, .format = "list", .duplicates = "drop"
  ),
  expand_with_margins = function(d, by, spec) expand_with_margins(
    d, .by = !!by, .grouping = spec, .duplicates = "drop",
    .margin_label = NULL, .id = "sid"
  ),
  nest_with_margins = function(d, by, spec) nest_with_margins(
    d, .by = !!by, .grouping = spec, .duplicates = "drop",
    .margin_label = NULL, .id = "sid"
  ),
  nest_by_with_margins = function(d, by, spec) nest_by_with_margins(
    d, .by = !!by, .grouping = spec, .duplicates = "drop",
    .margin_label = NULL, .id = "sid"
  ),
  summarize_with_margins = function(d, by, spec) summarize_with_margins(
    d, n = dplyr::n(), .by = !!by, .grouping = spec, .duplicates = "drop",
    .margin_label = NULL, .id = "sid"
  ),
  summarise_with_margins = function(d, by, spec) summarise_with_margins(
    d, n = dplyr::n(), .by = !!by, .grouping = spec, .duplicates = "drop",
    .margin_label = NULL, .id = "sid"
  )
)
construct <- function(maker, expr) {
  switch(maker,
    rollup_composite = rollup(grouping_set(!!expr)),
    cube_composite = cube(grouping_set(!!expr)),
    by = NULL,
    eval(rlang::call2(maker, expr), envir = globalenv())
  )
}
normalized_result <- function(x) {
  if (inherits(x, "dtplyr_step")) return(dplyr::collect(x))
  if (inherits(x, "tbl_lazy")) return(as.character(dbplyr::sql_render(x)))
  x
}
selected_cases <- unique(c(
  unname(cases), paste0("-", cases), paste0("!", cases),
  edge[c("bang_lhs","minus_rhs","mixed","namespace_data","double_bang",
         "double_minus","rlang_inject","rlang_splice","peek_vars","s3")],
  pairs[seq(1L, length(pairs), 134L)]
))
report <- list()
requested <- commandArgs(trailingOnly = TRUE)
for (backend in if (length(requested)) requested else names(backends)) {
  input <- backends[[backend]]$input
  oracle <- backends[[backend]]$oracle
  tested <- 0L
  failures <- 0L
  both_error <- 0L
  compatible <- names(verbs)
  if (backend == "postgres_sim") compatible <- setdiff(compatible,
    c("nest_with_margins", "nest_by_with_margins"))
  for (txt in selected_cases) {
    target <- expected_select(txt, oracle)
    if (inherits(target$value, "error")) next
    cols <- names(target$value)
    if (is.null(cols)) cols <- character()
    actual_expr <- selection(txt)
    ref_expr <- if (length(cols)) rlang::call2("c", !!!lapply(cols, as.name)) else
      quote(integer())
    for (maker in makers) {
      if (is.null(expected_sets(maker, cols))) next
      actual_spec <- construct(maker, actual_expr)
      ref_spec <- construct(maker, ref_expr)
      actual_by <- if (maker == "by") actual_expr else quote(NULL)
      ref_by <- if (maker == "by") ref_expr else quote(NULL)
      for (verb in compatible) {
        op <- verbs[[verb]]
        actual <- capture(normalized_result(op(input, actual_by, actual_spec)))$value
        ref <- capture(normalized_result(op(input, ref_by, ref_spec)))$value
        tested <- tested + 1L
        if (inherits(ref, "error") && inherits(actual, "error")) {
          both_error <- both_error + 1L
          equal <- identical(class(ref), class(actual)) &&
            identical(conditionMessage(ref), conditionMessage(actual))
        } else {
          equal <- identical(ref, actual)
        }
        if (!equal) {
          failures <- failures + 1L
          report[[length(report) + 1L]] <- list(backend = backend, verb = verb,
            maker = maker, txt = txt, actual = actual, ref = ref)
        }
      }
    }
  }
  cat(backend, tested, failures, both_error, "\n")
  if (length(report)) str(report, max.level = 2L)
  stopifnot(!length(report))
}
