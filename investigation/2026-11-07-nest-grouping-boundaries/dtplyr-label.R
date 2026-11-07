args <- commandArgs(trailingOnly = TRUE)
pkgload::load_all(args[[1L]], quiet = TRUE)
make_source <- function(keys) {
  scope <- new.env(parent = globalenv())
  scope$source <- data.table::data.table(g = keys, units = seq_along(keys) * 17L)
  scope$visits <- character()
  scope
}
set_resolver <- function(scope, namespace, member, replacement) {
  resolve_export <- getExportedValue
  scope[["::"]] <- function(pkg, name) {
    package <- as.character(substitute(pkg))
    exported_name <- as.character(substitute(name))
    if (identical(package, namespace) && identical(exported_name, member)) {
      scope$visits <- c(scope$visits, paste(package, exported_name, sep = "::"))
      return(replacement)
    }
    resolve_export(package, exported_name)
  }
}
force_result <- function(x) {
  if (inherits(x, "dtplyr_step")) dplyr::collect(x) else x
}
capture <- function(expr) tryCatch(expr, error = identity)
verbs <- list(
  nest_with_margins = function(x, plan, check) nest_with_margins(
    x, .grouping = plan, .check_margin_label = check, .id = "set"
  ),
  nest_by_with_margins = function(x, plan, check) nest_by_with_margins(
    x, .grouping = plan, .check_margin_label = check, .id = "set"
  ),
  expand_with_margins = function(x, plan, check) expand_with_margins(
    x, .grouping = plan, .check_margin_label = check, .id = "set"
  ),
  summarize_with_margins = function(x, plan, check) summarize_with_margins(
    x, units = sum(units), .grouping = plan, .check_margin_label = check, .id = "set"
  )
)
check_collision <- function() {
  targets <- list(
    list("base", "as.character", function(x, ...) rep("OTHER", length(x))),
    list("base", "sum", function(...) 0L),
    list("base", "as.vector", function(x, ...) rep(FALSE, length(x))),
    list("dplyr", "if_else", function(condition, true, false, ...) rep(0L, length(condition)))
  )
  records <- list()
  for (target in targets) {
    scope <- make_source("Total")
    input <- evalq(dtplyr::lazy_dt(source), scope)
    bytes <- serialize(scope$source, NULL)
    for (verb in names(verbs)) {
      scope$visits <- character()
      fun <- verbs[[verb]]
      before <- capture(fun(input, rollup(g), TRUE))
      stopifnot(inherits(before, "marginplyr_error"),
                grepl("already present", conditionMessage(before)))
      set_resolver(scope, target[[1L]], target[[2L]], target[[3L]])
      out <- capture(fun(input, rollup(g), TRUE))
      observed <- if (inherits(out, "error")) class(out)[[1L]] else "admitted"
      visits <- length(scope$visits)
      stopifnot(identical(dplyr::collect(input)$g, "Total"))
      unchecked <- capture(fun(input, rollup(g), FALSE))
      stopifnot(!inherits(unchecked, "error"), length(scope$visits) == visits,
                identical(serialize(scope$source, NULL), bytes))
      rm("::", envir = scope)
      restored <- capture(fun(input, rollup(g), TRUE))
      stopifnot(inherits(restored, "marginplyr_error"))
      records[[length(records) + 1L]] <- data.frame(
        verb = verb, helper = paste(target[[1L]], target[[2L]], sep = "::"),
        observed = observed, visits = visits
      )
    }
  }
  do.call(rbind, records)
}
check_display <- function() {
  scope <- make_source(c("east", "west"))
  input <- evalq(dtplyr::lazy_dt(source), scope)
  bytes <- serialize(scope$source, NULL)
  records <- list()
  for (verb in names(verbs)) {
    scope$visits <- character()
    fun <- verbs[[verb]]
    baseline <- force_result(fun(input, rollup(g), FALSE))
    query <- if (verb == "nest_by_with_margins") NULL else fun(input, rollup(g), FALSE)
    set_resolver(scope, "base", "rep", function(x, ...) {
      base::rep(if (identical(x, "Total")) "CHANGED" else x, ...)
    })
    altered <- if (is.null(query)) fun(input, rollup(g), FALSE) else force_result(query)
    visits <- length(scope$visits)
    native <- dplyr::collect(input)
    detail <- force_result(fun(input, grouping_set(g), FALSE))
    stopifnot(identical(native$g, c("east", "west")),
              identical(detail$g, c("east", "west")), length(scope$visits) == visits,
              identical(serialize(scope$source, NULL), bytes))
    rm("::", envir = scope)
    restored <- if (is.null(query)) fun(input, rollup(g), FALSE) else force_result(query)
    stopifnot(identical(baseline, restored))
    records[[length(records) + 1L]] <- data.frame(
      verb = verb, expected = paste(unique(baseline$g[baseline$set == 2L]), collapse = "/"),
      observed = paste(unique(altered$g[altered$set == 2L]), collapse = "/"), visits = visits
    )
  }
  do.call(rbind, records)
}
mode <- args[[2L]]
out <- switch(mode, collision = check_collision(), display = check_display())
print(out, row.names = FALSE)
if (length(args) >= 3L && args[[3L]] == "contract") {
  stopifnot(all(out$visits == 0L))
} else if (mode == "collision") {
  stopifnot(any(out$observed == "admitted" & out$visits > 0L))
} else {
  stopifnot(any(out$expected != out$observed & out$visits > 0L))
}
