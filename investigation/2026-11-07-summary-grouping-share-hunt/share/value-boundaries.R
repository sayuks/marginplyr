# Dated public-interface research harness; no implementation repair.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
family <- if (length(args) && !startsWith(args[[1L]], "--")) args[[1L]] else "all"
contract <- "--assert" %in% args
controls_only <- "--controls-only" %in% args
out_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))))
case <- function(expr, family, disposition = "witness", expected = NULL) {
  list(expr = expr, family = family, disposition = disposition, expected = expected)
}
listflow <- alist(
  concatenate = (function(prior) { fs <- c(list(function() sum(prior))); rm(prior); fs[[1L]]() })(1),
  append = (function(prior) { fs <- append(list(), list(function() sum(prior))); rm(prior); fs[[1L]]() })(1),
  subset = (function(prior) { fs <- list(function() sum(prior)); copied <- fs[1L]; rm(prior); copied[[1L]]() })(1)
)
listwrite <- alist(
  delete_index = (function(prior) { fs <- list(function() 7, function() sum(prior)); fs[[1L]] <- NULL; rm(prior); fs[[1L]]() })(1),
  delete_dollar = (function(prior) { fs <- list(drop = function() 7, read = function() sum(prior)); fs$drop <- NULL; rm(prior); fs[[1L]]() })(1),
  bracket_replace = (function(prior) { fs <- list(function() 7); fs[1L] <- list(function() sum(prior)); rm(prior); fs[[1L]]() })(1)
)
partial <- alist(
  partial_dollar = (function(prior) { fs <- list(reader = function() sum(prior)); rm(prior); fs$read() })(1),
  partial_bracket = (function(prior) { fs <- list(reader = function() sum(prior)); rm(prior); fs[["read", exact = FALSE]]() })(1)
)
environment_values <- alist(
  environment_block = { mask <- { parent.env(environment()) }; sum(mask$prior) },
  environment_if = { mask <- if (TRUE) parent.env(environment()) else globalenv(); sum(mask$prior) },
  environment_switch = { mask <- switch(1L, parent.env(environment())); sum(mask$prior) },
  environment_identity = { mask <- identity(parent.env(environment())); sum(mask$prior) }
)
deferred <- alist(
  delayed_closure = (function(prior) { delayedAssign("copied", function() sum(prior)); rm(prior); copied() })(1),
  qualified_delayed_closure = (function(prior) { base::delayedAssign("copied", function() sum(prior)); rm(prior); copied() })(1)
)
families <- list(listflow = listflow, listwrite = listwrite, partial = partial,
                 environment = environment_values, deferred = deferred)
stopifnot(family %in% c("all", names(families)))
if (family != "all") families <- families[family]
cases <- list()
for (fam in names(families)) {
  for (nm in names(families[[fam]])) cases[[nm]] <- case(families[[fam]][[nm]], fam)
}
without_removal <- function(expr) {
  if (!is.call(expr)) return(expr)
  if (identical(expr[[1L]], quote(rm))) return(quote(invisible(NULL)))
  as.call(lapply(as.list(expr), without_removal))
}
for (nm in names(cases)) {
  item <- cases[[nm]]
  if (item$family != "environment") {
    cases[[paste0(nm, "_retained")]] <- case(without_removal(item$expr), item$family, "legal", c(1, 1, 1))
  }
}
external <- new.env(parent = emptyenv())
external$prior <- 7
replace_mask <- function(expr) {
  if (identical(expr, quote(parent.env(environment())))) return(quote(external))
  if (!is.call(expr)) return(expr)
  as.call(lapply(as.list(expr), replace_mask))
}
if (family %in% c("all", "environment")) {
  for (nm in names(environment_values)) cases[[paste0(nm, "_external")]] <-
    case(replace_mask(environment_values[[nm]]), "environment", "legal", c(7, 7, 7))
}
refusals <- alist(
  direct_closure = (function(prior) { read <- function() sum(prior); rm(prior); read() })(1),
  literal_list = (function(prior) { fs <- list(function() sum(prior)); rm(prior); fs[[1L]]() })(1),
  exact_dollar = (function(prior) { fs <- list(reader = function() sum(prior)); rm(prior); fs$reader() })(1),
  element_replace = (function(prior) { fs <- list(function() 7); fs[[1L]] <- function() sum(prior); rm(prior); fs[[1L]]() })(1),
  literal_assign = (function(prior) { assign("copied", function() sum(prior)); rm(prior); copied() })(1),
  mask_alias = { mask <- parent.env(environment()); sum(mask$prior) },
  current_alias = { mask <- environment(); rlang::env_get(mask, "prior", inherit = TRUE) }
)
for (nm in names(refusals)) cases[[nm]] <- case(refusals[[nm]], "control", "refusal")
if (controls_only) cases <- Filter(function(x) x$disposition != "witness", cases)
input <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
inputs <- list(data.frame = input, tibble = tibble::as_tibble(input), data.table = data.table::as.data.table(input))
rows <- list()
for (input_name in names(inputs)) {
  data <- inputs[[input_name]]
  before <- serialize(data, NULL)
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(source))
    for (direction in c("forward", "reverse")) {
      for (nm in names(cases)) {
        item <- cases[[nm]]
        result <- tryCatch(if (direction == "forward") {
          summarize_with_margins(data, source = sum(x), prior = !!helper, observed = !!item$expr, .grouping = rollup(g))
        } else {
          summarize_with_margins(data, prior = sum(x), source = !!item$expr, ratio = !!helper, .grouping = rollup(g))
        }, error = identity)
        refused <- inherits(result, "marginplyr_error") && grepl(
          if (direction == "forward") "Ordinary summaries cannot use an earlier" else "depends on earlier summary alias",
          conditionMessage(result), fixed = TRUE
        )
        label <- paste(input_name, kind, direction, nm)
        if (item$disposition == "refusal" || (item$disposition == "witness" && contract)) {
          if (!refused) stop(paste("Missing required dependency refusal:", label))
        } else {
          if (inherits(result, "error")) stop(paste(label, conditionMessage(result)))
          observed <- as.numeric(if (direction == "forward") result$observed else result$source)
          expected <- if (item$disposition == "legal") item$expected else if (direction == "forward") c(100, 300, 400) else c(1, 3, 4)
          if (!identical(observed, expected)) stop(paste(label, "observed", paste(observed, collapse = ","), "expected", paste(expected, collapse = ",")))
        }
        rows[[length(rows) + 1L]] <- data.frame(input = input_name, kind = kind, direction = direction,
          case = nm, family = item$family, disposition = item$disposition, refused = refused)
      }
    }
  }
  stopifnot(identical(serialize(data, NULL), before))
  cat("Completed", input_name, "\n")
}
results <- do.call(rbind, rows)
print(with(results, table(disposition, input)))
cat("Comparisons:", nrow(results), "\n")
utils::write.csv(results, file.path(out_dir, paste0("value-boundaries-", family, if (controls_only) "-controls" else "", "-results.csv")), row.names = FALSE)
versions <- c(R = as.character(getRversion()), vapply(c("pkgload", "dplyr", "rlang", "tibble", "data.table"), function(x) as.character(utils::packageVersion(x)), character(1)))
writeLines(c(capture.output(print(versions)), capture.output(sessionInfo())), file.path(out_dir, "environment.txt"))
