# Dated research harness; not a package regression test or a repair.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) && !startsWith(args[[1L]], "--")) args[[1L]] else "all"
stopifnot(mode %in% c("all", "syntax", "transfer", "mutation"))
require_contract <- "--assert" %in% args
controls_only <- "--controls-only" %in% args
out_dir <- sub("/[^/]+$", "", normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))))
make_case <- function(expr, expected, family) list(expr = expr, expected = expected, family = family)
witnesses <- alist(
  block_literal = (function(prior) {
    copied <- { function() sum(prior) }
    rm(prior)
    copied()
  })(1),
  if_literal = (function(prior) {
    copied <- if (TRUE) function() sum(prior)
    rm(prior)
    copied()
  })(1),
  switch_literal = (function(prior) {
    copied <- switch(1L, function() sum(prior))
    rm(prior)
    copied()
  })(1),
  explicit_return_factory = (function(prior) {
    copied <- (function() return(function() sum(prior)))()
    rm(prior)
    copied()
  })(1),
  multistatement_factory = (function(prior) {
    copied <- (function() { invisible(NULL); function() sum(prior) })()
    rm(prior)
    copied()
  })(1),
  named_factory = (function(prior) {
    factory <- function() function() sum(prior)
    copied <- factory()
    rm(prior)
    copied()
  })(1),
  force_value = (function(prior) {
    read <- function() sum(prior)
    copied <- force(read)
    rm(prior)
    copied()
  })(1),
  get_value = (function(prior) {
    read <- function() sum(prior)
    copied <- get("read")
    rm(prior)
    copied()
  })(1),
  match_fun_value = (function(prior) {
    read <- function() sum(prior)
    copied <- match.fun(read)
    rm(prior)
    copied()
  })(1),
  list_append = (function(prior) {
    carrier <- list()
    carrier[[1L]] <- function() sum(prior)
    rm(prior)
    carrier[[1L]]()
  })(1),
  list_replace = (function(prior) {
    carrier <- list(function() 7)
    carrier[[1L]] <- function() sum(prior)
    rm(prior)
    carrier[[1L]]()
  })(1),
  list_dollar = (function(prior) {
    carrier <- list()
    carrier$read <- function() sum(prior)
    rm(prior)
    carrier$read()
  })(1)
)
families <- c(rep("syntax", 6L), rep("transfer", 3L), rep("mutation", 3L))
cases <- Map(make_case, witnesses, "missing-refusal", families)
refusals <- alist(
  direct = (function(prior) {
    read <- function() sum(prior)
    rm(prior)
    read()
  })(1),
  bare_copy = (function(prior) {
    read <- function() sum(prior)
    copied <- read
    rm(prior)
    copied()
  })(1),
  identity_copy = (function(prior) {
    read <- function() sum(prior)
    copied <- identity(read)
    rm(prior)
    copied()
  })(1),
  literal_factory = (function(prior) {
    copied <- (function() function() sum(prior))()
    rm(prior)
    copied()
  })(1),
  literal_list = (function(prior) {
    carrier <- list(function() sum(prior))
    rm(prior)
    carrier[[1L]]()
  })(1),
  removal_first = (function(prior) {
    rm(prior)
    copied <- { function() sum(prior) }
    copied()
  })(1)
)
# Every witness has a legal twin retaining the local binding. Removing only
# rm(prior) keeps the operation independent of any earlier alias/share.
without_removal <- function(expr) {
  if (!is.call(expr)) return(expr)
  if (identical(expr[[1L]], quote(rm))) return(quote(invisible(NULL)))
  as.call(lapply(as.list(expr), without_removal))
}
legal <- lapply(witnesses, without_removal)
names(legal) <- paste0(names(legal), "_retained")
cases <- c(cases, Map(make_case, refusals, "refusal", "control"),
           Map(make_case, legal, "legal", families))
if (mode != "all") cases <- Filter(function(x) x$family %in% c(mode, "control"), cases)
if (controls_only) cases <- Filter(function(x) x$expected != "missing-refusal", cases)
d <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
inputs <- list(data.frame = d, tibble = tibble::as_tibble(d),
               data.table = data.table::as.data.table(d), dtplyr = dtplyr::lazy_dt(d))
rows <- list()
for (input_name in names(inputs)) {
  data <- inputs[[input_name]]
  before <- serialize(data, NULL)
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(source))
    for (direction in c("forward", "reverse")) {
      for (nm in names(cases)) {
        case <- cases[[nm]]
        result <- tryCatch({
          value <- if (direction == "forward") {
            summarize_with_margins(data, source = sum(x), prior = !!helper,
                                   observed = !!case$expr, .grouping = rollup(g))
          } else {
            summarize_with_margins(data, prior = sum(x), source = !!case$expr,
                                   ratio = !!helper, .grouping = rollup(g))
          }
          if (input_name == "dtplyr") value <- dplyr::collect(value)
          value
        }, error = identity)
        refused <- inherits(result, "marginplyr_error") && grepl(
          if (direction == "forward") "Ordinary summaries cannot use an earlier" else "depends on earlier summary alias",
          conditionMessage(result), fixed = TRUE
        )
        # The RHS short-circuits for successful calls; never ask conditionMessage
        # of an ordinary result.
        if (case$expected == "refusal") {
          stopifnot(refused)
        } else if (case$expected == "legal") {
          stopifnot(!inherits(result, "error"))
          stopifnot(identical(as.numeric(if (direction == "forward") result$observed else result$source), c(1, 1, 1)))
        } else if (require_contract) {
          stopifnot(refused)
        } else {
          stopifnot(!inherits(result, "error"))
          expected <- if (direction == "forward" || input_name == "dtplyr") c(100, 300, 400) else c(1, 3, 4)
          observed <- if (direction == "forward") result$observed else result$source
          if (!identical(as.numeric(observed), expected)) stop(paste(input_name, kind, direction, nm, paste(observed, collapse = ","), "expected", paste(expected, collapse = ",")))
        }
        rows[[length(rows) + 1L]] <- data.frame(input = input_name, kind = kind,
          direction = direction, case = nm, expected = case$expected, refused = refused)
      }
    }
  }
  stopifnot(identical(serialize(data, NULL), before)); cat("Completed", input_name, "\n")
}
results <- do.call(rbind, rows)
print(with(results, table(expected, input)))
cat("Comparisons:", nrow(results), "\n")
if (!require_contract && !controls_only) {
  write.csv(results, file.path(out_dir, paste0("closure-values-", mode, "-results.csv")), row.names = FALSE)
}
