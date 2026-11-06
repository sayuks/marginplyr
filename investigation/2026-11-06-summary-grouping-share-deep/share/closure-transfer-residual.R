# Follow-up evidence for the already reported closure-transfer boundary #1361.
# Observation mode pins the residual; --assert requires the desired refusal.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
arguments <- commandArgs(trailingOnly = TRUE)
contract <- "--assert" %in% arguments
controls_only <- "--controls-only" %in% arguments
data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
inputs <- list(data.frame = data, tibble = tibble::as_tibble(data),
               data.table = data.table::as.data.table(data))
expressions <- alist(
  invisible_transfer = (function(prior) {
    read <- function() sum(prior)
    copied <- invisible(read)
    rm(prior)
    copied()
  })(1),
  assign_transfer = (function(prior) {
    original <- function() sum(prior)
    assign("copied", original)
    rm(prior)
    copied()
  })(1),
  direct_control = (function(prior) {
    read <- function() sum(prior)
    rm(prior)
    read()
  })(1),
  copy_control = (function(prior) {
    read <- function() sum(prior)
    copied <- read
    rm(prior)
    copied()
  })(1),
  identity_transfer = (function(prior) {
    read <- function() sum(prior)
    copied <- identity(read)
    rm(prior)
    copied()
  })(1),
  assign_after_control = (function(prior) {
    read <- function() sum(prior)
    rm(prior)
    assign("copied", read)
    copied()
  })(1),
  literal_list_control = (function(prior) {
    read <- function() sum(prior)
    functions <- list(read)
    rm(prior)
    functions[[1L]]()
  })(1),
  literal_factory_control = (function(prior) {
    read <- (function() function() sum(prior))()
    rm(prior)
    read()
  })(1),
  retained_invisible_control = (function(prior) {
    read <- function() sum(prior)
    copied <- invisible(read)
    copied()
  })(1),
  retained_assign_control = (function(prior) {
    read <- function() sum(prior)
    assign("copied", read)
    copied()
  })(1)
)
run <- function(input, expr, helper, direction) {
  share <- rlang::call2(helper, rlang::sym(if (direction == "forward") "source" else "observed"))
  tryCatch(if (direction == "forward") {
    summarize_with_margins(input, source = sum(x), prior = !!share, observed = !!expr,
                           .grouping = rollup(g), .sort = "last")
  } else {
    summarize_with_margins(input, prior = sum(x), observed = !!expr, ratio = !!share,
                           .grouping = rollup(g), .sort = "last")
  }, error = identity)
}
rows <- list()
cases <- names(expressions)
if (controls_only) cases <- cases[grepl("control$", cases)]
for (input_name in names(inputs)) for (helper in c("share_of_parent", "share_of_total")) {
  for (direction in c("forward", "reverse")) for (case in cases) {
    value <- run(inputs[[input_name]], expressions[[case]], helper, direction)
    legal <- startsWith(case, "retained_")
    witness <- !grepl("control$", case)
    refusal <- inherits(value, "marginplyr_error") && grepl(
      if (direction == "forward") "Ordinary summaries cannot use an earlier" else "depends on earlier summary alias",
      gsub("[[:space:]]+", " ", conditionMessage(value)), fixed = TRUE
    )
    if (legal) {
      stopifnot(!inherits(value, "condition"), all(value$observed == 1))
    } else if (contract || !witness) {
      stopifnot(refusal)
    } else {
      expected <- if (direction == "forward") c(100, 300, 400) else c(1, 3, 4)
      stopifnot(!inherits(value, "condition"), identical(as.double(value$observed), expected))
    }
    rows[[length(rows) + 1L]] <- data.frame(input = input_name, helper, direction, case,
      expected = if (legal) "valid" else "refusal", observed = if (refusal) "refusal" else "success")
  }
}
results <- do.call(rbind, rows)
print(with(results, table(expected, observed)))
cat("Comparisons:", nrow(results), "\n")
