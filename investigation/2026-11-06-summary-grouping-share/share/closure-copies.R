# Run from the archived package root; --assert requires the desired refusal.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
contract <- "--assert" %in% commandArgs(trailingOnly = TRUE)
data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
inputs <- list(data.frame = data, tibble = tibble::as_tibble(data),
               data.table = data.table::as.data.table(data))
witnesses <- alist(
  one_copy = (function(prior) {
    read <- function() sum(prior)
    copied <- read
    rm(prior)
    copied()
  })(1),
  two_copies = (function(prior) {
    read <- function() sum(prior)
    copied <- read
    again <- copied
    rm(prior)
    again()
  })(1)
)
controls <- alist(
  direct = (function(prior) {
    read <- function() sum(prior)
    rm(prior)
    read()
  })(1),
  copy_after_removal = (function(prior) {
    read <- function() sum(prior)
    rm(prior)
    copied <- read
    copied()
  })(1),
  retained_binding = (function(prior) {
    read <- function() sum(prior)
    copied <- read
    copied()
  })(1)
)
run <- function(input, expr, helper, direction) {
  share <- rlang::call2(helper, rlang::sym(if (direction == "forward") {
    "source"
  } else {
    "observed"
  }))
  tryCatch(if (direction == "forward") {
    rlang::inject(summarize_with_margins(
      input, source = sum(x), prior = !!share, observed = !!expr,
      .grouping = rollup(g), .sort = "last"
    ))
  } else {
    rlang::inject(summarize_with_margins(
      input, prior = sum(x), observed = !!expr, ratio = !!share,
      .grouping = rollup(g), .sort = "last"
    ))
  }, error = identity)
}
rows <- list()
for (input_name in names(inputs)) {
  for (helper in c("share_of_total", "share_of_parent")) {
    for (direction in c("forward", "reverse")) {
      for (case in c(names(witnesses), names(controls))) {
        expr <- c(witnesses, controls)[[case]]
        value <- run(inputs[[input_name]], expr, helper, direction)
        error <- inherits(value, "marginplyr_error")
        if (error) {
          fragment <- if (direction == "forward") {
            "Ordinary summaries cannot use an earlier"
          } else {
            "depends on earlier summary alias"
          }
          message <- gsub("[[:space:]]+", " ", conditionMessage(value))
          error <- grepl(fragment, message, fixed = TRUE)
        }
        witness <- case %in% names(witnesses)
        should_refuse <- witness || case != "retained_binding"
        if (contract && should_refuse) {
          stopifnot(error)
        } else if (witness) {
          stopifnot(!inherits(value, "condition"))
          expected <- if (direction == "forward") c(100, 300, 400) else c(1, 3, 4)
          stopifnot(identical(as.double(value$observed), expected))
        } else if (should_refuse) {
          stopifnot(error)
        } else {
          stopifnot(!inherits(value, "condition"), all(value$observed == 1))
        }
        rows[[length(rows) + 1L]] <- data.frame(
          input = input_name, helper = helper, direction = direction,
          case = case, expected = if (should_refuse) "refusal" else "valid",
          observed = if (error) "refusal" else "success"
        )
      }
    }
  }
}
results <- do.call(rbind, rows)
print(results, row.names = FALSE)
print(with(results, table(expected, observed)))
cat("\nMinimal forward witness:\n")
print(run(inputs[[1L]], witnesses[[1L]], "share_of_total", "forward"))
cat("\nEnvironment:\n")
cat(R.version.string, "\n", R.version$platform, "\n")
for (package in c("dplyr", "rlang", "pkgload", "tibble", "data.table")) {
  cat(package, as.character(utils::packageVersion(package)), "\n")
}
