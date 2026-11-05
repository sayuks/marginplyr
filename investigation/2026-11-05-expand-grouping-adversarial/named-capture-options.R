# Run from the tested package root. Add --assert to assert desired behavior.
pkgload::load_all(quiet = TRUE)
input <- data.frame(a = 1L)
constructors <- c("grouping_set", "grouping_sets", "grouping_spec", "rollup", "cube")
controls <- setdiff(names(formals(rlang::enquos)), "...")
options <- c(controls, ".unknown")
expect_contract <- "--assert" %in% commandArgs(TRUE)
checks <- list()
run <- function(constructor, option, nested) {
  attempts <- 0L
  sentinel <- function() {
    attempts <<- attempts + 1L
    stop("named value evaluated")
  }
  call <- as.call(c(list(as.name(constructor), quote(a)),
                    stats::setNames(list(quote(sentinel())), option)))
  if (nested) call <- call("grouping_sets", call)
  outcome <- tryCatch(eval(call("inspect_grouping", quote(input), .grouping = call)),
                      error = identity)
  stopifnot(inherits(outcome, "error"))
  package_condition <- inherits(outcome, "marginplyr_error") &&
    grepl("takes no named arguments", conditionMessage(outcome), fixed = TRUE) &&
    grepl(option, conditionMessage(outcome), fixed = TRUE)
  actual <- if (attempts == 1L && identical(conditionMessage(outcome), "named value evaluated")) {
    "value forced"
  } else if (attempts == 0L && package_condition) {
    "tag refused"
  } else {
    stop(conditionMessage(outcome))
  }
  if (expect_contract) stopifnot(identical(actual, "tag refused"))
  data.frame(constructor, option, nested, actual)
}
for (constructor in constructors) {
  for (option in options) {
    for (nested in c(FALSE, TRUE)) {
      checks[[length(checks) + 1L]] <- run(constructor, option, nested)
    }
  }
}
checks <- do.call(rbind, checks)
print(with(checks, table(option, actual)))
expected <- function(expr) tryCatch(inspect_grouping(input, .grouping = expr), error = identity)
tagged <- expected(grouping_set(a, .named = TRUE))
stopifnot(inherits(tagged, "marginplyr_error"))
cat(conditionMessage(tagged), "\n")
# Parse the deliberately empty arguments to preserve their exact R spelling.
empty <- eval(str2lang('expected(rollup(, a, .ignore_empty = "all"))'))
stopifnot(inherits(empty, "marginplyr_error"))
cat(conditionMessage(empty), "\n")
trailing <- eval(str2lang("inspect_grouping(input, .grouping = rollup(a, ))"))
stopifnot(identical(trailing$included, c("(a)", "()")))

attempts <- 0L
sentinel <- function() {
  attempts <<- attempts + 1L
  stop("named value evaluated")
}
dynamic <- expected(grouping_set(a, ".ignore_empty" := sentinel()))
spliced <- expected(grouping_set(a, !!!list(.ignore_empty = quote(sentinel()))))
for (condition in list(dynamic, spliced)) {
  stopifnot(inherits(condition, "marginplyr_error"),
            grepl("takes no named arguments", conditionMessage(condition),
                  fixed = TRUE),
            grepl(".ignore_empty", conditionMessage(condition), fixed = TRUE))
}
stopifnot(attempts == 0L)
