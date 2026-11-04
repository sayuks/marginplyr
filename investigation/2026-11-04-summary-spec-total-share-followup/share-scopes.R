# Run from the marginplyr source root. --assert requires the documented behavior.
pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
assert_contract <- "--assert" %in% commandArgs(trailingOnly = TRUE)

fixture <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
variants <- data.frame(
  family = c(rep("target", 6), rep("dots", 6), rep("callable", 7)),
  case = c(
    "immediate", "stack_pair", "literal_depth_two", "literal_inheritance",
    "numeric_inheritance", "local_target",
    "first", "second", "padded", "force", "named_formal", "not_demanded",
    "supplied_nonfunction", "default_nonfunction", "forced_nonfunction",
    "assigned_nonfunction", "supplied_closure", "default_closure",
    "assigned_closure"
  ),
  syntax = c(
    "(function(prior) { (function() rm('prior', envir = parent.frame()))(); sum(prior) })(1)",
    "(function(prior) { (function() rm('prior', envir = sys.frame(sys.parent())))(); sum(prior) })(1)",
    "(function(prior) { (function() (function() rm('prior', envir = parent.frame(2L)))())(); sum(prior) })(1)",
    "(function(prior) { (function() rm('prior', inherits = TRUE))(); sum(prior) })(1)",
    "(function(prior) { (function() rm('prior', inherits = 1L))(); sum(prior) })(1)",
    "(function(prior) { (function(prior) rm('prior'))(2); sum(prior) })(1)",
    "(function(prior) (function(...) { ..1; sum(prior) })({ rm('prior'); 1 }))(1)",
    "(function(prior) (function(...) { ..2; sum(prior) })(0, { rm('prior'); 1 }))(1)",
    "(function(prior) (function(...) { ..01; sum(prior) })({ rm('prior'); 1 }))(1)",
    "(function(prior) (function(...) { force(..1); sum(prior) })({ rm('prior'); 1 }))(1)",
    "(function(prior) (function(arg) { arg; sum(prior) })({ rm('prior'); 1 }))(1)",
    "(function(prior) (function(...) sum(prior))({ rm('prior'); 1 }))(1)",
    "(function(prior, f = { rm('prior'); identity }) { (function(f) f(0))(1); sum(prior) })(1)",
    "(function(prior, f = { rm('prior'); identity }) { (function(f = 1) f(0))(); sum(prior) })(1)",
    "(function(prior, f = { rm('prior'); identity }) { (function(f) { f; f(0) })(1); sum(prior) })(1)",
    "(function(prior, f = { rm('prior'); identity }) { (function() { f <- 1; f(0) })(); sum(prior) })(1)",
    "(function(prior, rm) { rm('prior'); sum(prior) })(1, function(...) NULL)",
    "(function(prior, rm = function(...) NULL) { rm('prior'); sum(prior) })(1)",
    "(function(prior) { rm <- function(...) NULL; rm('prior'); sum(prior) })(1)"
  ),
  should_refuse = c(
    TRUE, TRUE, TRUE, TRUE, TRUE, FALSE,
    TRUE, TRUE, TRUE, TRUE, TRUE, FALSE,
    TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE
  ),
  defect = c(
    FALSE, TRUE, TRUE, FALSE, TRUE, FALSE,
    TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
    TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE
  )
)

check_case <- function(data, expression, helper, direction, variant) {
  no_share <- summarize_with_margins(
    data, prior = sum(x), derived = !!expression, .grouping = rollup(g)
  )
  expect <- if (variant$should_refuse) c(1, 3, 4) else rep(1, 3)
  stopifnot(identical(no_share$derived, expect))
  result <- tryCatch(
    if (direction == "earlier_share") {
      summarize_with_margins(
        data, value = sum(x), prior = !!rlang::call2(helper, quote(value)),
        derived = !!expression, .grouping = rollup(g)
      )
    } else {
      summarize_with_margins(
        data, prior = sum(x), derived = !!expression,
        ratio = !!rlang::call2(helper, quote(derived)), .grouping = rollup(g)
      )
    },
    error = identity
  )
  refused <- inherits(result, "marginplyr_error")
  if (refused) {
    reason <- if (direction == "earlier_share") {
      "Ordinary summaries cannot use an earlier"
    } else {
      "depends on earlier summary alias"
    }
    stopifnot(grepl(reason, conditionMessage(result), fixed = TRUE))
  } else {
    stopifnot(!inherits(result, "error"))
    expect <- if (!variant$should_refuse) {
      rep(1, 3)
    } else if (direction == "earlier_share") {
      c(100, 300, 400)
    } else {
      c(1, 3, 4)
    }
    ratio <- if (direction == "earlier_share") result$prior else result$ratio
    expected_ratio <- if (direction == "earlier_share" ||
                          variant$should_refuse) {
      c(0.25, 0.75, 1)
    } else {
      rep(1, 3)
    }
    stopifnot(identical(result$derived, expect), identical(ratio, expected_ratio))
  }
  if (refused == variant$should_refuse) "agrees" else "defect"
}

outcomes <- character()
for (index in seq_len(nrow(variants))) {
  variant <- variants[index, ]
  expression <- parse(text = variant$syntax, keep.source = FALSE)[[1L]]
  for (input in list(fixture, tibble::as_tibble(fixture),
                     data.table::as.data.table(fixture))) {
    original <- serialize(input, NULL)
    for (helper in c("share_of_parent", "share_of_total")) {
      for (direction in c("earlier_share", "source")) {
        outcome <- check_case(input, expression, helper, direction, variant)
        stopifnot(identical(serialize(input, NULL), original))
        if (!assert_contract) {
          stopifnot(identical(outcome == "defect", variant$defect))
        }
        outcomes <- c(outcomes, outcome)
        cat(variant$family, variant$case, class(input)[[1L]], helper,
            direction, outcome, sep = ",")
        cat("\n")
      }
    }
  }
}
print(table(outcomes))
if (assert_contract) stopifnot(!any(outcomes == "defect"))
