pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
assert_contract <- "--assert" %in% args
scope <- setdiff(args, "--assert")
data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
external <- list2env(list(prior = 7), parent = emptyenv())

expressions <- list(
  parent_double_two = quote((function(prior) {
    (function() (function() rm("prior", envir = parent.frame(2)))())()
    sum(prior)
  })(1)),
  parent_integer_three = quote((function(prior) {
    (function() (function() (function() {
      rm("prior", envir = parent.frame(3L))
    })())())()
    sum(prior)
  })(1)),
  sys_parent_explicit_one = quote((function(prior) {
    (function() rm("prior", envir = sys.frame(sys.parent(1L))))()
    sum(prior)
  })(1)),
  sys_parent_explicit_two = quote((function(prior) {
    (function() (function() {
      rm("prior", envir = sys.frame(sys.parent(2L)))
    })())()
    sum(prior)
  })(1)),
  parent_integer_two = quote((function(prior) {
    (function() (function() rm("prior", envir = parent.frame(2L)))())()
    sum(prior)
  })(1)),
  sys_parent_default = quote((function(prior) {
    (function() rm("prior", envir = sys.frame(sys.parent())))()
    sum(prior)
  })(1)),
  closure_called_after_removal = quote((function(prior) {
    read_prior <- function() sum(prior)
    rm(prior)
    read_prior()
  })(1)),
  default_closure_called_after_removal = quote((function(
    prior, read_prior = function() sum(prior)
  ) {
    force(read_prior)
    rm(prior)
    read_prior()
  })(1)),
  closure_removes_binding = quote((function(prior) {
    remove_prior <- function() rm("prior", envir = parent.frame())
    remove_prior()
    sum(prior)
  })(1)),
  closure_defined_after_removal = quote((function(prior) {
    rm(prior)
    read_prior <- function() sum(prior)
    read_prior()
  })(1)),
  inline_removes_binding = quote((function(prior) {
    (function() rm("prior", envir = parent.frame()))()
    sum(prior)
  })(1)),
  closure_calls_before_removal = quote((function(prior) {
    read_prior <- function() sum(prior)
    observed <- read_prior()
    rm(prior)
    observed
  })(1)),
  mask_dollar = quote(sum(parent.env(environment())$prior)),
  mask_bracket = quote(sum(parent.env(environment())[["prior"]])),
  mask_env_get = quote(sum(rlang::env_get(
    rlang::current_env(), "prior", inherit = TRUE
  ))),
  mask_get = quote(sum(get("prior", envir = parent.env(environment())))),
  external_field = quote(sum(external$prior)),
  external_get = quote(sum(get("prior", envir = external)))
)
hypothesis <- c(rep("frames", 6), rep("closures", 6), rep("mask", 6))
bypassed <- c(rep(TRUE, 4), FALSE, FALSE, rep(TRUE, 3),
              rep(FALSE, 3), rep(TRUE, 3), rep(FALSE, 3))
independent <- c(rep(FALSE, 11), TRUE, rep(FALSE, 4), TRUE, TRUE)
independent_value <- c(rep(NA_real_, 11), 1, rep(NA_real_, 4), 7, 7)
design <- data.frame(
  name = names(expressions), hypothesis, bypassed, independent, independent_value
)
if (length(scope)) {
  stopifnot(length(scope) == 1L, scope %in% hypothesis)
  design <- design[design$hypothesis == scope, ]
}

inputs <- list(
  data.frame = data,
  tibble = tibble::as_tibble(data),
  data.table = data.table::as.data.table(data)
)
helpers <- list(Parent = quote(share_of_parent), Total = quote(share_of_total))
outcomes <- character()
for (input_name in names(inputs)) {
  input <- inputs[[input_name]]
  before <- serialize(as.data.frame(input), NULL)
  for (i in seq_len(nrow(design))) {
    row <- design[i, ]
    expr <- expressions[[row$name]]
    ordinary <- summarize_with_margins(
      input, observed = !!expr, .grouping = rollup(g), .sort = "last"
    )
    wanted <- if (row$independent) rep(row$independent_value, 3) else c(100, 300, 400)
    stopifnot(identical(ordinary$observed, wanted))
    for (helper_name in names(helpers)) {
      forward <- tryCatch(
        summarize_with_margins(
          input, source = sum(x), prior = !!call(as.character(helpers[[helper_name]]), quote(source)),
          observed = !!expr, .grouping = rollup(g), .sort = "last"
        ),
        error = identity
      )
      reverse <- tryCatch(
        summarize_with_margins(
          input, prior = sum(x), observed = !!expr,
          ratio = !!call(as.character(helpers[[helper_name]]), quote(observed)),
          .grouping = rollup(g), .sort = "last"
        ),
        error = identity
      )
      expected_ratio <- c(0.25, 0.75, 1)
      if (row$bypassed && !assert_contract) {
        outcomes <- c(outcomes, paste(input_name, helper_name, row$name, "bypassed 2"))
        stopifnot(
          identical(forward$prior, expected_ratio),
          identical(forward$observed, c(100, 300, 400)),
          identical(reverse$observed, c(1, 3, 4)),
          identical(reverse$ratio, expected_ratio)
        )
        next
      }
      if (row$independent) {
        stopifnot(
          identical(forward$observed, wanted),
          identical(reverse$observed, wanted),
          identical(reverse$ratio, rep(1, 3))
        )
        outcomes <- c(outcomes, paste(input_name, helper_name, row$name, "accepted 2"))
      } else {
        stopifnot(
          inherits(forward, "marginplyr_error"),
          grepl("cannot use an earlier", conditionMessage(forward), fixed = TRUE),
          inherits(reverse, "marginplyr_error"),
          grepl("depends on earlier summary alias", conditionMessage(reverse), fixed = TRUE)
        )
        outcomes <- c(outcomes, paste(input_name, helper_name, row$name, "refused 2"))
      }
    }
  }
  stopifnot(identical(before, serialize(as.data.frame(input), NULL)))
}
cat(outcomes, sep = "\n")
