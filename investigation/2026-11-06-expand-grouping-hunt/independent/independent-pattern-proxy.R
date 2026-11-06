Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)

# An inherited proxy method and a regex modifier challenge the fixed-pattern witness.
input <- tibble::tibble(region = c("east", "west"), value = c(17L, 23L))
pattern <- stringr::regex("^region$")
observations <- character()
vec_proxy.stringr_pattern <- function(x, ...) {
  type <- typeof(tidyselect::peek_data()$region)
  observations <<- c(observations, type)
  if (type != "character") stop("proxy requires typed region metadata")
  unclass(x)
}
direct <- tidyselect::eval_select(rlang::quo(matches(pattern)), input)
stopifnot(identical(names(direct), "region"), identical(observations, "character"))
observations <- character()
wrapped <- expand_with_margins(input, .grouping = rollup(matches(identity(pattern))),
                              .margin_label = NULL, .id = "sid")
stopifnot(nrow(wrapped) == 4L, identical(observations, "character"))
observations <- character()
actual <- tryCatch(expand_with_margins(input, .grouping = rollup(matches(pattern)),
                                     .margin_label = NULL, .id = "sid"), error = identity)
print(list(typed_reference = direct, wrapped_control = wrapped,
           observed = if (inherits(actual, "error")) conditionMessage(actual) else actual,
           proxy_contexts = observations))
stopifnot(identical(observations, "integer"))
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(!inherits(actual, "error"), identical(actual, wrapped))
}
