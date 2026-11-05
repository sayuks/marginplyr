pkgload::load_all(".", quiet = TRUE)
assert_contract <- "--assert" %in% commandArgs(trailingOnly = TRUE)
data <- data.frame(g = c("a", "b"), x = c(1, 3))
null <- NULL
inputs <- list(
  data.frame = data,
  tibble = tibble::as_tibble(data),
  data.table = data.table::as.data.table(data),
  dtplyr = dtplyr::lazy_dt(data)
)
helpers <- list(Parent = quote(share_of_parent), Total = quote(share_of_total))
forms <- list(
  literal_null = list(.unpack = NULL),
  injected_null = rlang::inject(list(.unpack = !!null)),
  parenthesized_null = list(.unpack = quote((NULL))),
  bound_null = list(.unpack = quote(null)),
  false = list(.unpack = FALSE),
  true = list(.unpack = TRUE),
  omitted = list()
)
for (input_name in names(inputs)) {
  input <- inputs[[input_name]]
  for (helper_name in names(helpers)) {
    for (form in names(forms)) {
      expr <- rlang::call2(
        "across", quote(value), helpers[[helper_name]],
        .names = "{.col}_share", !!!forms[[form]]
      )
      ans <- tryCatch(
        summarize_with_margins(
          input, value = sum(x), !!expr, .grouping = rollup(g), .sort = "last"
        ),
        error = identity
      )
      legal <- form %in% c("false", "omitted")
      bypass <- form %in% c("literal_null", "injected_null")
      if (legal || (bypass && !assert_contract)) {
        stopifnot(!inherits(ans, "error"))
        if (inherits(ans, "dtplyr_step")) ans <- dplyr::collect(ans)
        stopifnot(identical(ans$value_share, c(0.25, 0.75, 1)))
        outcome <- if (bypass) "bypassed" else "accepted"
      } else {
        stopifnot(
          inherits(ans, "marginplyr_error"),
          grepl("requires `.unpack = FALSE`", conditionMessage(ans), fixed = TRUE)
        )
        outcome <- "refused"
      }
      cat(input_name, helper_name, form, outcome, "\n")
    }
  }
}
