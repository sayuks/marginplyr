# jarl-ignore unnecessary_parentheses: Repeated pairs are syntax under test.
test_that("ordinary across keeps the written dplyr evaluation context", {
  data <- tibble::tibble(g = c("a", "b", "b"), x = c(1, 10, 20))
  expressions <- alist(
    across = across(x, factory()),
    head = (across)(x, factory()),
    call = (across(x, factory())),
    repeated_head = ((across))(x, factory()),
    repeated_call = ((across(x, factory()))),
    both = (((across))(x, factory())),
    qualified_head = (dplyr::across)(x, factory()),
    qualified_call = ((dplyr::across(x, factory()))),
    identity = identity(across(x, factory()))
  )
  for (name in names(expressions)) {
    observed <- new.env(parent = emptyenv())
    observed$sizes <- integer()
    factory <- function() {
      offset <- dplyr::n()
      observed$sizes <- c(observed$sizes, offset)
      function(x) sum(x) + offset
    }
    across <- dplyr::across
    expression <- expressions[[name]]
    expected <- dplyr::summarise(data, !!expression, .by = g)
    expected_observed <- observed$sizes
    observed$sizes <- integer()
    across <- function(...) stop("caller binding ran")
    actual <- summarize_with_margins(
      data, !!expression, .grouping = grouping_set(g)
    )
    expect_equal(actual, expected, info = name)
    expect_identical(observed$sizes, expected_observed, info = name)
    expect_equal(
      actual$x, if (name == "across") c(1, 30) else c(2, 32), info = name
    )
  }
})
