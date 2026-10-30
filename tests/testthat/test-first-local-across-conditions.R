test_that("first across planning preserves naming and selector conditions", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  cause <- rlang::error_cnd("naming_cause", message = "naming cause")
  failure <- rlang::error_cnd(
    "naming_failure", message = "naming failure", parent = cause
  )
  for (fails in c(FALSE, TRUE)) {
    for (source in c(FALSE, TRUE)) {
      warnings <- list()
      reads <- 0L
      template <- function() {
        reads <<- reads + 1L
        warning("naming warning")
        if (fails) stop(failure)
        if (source) "total" else "unused"
      }
      choose <- function() {
        warning("selector warning")
        "x"
      }
      result <- tryCatch(withCallingHandlers(summarize_with_margins(
        data,
        dplyr::across(dplyr::all_of(choose()), sum, .names = template()),
        ordinary = sum(x),
        dplyr::across(
          dplyr::starts_with(if (source) "total" else "ordinary"),
          share_of_total, .names = "share"
        ),
        .grouping = rollup(g)
      ), warning = function(cnd) {
        warnings[[length(warnings) + 1L]] <<- cnd
        invokeRestart("muffleWarning")
      }), error = identity)
      expect_identical(reads, if (fails || source) 1L else 2L)
      expect_length(warnings, 2L)
      messages <- vapply(warnings, conditionMessage, character(1))
      expect_identical(sum(grepl("naming warning", messages)), 1L)
      expect_identical(sum(grepl("selector warning", messages)), 1L)
      if (fails) {
        expect_identical(result, failure)
        expect_identical(result$parent, cause)
        expect_identical(messages, c("selector warning", "naming warning"))
      } else {
        expect_equal(result$share, c(0.25, 0.75, 1))
        expect_identical(messages[[1L]], "naming warning")
        if (source) {
          expect_identical(messages[[2L]], "selector warning")
        } else {
          expect_match(messages[[2L]], "1 further grouping set")
        }
      }
    }
  }
})

test_that("nested selection detection unwraps injected expressions", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  selection <- rlang::quo(dplyr::across(x, sum))
  actual <- summarize_with_margins(
    data, identity(!!selection), .grouping = rollup(g)
  )
  expect_identical(actual$x, c(1, 3, 4))
  value <- rlang::quo(1)
  scalar <- summarize_with_margins(
    data, identity(!!value), .grouping = rollup(g)
  )
  expect_identical(scalar[["identity(1)"]], c(1, 1, 1))
})
