test_that("a literal removal keeps a special input measure", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only one optional backend is required.
  input <- data.table::data.table(f = c("P", "P"), g = c("a", "b"),
                                  .BY = c(1, 3))
  source <- dtplyr::lazy_dt(input)
  summarize <- function(read) {
    summarize_with_margins(
      source, value = sum(.BY), fraction = share_of_parent(value),
      observed = !!read, .by = f, .grouping = rollup(g)
    )
  }
  kept <- dplyr::collect(summarize(quote((function() {
    other <- 1
    rm("other")
    mean(.BY)
  })())))
  expect_identical(kept$observed, c(1, 3, 2))
  expect_identical(kept$fraction, c(0.25, 0.75, 1))
  # A name computed at run time can remove the measure itself.
  for (read in alist(
    (function() {
      other <- 1
      name <- "other"
      rm(list = name)
      mean(.BY)
    })(),
    (function() {
      other <- 1
      rm(paste0("other"))
      mean(.BY)
    })()
  )) {
    expect_error(
      summarize(read),
      "cannot safely distinguish input and lexical `.BY` reads",
      class = "marginplyr_error"
    )
  }
})

test_that("a literal list removal of another binding keeps the measure", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only one optional backend is required.
  input <- data.frame(f = c("P", "P"), g = c("a", "b"), .BY = c(1, 3))
  summarize <- function(source, read) {
    summarize_with_margins(
      source, value = sum(.BY), fraction = share_of_parent(value),
      observed = !!read, .by = f, .grouping = rollup(g)
    )
  }
  removal <- function(removed) {
    rlang::expr((function() {
      other <- 1
      rm(list = !!removed)
      mean(.BY)
    })())
  }
  # `rm()` names `.N` without removing a binding, so it only warns.
  for (removed in alist("other", c("other"), c(".N"))) {
    read <- removal(removed)
    expected <- suppressWarnings(summarize(input, read))
    expect_identical(
      suppressWarnings(as.data.frame(dplyr::collect(summarize(
        dtplyr::lazy_dt(data.table::as.data.table(input)), read
      )))),
      expected
    )
  }
  for (removed in alist(".BY", c("other", ".BY"))) {
    expect_error(
      summarize(dtplyr::lazy_dt(data.table::as.data.table(input)),
                removal(removed)),
      "cannot safely distinguish input and lexical `.BY` reads",
      class = "marginplyr_error"
    )
  }
})

test_that("a list concatenation is unreadable where `c` may be rebound", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only one optional backend is required.
  input <- data.table::data.table(f = c("P", "P"), g = c("a", "b"),
                                  .BY = c(1, 3))
  summarize <- function(read) {
    summarize_with_margins(
      dtplyr::lazy_dt(input), value = sum(.BY),
      fraction = share_of_parent(value), observed = !!read,
      .by = f, .grouping = rollup(g)
    )
  }
  # Each rebound `c` returns ".BY", so the removal or assignment reaches the
  # measure while the spelling names another binding.
  for (read in c(
    paste0("(function(.BY = 5) {c <- function(...) \".BY\"; ",
           "rm(list = c(\"x\")); mean(.BY)})()"),
    paste0("(function(.BY = 5, c = function(...) \".BY\") ",
           "{rm(list = c(\"x\")); mean(.BY)})()"),
    paste0("(function() {c <- function(...) \".BY\"; ",
           "assign(c(\"x\"), 5); mean(.BY)})()"),
    "(function(c = function(...) \".BY\") {assign(c(\"x\"), 5); mean(.BY)})()"
  )) {
    expect_error(
      summarize(rlang::parse_expr(read)),
      "cannot safely distinguish input and lexical `.BY` reads",
      class = "marginplyr_error"
    )
  }
  # The caller's environment is where dtplyr looks `c` up.
  rebound <- local({
    c <- function(...) ".BY"
    function(read) {
      summarize_with_margins(
        dtplyr::lazy_dt(input), value = sum(.BY),
        fraction = share_of_parent(value),
        observed = !!rlang::parse_expr(read), .by = f, .grouping = rollup(g)
      )
    }
  })
  expect_error(
    rebound("(function(.BY = 5) {rm(list = c(\"x\")); mean(.BY)})()"),
    "cannot safely distinguish input and lexical `.BY` reads",
    class = "marginplyr_error"
  )
  # A namespaced `base::c()` is concatenation whatever `c` names.
  kept <- dplyr::collect(rebound(
    "(function() {other <- 1; rm(list = base::c(\"other\")); mean(.BY)})()"
  ))
  expect_identical(kept$observed, c(1, 3, 2))
})

test_that("a removal beside a formal measure exposes the input frame", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only one optional backend is required.
  input <- data.table::data.table(f = c("P", "P"), g = c("a", "b"),
                                  .BY = c(1, 3))
  summarize <- function(source, read) {
    summarize_with_margins(
      source, value = sum(.BY), fraction = share_of_parent(value),
      observed = !!rlang::parse_expr(read), .by = f, .grouping = rollup(g)
    )
  }
  # The formal owns `.BY` while the body assigns it or removes another name.
  kept <- dplyr::collect(summarize(dtplyr::lazy_dt(input), paste0(
    "(function(.BY = 5) {other <- 1; rm(\"other\"); .BY <- .BY + 1; ",
    "mean(.BY)})()"
  )))
  expect_identical(kept$observed, c(6, 6, 6))
  # Once the formal is removed, dtplyr's enclosing `.BY` is data.table's
  # grouping value, while a local input reads the column.
  removals <- c(
    "(function(.BY = 5) {rm(\".BY\"); mean(.BY)})()",
    "(function(.BY = 5) {rm(list = c(\".BY\")); mean(.BY)})()"
  )
  expect_identical(summarize(input, removals[[1L]])$observed, c(1, 3, 2))
  for (read in removals) {
    expect_error(
      summarize(dtplyr::lazy_dt(input), read),
      "cannot safely distinguish input and lexical `.BY` reads",
      class = "marginplyr_error"
    )
  }
})
