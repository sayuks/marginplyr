test_that("across reads functions rebound by a preceding summary", {
  data <- data.frame(x = c(1L, 3L))
  fns <- sum

  actual <- summarize_with_margins(
    data,
    ready = {
      fns <<- list(s = sum, m = max)
      0L
    },
    dplyr::across(x, fns, .names = "answer_{.fn}"),
    .id = "answer_1"
  )

  expect_identical(actual, data.frame(
    answer_1 = 1L, ready = 0L, answer_s = 4L, answer_m = 3L
  ))
})

test_that("across reads unpacking rebound by a preceding summary", {
  unpack <- FALSE
  frame_fn <- function(x) data.frame(z = sum(x))

  actual <- summarize_with_margins(
    data.frame(x = c(1L, 3L)),
    ready = {
      unpack <<- TRUE
      0L
    },
    dplyr::across(x, frame_fn, .names = "answer", .unpack = unpack),
    .id = "answer"
  )

  expect_identical(actual, data.frame(answer = 1L, ready = 0L, answer_z = 4L))
})

prior_rebinding_dots <- function(entrance, literal = FALSE, reverse = FALSE,
                                 operator = "::") {
  caller <- rlang::env(
    fns = if (reverse) list(s = sum, m = max) else sum,
    list = base::list, unpack = FALSE,
    frame_fn = function(x) data.frame(z = sum(x))
  )
  caller[[operator]] <- get(operator, baseenv())
  ready <- switch(if (entrance == "namespace-list") "namespace" else entrance,
    binding = rlang::expr({
      fns <<- !!if (reverse) sum else list(s = sum, m = max)
      0L
    }),
    list = quote({
      list <<- function(...) base::list(s = sum, m = max)
      0L
    }),
    unpack = quote({
      unpack <<- TRUE
      0L
    }),
    namespace = rlang::expr({
      assign(!!operator, function(pkg, name) {
        package <- as.character(substitute(pkg))
        member <- as.character(substitute(name))
        if (package == "base" && member == "sum") {
          return(list(s = sum, m = max))
        }
        if (package == "base" && member == "list") {
          return(function(...) list(s = sum, m = max))
        }
        get(member, envir = asNamespace(package))
      }, envir = !!caller)
      0L
    })
  )
  fns <- switch(entrance,
    binding = quote(fns),
    list = quote(list(fake = sum)),
    unpack = quote(frame_fn),
    namespace = rlang::call2(operator, quote(base), quote(sum)),
    `namespace-list` = rlang::call2(
      rlang::call2(operator, quote(base), quote(list)), fake = quote(sum)
    )
  )
  if (literal && entrance != "unpack") {
    fns <- if (reverse) quote(sum) else quote(base::list(s = sum, m = max))
  }
  dot <- if (entrance == "unpack") {
    rlang::expr(dplyr::across(
      !!quote(x), !!fns, .names = "answer",
      .unpack = !!if (literal) TRUE else quote(unpack)
    ))
  } else {
    rlang::expr(dplyr::across(!!quote(x), !!fns, .names = "answer_{.fn}"))
  }
  list(ready = rlang::new_quosure(ready, caller),
       rlang::new_quosure(dot, caller))
}

test_that("across reads a namespace operation rebound by a preceding summary", {
  for (operator in c("::", ":::")) {
    for (entrance in c("namespace", "namespace-list")) {
      protected <- if (entrance == "namespace") "answer_1" else "answer_fake"
      actual <- summarize_with_margins(
        data.frame(x = c(1L, 3L)),
        !!!prior_rebinding_dots(entrance, operator = operator), .id = protected
      )
      expected <- data.frame(id = 1L, ready = 0L, answer_s = 4L, answer_m = 3L)
      names(expected)[[1L]] <- protected
      expect_identical(actual, expected)
    }
  }
})

for (input_kind in c("data.frame", "tibble", "data.table")) {
  test_that(paste("prior across rebindings preserve names on", input_kind), {
    if (input_kind == "data.table") skip_if_suggest_absent("data.table")
    entrances <- c("binding", "list", "namespace", "namespace-list", "unpack")
    for (entrance in entrances) {
      for (role in c("id", "fixed", "dimension")) {
        protected <- switch(entrance,
          list = "answer_fake", `namespace-list` = "answer_fake",
          unpack = "answer", "answer_1"
        )
        data <- data.frame(x = c(1L, 3L))
        if (role != "id") data[[protected]] <- c("a", "a")
        data <- switch(input_kind,
          tibble = tibble::as_tibble(data),
          data.table = data.table::as.data.table(data), data
        )
        before <- serialize(data, NULL)
        run <- function(dots) {
          switch(role,
            id = summarize_with_margins(data, !!!dots, .id = protected),
            fixed = summarize_with_margins(
              data, !!!dots, .by = dplyr::all_of(protected)
            ),
            dimension = summarize_with_margins(
              data, !!!dots, .grouping = grouping_set(dplyr::all_of(protected))
            )
          )
        }
        literal <- run(prior_rebinding_dots(entrance, literal = TRUE))
        actual <- run(prior_rebinding_dots(entrance))
        expect_identical(actual, literal)
        expect_identical(names(actual), c(
          protected, "ready",
          if (entrance == "unpack") "answer_z" else c("answer_s", "answer_m")
        ))
        expect_identical(actual$ready, 0L)
        if (entrance == "unpack") {
          expect_identical(actual$answer_z, 4L)
        } else {
          expect_identical(actual$answer_s, 4L)
          expect_identical(actual$answer_m, 3L)
        }
        expect_identical(serialize(data, NULL), before)

        collision <- if (entrance == "unpack") "answer_z" else "answer_s"
        if (role != "id") names(data)[names(data) == protected] <- collision
        protected <- collision
        before <- serialize(data, NULL)
        expect_error(run(prior_rebinding_dots(entrance)),
                     if (role == "id") "conflicts with a summary output" else
                       "cannot overwrite grouping column",
                     class = "marginplyr_error")
        expect_identical(serialize(data, NULL), before)
      }
    }
  })

  test_that(paste("reverse across rebinding preserves names on", input_kind), {
    if (input_kind == "data.table") skip_if_suggest_absent("data.table")
    for (role in c("id", "fixed", "dimension")) {
      data <- data.frame(x = c(1L, 3L))
      if (role != "id") data$answer_s <- c("a", "a")
      data <- switch(input_kind,
        tibble = tibble::as_tibble(data),
        data.table = data.table::as.data.table(data), data
      )
      before <- serialize(data, NULL)
      run <- function(dots) {
        switch(role,
          id = summarize_with_margins(data, !!!dots, .id = "answer_s"),
          fixed = summarize_with_margins(data, !!!dots, .by = answer_s),
          dimension = summarize_with_margins(
            data, !!!dots, .grouping = grouping_set(answer_s)
          )
        )
      }
      actual <- run(prior_rebinding_dots("binding", reverse = TRUE))
      expect_identical(actual, run(prior_rebinding_dots(
        "binding", literal = TRUE, reverse = TRUE
      )))
      expect_identical(names(actual), c("answer_s", "ready", "answer_1"))
      expect_identical(actual$ready, 0L)
      expect_identical(actual$answer_1, 4L)
      expect_identical(serialize(data, NULL), before)
    }
  })
}

test_that("prior rebindings retain ordinary function configuration timing", {
  run <- function(margin) {
    events <- character()
    fns <- sum
    configure <- function() {
      events <<- c(events, "configure")
      list(s = sum, m = max)
    }
    data <- data.frame(g = c("a", "a", "b"), x = c(1L, 3L, 2L))
    dots <- rlang::quos(
      ready = {
        events <<- c(events, "ready")
        fns <<- configure()
        0L
      },
      dplyr::across(x, fns, .names = "answer_{.fn}")
    )
    result <- if (margin) {
      summarize_with_margins(data, !!!dots, .by = g, .id = "answer_1")
    } else {
      dplyr::summarise(data, !!!dots, .by = g)
    }
    list(result = result, events = events)
  }
  ordinary <- run(FALSE)
  actual <- run(TRUE)
  expect_identical(actual$result[names(ordinary$result)], ordinary$result)
  expect_identical(actual$events, ordinary$events)
  expect_identical(actual$events, c("ready", "configure", "ready", "configure"))
})

test_that("unchanged across shapes still refuse collisions before evaluation", {
  for (entrance in c("binding", "list", "namespace", "unpack")) {
    dots <- prior_rebinding_dots(entrance)
    caller <- rlang::quo_get_env(dots[[1L]])
    caller$other <- FALSE
    dots[[1L]] <- rlang::new_quosure(quote({
      other <<- TRUE
      0L
    }), caller)
    collision <- switch(entrance,
      list = "answer_fake", unpack = "answer", "answer_1"
    )
    expect_error(summarize_with_margins(
      data.frame(x = c(1L, 3L)), !!!dots, .id = collision
    ), "conflicts with a summary output", class = "marginplyr_error")
    expect_false(caller$other)
  }
})
