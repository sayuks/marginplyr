for (frame_class in c("data.frame", "tibble", "data.table")) {
  test_that(paste("runtime selections work on", frame_class), {
    if (identical(frame_class, "data.table")) {
      skip_if_suggest_absent("data.table")
    }
    data <- data.frame(
      fixed = rep(c("p", "q"), each = 3), g = rep(c("a", "a", "b"), 2),
      x = 1:6, y = as.double(5:10)
    )
    data <- switch(
      frame_class,
      data.frame = data,
      tibble = tibble::as_tibble(data),
      data.table = data.table::as.data.table(data)
    )
    selectors <- rlang::exprs(
      dplyr::across(dplyr::all_of(cols), sum),
      as.data.frame(lapply(dplyr::pick(dplyr::all_of(cols)), sum)),
      sum(dplyr::if_any(dplyr::all_of(cols), function(x) x > 2)),
      sum(dplyr::if_all(dplyr::all_of(cols), function(x) x > 2)),
      dplyr::across(dplyr::where(predicate), sum)
    )
    for (selector in selectors) {
      expressions <- list(
        rlang::expr((function(cols, predicate) !!selector)("x", is.integer)),
        rlang::expr((function(cols = "x", predicate = is.integer) !!selector)())
      )
      if (!rlang::is_call(selector, "as.data.frame")) {
        expressions <- c(expressions, list(rlang::expr(local({
          cols <- "x"
          predicate <- is.integer
          !!selector
        }))))
      }
      for (expression in expressions) {
        for (outer in c("absent", "matching", "conflicting")) {
          env <- rlang::env(rlang::current_env())
          if (outer != "absent") {
            env$cols <- if (outer == "matching") "x" else "y"
            env$predicate <- if (outer == "matching") is.integer else is.double
          }
          summary <- rlang::new_quosure(expression, env)
          expected <- dplyr::summarise(data, !!summary, .by = c(fixed, g))
          for (plan in c("keys", "fixed", "rollup")) {
            grouping <- switch(plan, keys = grouping_set(g),
                               fixed = grouping_set(), rollup = rollup(g))
            ordinary <- expected
            if (plan != "keys") {
              subtotal <- dplyr::summarise(data, !!summary, .by = fixed)
            }
            if (plan == "fixed") {
              ordinary <- subtotal
            } else if (plan == "rollup") {
              subtotal$g <- NA_character_
              ordinary <- dplyr::bind_rows(ordinary, subtotal)
            }
            shares <- list(list())
            if (plan != "keys") {
              shares <- c(shares, list(rlang::quos(
                total = sum(y), share = share_of_total(total)
              )))
            }
            if (plan == "rollup") {
              shares <- c(shares, list(rlang::quos(
                total = sum(y), share = share_of_parent(total)
              )))
            }
            for (share in shares) {
              actual <- rlang::inject(summarize_with_margins(
                data, !!summary, !!!share, .by = fixed, .grouping = grouping,
                .margin_label = NULL
              ))
              expect_equal(
                as.data.frame(actual)[names(ordinary)], as.data.frame(ordinary),
                info = paste(rlang::as_label(summary), outer, plan,
                             rlang::as_label(share))
              )
            }
          }
        }
      }
    }
  })
}
