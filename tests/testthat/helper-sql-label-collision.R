# Exercise the same public contract against each live SQL backend.
check_sql_label_collision <- function(con) {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  copy <- function(name, values) {
    data <- tibble::tibble(value = seq_along(values))
    data[[name]] <- values
    dplyr::copy_to(
      con, data, "label_source", temporary = TRUE, overwrite = TRUE
    )
  }
  summarize <- function(data, name, check = TRUE, label = "Total") {
    summarize_with_margins(
      data, n = dplyr::n(), .grouping = rollup(dplyr::all_of(name)),
      .check_margin_label = check, .margin_label = label, .id = "set"
    )
  }
  for (name in c(".env", ".data", "ordinary")) {
    remote <- copy(name, "a")
    for (audit in c(FALSE, TRUE)) {
      options(marginplyr.audit_sql = audit)
      query <- summarize(remote, name)
      if (audit) {
        details <- last_sent_queries(details = TRUE)
        internal <- last_sent_queries(include_result = FALSE, details = TRUE)
        expect_equal(last_sent_queries(), details[c("purpose", "sql")])
        expect_equal(internal, details[details$purpose != "result", ])
        count <- internal[internal$purpose == "observed_label_collision", ]
        expect_equal(nrow(count), 1L)
        expect_identical(count$status, "returned")
        expect_false(anyNA(count$sql))
        expect_equal(nrow(count$conditions[[1L]]), 0L)
      }
      result <- dplyr::collect(query)
      result <- result[order(result$set), ]
      expect_identical(result[[name]], c("a", "Total"))
      expect_equal(result$n, c(1, 1))

      expanded <- expand_with_margins(
        remote, .grouping = rollup(dplyr::all_of(name)),
        .check_margin_label = TRUE, .id = "set"
      ) |>
        dplyr::collect()
      expanded <- expanded[order(expanded$set), ]
      expect_identical(expanded[[name]], c("a", "Total"))
      expect_equal(expanded$value, c(1, 1))
    }
  }
  remote <- copy(".env", "Total")
  errors <- lapply(c(FALSE, TRUE), function(audit) {
    options(marginplyr.audit_sql = audit)
    error <- rlang::catch_cnd(summarize(remote, ".env"), classes = "error")
    expect_s3_class(error, "marginplyr_error")
    expect_match(conditionMessage(error), ".env", fixed = TRUE)
    expect_match(conditionMessage(error), "already present", fixed = TRUE)
    if (audit) {
      details <- last_sent_queries(details = TRUE)
      count <- details[details$purpose == "observed_label_collision", ]
      expect_equal(nrow(count), 1L)
      expect_identical(count$status, "returned")
      expect_false(anyNA(count$sql))
      expect_equal(nrow(count$conditions[[1L]]), 0L)
      expect_equal(last_sent_queries(FALSE, TRUE), details)
      expect_equal(last_sent_queries(), details[c("purpose", "sql")])
    }
    error
  })
  for (audit in c(FALSE, TRUE)) {
    options(marginplyr.audit_sql = audit)
    error <- rlang::catch_cnd(expand_with_margins(
      remote, .grouping = rollup(dplyr::all_of(".env")),
      .check_margin_label = TRUE
    ), classes = "error")
    expect_s3_class(error, "marginplyr_error")
    expect_match(conditionMessage(error), ".env", fixed = TRUE)
    expect_match(conditionMessage(error), "already present", fixed = TRUE)
  }
  expect_identical(class(errors[[1L]]), class(errors[[2L]]))
  expect_identical(
    conditionMessage(errors[[1L]]), conditionMessage(errors[[2L]])
  )
  options(marginplyr.audit_sql = TRUE)
  unchecked <- summarize(remote, ".env", check = FALSE) |> dplyr::collect()
  expect_identical(unchecked[[".env"]], c("Total", "Total"))
  expect_false("observed_label_collision" %in% last_sent_queries()$purpose)
  for (label in list(NULL, NA_character_)) {
    result <- summarize(remote, ".env", label = label) |> dplyr::collect()
    expect_equal(nrow(result), 2L)
    expect_false("observed_label_collision" %in% last_sent_queries()$purpose)
  }
  mixed <- dplyr::copy_to(
    con, tibble::tibble(.env = "a", .data = "b", ordinary = "c"),
    "label_source", temporary = TRUE, overwrite = TRUE
  )
  result <- summarize_with_margins(
    mixed, n = dplyr::n(),
    .grouping = rollup(dplyr::all_of(c(".env", ".data", "ordinary"))),
    .check_margin_label = TRUE, .id = "set"
  ) |> dplyr::collect()
  expect_equal(nrow(result), 4L)
  expect_true(all(c(".env", ".data", "ordinary") %in% names(result)))
  expect_equal(result$n, rep(1, 4))
  empty <- copy(".env", character())
  result <- summarize(empty, ".env") |> dplyr::collect()
  expect_identical(result[[".env"]], "Total")
  expect_equal(result$n, 0)
}

# The observed label check over an input the caller has arranged, with and
# without a row limit beneath the ordering (#1585).
check_arranged_label_collision <- function(con) {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(
    con, data.frame(g = c("Total", "a", "b"), v = c(3L, 1L, 2L)),
    "arranged_label_source", temporary = TRUE, overwrite = TRUE
  )
  verbs <- list(
    summarize = function(x, check, label = "Total") {
      summarize_with_margins(
        x, s = sum(.data$v, na.rm = TRUE),
        .grouping = rollup(dplyr::all_of("g")),
        .margin_label = label, .check_margin_label = check, .id = "set"
      )
    },
    expand = function(x, check, label = "Total") {
      expand_with_margins(
        x, .grouping = rollup(dplyr::all_of("g")), .margin_label = label,
        .check_margin_label = check, .id = "set"
      )
    }
  )
  bag <- function(query) {
    # dbplyr warns about the ordering the result query inherits whether or not
    # the check ran, so the warning says nothing about the check.
    result <- withCallingHandlers(
      as.data.frame(dplyr::collect(query)),
      warning = function(cnd) {
        if (grepl("ORDER BY is ignored", conditionMessage(cnd), fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
    result <- result[do.call(order, unname(as.list(result))), ]
    rownames(result) <- NULL
    result
  }
  label_query <- function() {
    details <- last_sent_queries(include_result = FALSE)
    details[details$purpose == "observed_label_collision", ]
  }
  for (key in c("column", "expression")) {
    arranged <- if (key == "column") {
      dplyr::arrange(remote, .data$v)
    } else {
      dplyr::arrange(remote, dplyr::desc(.data$v * 2L))
    }
    # The first two rows by `v` exclude the one holding the label.
    selected <- utils::head(dplyr::arrange(remote, .data$v), 2L)
    for (verb in names(verbs)) {
      info <- paste(key, verb)
      run <- verbs[[verb]]
      checked <- run(arranged, TRUE, "All")
      sent <- label_query()
      expect_equal(nrow(sent), 1L, info = info)
      # `sqlite_margin_input()` has already rendered a SQLite input, ordering
      # included, into an opaque relation, so only other dialects send the
      # count straight against the caller's query.
      if (!inherits(con, "SQLiteConnection")) {
        expect_false(grepl("ORDER BY", sent$sql, fixed = TRUE), info = info)
        expect_false(grepl("\\bv\\b", sent$sql), info = info)
      }
      expect_identical(
        bag(checked), bag(run(arranged, FALSE, "All")), info = info
      )

      checked <- run(selected, TRUE)
      expect_equal(nrow(label_query()), 1L, info = info)
      expect_identical(
        bag(checked), bag(run(selected, FALSE)), info = info
      )
      expect_error(
        run(arranged, TRUE), "already present", class = "marginplyr_error",
        info = info
      )
    }
  }
}
