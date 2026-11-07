list_write_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
}

list_write_reads <- function() {
  alist(
    (function(prior) {
      carrier <- list()
      carrier[[1L]] <- function() sum(prior)
      rm(prior)
      carrier[[1L]]()
    })(1),
    (function(prior) {
      carrier <- list(function() 7)
      carrier[[1L]] <- function() sum(prior)
      rm(prior)
      carrier[[1L]]()
    })(1),
    (function(prior) {
      carrier <- list()
      carrier$read <- function() sum(prior)
      rm(prior)
      carrier$read()
    })(1)
  )
}


list_write_finish <- function(result) {
  if (is.data.frame(result)) result else dplyr::collect(result)
}

expect_list_write_refusals <- function(data, reads = list_write_reads()) {
  for (kind in c("parent", "total")) {
    for (read in reads) {
      helper <- rlang::call2(paste0("share_of_", kind), quote(value))
      expect_error(
        list_write_finish(summarize_with_margins(
          data, value = sum(.data$x), prior = !!helper, observed = !!read,
          .grouping = rollup("g")
        )),
        "Ordinary summaries cannot use an earlier", class = "marginplyr_error"
      )
      expect_error(
        list_write_finish(summarize_with_margins(
          data, prior = sum(.data$x), value = !!read, ratio = !!helper,
          .grouping = rollup("g")
        )),
        "depends on earlier summary alias", class = "marginplyr_error"
      )
    }
  }
}

list_write_controls <- function() {
  alist(
    (function(prior) {
      carrier <- list()
      carrier[[1L]] <- function() sum(prior)
      carrier[[1L]]()
    })(1),
    (function(prior) {
      carrier <- list(function() 7)
      carrier[[1L]] <- function() sum(prior)
      carrier[[1L]]()
    })(1),
    (function(prior) {
      carrier <- list()
      carrier$read <- function() sum(prior)
      carrier$read()
    })(1),
    (function(prior) {
      carrier <- list(function() sum(prior))
      carrier[[1L]] <- function() 7
      rm(prior)
      carrier[[1L]]()
    })(1),
    (function(prior) {
      carrier <- list(read = function() sum(prior))
      carrier$read <- function() 7
      rm(prior)
      carrier$read()
    })(1)
  )
}

expect_list_write_values <- function(data) {
  reads <- list_write_controls()
  expected <- c(1, 1, 1, 7, 7)
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (index in seq_along(reads)) {
      read <- reads[[index]]
      forward <- list_write_finish(summarize_with_margins(
        data, value = sum(.data$x), prior = !!helper, observed = !!read,
        .grouping = rollup("g")
      ))
      reverse <- list_write_finish(summarize_with_margins(
        data, prior = sum(.data$x), value = !!read, ratio = !!helper,
        .grouping = rollup("g")
      ))
      expect_equal(forward$observed, rep(expected[[index]], 3L))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      expect_equal(reverse$value, rep(expected[[index]], 3L))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
}

expect_list_writes <- function(data) {
  before <- serialize(data, NULL)
  expect_list_write_refusals(data)
  expect_list_write_refusals(data, alist((function(prior) {
    carrier <- list(function() sum(prior))
    rm(prior)
    carrier[[1L]]()
  })(1)))
  expect_list_write_values(data)
  expect_identical(serialize(data, NULL), before)
}

test_that("literal list writes retain eager invocation dependencies", {
  expect_list_writes(list_write_data())
  expect_list_writes(tibble::as_tibble(list_write_data()))
})

test_that("data.table literal list writes retain invocation dependencies", {
  skip_if_suggest_absent("data.table")
  expect_list_writes(data.table::as.data.table(list_write_data()))
})

test_that("collected dtplyr list writes retain invocation dependencies", {
  skip_if_suggest_absent("dtplyr")
  expect_list_writes(dtplyr::lazy_dt(list_write_data()))
})

test_that("list write analysis never invokes installed caller closures", {
  probe <- rlang::env(calls = 0L)
  read <- quote((function(prior) {
    carrier <- list()
    carrier[[1L]] <- function() {
      probe$calls <- probe$calls + 1L
      sum(prior)
    }
    rm(prior)
    carrier[[1L]]()
  })(1))
  expect_list_write_refusals(list_write_data(), list(read))
  expect_identical(probe$calls, 0L)
})

test_that("a singleton loop retains invocation dependencies of list writes", {
  probe <- rlang::env(calls = 0L)
  read <- function(sequence) {
    substitute((function(prior) {
      callback <- function() {
        probe$calls <- probe$calls + 1L
        sum(prior)
      }
      carrier <- list(read = function() 7)
      for (i in SEQUENCE) carrier$read <- callback
      rm(prior)
      carrier$read()
    })(1), list(SEQUENCE = sequence))
  }
  expect_share_dependencies(
    list_write_data(), list(read(1L), read(quote((1L))))
  )
  expect_identical(probe$calls, 0L)

  caller <- rlang::env(`(` = function(value) {
    if (identical(value, 1L)) integer() else value
  })
  overridden <- rlang::new_quosure(read(quote((1L))), caller)
  expect_share_dependency_values(list_write_data(), list(overridden), 7)
})

test_that("literal loops preserve legal closures and function lookup", {
  reads <- alist(
    (function(prior) {
      carrier <- list(read = function() 1)
      for (i in 1L) carrier$read <- function() sum(prior)
      carrier$read()
    })(7),
    (function(prior) {
      carrier <- list(read = function() sum(prior))
      for (i in 1L) {
        carrier$read <- function() 7
      }
      rm(prior)
      carrier$read()
    })(1),
    (function(prior) {
      carrier <- list(read = function() sum(prior))
      for (i in NULL) rm(prior)
      carrier$read()
    })(7),
    (function(prior) {
      `for` <- function(...) NULL
      carrier <- list(read = function() 7)
      for (i in 1L) carrier$read <- function() sum(prior)
      rm(prior)
      carrier$read()
    })(1)
  )
  expect_share_dependency_values(list_write_data(), reads, expected = 7)

  for (sequence in alist(1L, NULL)) {
    read <- substitute((function(prior) {
      reader <- function() sum(prior)
      rm(prior)
      (function() {
        for (reader in SEQUENCE) NULL
        reader()
      })()
    })(1), list(SEQUENCE = sequence))
    expect_share_dependencies(list_write_data(), list(read))
  }
})

test_that("literal loops leave writes after loop exits unreachable", {
  for (exit in alist(break, next, eval(parse(text = "break")))) {
    read <- substitute((function(prior) {
      carrier <- list(read = function() 7)
      for (i in 1L) {
        EXIT
        carrier$read <- function() sum(prior)
      }
      rm(prior)
      carrier$read()
    })(1), list(EXIT = exit))
    expect_share_dependency_values(list_write_data(), list(read), expected = 7)
  }
})

test_that("list write refusal leaves lazy input unread", {
  skip_if_suggest_absent("dtplyr")
  probe <- rlang::env(calls = 0L)
  observe <- function(x) {
    probe$calls <- probe$calls + 1L
    x
  }
  data <- dplyr::filter(dtplyr::lazy_dt(list_write_data()), observe(x) > 0)
  expect_list_write_refusals(data)
  expect_identical(probe$calls, 0L)
  expect_equal(dplyr::collect(data)$x, c(1, 3))
  expect_identical(probe$calls, 1L)
})

structural_list_write_reads <- function(body = quote(sum(prior)),
                                        remove = quote(rm(prior)),
                                        argument = 1, replacement = FALSE,
                                        first = quote(7)) {
  writes <- alist(carrier[[1L]] <- NULL, carrier[["drop"]] <- NULL,
                  carrier$drop <- NULL)
  if (replacement) {
    writes <- as.list(quote(alist(
      carrier[1L] <- list(function() BODY),
      carrier["drop"] <- list(function() BODY)
    )))[-1L]
    writes <- lapply(writes, function(write) {
      do.call(substitute, list(write, list(BODY = body)))
    })
  }
  lapply(writes, function(write) {
    substitute((function(prior) {
      carrier <- list(drop = function() FIRST, read = function() BODY)
      WRITE
      REMOVE
      carrier[[1L]]()
    })(ARGUMENT), list(BODY = body, WRITE = write, REMOVE = remove,
                       ARGUMENT = argument, FIRST = first))
  })
}

test_that("list deletion preserves surviving closure dependencies", {
  expect_share_dependencies(list_write_data(), structural_list_write_reads())
})

test_that("single-bracket replacement retains closure dependencies", {
  expect_share_dependencies(
    list_write_data(), structural_list_write_reads(replacement = TRUE)
  )
})

expect_structural_list_writes <- function(data) {
  before <- serialize(data, NULL)
  for (replacement in c(FALSE, TRUE)) {
    expect_share_dependencies(
      data, structural_list_write_reads(replacement = replacement)
    )
    expect_share_dependency_values(
      data, structural_list_write_reads(remove = quote(NULL),
                                        replacement = replacement),
      expected = 1
    )
    expect_share_dependency_values(
      data, structural_list_write_reads(body = quote(7),
                                        first = quote(sum(prior)),
                                        replacement = replacement),
      expected = 7
    )
  }
  expect_identical(serialize(data, NULL), before)
}

test_that("structural list writes preserve eager legal controls and inputs", {
  expect_structural_list_writes(list_write_data())
  expect_structural_list_writes(tibble::as_tibble(list_write_data()))
})

test_that("data.table structural writes preserve dependencies and inputs", {
  skip_if_suggest_absent("data.table")
  expect_structural_list_writes(data.table::as.data.table(list_write_data()))
})

test_that("structural write planning does not invoke or force caller code", {
  probe <- rlang::env(invoked = 0L, forced = 0L)
  for (replacement in c(FALSE, TRUE)) {
    reads <- structural_list_write_reads(
      body = quote({
        probe$invoked <- probe$invoked + 1L
        sum(prior)
      }),
      argument = quote({
        probe$forced <- probe$forced + 1L
        1
      }),
      replacement = replacement
    )
    expect_share_dependencies(list_write_data(), reads)
  }
  expect_identical(probe$invoked, 0L)
  expect_identical(probe$forced, 0L)
})
