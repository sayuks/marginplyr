test_that("omitted scalar dimensions use ordinary typed-missing composition", {
  input <- tibble::tibble(
    key = stats::dist(matrix(c(0, 1), ncol = 1L)), row = 1L
  )
  expected <- dplyr::union_all(
    input, dplyr::mutate(input, key = vctrs::vec_init(key, dplyr::n()))
  )

  expect_identical(as.numeric(expected$key), c(1, NA_real_))
  expect_identical(
    expand_with_margins(input, .grouping = rollup(key), .margin_label = NULL),
    expected
  )
})

# Genuine scalar-class fixtures, sliced with vctrs to keep their input types.
scalar_expansion_keys <- function(size) {
  distance <- stats::dist(matrix(c(0, 2, 5), ncol = 1L))
  likelihood <- stats::logLik(stats::lm(c(1, 3, 2) ~ 1))
  list(
    dist = vctrs::vec_slice(distance, seq_len(size)),
    logLik = vctrs::vec_slice(likelihood, rep(1L, size))
  )
}

# Ordinary branch mutation and combination supply the class/attribute oracle
# (ADR 0016); literal row and identifier assertions pin expansion multiplicity.
expect_scalar_expansion <- function(input) {
  size <- nrow(input)
  for (included in list(TRUE, c(TRUE, FALSE), c(FALSE, TRUE),
                        c(FALSE, TRUE, FALSE, TRUE))) {
    sets <- lapply(included, function(keep) {
      if (keep) grouping_set("key") else grouping_set()
    })
    specification <- grouping_sets(!!!sets)
    for (id in list(NULL, "set")) {
      branches <- lapply(seq_along(included), function(i) {
        branch <- if (included[[i]]) input else dplyr::mutate(
          input, key = !!vctrs::vec_init(input$key, size)
        )
        if (!is.null(id)) branch <- dplyr::mutate(branch, set = i)
        branch
      })
      expected <- Reduce(dplyr::union_all, branches)
      expected <- dplyr::relocate(
        expected, dplyr::any_of(c("fixed", "key", id))
      )
      for (label in list(NULL, NA_character_)) {
        result <- expand_with_margins(
          input, .by = "fixed", .grouping = specification,
          .margin_label = label, .duplicates = "keep", .id = id, .sort = "none"
        )
        expect_identical(result, expected)
        expect_identical(result$row, rep(seq_len(size), length(included)))
        if (!is.null(id)) {
          expect_identical(result$set, rep(seq_along(included), each = size))
        }
      }
    }
  }
}

test_that("local scalar-class expansion follows ordinary type rules", {
  for (size in c(0L, 1L, 3L)) {
    for (key in scalar_expansion_keys(size)) {
      input <- tibble::tibble(
        fixed = rep(c("a", "b"), length.out = size),
        key = key, row = seq_len(size)
      )
      expect_scalar_expansion(input)
      expect_scalar_expansion(as.data.frame(input))
    }
  }
})

test_that("data.table scalar-class expansion follows ordinary type rules", {
  skip_if_suggest_absent("data.table")
  for (size in c(0L, 1L, 3L)) {
    for (key in scalar_expansion_keys(size)) {
      input <- tibble::tibble(
        fixed = rep(c("a", "b"), length.out = size),
        key = key, row = seq_len(size)
      )
      expect_scalar_expansion(data.table::as.data.table(input))
    }
  }
})

test_that("carried scalar classes follow ordinary composition", {
  for (size in c(0L, 1L, 3L)) {
    for (key in scalar_expansion_keys(size)) {
      input <- tibble::tibble(
        key = key, group = rep("a", size), row = seq_len(size)
      )
      expected <- dplyr::union_all(
        input, dplyr::mutate(input, group = NA_character_)
      )
      for (label in list(NULL, NA_character_)) {
        expect_identical(
          expand_with_margins(
            input, .by = key, .grouping = rollup(group), .margin_label = label
          ),
          expected
        )
        expect_identical(
          expand_with_margins(
            input, .grouping = rollup(group), .margin_label = label
          ),
          dplyr::relocate(expected, group)
        )
      }
    }
  }
})

test_that("packed scalar classes and ordinary dimensions expand as before", {
  for (size in c(0L, 1L, 3L)) {
    keys <- c(
      lapply(scalar_expansion_keys(size), function(x) {
        tibble::tibble(component = x)
      }),
      list(
        factor = factor(rep("a", size), levels = c("a", "unused")),
        integer = seq_len(size),
        table = as.table(array(seq_len(size), dim = size))
      )
    )
    for (key in keys) {
      expect_scalar_expansion(tibble::tibble(
        fixed = rep(c("a", "b"), length.out = size),
        key = key, row = seq_len(size)
      ))
    }
  }
})
