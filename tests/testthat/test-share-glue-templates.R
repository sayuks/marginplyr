# The `{}` components of a literal template evaluate in the caller's frame,
# which inside a summary expression reaches the mask (#1824).
glue_template_reads <- alist(
  as.numeric(glue::glue("{sum(prior)}")),
  glue::glue("{round(100 * prior)}%"),
  as.numeric(glue::glue_data(list(), "{sum(prior)}")),
  as.numeric(glue::glue("{sum(other)}", other = 1, "{sum(prior)}")),
  as.numeric(stringr::str_glue("{sum(prior)}")),
  as.numeric(stringr::str_glue_data(list(), "{sum(prior)}")),
  as.numeric(rlang::englue("{sum(prior)}")),
  as.numeric(rlang::englue("{sum(prior)}", env = rlang::current_env())),
  as.numeric(cli::format_inline("{sum(prior)}")),
  cli::format_inline("{.val {sum(prior)}}"),
  cli::pluralize("{sum(prior)} item{?s}"),
  cli::format_message("{sum(prior)}"),
  as.numeric(glue::glue("{sum(prior)}", .envir = parent.env(environment()))),
  as.numeric(glue::glue("{sum(prior}")),
  as.numeric(glue::glue("{sum(prior)")),
  as.numeric(glue("{sum(prior)}"))
)

test_that("literal template components keep both share dependencies", {
  glue <- glue::glue
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_share_dependencies(data, glue_template_reads)
  expect_share_dependencies(data[c("g", "x")], glue_template_reads)
  expect_share_dependencies(tibble::as_tibble(data), glue_template_reads)
})

test_that("literal template components keep data.table share dependencies", {
  skip_if_suggest_absent("data.table")
  glue <- glue::glue
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(100, 300)
  )
  expect_share_dependencies(data, glue_template_reads)
})

expect_independent_templates <- function(data) {
  external <- rlang::new_environment(list(prior = 7), parent = baseenv())
  # A caller function named like glue's.
  glue <- function(...) 7
  reads <- alist(
    as.numeric(glue::glue("{prior}", prior = 7)),
    nchar(glue::glue("{{prior}}")),
    as.numeric(glue::glue("{7 + 0 * sum(x)}")),
    {
      prior <- 7
      as.numeric(glue::glue("{prior}"))
    },
    (function(prior) as.numeric(glue::glue("{prior}")))(7),
    as.numeric(sub(" .*", "", cli::pluralize("{7 + 0 * sum(x)} item{?s}"))),
    as.numeric(glue::glue_data(list(prior = 7), "{prior}")),
    as.numeric(glue::glue("{prior}", .envir = list2env(list(prior = 7)))),
    as.numeric(glue::glue("{prior}", .envir = external)),
    as.numeric(glue::glue(
      "{prior}", .envir = list2env(list(other = 1), parent = external)
    )),
    as.numeric(rlang::englue("{prior}", env = external)),
    # Custom delimiters stay outside the walk.
    as.numeric(glue::glue("<<7 + 0 * sum(x)>>", .open = "<<", .close = ">>")),
    nchar(paste0("{", "prior}")),
    nchar(sprintf("%s", "{prior}")),
    glue("{prior}"),
    {
      template <- "{7 + 0 * sum(x)}"
      as.numeric(glue::glue(template))
    }
  )
  for (kind in c("parent", "total")) {
    for (read in reads) {
      share <- rlang::call2(paste0("share_of_", kind), quote(source))
      forward <- summarize_with_margins(
        data, source = sum(.data$x), prior = !!share, observed = !!read,
        .grouping = rollup("g")
      )
      expect_equal(forward$observed, c(7, 7, 7))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      share <- rlang::call2(paste0("share_of_", kind), quote(observed))
      reverse <- summarize_with_margins(
        data, prior = sum(.data$x), observed = !!read, ratio = !!share,
        .grouping = rollup("g")
      )
      expect_equal(reverse$observed, c(7, 7, 7))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
}

test_that("templates that read no earlier summary remain valid", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_independent_templates(data)
})

test_that("data.table templates that read no earlier summary remain valid", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(100, 300)
  )
  expect_independent_templates(data)
})
