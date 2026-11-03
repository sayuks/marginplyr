# Dated finite investigation; the ordinary branch binder supplies value semantics.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
library(dplyr)
capture <- function(expr) tryCatch(suppressWarnings(suppressMessages(force(expr))), error = identity)
out <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", "investigation/2026-11-03-expand-grouping-greedy/local")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
families <- list(
  complex = c(1+2i, NA_complex_, NaN+3i),
  expression = expression(a, NA, b + c),
  POSIXlt = as.POSIXlt(c("2026-01-01", NA, "2026-03-01"), tz = "Asia/Tokyo"),
  timeDate = timeDate::timeDate(c("2026-01-01", NA, "2026-03-01")),
  mcmc = coda::mcmc(matrix(c(1, NA, 3, 4, 5, 6), 3)),
  matrix_sparse = Matrix::Matrix(matrix(c(1, NA, 3, 4, 0, 0), 3), sparse = TRUE),
  dist_normal = distributional::dist_normal(c(1, NA, 3), c(1, 2, 3)),
  dist_mixture = distributional::dist_mixture(distributional::dist_normal(c(1, NA, 3), 1), distributional::dist_poisson(c(1, 2, 3)), weights = c(0.5, 0.5)),
  hilo = distributional::new_hilo(c(1, NA, 3), c(2, NA, 4), c(50, 80, 90)),
  rvar_integer = posterior::rvar(matrix(c(1L, 2L, NA_integer_, NA_integer_, 3L, 4L), 2)),
  rvar_logical = posterior::rvar(matrix(c(TRUE, FALSE, NA, NA, TRUE, TRUE), 2)),
  rvar_factor = posterior::rvar_factor(matrix(c("a", "b", NA, NA, "c", "c"), 2)),
  rvar_ordered = posterior::rvar_ordered(matrix(c("a", "b", NA, NA, "c", "c"), 2), levels = c("c", "a", "b")),
  wk_wkt = wk::wkt(c("POINT (1 2)", NA, "POINT (3 4)")),
  wk_wkb = wk::as_wkb(wk::wkt(c("POINT (1 2)", NA, "POINT (3 4)"))),
  json = jsonlite::toJSON(c("a", NA, "c")),
  html = htmltools::HTML(c("<a>", NA, "<c>")),
  utf8 = utf8::as_utf8(c("a", NA, "c")),
  list_of_int = vctrs::list_of(1L, NULL, c(3L, 4L)),
  list_of_frame = vctrs::list_of(tibble(a = 1L), NULL, tibble(a = c(3L, 4L))),
  df_zero_width = tibble(.rows = 3L),
  numeric_version = numeric_version(c("1.2", "3.4", "5.6"))
)
rows <- list()
add <- function(family, n, shape, role, container, sort, status, detail = "") {
  rows[[length(rows) + 1L]] <<- data.frame(family, n, shape, role, container, sort, status, detail)
}
for (family in names(families)) for (n in c(0L, 1L, 3L)) {
  x <- capture(vctrs::vec_slice(families[[family]], seq_len(n)))
  if (inherits(x, "error")) {
    add(family, n, "fixture", "fixture", "fixture", "none", "fixture_error", conditionMessage(x))
    next
  }
  for (shape in c("direct", "packed", "nested")) {
    key <- switch(shape, direct = x, packed = tibble(inner = x), nested = tibble(mid = tibble(inner = x)))
    for (role in c("dimension", "fixed", "payload")) for (container in c("tibble", "data.frame", "data.table")) {
      dat <- capture(tibble(g = rep(c("b", "a", NA), length.out = n), key = key, row = seq_len(n)))
      if (!inherits(dat, "error")) dat <- capture(switch(container, tibble = dat, data.frame = as.data.frame(dat), data.table = data.table::as.data.table(dat)))
      if (inherits(dat, "error")) {
        add(family, n, shape, role, container, "none", "fixture_error", conditionMessage(dat)); next
      }
      if (!all(c("g", "key", "row") %in% names(dat))) {
        add(family, n, shape, role, container, "none", "fixture_error", "Ordinary container conversion expanded or renamed the requested key column"); next
      }
      original <- serialize(dat, NULL)
      grouping <- if (role == "dimension") rollup(key) else rollup(g)
      by <- if (role == "fixed") rlang::quo(key) else rlang::quo(NULL)
      dimension <- if (role == "dimension") "key" else "g"
      missing <- capture(vctrs::vec_init(dat[[dimension]], n))
      control <- capture({
        omitted <- mutate(dat, !!dimension := !!missing)
        leading <- c(if (role == "fixed") "key", dimension)
        included <- relocate(dat, all_of(leading))
        omitted <- relocate(omitted, all_of(leading))
        bind_rows(mutate(included, occurrence = 1L), mutate(omitted, occurrence = 2L))
      })
      for (sort in c("none", "first", "last")) {
        got <- capture(expand_with_margins(dat, .by = !!by, .grouping = grouping, .margin_label = NULL, .id = "occurrence", .sort = sort))
        unchanged <- identical(serialize(dat, NULL), original)
        if (!unchanged) stop("Source changed: ", family, "/", container)
        if (inherits(control, "error")) {
          add(family, n, shape, role, container, sort, "ordinary_control_error", conditionMessage(control)); next
        }
        if (inherits(got, "error")) {
          ordering <- capture(arrange(dat, across(all_of(c(if (role == "fixed") "key", dimension)))))
          classification <- if (sort != "none" && inherits(ordering, "error")) "ordinary_order_error" else "package_error"
          add(family, n, shape, role, container, sort, classification, conditionMessage(got)); next
        }
        if (nrow(got) != nrow(control) || any(vapply(got, vctrs::vec_size, integer(1)) != nrow(got))) {
          shared <- nrow(got) == nrow(control) && identical(vapply(got, vctrs::vec_size, integer(1)), vapply(as.list(control)[names(got)], vctrs::vec_size, integer(1)))
          add(family, n, shape, role, container, sort, if (shared) "ordinary_shape_limitation" else "shape_difference", paste("got nrow", nrow(got), "control nrow", nrow(control), "column sizes", paste(vapply(got, vctrs::vec_size, integer(1)), collapse = ",")))
          next
        }
        got <- capture(arrange(got, occurrence, row))
        expected <- capture(arrange(control, occurrence, row))
        if (inherits(got, "error") || inherits(expected, "error")) {
          add(family, n, shape, role, container, sort, "alignment_error", paste(if (inherits(got, "error")) conditionMessage(got), if (inherits(expected, "error")) conditionMessage(expected))); next
        }
        comparison <- capture(all.equal(as.list(got)[names(expected)], as.list(expected), check.attributes = TRUE))
        add(family, n, shape, role, container, sort, if (isTRUE(comparison)) "match" else "difference", if (isTRUE(comparison)) "" else paste(comparison, collapse = "; "))
      }
    }
  }
}
results <- bind_rows(rows)
write.csv(results, file.path(out, "additional-types.csv"), row.names = FALSE)
print(table(results$status))
print(as.data.frame(results |> filter(!status %in% c("match", "ordinary_control_error", "ordinary_order_error"))) |> distinct(family, n, shape, role, container, status, detail), row.names = FALSE)
writeLines(c(capture.output(sessionInfo()), paste("SHA", system2("git", c("rev-parse", "HEAD"), stdout = TRUE))), file.path(out, "environment.txt"))
