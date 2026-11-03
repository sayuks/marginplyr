# Finite row-copy algebra with an independent occurrence/label/order oracle.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
library(dplyr)
capture <- function(x) tryCatch(suppressWarnings(suppressMessages(force(x))), error = identity)
out <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", "investigation/2026-11-03-expand-grouping-greedy/local")
families <- list(
  integer = c(3L, NA_integer_, 1L, 3L),
  factor = factor(c("b", NA, "a", "b"), levels = c("b", "a", "unused")),
  ordered = ordered(c("b", NA, "a", "b"), levels = c("b", "a", "unused")),
  factor_na_level = factor(c("b", NA, "a", "b"), exclude = NULL),
  date = as.Date(c("2026-03-01", NA, "2026-01-01", "2026-03-01")),
  list = list(1L, NULL, NA_integer_, c(3L, 4L)),
  packed = tibble(x = c(3L, NA_integer_, NA_integer_, 1L), y = c(NA, "b", NA, "a")),
  matrix = matrix(c(3L, NA_integer_, NA_integer_, 1L, NA_integer_, 2L, NA_integer_, 4L), 4L)
)
plan_defs <- list(
  rollup = list(spec = rollup(g, h), sets = list(c("g", "h"), "g", character())),
  cube = list(spec = cube(g, h), sets = list(c("g", "h"), "g", "h", character())),
  omitted_first = list(spec = grouping_sets(grouping_set(), grouping_set(h, g)), sets = list(character(), c("h", "g"))),
  duplicates = list(spec = grouping_sets(grouping_set(g, h), grouping_set(), grouping_set(g, h)), sets = list(c("g", "h"), character(), c("g", "h")))
)
expand_oracle <- function(dat, definition, id, label, by = character()) {
  dims <- unique(unlist(definition$sets, use.names = FALSE))
  factor_names <- names(dat)[vapply(dat, is.factor, logical(1))]
  factor_levels <- lapply(factor_names, function(nm) unique(c(levels(dat[[nm]]), if (nm %in% dims) label)))
  names(factor_levels) <- factor_names
  branches <- lapply(seq_along(definition$sets), function(i) {
    branch <- dat
    for (name in setdiff(dims, definition$sets[[i]])) {
      x <- dat[[name]]
      if (is.null(label)) branch[[name]] <- vctrs::vec_init(x, nrow(dat)) else if (is.factor(x)) {
        lev <- unique(c(levels(x), label))
        branch[[name]] <- structure(rep(match(label, lev), nrow(dat)), levels = lev, class = class(x))
      } else branch[[name]] <- rep(label, nrow(dat))
    }
    for (name in definition$sets[[i]]) {
      if (!is.null(label) && !is.factor(dat[[name]])) branch[[name]] <- as.character(dat[[name]]) else if (!is.null(label)) {
        x <- dat[[name]]
        branch[[name]] <- structure(as.integer(x), levels = unique(c(levels(x), label)), class = class(x))
      }
    }
    branch[[id]] <- rep(as.integer(i), nrow(dat))
    for (name in factor_names) branch[[name]] <- as.integer(branch[[name]])
    relocate(branch, all_of(c(by, dims, id)))
  })
  result <- bind_rows(branches)
  for (name in factor_names) result[[name]] <- structure(result[[name]], levels = factor_levels[[name]], class = class(dat[[name]]))
  result
}
order_oracle <- function(dat, definition, by, id, sort) {
  if (sort == "none") return(dat)
  dims <- unique(unlist(definition$sets, use.names = FALSE))
  key <- list()
  for (nm in c(by, dims)) {
    x <- dat[[nm]]
    if (nm %in% dims) {
      bits <- as.integer(!vapply(definition$sets[dat[[id]]], function(s) nm %in% s, logical(1)))
      key[[length(key) + 1L]] <- if (sort == "first") -bits else bits
    }
    key[[length(key) + 1L]] <- as.integer(vctrs::vec_detect_missing(x))
    key[[length(key) + 1L]] <- x
  }
  key[[length(key) + 1L]] <- dat[[id]]
  key <- tibble::new_tibble(rlang::set_names(key, paste0("k", seq_along(key))), nrow = nrow(dat))
  vctrs::vec_slice(dat, vctrs::vec_order(key))
}
rows <- list()
for (family in names(families)) for (n in c(0L, 1L, 4L)) for (first in names(plan_defs)) for (second in c("rollup", "omitted_first", "duplicates")) for (text in c(FALSE, TRUE)) {
  if (text && family %in% c("list", "packed", "matrix")) next
  dat <- tibble(f = rep(c(NA_integer_, 1L), length.out = n), g = vctrs::vec_slice(families[[family]], seq_len(n)), h = rep(c("x", "y", NA, "x"), length.out = n), row = seq_len(n))
  before <- serialize(dat, NULL)
  first_label <- if (text) "First total" else NULL
  second_label <- if (text) "Second total" else NULL
  def1 <- plan_defs[[first]]; def2 <- plan_defs[[second]]
  expected1 <- capture(expand_oracle(dat, def1, "set1", first_label, "f"))
  got1 <- capture(expand_with_margins(dat, .by = f, .grouping = def1$spec, .margin_label = first_label, .id = "set1", .duplicates = "keep", .check_margin_label = FALSE))
  for (fixed in c(FALSE, TRUE)) for (sort in c("none", "first", "last")) {
    by_names <- if (fixed) c("f", "set1") else character()
    expected <- capture(order_oracle(expand_oracle(expected1, def2, "set2", second_label, by_names), def2, by_names, "set2", sort))
    got <- capture(expand_with_margins(got1, .by = all_of(by_names), .grouping = def2$spec, .margin_label = second_label, .id = "set2", .duplicates = "keep", .sort = sort, .check_margin_label = FALSE))
    status <- "match"; detail <- ""
    if (inherits(expected, "error")) {status <- "ordinary_control_error"; detail <- conditionMessage(expected)} else if (inherits(got, "error")) {status <- "package_error"; detail <- conditionMessage(got)} else {
      # Compare row copies independently from delegated attribute details.
      if (sort == "none") {got <- arrange(got, set2, set1, row); expected <- arrange(expected, set2, set1, row)}
      cmp <- all.equal(as.list(got)[names(expected)], as.list(expected), check.attributes = TRUE)
      if (!isTRUE(cmp)) {status <- "difference"; detail <- paste(cmp, collapse = "; ")}
    }
    stopifnot(identical(serialize(dat, NULL), before))
    rows[[length(rows) + 1L]] <- data.frame(family, n, first, second, text, fixed, sort, status, detail)
  }
}
results <- bind_rows(rows)
write.csv(results, file.path(out, "re-expansion.csv"), row.names = FALSE)
print(table(results$status))
print(as.data.frame(results |> filter(status != "match") |> distinct(family, status, detail)), row.names = FALSE)
