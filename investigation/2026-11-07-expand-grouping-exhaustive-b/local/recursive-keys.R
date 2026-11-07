args <- commandArgs(TRUE)
stopifnot(length(args) == 2L)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(args[[1L]], quiet = TRUE)
library(dplyr)
set.seed(20261107L)

# Typed-missing keys can change shape when sliced; compare the public operation
# with ordinary mutate/union composition over independently enumerated sets.
scalar <- function(kind, n) {
  x <- switch(kind,
    integer = c(2L, NA_integer_, -1L, 2L, 0L, NA_integer_, 7L),
    double = c(NaN, -Inf, Inf, NA_real_, -0, 1.5, NaN),
    character = c("b", NA_character_, "a", "", "Total", "a", NA_character_),
    factor = structure(c(2L, NA_integer_, 1L, 3L, 2L, 1L, NA_integer_),
      levels = c("z", NA_character_, "a", "unused"), class = "factor"),
    ordered = structure(c(2L, NA_integer_, 1L, 3L, 2L, 1L, NA_integer_),
      levels = c("z", NA_character_, "a", "unused"), class = c("ordered", "factor")),
    date = as.Date(c(2, NA, -1, 2, 0, NA, 7), origin = "1970-01-01"),
    time = as.POSIXct(c(2, NA, -1, 2, 0, NA, 7), origin = "1970-01-01", tz = "Pacific/Apia"),
    posixlt = I(as.POSIXlt(as.POSIXct(c(2, NA, -1, 2, 0, NA, 7), origin = "1970-01-01", tz = "Pacific/Apia"))),
    list = list(NULL, NA, list(1L, 2L), quote(a + b), matrix(1:4, 2), "a", NULL),
    list_of = vctrs::list_of(1:2, NULL, NA_integer_, integer(), 7L, 1:3, NULL, .ptype = integer())
  )
  vctrs::vec_slice(x, rep(seq_len(7L), length.out = n))
}
kinds <- c("integer", "double", "character", "factor", "ordered", "date", "time", "posixlt", "list", "list_of")
make_key <- function(n, depth = 0L) {
  shape <- if (depth >= 3L) "scalar" else sample(c("scalar", "scalar", "packed", "array"), 1L)
  if (shape == "scalar") return(scalar(sample(kinds, 1L), n))
  if (shape == "array") {
    tails <- sample(list(0L, 1L, 2L, c(0L, 2L), c(2L, 0L), c(2L, 2L), c(1L, 2L, 2L)), 1L)[[1L]]
    # Classed arrays whose ordinary ordering recurses were measured separately
    # by the earlier hunt; these are unclassed storage shapes.
    x <- scalar(sample(c("integer", "double", "character", "list"), 1L), n * prod(tails))
    dim(x) <- c(n, tails)
    dimnames(x) <- lapply(dim(x), function(size) paste0("v", seq_len(size), recycle0 = TRUE))
    return(x)
  }
  width <- sample(0:3, 1L)
  columns <- lapply(seq_len(width), function(i) make_key(n, depth + 1L))
  names(columns) <- paste0("inner", seq_len(width), recycle0 = TRUE)
  tibble::new_tibble(columns, nrow = n)
}
plans <- list(
  rollup = list(expr = quote(rollup(a, b, c)), sets = list(c("a", "b", "c"), c("a", "b"), "a", character())),
  cube = list(expr = quote(cube(a, b, c)), sets = list(c("a", "b", "c"), c("a", "b"), c("a", "c"), c("b", "c"), "a", "b", "c", character())),
  reversed = list(expr = quote(grouping_sets(grouping_set(c), grouping_set(), grouping_set(a,b,c), grouping_set(b,c), grouping_set(c))), sets = list("c", character(), c("a", "b", "c"), c("b", "c"), "c")),
  product = list(expr = quote(grouping_spec(rollup(grouping_set(a,b)), cube(c))), sets = list(c("a", "b", "c"), c("a", "b"), "c", character()))
)
records <- list()
for (fixture in seq_len(80L)) for (n in c(0L, 1L, 7L)) {
  x <- tibble::new_tibble(list(fixed = make_key(n), a = make_key(n), b = make_key(n), c = make_key(n), rid = seq_len(n)), nrow = n)
  before <- serialize(x, NULL)
  for (plan_name in names(plans)) {
    plan <- plans[[plan_name]]
    dimensions <- unique(unlist(plan$sets, use.names = FALSE))
    base <- tryCatch({
      branches <- lapply(seq_along(plan$sets), function(s) {
        y <- x
        for (key in setdiff(c("a", "b", "c"), plan$sets[[s]])) y <- mutate(y, !!key := !!vctrs::vec_init(x[[key]], n))
        mutate(y, sid = rep(s, n))
      })
      select(Reduce(union_all, branches), all_of(c("fixed", dimensions, "sid", "rid")))
    }, error = identity)
    for (sorting in c("none", "first", "last")) {
      expected <- tryCatch({
        if (inherits(base, "error")) stop(base)
        y <- base
        if (sorting != "none") {
          terms <- list(vctrs::vec_detect_missing(y$fixed), y$fixed)
          for (key in dimensions) {
            bit <- vapply(y$sid, function(s) as.integer(!key %in% plan$sets[[s]]), integer(1))
            if (sorting == "first") bit <- -bit
            terms <- c(terms, list(bit, vctrs::vec_detect_missing(y[[key]]), y[[key]]))
          }
          terms <- c(terms, list(y$sid))
          names(terms) <- paste0("k", seq_along(terms))
          # Ordinary dplyr ordering owns heterogeneous-list ranking; base
          # vec_order can answer differently for those values.
          order_table <- tibble::new_tibble(terms, nrow = nrow(y))
          order_table$row_origin <- seq_len(nrow(y))
          order_table <- arrange(order_table, !!!rlang::syms(names(terms)))
          y <- vctrs::vec_slice(y, order_table$row_origin)
        }
        y
      }, error = identity)
      actual <- tryCatch(rlang::inject(expand_with_margins(x, .by = fixed, .grouping = !!plan$expr, .margin_label = NULL, .id = "sid", .duplicates = "keep", .sort = sorting)), error = identity)
      status <- if (inherits(expected, "error")) "ordinary_error" else if (inherits(actual, "error")) "package_error" else if (identical(actual, expected)) "match" else "difference"
      detail <- if (inherits(actual, "error")) conditionMessage(actual) else if (inherits(expected, "error")) conditionMessage(expected) else if (status == "difference") paste(all.equal(actual, expected), collapse = "; ") else ""
      immutable <- identical(before, serialize(x, NULL))
      records[[length(records) + 1L]] <- data.frame(fixture, n, plan = plan_name, sort = sorting, status, immutable, detail)
      if (status %in% c("package_error", "difference") || !immutable) saveRDS(list(x = x, plan = plan, sorting = sorting, expected = expected, actual = actual), paste0(args[[2L]], "-", length(records), ".rds"))
    }
  }
}
result <- bind_rows(records)
write.csv(result, args[[2L]], row.names = FALSE)
print(table(result$status))
print(head(result[result$status %in% c("package_error", "difference") | !result$immutable, ], 30L))
cat("Comparisons:", nrow(result), "\n")
print(sessionInfo())
