# Missingness is calculated from scalar components independently of package helpers.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
out_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempdir())
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
capture <- function(expr) tryCatch(force(expr), error = identity)
row_missing <- function(x) {
  if (is.data.frame(x)) return(Reduce(`&`, lapply(x, row_missing), init = rep(TRUE, nrow(x))))
  if (is.array(x)) {
    return(vapply(seq_len(dim(x)[[1L]]), function(i) all(is.na(x[i, , drop = FALSE])), logical(1)))
  }
  if (is.list(x)) return(vapply(x, is.null, logical(1)))
  is.na(x)
}
families <- list(
  matrix = matrix(c(2, NA, NA, 1, NA, 1, NA, NA, 1, NA, 2, NA, NA, 2, NA, NA), 8L),
  zero_width = matrix(integer(), nrow = 8L, ncol = 0L),
  packed = tibble(a = c(2, NA, NA, 1, NA, 1, NA, NA), b = c(1, NA, 2, NA, NA, 2, NA, NA)),
  nested_packed = tibble(
    a = tibble(u = c(2, NA, NA, 1, NA, 1, NA, NA)),
    b = matrix(c(1, NA, 2, NA, NA, 2, NA, NA), 8L)
  ),
  list = list(2L, NA_integer_, NULL, 1L, NULL, integer(), 2:3, NA_real_)
)
plan_bits <- list(rollup = rbind(c(0L, 0L), c(0L, 1L), c(1L, 1L)), cube = rbind(c(0L, 0L), c(0L, 1L), c(1L, 0L), c(1L, 1L)))
rows <- list()
for (family in names(families)) {
  for (n in c(0L, 1L, 8L)) {
    x <- vctrs::vec_slice(families[[family]], seq_len(n))
    for (role in c("dimension", "fixed")) {
      dat <- tibble(key = x, g = rep(c("b", NA_character_, "a"), length.out = n), h = rep(c(2L, 1L), length.out = n), row = seq_len(n))
      before <- serialize(dat, NULL)
      for (plan_name in names(plan_bits)) {
        bits <- plan_bits[[plan_name]]
        dimensions <- if (role == "dimension") c("key", "g") else c("g", "h")
        by_quo <- if (role == "fixed") rlang::quo(key) else rlang::quo(NULL)
        grouping <- if (plan_name == "cube") cube(dplyr::all_of(dimensions)) else rollup(dplyr::all_of(dimensions))
        branches <- lapply(seq_len(nrow(bits)), function(k) {
          branch <- dat
          for (j in seq_along(dimensions)) {
            if (bits[k, j] == 1L) branch[[dimensions[[j]]]] <- vctrs::vec_init(dat[[dimensions[[j]]]], n)
          }
          mutate(branch, occurrence = k, bit1 = bits[k, 1L], bit2 = bits[k, 2L])
        })
        expected <- capture(bind_rows(branches))
        for (sort in c("none", "first", "last")) {
          reference <- capture({
            ordered <- expected
            if (sort != "none") {
              terms <- list()
              if (role == "fixed") terms <- list(row_missing(ordered$key), ordered$key)
              for (j in seq_along(dimensions)) {
                bit <- ordered[[paste0("bit", j)]]
                value <- ordered[[dimensions[[j]]]]
                terms <- c(terms, list(if (sort == "first") -bit else bit, row_missing(value), value))
              }
              terms <- c(terms, list(ordered$occurrence))
              # Value ordering is delegated; the component-wise missingness flags are independent.
              ordered <- arrange(ordered, !!!lapply(terms, rlang::new_quosure, env = emptyenv()))
            }
            ordered
          })
          got <- capture(expand_with_margins(dat, .by = !!by_quo, .grouping = grouping, .margin_label = NULL, .id = "occurrence", .sort = sort))
          status <- if (inherits(reference, "error")) "ordinary_order_error" else if (inherits(got, "error")) "package_error" else {
            comparison <- all.equal(as.list(arrange(got, occurrence, row))[names(dat)], as.list(arrange(reference, occurrence, row))[names(dat)])
            equal <- isTRUE(comparison)
            order_equal <- sort == "none" || identical(got$occurrence, reference$occurrence) && identical(got$row, reference$row)
            if (equal && order_equal) "match" else "difference"
          }
          rows[[length(rows) + 1L]] <- data.frame(family, n, role, plan_name, sort, status, detail = if (inherits(reference, "error")) conditionMessage(reference) else if (inherits(got, "error")) conditionMessage(got) else "")
          stopifnot(identical(serialize(dat, NULL), before))
        }
      }
    }
  }
}
results <- bind_rows(rows)
write.csv(results, file.path(out_dir, "structured-order.csv"), row.names = FALSE)
print(table(results$status))
print(results[!results$status %in% c("match", "ordinary_order_error"), ], row.names = FALSE)
