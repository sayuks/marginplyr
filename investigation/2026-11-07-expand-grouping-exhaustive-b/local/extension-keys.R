args <- commandArgs(TRUE)
stopifnot(length(args) == 2L)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(args[[1L]], quiet = TRUE)
library(dplyr)
factories <- list(
  integer64 = function(n) bit64::as.integer64(rep(c("9007199254740993", NA_character_, "-9007199254740993", "0", "9007199254740994", "3", NA_character_), length.out = n)),
  labelled = function(n) haven::labelled(rep(c(2, NA, 1, 0, 2, 7, NA), length.out = n), labels = c(yes = 1, no = 2, missing = NA)),
  labelled_spss = function(n) haven::labelled_spss(rep(c(2, NA, 1, 99, 97, 7, NA), length.out = n), labels = c(yes = 1, no = 2), na_values = 99, na_range = c(97, 98)),
  tagged_na = function(n) haven::labelled(rep(c(2, haven::tagged_na("a"), 1, haven::tagged_na("z"), NA, 7, NA), length.out = n), labels = c(yes = 1, no = 2)),
  units = function(n) units::set_units(rep(c(2, NA, 1, 0, -1, 7, NA), length.out = n), "m"),
  hms = function(n) hms::hms(seconds = rep(c(2, NA, 1, 0, -1, 86401, NA), length.out = n)),
  difftime = function(n) as.difftime(rep(c(2, NA, 1, 0, -1, 7, NA), length.out = n), units = "weeks"),
  surv_right = function(n) survival::Surv(rep(c(2, NA, 1, 0, 9, 7, NA), length.out = n), event = rep(c(1, NA, 0, 1, 0, 1, NA), length.out = n)),
  surv_interval = function(n) survival::Surv(rep(c(2, NA, 1, 0, 9, 7, NA), length.out = n), time2 = rep(c(3, NA, 2, 1, 10, 8, NA), length.out = n), event = rep(c(3, NA, 0, 1, 2, 1, NA), length.out = n), type = "interval"),
  surv_multi = function(n) survival::Surv(rep(c(2, NA, 1, 0, 9, 7, NA), length.out = n), event = factor(rep(c("recovered", NA, "censored", "dead", "recovered", "censored", NA), length.out = n), levels = c("censored", "recovered", "dead"))),
  dense_matrix = function(n) Matrix::Matrix(matrix(rep(c(2, NA, 1, 0, -1, 7, NA), length.out = 2L * n), nrow = n, ncol = 2L), sparse = FALSE),
  sparse_matrix = function(n) Matrix::Matrix(matrix(rep(c(2, NA, 1, 0, -1, 7, NA), length.out = 2L * n), nrow = n, ncol = 2L), sparse = TRUE)
)
records <- list()
for (kind in names(factories)) for (n in c(0L, 1L, 7L)) for (depth in 0:2) for (role in c("dimension", "fixed", "payload")) for (label in c("null", "na", "text")) {
  x <- tryCatch({
    g <- factories[[kind]](n)
    for (j in seq_len(depth)) g <- tibble(inner = g, keeper = rep(c(1L, NA_integer_, 2L), length.out = n))
    tibble(g = g, a = rep(c("b", NA_character_, "a"), length.out = n), rid = seq_len(n))
  }, error = identity)
  for (sorting in c("none", "first", "last")) {
    add_record <- function(status, detail = "", immutable = TRUE) {
      records[[length(records) + 1L]] <<- data.frame(kind, n, depth, role, label, sort = sorting, status, immutable, detail)
    }
    if (inherits(x, "error")) { add_record("fixture_exclusion", conditionMessage(x)); next }
    before <- serialize(x, NULL)
    dimension <- if (role == "dimension") "g" else "a"
    fixed <- if (role == "fixed") "g" else character()
    margin_label <- switch(label, null = NULL, na = NA_character_, text = "All observations")
    expected <- tryCatch({
      y <- x
      if (label == "text") {
        x_included <- mutate(x, !!dimension := as.vector(as.character(.data[[dimension]]), mode = "character"))
        y <- mutate(y, !!dimension := rep(margin_label, n))
      } else {
        x_included <- x
        y <- mutate(y, !!dimension := !!vctrs::vec_init(x[[dimension]], n))
      }
      z <- union_all(mutate(x_included, sid = rep(1L, n)), mutate(y, sid = rep(2L, n)))
      z <- select(z, all_of(c(fixed, dimension, "sid", setdiff(names(x), c(fixed, dimension)))))
      if (sorting != "none") {
        terms <- list()
        for (k in fixed) terms <- c(terms, list(vctrs::vec_detect_missing(z[[k]]), z[[k]]))
        bit <- as.integer(z$sid == 2L)
        if (sorting == "first") bit <- -bit
        terms <- c(terms, list(bit, vctrs::vec_detect_missing(z[[dimension]]), z[[dimension]], z$sid))
        names(terms) <- paste0("k", seq_along(terms))
        order_table <- tibble::new_tibble(terms, nrow = nrow(z))
        order_table$row_origin <- seq_len(nrow(z))
        order_table <- arrange(order_table, !!!rlang::syms(names(terms)))
        z <- vctrs::vec_slice(z, order_table$row_origin)
      }
      z
    }, error = identity)
    by <- if (length(fixed)) rlang::sym(fixed) else NULL
    actual <- tryCatch(rlang::inject(expand_with_margins(x, .by = !!by, .grouping = rollup(!!rlang::sym(dimension)), .margin_label = margin_label, .id = "sid", .sort = sorting)), error = identity)
    structured <- is.data.frame(x[[dimension]]) || (is.array(x[[dimension]]) && length(dim(x[[dimension]])) > 1L)
    required_refusal <- role == "dimension" && label == "text" && structured
    status <- if (required_refusal && inherits(actual, "marginplyr_error")) "required_refusal" else if (required_refusal) "missing_refusal" else if (inherits(expected, "error")) "ordinary_error" else if (inherits(actual, "error")) "package_error" else if (identical(actual, expected)) "match" else "difference"
    detail <- if (inherits(actual, "error")) conditionMessage(actual) else if (inherits(expected, "error")) conditionMessage(expected) else if (status == "difference") paste(all.equal(actual, expected), collapse = "; ") else ""
    immutable <- identical(before, serialize(x, NULL))
    add_record(status, detail, immutable)
    if (status %in% c("package_error", "difference", "missing_refusal") || !immutable) saveRDS(list(x = x, role = role, label = label, sorting = sorting, actual = actual, expected = expected), paste0(args[[2L]], "-", length(records), ".rds"))
  }
}
result <- bind_rows(records)
write.csv(result, args[[2L]], row.names = FALSE)
print(table(result$status))
print(result[result$status %in% c("package_error", "difference", "missing_refusal"), c("kind", "n", "depth", "role", "label", "sort", "status")])
cat("Comparisons:", nrow(result), "\n")
print(sessionInfo())
