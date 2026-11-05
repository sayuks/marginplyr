# A dated, finite expansion comparison, not package regression tests.
pkgload::load_all(quiet = TRUE)
options(warn = 1)
`%||%` <- function(x, y) if (is.null(x)) y else x
Sys.setenv(TESTTHAT_PARALLEL = "false")

families <- list(
  integer = function(n) rep(c(2L, NA_integer_, 1L, 2L), length.out = n),
  double = function(n) rep(c(2, NaN, 1, NA_real_), length.out = n),
  character = function(n) rep(c("b", NA_character_, "a", "b"), length.out = n),
  logical = function(n) rep(c(TRUE, NA, FALSE, TRUE), length.out = n),
  complex = function(n) rep(c(2 + 1i, NA_complex_, 1 + 2i, 2 + 1i), length.out = n),
  factor = function(n) factor(rep(c("b", NA, "a", "b"), length.out = n),
                            levels = c("b", "a", "unused")),
  ordered = function(n) ordered(rep(c("b", NA, "a", "b"), length.out = n),
                              levels = c("b", "a", "unused")),
  factor_na_level = function(n) factor(rep(c("b", NA, "a", "b"), length.out = n),
                                    levels = c("b", NA, "a"), exclude = NULL),
  Date = function(n) as.Date(rep(c(2, NA, 1, 2), length.out = n), origin = "2000-01-01"),
  POSIXct = function(n) as.POSIXct(rep(c(2, NA, 1, 2), length.out = n),
                                 origin = "2000-01-01", tz = "Asia/Tokyo"),
  difftime = function(n) as.difftime(rep(c(2, NA, 1, 2), length.out = n), units = "days"),
  hms = function(n) hms::as_hms(rep(c(2, NA, 1, 2), length.out = n)),
  integer64 = function(n) bit64::as.integer64(rep(c(2, NA, 1, 2), length.out = n)),
  units = function(n) units::set_units(rep(c(2, NA, 1, 2), length.out = n), "m"),
  labelled = function(n) haven::labelled(rep(c(2, NA, 1, 2), length.out = n),
                                       labels = c(one = 1, two = 2)),
  list = function(n) rep(list(2L, NULL, NA_integer_, 1L), length.out = n),
  list_of = function(n) vctrs::list_of(!!!rep(list(2L, NULL, NA_integer_, 1L),
                                            length.out = n), .ptype = integer()),
  packed = function(n) tibble::tibble(
    a = rep(c(2L, NA_integer_, 1L, 2L), length.out = n),
    b = rep(c("z", NA_character_, "a", "b"), length.out = n)
  ),
  matrix = function(n) matrix(rep(c(2L, NA_integer_, 1L, 2L,
                                   1L, NA_integer_, 2L, NA_integer_),
                                 length.out = n * 2L), nrow = n, ncol = 2L)
)

plans <- list(
  rollup = list(spec = rlang::expr(rollup(g, h)),
                sets = list(c("g", "h"), "g", character())),
  mixed = list(spec = rlang::expr(grouping_sets(grouping_set(g, h),
                                               grouping_set(), grouping_set(h))),
               sets = list(c("g", "h"), character(), "h")),
  duplicate = list(spec = rlang::expr(grouping_sets(grouping_set(g, h),
                                                   grouping_set(g), grouping_set(g))),
                   sets = list(c("g", "h"), "g", "g"))
)

capture <- function(expr) tryCatch(list(value = force(expr)), error = function(e) {
  list(error = class(e)[[1L]], message = conditionMessage(e))
})
missing_rows <- function(x) {
  if (is.list(x) || is.array(x)) vctrs::vec_detect_missing(x) else is.na(x)
}
row_order <- function(x, sets, direction) {
  if (direction == "none") return(x)
  args <- list()
  args[[1L]] <- missing_rows(x$k)
  args[[2L]] <- x$k
  for (col in c("g", "h")) {
    bit <- vapply(x$sid, function(sid) !(col %in% sets[[sid]]), logical(1))
    args <- c(args, list(if (direction == "first") dplyr::desc(bit) else bit,
                        missing_rows(x[[col]]), x[[col]]))
  }
  args <- c(args, list(x$sid))
  dplyr::arrange(x, !!!args)
}

results <- list()
index <- 0L
for (family in names(families)) for (n in c(0L, 1L, 4L)) {
  for (role in c("dimension", "fixed", "payload")) {
    for (container in c("tibble", "data.frame", "data.table")) {
      d <- tibble::tibble(k = rep(c("b", NA, "a", "b"), length.out = n),
                          g = rep(c("y", NA, "x", "y"), length.out = n),
                          h = rep(c("q", "q", "p", NA), length.out = n),
                          row = seq_len(n), payload = seq_len(n))
      col <- switch(role, dimension = "g", fixed = "k", payload = "payload")
      d[[col]] <- families[[family]](n)
      if (container == "data.frame") d <- as.data.frame(d)
      if (container == "data.table") {
        source_names <- names(d)
        source_column <- d[[col]]
        d <- data.table::as.data.table(d)
        if (!identical(names(d), source_names) ||
              !identical(d[[col]], source_column)) {
          message("Excluded source conversion: ", family, " / ", n, " / ", role)
          next
        }
      }
      before <- serialize(d, NULL)
      for (plan in names(plans)) for (sort in c("none", "first", "last")) {
        sets <- plans[[plan]]$sets
        oracle <- capture({
          parts <- lapply(seq_along(sets), function(sid) {
            absent <- setdiff(c("g", "h"), sets[[sid]])
            replacements <- lapply(absent, function(key) vctrs::vec_init(d[[key]], n))
            names(replacements) <- absent
            dplyr::mutate(d, !!!replacements, sid = sid)
          })
          expected <- dplyr::bind_rows(parts)
          expected <- dplyr::select(expected, dplyr::all_of(c("k", "g", "h", "sid",
                                                             "row", "payload")))
          row_order(expected, sets, sort)
        })
        spec <- rlang::eval_bare(plans[[plan]]$spec)
        actual <- capture(expand_with_margins(d, .by = k, .grouping = spec,
                                              .margin_label = NULL, .duplicates = "keep",
                                              .sort = sort, .id = "sid"))
        verdict <- if (!is.null(oracle$error)) {
          if (!is.null(actual$error)) "ordinary control error" else "oracle error only"
        } else if (!is.null(actual$error)) {
          "expansion error only"
        } else if (!identical(names(actual$value), names(oracle$value)) ||
                     !isTRUE(all.equal(actual$value, oracle$value,
                                       check.attributes = TRUE))) {
          "difference"
        } else "match"
        stopifnot(identical(before, serialize(d, NULL)))
        index <- index + 1L
        results[[index]] <- data.frame(
          family = family, rows = n, role = role, container = container,
          plan = plan, sort = sort, verdict = verdict,
          control_error = oracle$error %||% "",
          expansion_error = actual$error %||% ""
        )
      }
    }
  }
}
results <- dplyr::bind_rows(results)
print(table(results$verdict))
print(results[results$verdict != "match", ], row.names = FALSE)
output <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", unset = tempdir())
dir.create(output, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(results, file.path(output, "local-oracle.csv"), row.names = FALSE)
cat("Comparisons:", nrow(results), "\n")
print(utils::sessionInfo())
stopifnot(!any(results$verdict %in% c("expansion error only", "difference")))
