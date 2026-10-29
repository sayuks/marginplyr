# Dated investigation evidence; investigated 2026-10-29 on main 4c63bb9.
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1L]] else "."
pkgload::load_all(repo, quiet = TRUE)
checks <- 0L
for (width in c(0L, 1L, 30L, 31L, 32L, 33L, 64L, 65L)) {
  dimensions <- sprintf("d%02d", seq_len(width))
  if (width > 0L) dimensions[[1L]] <- ".env"
  if (width > 1L) dimensions[[width]] <- ".data"
  data <- as.data.frame(stats::setNames(rep(list(c(1L, 2L)), width), dimensions),
                        check.names = FALSE)
  if (!width) data <- data.frame(row.names = 1:2)
  data$fixed <- c("P", "Q")
  data$payload <- 1:2
  sets <- list(dimensions, dimensions[seq.int(1L, max(1L, width), by = 2L)],
               dimensions[-1L], character(), dimensions)
  if (!width) sets <- rep(list(character()), 5L)
  specifications <- lapply(sets, function(columns) grouping_set(all_of(!!columns)))
  spec <- grouping_sets(!!!specifications)
  masks <- lapply(sets, function(columns) {
    if (!width) return(integer())
    stats::setNames(as.integer(!dimensions %in% columns), dimensions)
  })
  ids <- vapply(masks, function(mask) {
    if (width > 31L) return(NA_integer_)
    if (!width) return(0L)
    as.integer(sum(mask * 2^rev(seq.int(0L, width - 1L))))
  }, integer(1))
  for (input in list(data, dtplyr::lazy_dt(data))) {
    result <- inspect_grouping(input, .by = fixed, .grouping = spec,
                               .duplicates = "keep", .format = "list")
    stopifnot(identical(result$included, sets),
              identical(result$grouping_bits, masks),
              identical(result$grouping_id, ids),
              identical(result$set_id, 1:5))
    checks <- checks + 1L

    selected <- head(dimensions, 31L)
    helper <- if (length(selected)) {
      rlang::call2("grouping_id", !!!rlang::syms(selected), .ns = "marginplyr")
    } else {
      quote(grouping_id())
    }
    summary <- summarize_with_margins(input, n = dplyr::n(),
                                     gid = !!helper, fixed_bit = grouping_bit(fixed),
                                     .by = fixed, .grouping = spec,
                                     .duplicates = "keep", .id = "set_id",
                                     .margin_label = NULL)
    if (inherits(summary, "dtplyr_step")) summary <- dplyr::collect(summary)
    expected <- vapply(sets, function(columns) {
      if (!length(selected)) return(0L)
      as.integer(sum(as.integer(!selected %in% columns) *
                       2^rev(seq.int(0L, length(selected) - 1L))))
    }, integer(1))
    stopifnot(identical(summary$gid, expected[summary$set_id]),
              all(summary$fixed_bit == 0L), all(summary$n == 1L),
              nrow(summary) == 10L)
    checks <- checks + 1L
    implicit <- tryCatch(summarize_with_margins(input, gid = grouping_id(),
                                               .by = fixed, .grouping = spec,
                                               .duplicates = "keep", .id = "set_id",
                                               .margin_label = NULL),
                         error = identity)
    if (width > 31L) {
      stopifnot(inherits(implicit, "marginplyr_error"),
                grepl("31", conditionMessage(implicit), fixed = TRUE))
    } else {
      if (inherits(implicit, "dtplyr_step")) implicit <- dplyr::collect(implicit)
      stopifnot(identical(implicit$gid, ids[implicit$set_id]))
    }
    checks <- checks + 1L

    for (nest in list(nest_with_margins, nest_by_with_margins)) {
      nested <- nest(input, .by = fixed, .grouping = spec, .duplicates = "drop",
                     .id = "set_id", .margin_label = NULL, .keep = TRUE)
      if (inherits(nested, "dtplyr_step")) nested <- dplyr::collect(nested)
      expected_sets <- sum(!duplicated(vapply(sets, paste, character(1), collapse = "|")))
      stopifnot(nrow(nested) == 2L * expected_sets)
      stopifnot(all(vapply(nested$data, nrow, integer(1)) == 1L))
      checks <- checks + 1L
    }
  }
}
cat("PASS:", checks, "wide identity / membership checks at 0, 1, 30, 31, 32, 33, 64 and 65 dimensions\n")
