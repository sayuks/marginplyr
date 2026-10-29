pkgload::load_all(quiet = TRUE)
results <- list()
for (size in c(0L, 1L, 3L)) {
  for (has_na_level in c(FALSE, TRUE)) {
    for (kind in c('matrix', 'array')) {
      for (label_name in c('NULL', 'NA')) {
        shape <- if (kind == 'matrix') c(size, 1L) else c(size, 1L, 1L)
        declared <- if (has_na_level) c('a', 'b', NA_character_) else c('a', 'b')
        key <- factor(rep(c('a', 'b', NA_character_), length.out = size),
                      levels = declared, exclude = if (has_na_level) NULL else NA)
        dim(key) <- shape
        input <- tibble::tibble(g = key, row = seq_len(size))
        label <- if (label_name == 'NULL') NULL else NA_character_
        case <- paste(size, has_na_level, kind, label_name, sep = '/')
        first <- dplyr::mutate(input, sid = 1L)
        missing <- dplyr::mutate(input, g = vctrs::vec_init(g, size), sid = 2L)
        expected <- dplyr::union_all(first, missing)
        stopifnot(nrow(expected) == 2L * size,
                  identical(dim(expected$g), c(2L * size, shape[-1L])),
                  identical(levels(expected$g), declared),
                  identical(expected$row, rep(seq_len(size), 2L)),
                  identical(expected$sid, rep(1:2, each = size)),
                  identical(as.integer(expected$g),
                            c(as.integer(key), rep(NA_integer_, size))))
        included <- tryCatch(
          expand_with_margins(input, .grouping = grouping_set(g),
                              .margin_label = label, .id = 'sid'),
          error = identity
        )
        actual <- tryCatch(
          expand_with_margins(input, .grouping = rollup(g),
                              .margin_label = label, .id = 'sid'),
          error = identity
        )
        declared_refusal <- has_na_level && label_name == 'NA'
        if (declared_refusal) {
          stopifnot(inherits(included, 'marginplyr_error'),
                    grepl('already a factor level', conditionMessage(included), fixed = TRUE))
          correct_refusal <- inherits(actual, 'marginplyr_error') &&
            grepl('already a factor level', conditionMessage(actual), fixed = TRUE)
          verdict <- if (correct_refusal) 'expected NA-level refusal' else
            paste('expected refusal; actual', class(actual)[[1L]])
        } else {
          stopifnot(!inherits(included, 'error'),
                    identical(included$g, key),
                    identical(included$row, seq_len(size)),
                    identical(included$sid, rep(1L, size)))
          verdict <- if (inherits(actual, 'error')) class(actual)[[1L]] else 'success'
        }
        results[[case]] <- verdict
        cat(case, ': ordinary union passed; ', verdict, '\n', sep = '')
      }
    }
  }
}
cat('COUNTS\n')
print(table(unlist(results)))
if ('--assert' %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(sum(unlist(results) == 'success') == 18L,
            sum(unlist(results) == 'expected NA-level refusal') == 6L)
}
