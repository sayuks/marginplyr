script <- normalizePath(sub('^--file=', '', grep('^--file=', commandArgs(), value = TRUE)))
evidence_dir <- dirname(script)
source(file.path(evidence_dir, 'frame-casts.R'))
results <- list()
i <- 0L
for (input in c('data.frame', 'tibble', 'data.table')) {
  original <- switch(input, data.frame = as.data.frame(d), tibble = d,
                     data.table = data.table::as.data.table(d))
  for (plan in c('fixed', 'included', 'rollup')) {
    for (selected in c(FALSE, TRUE)) {
      expr <- if (selected) quote(frame_from_pick(pick(x))) else quote(make_distance(x))
      controls <- if (plan == 'fixed') list(.by = quote(g)) else
        list(.grouping = if (plan == 'included') quote(grouping_set(g)) else quote(rollup(g)))
      actual <- rlang::eval_tidy(rlang::call2(summarize_with_margins, original, !!!list(expr), !!!controls))
      expected <- rlang::eval_tidy(rlang::call2(dplyr::summarise, original, !!!list(expr), .by = quote(g)))
      if (plan == 'rollup') {
        root <- rlang::eval_tidy(rlang::call2(dplyr::summarise, original, !!!list(expr)))
        root$g <- NA_integer_
        expected <- dplyr::bind_rows(expected, root)
      }
      i <- i + 1L
      wrong <- !identical(actual$distance, expected$distance)
      stopifnot(wrong)
      results[[i]] <- data.frame(input, plan, selected, wrong,
                                 expected=paste(expected$distance,collapse=','),
                                 actual=paste(actual$distance,collapse=','))
      named <- rlang::call2(summarize_with_margins, original, result = expr, !!!controls)
      actual_named <- rlang::eval_tidy(named)
      stopifnot(identical(actual_named$result$distance, expected$distance))
    }
  }
}
results <- do.call(rbind, results)
write.csv(results,file.path(evidence_dir, 'frame-cast-matrix.csv'),row.names=FALSE)
print(results)
cat('MATRIX',nrow(results),'wrong-value cases and named controls confirmed\n')
