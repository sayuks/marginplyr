args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
near <- readRDS(file.path(args[[1L]], "values.rds"))
current <- readRDS(file.path(args[[2L]], "values.rds"))
stopifnot(identical(names(near), names(current)))
comparisons <- lapply(names(near), function(name) {
  a <- near[[name]]
  b <- current[[name]]
  data.frame(
    case = name,
    values_identical = identical(a, b),
    names_identical = identical(names(a), names(b)),
    rows_identical = identical(nrow(a), nrow(b)),
    types_identical = identical(vapply(a, typeof, character(1)),
                                vapply(b, typeof, character(1))),
    classes_identical = identical(lapply(a, class), lapply(b, class))
  )
})
comparisons <- do.call(rbind, comparisons)
utils::write.csv(comparisons, args[[3L]], row.names = FALSE)
print(comparisons, row.names = FALSE)
stopifnot(all(unlist(comparisons[-1L])))
