args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
evidence <- args[[1L]]
matrix <- readRDS(file.path(evidence, 'matrix-s3.rds'))
stopifnot(length(matrix) == 128L)
for (x in matrix) {
  stopifnot(x$outcome_equal, x$delivery_equal,
            identical(x$off$outcome, x$on$outcome),
            identical(x$off$delivered, x$on$delivered))
  for (summary in x$on$rows$conditions) {
    stopifnot(identical(names(summary), c('classes', 'message', 'phase', 'handling')),
              is.character(summary$message), is.null(attributes(summary$message)),
              all(vapply(summary$classes, function(classes) {
                is.character(classes) && is.null(attributes(classes))
              }, logical(1))))
  }
}
generic <- readRDS(file.path(evidence, 'generic-delivery.rds'))
stopifnot(length(generic$off$conditions) == 1L,
          length(generic$on$conditions) == 4L,
          identical(generic$off$classes, generic$on$classes),
          identical(generic$off$values, generic$on$values),
          generic$off$delivered_unchanged, generic$on$delivered_unchanged,
          identical(generic$on$rows$status, c('returned', 'prepared')),
          all(vapply(generic$on$rows$conditions, nrow, integer(1)) == 0L))
cat('S3 formatter/condition controls: 128 pairs passed.\n')
cat('General-condition defect capture: 1 off / 4 on; values and classes agree.\n')
formatter <- readRDS(file.path(evidence, 'formatter-generic.rds'))
stopifnot(length(formatter) == 8L,
          all(vapply(formatter, function(x) {
            x$outcome_equal && !x$delivery_equal &&
              identical(x$off$outcome, x$on$outcome) &&
              length(x$off$delivered) < length(x$on$delivered) &&
              !any(vapply(x$off$delivered, function(cnd) {
                identical(cnd$message, 'secondary formatter condition')
              }, logical(1))) &&
              any(vapply(x$on$delivered, function(cnd) {
                identical(cnd$message, 'secondary formatter condition') &&
                  identical(cnd$classes, c('simpleCondition', 'condition'))
              }, logical(1)))
          }, logical(1))))
minimal <- readRDS(file.path(evidence, 'formatter-generic-minimal.rds'))
stopifnot(length(minimal$off$conditions) == 1L,
          length(minimal$on$conditions) == 5L,
          minimal$off$primary_unchanged, minimal$on$primary_unchanged,
          identical(minimal$off$values, minimal$on$values),
          identical(minimal$off$classes, minimal$on$classes))
cat('Secondary formatter conditions: all 8 defect pairs reproduced.\n')
cat('Minimal secondary-formatter capture: 1 off / 5 on; primary warning unchanged.\n')
