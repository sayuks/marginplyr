# A collected query's rows as a data frame sorted by every column, for
# comparing two results whose row order is not part of what is compared.
collected_row_bag <- function(query) {
  result <- as.data.frame(dplyr::collect(query))
  result <- result[do.call(order, unname(as.list(result))), ]
  rownames(result) <- NULL
  result
}
