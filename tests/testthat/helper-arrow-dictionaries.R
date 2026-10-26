# Dictionary values shared by the public-result and no-source-read tests.
arrow_dictionary_values <- function() {
  list(
    integer = c(2L, 1L, 2L, NA_integer_),
    double = c(2.5, 1.5, 2.5, NA_real_),
    logical = c(TRUE, FALSE, TRUE, NA),
    date = as.Date(c("2020-01-02", "2020-01-01", "2020-01-02", NA)),
    timestamp = as.POSIXct(
      c("2020-01-02 01:02:03", "2020-01-01 01:02:03",
        "2020-01-02 01:02:03", NA), tz = "UTC"
    ),
    text = c("b", "a", "b", NA_character_)
  )
}

# A narrow ordered index makes loss of dictionary metadata observable too.
# The query's empty case filters a populated table rather than an empty root.
arrow_dictionary_sources <- function(values, empty) {
  encoded <- arrow::call_function(
    "dictionary_encode", arrow::Array$create(values)
  )
  type <- arrow::dictionary(
    arrow::int8(), encoded$type$value_type, ordered = TRUE
  )
  encoded <- encoded$cast(type)
  populated <- arrow::Table$create(g = encoded, row_id = seq_along(values))
  table <- if (empty) populated$Slice(0L, 0L) else populated
  query <- suppressWarnings(dplyr::filter(
    populated, .data$row_id > !!if (empty) length(values) else 0L
  ))
  list(table = table, query = query,
       dataset = arrow::InMemoryDataset$create(table))
}
