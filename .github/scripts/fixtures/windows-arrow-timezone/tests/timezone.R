values <- as.POSIXct("2020-01-02 01:02:03", tz = "UTC")
source <- arrow::Table$create(g = arrow::call_function(
  "dictionary_encode", arrow::Array$create(values)
))
# Arrow exposes dictionary values as R factors while collecting them.
actual <- suppressWarnings(dplyr::collect(dplyr::mutate(
  source, g = as.character(g)
)))
stopifnot(
  is.character(actual$g),
  identical(substr(actual$g, 1L, 19L), "2020-01-02 01:02:03")
)
