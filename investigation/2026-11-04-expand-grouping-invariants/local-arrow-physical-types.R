pkgload::load_all(".", quiet = TRUE)
library(dplyr, warn.conflicts = FALSE)
library(arrow, warn.conflicts = FALSE)

types <- list(
  int8 = int8(), uint8 = uint8(), uint32 = uint32(), uint64 = uint64(),
  binary = binary(), large_binary = large_binary(),
  fixed_binary = fixed_size_binary(3), map = map_of(utf8(), int32()),
  fixed_list = fixed_size_list_of(int32(), 2), duration_s = duration("s"),
  duration_ms = duration("ms"), duration_ns = duration("ns"),
  decimal32 = decimal32(8, 2), decimal64 = decimal64(15, 4),
  decimal128 = decimal128(25, 8), decimal256 = decimal256(45, 12),
  time32 = time32("ms"), time64 = time64("ns"),
  ts_us = timestamp("us", "Europe/Paris"), f32 = float32(), f16 = float16(),
  str = large_utf8(), date64 = date64(), null = null()
)

chars <- c("bbb", "aaa", NA_character_)
numbers <- c(8L, 5L, NA_integer_)
wide_numbers <- bit64::as.integer64(numbers)
dates <- as.Date(c("1969-12-31", "2000-01-01", NA))
fixtures <- list(
  uint64 = c("18446744073709551615", "9007199254740995", NA_character_),
  uint32 = c(4294967295, 1, NA_real_),
  binary = chars, large_binary = chars, fixed_binary = chars,
  duration_s = wide_numbers, duration_ms = wide_numbers,
  duration_ns = wide_numbers,
  decimal32 = c("54321.98", "0.01", NA_character_),
  decimal64 = c("54321.9876", "0.0001", NA_character_),
  decimal128 = c("12345678901234567.98765432", "0.00000001", NA_character_),
  decimal256 = c(
    "123456789012345678901234567890123.987654321012",
    "0.000000000001", NA_character_
  ),
  time64 = wide_numbers, ts_us = wide_numbers, f16 = c(0.5, 0.25, NA_real_),
  str = chars, date64 = dates
)
make_array <- function(name, type) {
  if (name == "map") {
    return(Array$create(list(
      data.frame(key = c("a", "b"), value = c(5L, 8L)),
      data.frame(key = "c", value = 1L),
      NULL
    ), type = type))
  }
  if (name == "fixed_list") {
    return(Array$create(list(1:2, 3:4, NULL), type = type))
  }
  if (name == "null") {
    return(Array$create(rep(NA, 3), type = type))
  }
  values <- fixtures[[name]]
  if (is.null(values)) {
    values <- numbers
  }
  Array$create(values)$cast(type)
}

# Compare exact Arrow values and physical types, without converting wide
# integers, decimals, timestamps or nested columns to R data frames.
canonical <- function(query) {
  collect(arrange(query, sid, rid), as_data_frame = FALSE)
}
matches <- logical()
for (name in names(types)) {
  type <- types[[name]]
  source <- Table$create(g = make_array(name, type), rid = 1:3)
  missing_value <- Scalar$create(NA, type = null())$cast(type)
  detail <- mutate(source, sid = 1L)
  for (role in c("dimension", "payload")) {
    if (role == "dimension") {
      other <- mutate(source, g = !!missing_value, sid = 2L)
      spec <- rollup(g)
    } else {
      other <- mutate(source, sid = 2L)
      spec <- grouping_sets(grouping_set(), grouping_set())
    }
    expected <- canonical(union_all(detail, other))
    actual <- canonical(expand_with_margins(
      source, .grouping = spec, .id = "sid",
      .duplicates = "keep", .margin_label = NULL
    ))
    matched <- setequal(names(actual), names(expected)) &&
      all(vapply(names(expected), function(col) {
        actual$GetColumnByName(col)$Equals(expected$GetColumnByName(col))
      }, logical(1)))
    matches <- c(matches, matched)
    cat(name, role, matched, "\n")
  }
}
cat("Exact Arrow array matches:", sum(matches), "of", length(matches), "\n")
stopifnot(all(matches))
