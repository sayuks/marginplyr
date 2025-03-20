# create dummy data data for using in testthat context
get_data_dummy <- function(factor = FALSE) {
  # nolint start: line_length_linter
  data <- tibble::tribble(
    ~year,       ~id,   ~g1, ~g2, ~g3,           ~h1,           ~k1,           ~value,
    2022L,       "101", "B", "C", "RD220",       "JBB",         "WQ6",         NA_integer_,
    2022L,       "102", "B", "C", "RD220",       "SIO",         NA_character_, 69L,
    2022L,       "103", "B", "C", NA_character_, "YZU",         "WQ5",         60L,
    2022L,       "104", "B", "C", "RD260",       "JBB",         "HU1",         69L,
    2022L,       "105", "B", "C", "RD360",       "JBB",         NA_character_, 59L,
    2022L,       "106", "B", "C", "RD360",       "JBB",         "HU2",         93L,
    2022L,       "107", "B", "D", "RS690",       "SIO",         "CK1",         55L,
    2022L,       "108", "B", "D", "ZS700",       "SIO",         "CK3",         76L,
    2022L,       "108", "B", "D", "RS700",       "YZU",         "CK3",         NA_integer_,
    2022L,       "105", "B", "D", "ZD360",       "YAL",         "HU2",         80L,
    2022L,       "108", "B", "S", "AS620",       "JBB",         "CK3",         91L,
    2022L,       "109", "B", "S", "DS610",       "SIO",         "HU3",         72L,
    2022L,       "201", "A", "Q", "RD126",       "JBB",         "WQ4",         NA_integer_,
    2022L,       "202", "A", "Q", "RD126",       "KLS",         "WQ3",         74L,
    2022L,       "203", "A", "Q", "RD150",       NA_character_, "WQ3",         58L,
    2022L,       "204", "A", "Q", "RD150",       "KLS",         "WQ4",         86L,
    2022L,       "205", "A", "E", "RC210",       "YZU",         "WQ1",         80L,
    2022L,       "204", "A", "E", "RC360",       "JBB",         "WQ4",         75L,
    2023L,       "101", "B", "C", "RD220",       "JBB",         "WQ6",         80L,
    2023L,       "102", "B", "C", "RD220",       "SIO",         NA_character_, 52L,
    2023L,       "103", "B", "C", NA_character_, "YZU",         "WQ5",         33L,
    2023L,       "104", "B", "C", "RD260",       "JBB",         "HU1",         49L,
    2023L,       "105", "B", "C", "RD360",       "JBB",         NA_character_, 72L,
    2023L,       "106", "B", "C", "RD360",       "JBB",         "HU2",         68L,
    2023L,       "107", "B", "D", "RS690",       "SIO",         "CK1",         59L,
    2023L,       "108", "B", "D", "ZS700",       "SIO",         "CK3",         NA_integer_,
    2023L,       "108", "B", "D", "RS700",       "YZU",         "CK3",         31L,
    2023L,       "105", "B", "D", "ZD360",       "YAL",         "HU2",         37L,
    2023L,       "108", "B", "S", "AS620",       "JBB",         "CK3",         84L,
    2023L,       "109", "B", "S", "DS610",       "SIO",         "HU3",         95L,
    2023L,       "201", "A", "Q", "RD126",       "JBB",         "WQ4",         78L,
    2023L,       "202", "A", "Q", "RD126",       "KLS",         "WQ3",         31L,
    2023L,       "203", "A", "Q", "RD150",       NA_character_, "WQ3",         63L,
    2023L,       "204", "A", "Q", "RD150",       "KLS",         "WQ4",         82L,
    2023L,       "205", "A", "E", "RC210",       "YZU",         "WQ1",         68L,
    2023L,       "204", "A", "E", "RC360",       "JBB",         "WQ4",         61L,
    NA_integer_, "101", "B", "C", "RD220",       "JBB",         "WQ6",         100L,
  )
  # nolint end

  if (factor) {
    # g1 does not have NAs in its values, but can contain NA in the level
    data$g1 <- factor(data$g1, c("A", "B", "APL", "SSD", NA), exclude = NULL)
    # g2 has no NAs, but ordered in this example
    data$g2 <- factor(
      data$g2,
      c("Q", "E", "C", "D", "S", "APL", "SSD"),
      ordered = TRUE
    )
    # add level "(all)" to the last
    data$h1 <- factor(
      data$h1,
      c("JBB", "SIO", "KLS", "YZU", "YAL", "CKE", "(all)")
    )
    # include NA in the level (k1 has NAs in its values)
    # k1 is a ordered factor
    data$k1 <- factor(
      data$k1,
      c(
        "WQ1",
        "WQ2",
        "WQ3",
        "WQ4",
        "WQ5",
        "WQ6",
        "HU1",
        "HU2",
        "HU3",
        "CK1",
        "CK2",
        "CK3",
        NA
      ),
      exclude = NULL,
      ordered = TRUE
    )
  }

  data
}

# for easier debugging for lazy tables
#
# There is also a way to test lazy tables without collecting and reorder rows,
# but to make it easier to understand the difference between specific rows if
# test is failed, reorder rows (using all columns) and then collect. If two data
# to which this function is applied are equal, it means that the two data before
# this function is applied are equal, ignoring row order.
arrange_then_collect <- function(data) {
  data <- dplyr::arrange(.data = data, dplyr::pick(tidyselect::everything()))
  dplyr::collect(x = data)
}
