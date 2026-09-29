# dbplyr 2.6.0 imports dplyr::filter_out(), first exported by dplyr 1.2.0.
# Keep our direct bound aligned with the version its namespace can load against.
test_that("required dplyr floor permits dbplyr 2.6.0 to load", {
  imports <- utils::packageDescription("marginplyr", fields = "Imports")
  expect_match(
    paste0(",", imports),
    ",\\s*dplyr\\s*\\(>=\\s*1\\.2\\.0\\)"
  )
})
