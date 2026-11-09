Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_SOURCE", unset = getwd()), quiet = TRUE)
main <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  source <- dplyr::copy_to(con, data.frame(g = c("a", "b"), x = c(1L, 3L)), "facts")
  before <- dplyr::collect(source)
  summarize <- function(marker, members) {
    sql <- paste0("SUM(x) + UNNEST(CAST([", members,
                  "] AS INTEGER[])) + LENGTH('", marker, "')")
    dplyr::collect(marginplyr::summarize_with_margins(
      source, value = !!dbplyr::sql(sql), p = marginplyr::share_of_total(value),
      .grouping = marginplyr::rollup(g), .id = "set", .margin_label = NULL
    ))
  }
  legal <- dplyr::arrange(summarize("--", "10"), .data$set, .data$g)
  stopifnot(identical(as.double(legal$value), c(13, 15, 16)),
            isTRUE(all.equal(legal$p, c(13 / 16, 15 / 16, 1))))
  control <- tryCatch(summarize("xx", "10,20"), error = identity)
  stopifnot(inherits(control, "error"),
            grepl("exactly one value per grouping row", conditionMessage(control), fixed = TRUE))
  actual <- tryCatch(summarize("--", "10,20"), error = identity)
  stopifnot(identical(before, dplyr::collect(source)))
  print(actual)
  if (!inherits(actual, "error")) {
    stop("Documented cardinality refusal missing: standard-quoted '--' produced ", nrow(actual), " rows.")
  }
  stopifnot(grepl("exactly one value per grouping row", conditionMessage(actual), fixed = TRUE))
}
main()
