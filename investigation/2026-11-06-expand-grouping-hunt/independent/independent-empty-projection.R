Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)

source <- data.table::data.table(region = c("west", "east"), value = c(17L, 23L))
before <- data.table::copy(source)
step <- dplyr::select(dtplyr::lazy_dt(source), dplyr::any_of("absent"))
stopifnot(identical(step$j, 0L), length(dplyr::tbl_vars(step)) == 0L)
eager <- dplyr::collect(step)
ordinary <- expand_with_margins(eager, .id = "occurrence")
direct <- dplyr::collect(expand_with_margins(dtplyr::lazy_dt(data.frame()), .id = "occurrence"))
stopifnot(nrow(eager) == 0L, ncol(eager) == 0L,
          identical(ordinary$occurrence, integer()), identical(direct$occurrence, integer()))
actual <- tryCatch(expand_with_margins(step, .id = "occurrence"), error = identity)
stopifnot(identical(source, before))
print(list(eager_control = ordinary, direct_control = direct,
           observed = if (inherits(actual, "error")) conditionMessage(actual) else actual))
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(!inherits(actual, "error"),
            identical(dplyr::collect(actual)$occurrence, integer()))
}
