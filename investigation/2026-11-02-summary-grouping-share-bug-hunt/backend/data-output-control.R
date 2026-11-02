pkgload::load_all(quiet = TRUE)
run <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- dplyr::copy_to(con, data.frame(g = c("a", "b"), x = c(1, 3)), "data_control")
  dots <- stats::setNames(list(rlang::quo(sum(x, na.rm = TRUE))), ".data")
  query <- dplyr::summarise(input, !!!dots)
  print(dplyr::collect(query))
  error <- tryCatch(dplyr::compute(query, name = "data_result"), error = identity)
  stopifnot(inherits(error, "error"), grepl("Can't select columns with `.data`", conditionMessage(error), fixed = TRUE))
  cat(conditionMessage(error), "\n")
}
run()
