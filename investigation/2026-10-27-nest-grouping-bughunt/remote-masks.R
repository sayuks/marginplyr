pkgload::load_all(".", quiet = TRUE)

set.seed(102727)

con <- DBI::dbConnect(duckdb::duckdb())

x <- tibble::tibble(a = c("x", "y"), b = c("u", "v"), c = c("p", "p"), v = 1:2)

remote <- dplyr::copy_to(con, x, "hunt_input", temporary = TRUE)

for (i in seq_len(80)) {
    leaves <- replicate(sample(1:5, 1), sample(c("a", "b", "c"), sample(0:3, 1)), simplify = FALSE)
    nodes <- lapply(leaves, function(z) rlang::inject(grouping_set(dplyr::all_of(!!z))))
    spec <- rlang::inject(grouping_sets(!!!nodes))
    plan <- inspect_grouping(x, .grouping = spec, .duplicates = "keep")
    for (policy in c("keep", "drop")) for (backend in c("duckdb", "dtplyr", "arrow")) {
        data <- switch(backend, duckdb = remote, dtplyr = dtplyr::lazy_dt(x), arrow = arrow::arrow_table(x))
        y <- dplyr::collect(summarize_with_margins(data, n = dplyr::n(), mask = grouping_id(),
            .grouping = spec, .duplicates = policy, .id = "set", .sort = "last"))
        p <- inspect_grouping(x, .grouping = spec, .duplicates = policy)
        stopifnot(identical(as.integer(y$mask), p$grouping_id[y$set]))
    }
}

DBI::dbDisconnect(con, shutdown = TRUE)

cat("PASS remote masks: 80 specifications x 2 policies x 3 backends\n")
