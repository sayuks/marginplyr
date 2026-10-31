pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
source <- copy_to(con, tibble(g = c("a", "b"), v = 1:2, .env = c("x", "y")), "facts")
expanded <- expand_with_margins(source, .grouping = rollup(g), .margin_label = NULL)
requests <- list(
  null_entry = list(NULL), empty_entry = list(character()), numeric = list(1),
  logical_empty = list(logical()), missing = list(NA_character_),
  named = c(alias = "v"), nested = list(list("v")),
  expression = list(quote(v)), logical = list(TRUE), empty_list = list(list())
)
for (name in names(requests)) for (arg in c("indexes", "unique_indexes")) {
  for (kind in c("ordinary", "expansion")) {
    destination <- paste(name, arg, kind, sep = "_")
    copy_to(con, tibble(sentinel = 777L), destination)
    args <- list(x = if (kind == "ordinary") select(source, -all_of(".env")) else expanded,
                 name = destination, overwrite = TRUE, analyze = FALSE)
    args[[arg]] <- requests[[name]]
    value <- tryCatch(do.call(compute, args), error = identity)
    changed <- !identical(DBI::dbReadTable(con, destination), data.frame(sentinel = 777L))
    cat(name, arg, kind, if (inherits(value, "error")) class(value)[[1L]] else "success",
        "changed", changed,
        if (inherits(value, "error")) gsub("\n", " ", conditionMessage(value)) else "", "\n")
  }
}
DBI::dbDisconnect(con)
