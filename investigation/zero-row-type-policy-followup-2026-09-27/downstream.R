suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
on.exit(DBI::dbDisconnect(con))
source <- dplyr::copy_to(
  con, data.frame(p = "A", g = "a", v = 1),
  "issue_706_downstream", temporary = TRUE
)
empty_source <- dplyr::filter(source, v < 0)

summarize <- function(x) {
  summarize_with_margins(
    x,
    nn = dplyr::n(),
    bit = grouping_bit(g),
    mask = grouping_id(),
    share = share_of_total(nn),
    .by = p,
    .grouping = rollup(g),
    .id = "sid",
    .margin_label = NULL,
    .check_share_source = FALSE
  )
}

query <- summarize(empty_source)
variants <- list(
  direct = query,
  filter = dplyr::filter(query, sid > 0),
  select_subset = dplyr::select(query, sid, bit, share),
  rename = dplyr::rename(query, sid2 = sid),
  mutate = dplyr::mutate(query, added = sid + 1L)
)
for (name in names(variants)) {
  cat("\n", name, "\n", sep = "")
  for (mode in c("collect", "compute_then_collect")) {
    result <- if (mode == "collect") {
      dplyr::collect(variants[[name]])
    } else {
      dplyr::collect(dplyr::compute(variants[[name]]))
    }
    cat(mode, "rows", nrow(result), "\n")
    print(vapply(result, typeof, character(1)))
  }
}

empty <- dplyr::collect(query)
populated <- dplyr::collect(summarize(source))
cat("\nbind_rows(empty, populated)\n")
print(vapply(dplyr::bind_rows(empty, populated), typeof, character(1)))
cat("\nbind_rows(empty, empty)\n")
print(vapply(dplyr::bind_rows(empty, empty), typeof, character(1)))
for (name in c("empty", "populated")) {
  x <- get(name)
  cat("\n", name, " numeric columns\n", sep = "")
  print(names(dplyr::select(x, dplyr::where(is.numeric))))
  cat(name, " across-created columns\n")
  print(setdiff(
    names(dplyr::mutate(
      x,
      dplyr::across(dplyr::where(is.numeric), identity,
                    .names = "{.col}_copy")
    )), names(x)
  ))
}

cat("\nversions\n")
print(vapply(
  c("dplyr", "dbplyr", "RSQLite", "DBI", "tibble"),
  function(x) as.character(utils::packageVersion(x)), character(1)
))
