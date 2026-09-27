args <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages(pkgload::load_all(args[[1]], quiet = TRUE))
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
records <- list()
case_id <- 0L
table_id <- 0L
copy <- function(data) {
  table_id <<- table_id + 1L
  dplyr::copy_to(con, data, paste0("probe_", table_id), temporary = TRUE)
}
record <- function(key, query) {
  computed <- dplyr::compute(query)
  results <- list(
    collect = dplyr::collect(query),
    collect1 = dplyr::collect(query, n = 1L),
    collect0 = dplyr::collect(query, n = 0L),
    compute = dplyr::collect(computed),
    compute1 = dplyr::collect(computed, n = 1L)
  )
  for (boundary in names(results)) {
    records[[paste(key, boundary, sep = "/")]] <<- results[[boundary]]
  }
}
for (empty in c(FALSE, TRUE)) {
  source <- copy(data.frame(part = "A", g = "a", v = 2))
  if (empty) source <- dplyr::filter(source, v < 0)
  for (id in list(NULL, "sid")) for (sort in c("none", "first", "last")) {
    for (label in list(NULL, "Total")) for (plan in c("one", "rollup")) {
      case_id <- case_id + 1L
      spec <- if (plan == "one") grouping_set(g) else rollup(g)
      query <- summarize_with_margins(
        source, nn = dplyr::n(), bit = grouping_bit(g), mask = grouping_id(),
        wrapped = as.integer(grouping_id()), .by = part, .grouping = spec,
        .id = id, .sort = sort, .margin_label = label
      )
      record(paste0("helper-", case_id), query)
    }
  }
}
for (missing in list(NA_character_, NA_integer_, NA_real_)) {
  source <- copy(data.frame(g = missing, v = 1))
  for (sort in c("none", "first", "last")) {
    for (verb in c("summary", "expand")) for (plan in c("one", "rollup")) {
      case_id <- case_id + 1L
      spec <- if (plan == "one") grouping_set(g) else rollup(g)
      query <- if (verb == "summary") {
        summarize_with_margins(source, z = sum(v), bit = grouping_bit(g),
          mask = grouping_id(), .grouping = spec, .id = "sid",
          .margin_label = NULL, .sort = sort)
      } else {
        expand_with_margins(source, .grouping = spec, .id = "sid",
          .margin_label = NULL, .sort = sort)
      }
      record(paste0("missing-key-", case_id), query)
    }
  }
}
for (fixture in c("missing", "zero")) {
  source <- copy(if (fixture == "missing") {
    data.frame(part = c("A", "B"), g = c("a", "b"), v = c(NA_real_, 1))
  } else {
    data.frame(part = c("A", "A", "B"), g = c("x", "y", "z"), v = c(2, -2, 3))
  })
  for (sort in c("none", "first", "last")) for (label in list(NULL, "Total")) {
    for (id in list(NULL, "sid")) {
      case_id <- case_id + 1L
      query <- summarize_with_margins(source, z = mean(v),
        parent = share_of_parent(z), total = share_of_total(z),
        dplyr::across(z, share_of_total, .names = "share_{.col}"),
        bit = grouping_bit(g), mask = grouping_id(), .by = part,
        .grouping = rollup(g), .id = id, .sort = sort,
        .margin_label = label, .check_share_source = FALSE)
      record(paste0("share-", case_id), query)
    }
  }
}
for (id in list(NULL, "sid")) {
  source <- copy(data.frame(part = "A", g = "a", v = 2))
  source <- dplyr::filter(source, v < 0)
  case_id <- case_id + 1L
  # jarl-ignore unnecessary_parentheses: Nested pairs are syntax under investigation.
  query <- summarize_with_margins(source,
    ((grouping_id())), qualified = (marginplyr::grouping_id)(),
    bit = ((grouping_bit))(g), text = as.character(grouping_id()),
    wrapped = as.integer(grouping_id()), nn = dplyr::n(),
    dplyr::across(v, ~ grouping_id(), .names = "from_across"),
    .grouping = grouping_set(g), .by = part, .id = id,
    .margin_label = NULL)
  record(paste0("spelling-", case_id), query)
}
for (sort in c("none", "last")) for (id in list(NULL, "sid")) {
  source <- copy(data.frame(g = "a", v = 2))
  source <- dplyr::filter(source, v < 0)
  case_id <- case_id + 1L
  query <- summarize_with_margins(source, z = sum(v),
    parent = share_of_parent(z), total = share_of_total(z),
    bit = grouping_bit(g), mask = grouping_id(), .grouping = rollup(g),
    .id = id, .sort = sort, .margin_label = NULL,
    .check_share_source = FALSE)
  record(paste0("empty-input-root-", case_id), query)
}
saveRDS(records, args[[2]])
DBI::dbDisconnect(con)
cat("queries:", case_id, "observations:", length(records), "\n")
