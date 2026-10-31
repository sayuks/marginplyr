pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
options(warn = 1)
conns <- list(
  sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
  duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
)
observations <- list()
capture <- function(expr) tryCatch(force(expr), error = identity)
normalize <- function(x) {
  x <- as.data.frame(x)
  x[] <- lapply(x, function(col) {
    if (is.list(col)) vapply(col, function(v) paste(as.character(v), collapse = ","), "")
    else as.character(col)
  })
  if (nrow(x)) x <- x[do.call(order, c(unname(x), list(na.last = TRUE))), , drop = FALSE]
  rownames(x) <- NULL
  x
}
record <- function(key, result, expected) {
  status <- if (inherits(result, "error")) "error" else if (
    identical(normalize(result), normalize(expected))
  ) "match" else "different"
  observations[[length(observations) + 1L]] <<- data.frame(
    case = key, status = status,
    detail = if (inherits(result, "error")) conditionMessage(result) else ""
  )
  if (status != "match") cat(status, key, observations[[length(observations)]]$detail, "\n")
}
name_sets <- list(
  c("g", "h", "v"), c(".env", "TRUE", "v"),
  c("FALSE", ".env", "TRUE"), c("..1", "...", "v"),
  c("rowid", "_rowid_", "oid"), c("a b", "a\nb", "a`b"),
  c("..marginplyr_input_1", "..marginplyr_order_1", "..marginplyr_set_1")
)
derivations <- c("identity", "head", "filter", "select", "rename", "grouped", "window", "union", "join")
for (backend in names(conns)) for (ni in seq_along(name_sets)) {
  nms <- name_sets[[ni]]
  input <- tibble(g = c("b", "a", NA), h = c("x", NA, "y"), v = 1:3)
  names(input) <- nms
  DBI::dbWriteTable(conns[[backend]], paste0("facts", ni), as.data.frame(input), temporary = TRUE)
  src <- tbl(conns[[backend]], paste0("facts", ni), vars = nms)
  for (empty in c(FALSE, TRUE)) for (sort in c("none", "first", "last")) {
    base <- capture(if (empty) filter(src, FALSE) else src)
    if (inherits(base, "error")) {
      observations[[length(observations) + 1L]] <- data.frame(
        case = paste(backend, ni, empty, sort, "base"),
        status = "source-error", detail = conditionMessage(base)
      )
      next
    }
    first <- capture(expand_with_margins(
      base, .grouping = rollup(all_of(nms[[1L]])),
      .margin_label = NULL, .id = "occurrence", .sort = sort
    ))
    if (inherits(first, "error")) {
      record(paste(backend, ni, empty, sort, "first"), first, input)
      next
    }
    for (derive in derivations) {
      query <- capture(switch(derive,
        identity = first,
        head = head(first, 3L),
        filter = filter(first, .data[[nms[[3L]]]] > 1L),
        select = select(first, all_of(c(nms[[2L]], nms[[1L]], "occurrence", nms[[3L]]))),
        rename = rename(first, renamed = all_of(nms[[2L]])),
        grouped = group_by(first, across(all_of(nms[[2L]]))),
        window = mutate(dbplyr::window_order(first, occurrence, .data[[nms[[3L]]]]), rn = row_number()),
        union = union_all(first, first),
        join = left_join(first, first, by = c("occurrence", nms), relationship = "many-to-many", na_matches = "na")
      ))
      observed_source <- capture(collect(query))
      key <- paste(backend, ni, empty, sort, derive, sep = "/")
      if (inherits(observed_source, "error")) {
        observations[[length(observations) + 1L]] <- data.frame(case = key, status = "source-error", detail = conditionMessage(observed_source))
        next
      }
      grouping <- grouping_sets(grouping_set(), grouping_set(all_of(nms[[1L]])))
      expected <- capture(expand_with_margins(
        observed_source, .grouping = grouping, .id = "second", .margin_label = NULL,
        .sort = sort
      ))
      if (inherits(expected, "error")) {
        observations[[length(observations) + 1L]] <- data.frame(case = key, status = "oracle-error", detail = conditionMessage(expected))
        next
      }
      result <- capture(expand_with_margins(query, .grouping = grouping, .id = "second", .margin_label = NULL, .sort = sort))
      record(paste0(key, "/collect"), capture(collect(result)), expected)
      if (derive %in% c("identity", "head", "rename", "select")) {
        record(paste0(key, "/compute"), capture(collect(compute(result, analyze = FALSE))), expected)
      }
    }
  }
}
out <- do.call(rbind, observations)
write.csv(out, "/private/tmp/expand-grouping-20261031-sql-composition.csv", row.names = FALSE)
print(table(out$status))
for (con in conns) DBI::dbDisconnect(con)
