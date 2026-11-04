# Replay from the source checkout root. --assert checks the desired result.
pkgload::load_all(quiet = TRUE)

observe <- function(source_form, destination_form, variant) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source_id <- DBI::Id(schema = "main", table = "facts")
  input <- data.frame(g = c("b", "a"))
  DBI::dbWriteTable(con, source_id, input)

  bare <- dplyr::tbl(con, "facts")
  source <- switch(
    source_form,
    bare = bare,
    filtered = dplyr::filter(bare, g != "absent"),
    opaque = dplyr::tbl(
      con, dbplyr::sql("SELECT g FROM facts"), vars = "g"
    ),
    qualified = dplyr::tbl(con, source_id)
  )
  destination <- switch(
    destination_form,
    same = "facts",
    case_equivalent = "FACTS",
    explicit_temp = DBI::Id(schema = "temp", table = "facts"),
    different = "expanded"
  )
  destination_id <- DBI::Id(
    schema = "temp",
    table = if (identical(destination_form, "different")) "expanded" else "facts"
  )
  sort <- if (identical(variant, "id")) "none" else "last"
  id <- if (identical(variant, "sorted")) NULL else "sid"

  query <- expand_with_margins(
    source, .grouping = rollup(g), .sort = sort, .id = id
  )
  expected <- data.frame(g = c("a", "b", "Total", "Total"))
  if (!is.null(id)) expected$sid <- c(1L, 1L, 2L, 2L)
  normalize <- function(result) {
    out <- as.data.frame(result)
    if (identical(sort, "none")) {
      out <- out[order(out$sid, out$g), , drop = FALSE]
      row.names(out) <- NULL
    }
    out
  }
  stopifnot(identical(normalize(dplyr::collect(query)), expected))

  computed <- tryCatch(dplyr::compute(query, name = destination),
                       error = identity)
  if (inherits(computed, "error")) {
    stopifnot(!DBI::dbExistsTable(con, destination_id))
    actual <- computed
  } else {
    actual <- normalize(dplyr::collect(computed))
    physical <- DBI::dbReadTable(con, destination_id)
    stopifnot(
      identical(names(physical), names(expected)),
      identical(nrow(physical), nrow(actual))
    )
    DBI::dbRemoveTable(con, destination_id)
  }
  stopifnot(identical(DBI::dbReadTable(con, source_id), input))

  # Ordinary dbplyr creates the destination and copies its source in one
  # statement. Run it after removing the probe's temporary destination.
  control <- dplyr::compute(dplyr::filter(source, g != "absent"),
                            name = destination)
  stopifnot(identical(sort(dplyr::collect(control)$g), sort(input$g)))

  list(actual = actual, expected = expected)
}

args <- commandArgs(trailingOnly = TRUE)
witness <- observe("bare", "same", "sorted")
cat("Materialized expansion rows:", nrow(witness$actual), "\n")
print(witness$actual)
cat("Expected rows, verified by direct collection:", nrow(witness$expected), "\n")
print(witness$expected)

if ("--matrix" %in% args) {
  cases <- expand.grid(
    source = c("bare", "filtered", "opaque", "qualified"),
    destination = c("same", "case_equivalent", "explicit_temp", "different"),
    variant = c("sorted", "id", "sorted_id"),
    stringsAsFactors = FALSE
  )
  cases$outcome <- vapply(seq_len(nrow(cases)), function(i) {
    result <- observe(cases$source[[i]], cases$destination[[i]],
                      cases$variant[[i]])
    if (inherits(result$actual, "error")) {
      return(conditionMessage(result$actual))
    }
    if (identical(result$actual, result$expected)) return("match")
    paste(nrow(result$actual), "rows")
  }, character(1))
  print(table(cases$outcome))
}
if ("--assert" %in% args) {
  stopifnot(identical(witness$actual, witness$expected))
}
