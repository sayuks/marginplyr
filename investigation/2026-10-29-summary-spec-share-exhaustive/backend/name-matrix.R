pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
data.table::setDTthreads(1L)
cons <- list(
  sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
  duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
)
DBI::dbExecute(cons$duckdb, "SET threads = 1")
data <- tibble(g = c("b", "a", "b"), h = c("z", "y", "y"), v = c(2, 3, 5))
inputs <- list(local = data, dt = dtplyr::lazy_dt(data))
for (backend in names(cons)) {
  inputs[[backend]] <- copy_to(cons[[backend]], data, "name_matrix")
}
spellings <- c(
  "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_", "NaN",
  "Inf", "NULL", "TRUE", "FALSE", "T", "F", "if", "else", "repeat", "while",
  "function", "for", "in", "next", "break", "...", "..0", "..00", "..01",
  "..-0", "..+0", "..+1", "..-1", ".. 1", ".", ".data", ".env", ".BY",
  ".N", ".I", ".SD", ".GRP", ".NGRP", ".SDcols", ".EACHI", "value",
  "rowid", "oid", "_rowid_", "select", "from", "group", "order", "sum",
  "n", "get", "c", "a\nb", "a\rb", "a\tb", "a`b", "a\"b", "a'b",
  "a\\b", "地域", "é", "e\u0301", "\u200b", "\u00a0", "\u2028"
)
failures <- list()
checks <- 0L
run <- function(input, name, role, sort, duplicates) {
  output <- if (role == "source") name else "amount"
  share_output <- if (role == "share") name else "fraction"
  dots <- list(rlang::quo(sum(v, na.rm = TRUE)))
  names(dots) <- output
  if (role != "summary" && role != "identifier") {
    dots[[share_output]] <- rlang::new_quosure(
      rlang::call2("share_of_total", rlang::sym(output)), globalenv()
    )
  } else if (role == "summary") {
    names(dots) <- name
  }
  query <- summarize_with_margins(
    input, !!!dots, .grouping = rollup(g, h), .sort = sort,
    .duplicates = duplicates, .id = if (role == "identifier") name else "sid",
    .check_share_source = FALSE
  )
  as.data.frame(collect(query))
}
for (name in spellings) {
  for (role in c("summary", "source", "share", "identifier")) {
    for (sort in c("none", "last")) {
      for (duplicates in c("drop", "keep")) {
        results <- lapply(inputs, function(input) {
          tryCatch(run(input, name, role, sort, duplicates), error = identity)
        })
        reference <- results$local
        for (backend in names(inputs)[-1L]) {
          checks <- checks + 1L
          actual <- results[[backend]]
          if (inherits(reference, "error")) {
            next
          }
          difference <- if (inherits(actual, "error")) {
            conditionMessage(actual)
          } else {
            if (sort == "none") {
              actual <- actual[vctrs::vec_order(actual), , drop = FALSE]
              reference <- reference[vctrs::vec_order(reference), , drop = FALSE]
            }
            rownames(actual) <- NULL
            rownames(reference) <- NULL
            all.equal(actual, reference, check.attributes = FALSE)
          }
          if (!isTRUE(difference)) {
            key <- paste(encodeString(name), role, sort, duplicates, backend, sep = "/")
            failures[[key]] <- list(actual = actual, expected = reference, difference = difference)
            cat("SIGNAL", key, paste(difference, collapse = "; "), "\n")
          }
        }
      }
    }
  }
}
for (con in cons) DBI::dbDisconnect(con)
cat("CHECKS", checks, "SIGNALS", length(failures), "\n")
write.csv(data.frame(
  case = names(failures),
  difference = vapply(failures, function(value) {
    paste(value$difference, collapse = "; ")
  }, character(1))
), "investigation/2026-10-29-summary-spec-share-exhaustive/backend/name-signals.csv", row.names = FALSE)
