# Clause depth is observed through public SQL rendering: an input selection
# must be nested more deeply than the UNION that copies its selected rows.
expansion_clause_depths <- function(sql, pattern) {
  positions <- gregexpr(pattern, sql, perl = TRUE)[[1L]]
  vapply(positions[positions > 0L], function(position) {
    prefix <- strsplit(substr(sql, 1L, position - 1L), "", fixed = TRUE)[[1L]]
    sum(prefix == "(") - sum(prefix == ")")
  }, integer(1))
}

test_that("PostgreSQL selection clauses stay inside expansion operands", {
  source <- dplyr::tbl(
    dbplyr::simulate_postgres(), "facts", vars = c("g", "v")
  )
  ordered <- dplyr::arrange(source, .data$v)
  inputs <- list(head(source, 2L), ordered, head(ordered, 2L),
                 dplyr::collapse(head(ordered, 2L)))
  for (input in inputs) {
    for (spec in list(grouping_sets(grouping_set(), grouping_set("g")),
                      rollup("g"))) {
      for (label in list(NULL, "Total")) {
        query <- expand_with_margins(
          input, .grouping = spec, .margin_label = label, .id = "set"
        )
        sql <- as.character(dbplyr::sql_render(query))
        unions <- expansion_clause_depths(sql, "\\bUNION ALL\\b")
        clauses <- expansion_clause_depths(sql, "\\b(?:ORDER BY|LIMIT)\\b")
        expect_true(length(unions) > 0L)
        expect_true(length(clauses) > 0L)
        expect_gt(min(clauses), min(unions))
      }
    }
  }
})
