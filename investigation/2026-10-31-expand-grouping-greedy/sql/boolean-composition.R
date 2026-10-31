pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
input <- tibble(g = "a", `TRUE` = 7L)
src <- copy_to(con, input, "facts")
ordinary <- union_all(src, mutate(src, g = "Total"))
margin <- expand_with_margins(src, .grouping = rollup(g))
stopifnot(nrow(collect(margin)) == 2L)
capture <- function(expr) tryCatch(force(expr), error = identity)
materialized <- compute(margin, name = "saved", analyze = FALSE)
stopifnot(identical(collect(head(src, 1L))[["TRUE"]], 7L))
stopifnot(identical(collect(head(materialized, 1L))[["TRUE"]], 7L))
cat("Source and explicitly materialized public-name controls passed.\n")
for (kind in c("ordinary", "margin")) {
  x <- if (kind == "ordinary") ordinary else margin
  for (shape in c("head", "filter", "grouped", "window", "union", "reexpand")) {
    value <- capture({
      derived <- switch(shape,
        head = head(x, 1L), filter = filter(x, TRUE),
        grouped = group_by(x, g),
        window = mutate(dbplyr::window_order(x, g), rn = row_number()),
        union = union_all(x, x),
        reexpand = expand_with_margins(x)
      )
      collect(derived)
    })
    cat(kind, shape, if (inherits(value, "error")) paste(class(value)[[1L]], gsub("\n", " ", conditionMessage(value))) else paste("success", nrow(value)), "\n")
  }
}
limited <- capture(collect(head(margin, 1L)))
filtered <- capture(collect(filter(margin, TRUE)))
passed <- !inherits(limited, "error") && !inherits(filtered, "error") &&
  identical(limited[["TRUE"]], 7L) && identical(filtered[["TRUE"]], c(7L, 7L)) &&
  identical(names(limited), names(collect(margin)))
is_direct <- utils::getFromNamespace("sqlite_typed_result_direct", "marginplyr")
cat("Direct companion", is_direct(margin),
    "derived companion", is_direct(head(margin, 1L)), "\n")
cat("Direct SQL:\n", as.character(dbplyr::sql_render(margin)), "\n")
cat("Derived SQL:\n", as.character(dbplyr::sql_render(head(margin, 1L))), "\n")
DBI::dbDisconnect(con)
if ("--assert" %in% commandArgs(trailingOnly = TRUE) && !passed) {
  stop("An ordinary head() over a valid Boolean-word expansion must remain executable.")
}
