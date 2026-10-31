pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
input <- data.frame(g = c("b", "a"), h = c("x", "y"), v = 1:2)
names(input) <- c(".env", "TRUE", "v")
DBI::dbWriteTable(con, "facts", input)
src <- tbl(con, "facts")
ordinary <- union_all(src, mutate(src, .env = NA_character_))
margin <- expand_with_margins(src, .grouping = rollup(all_of(".env")), .margin_label = NULL)
for (kind in c("ordinary", "margin")) {
  x <- if (kind == "ordinary") ordinary else margin
  for (shape in c("identity", "head", "filter", "select", "rename", "grouped", "window", "union", "join")) {
    result <- tryCatch({
      derived <- switch(shape,
        identity = x, head = head(x, 2L), filter = filter(x, v > 1L),
        select = select(x, all_of(c("TRUE", ".env", "v"))),
        rename = rename(x, renamed = all_of("TRUE")),
        grouped = group_by(x, across(all_of("TRUE"))),
        window = mutate(dbplyr::window_order(x, v), rn = row_number()),
        union = union_all(x, x),
        join = left_join(x, x, by = c(".env", "TRUE", "v"), na_matches = "na")
      )
      collect(derived)
    }, error = identity)
    cat(kind, shape, if (inherits(result, "error")) paste(class(result)[[1L]], gsub("\n", " ", conditionMessage(result))) else "success", "\n")
  }
}
plain <- copy_to(con, tibble(g = c("a", "b"), v = 1:2), "plain")
ordinary <- arrange(union_all(plain, mutate(plain, g = NA_character_)), v)
margin <- expand_with_margins(plain, .grouping = rollup(g), .margin_label = NULL, .sort = "last")
for (kind in c("ordinary", "margin")) {
  x <- if (kind == "ordinary") ordinary else margin
  result <- tryCatch(collect(union_all(x, x)), error = identity)
  cat("ordered union", kind, if (inherits(result, "error")) gsub("\n", " ", conditionMessage(result)) else "success", "\n")
}
DBI::dbDisconnect(con)
