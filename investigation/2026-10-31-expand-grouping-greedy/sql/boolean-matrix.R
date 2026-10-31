pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
capture <- function(expr) tryCatch(force(expr), error = identity)
normalize <- function(x) {
  x <- as.data.frame(x)
  if (nrow(x)) x <- x[do.call(order, c(unname(x), list(na.last = TRUE))), , drop = FALSE]
  rownames(x) <- NULL
  x
}
rows <- list()
for (word in c("TRUE", "FALSE", "TrUe", "FaLsE")) for (role in c("payload", "dimension", "fixed", "identifier")) {
  for (empty in c(FALSE, TRUE)) for (label in list(NULL, "Total")) for (sort in c("none", "first", "last")) {
    input <- tibble(g = c("b", "a"), v = 1:2)
    dimension <- if (role == "dimension") word else "g"
    fixed <- if (role == "fixed") word else character()
    identifier <- if (role == "identifier") word else "sid"
    if (role == "dimension") names(input)[[1L]] <- word
    if (role %in% c("payload", "fixed")) input[[word]] <- c(7L, 8L)
    if (empty) input <- input[FALSE, ]
    src <- copy_to(con, input, "facts", overwrite = TRUE)
    margin <- expand_with_margins(
      src, .by = all_of(fixed), .grouping = rollup(all_of(dimension)),
      .id = identifier, .margin_label = label, .sort = sort
    )
    branch1 <- mutate(src, !!identifier := 1L)
    value <- if (is.null(label)) NA_character_ else label
    branch2 <- mutate(src, !!dimension := !!value, !!identifier := 2L)
    ordinary <- union_all(branch1, branch2)
    direct <- collect(margin)
    expected <- collect(ordinary)
    stopifnot(nrow(direct) == nrow(expected))
    local_expected <- expand_with_margins(
      input, .by = all_of(fixed), .grouping = rollup(all_of(dimension)),
      .id = identifier, .margin_label = label, .sort = sort
    )
    stopifnot(identical(normalize(direct), normalize(local_expected)))
    for (derive in c("head", "filter")) {
      transform <- if (derive == "head") function(x) head(x, 1L) else function(x) filter(x, TRUE)
      control <- capture(collect(transform(ordinary)))
      result <- capture(collect(transform(margin)))
      key <- paste(word, role, empty, if (is.null(label)) "NULL" else label, sort, derive, sep = "/")
      rows[[length(rows) + 1L]] <- data.frame(
        case = key,
        control = if (inherits(control, "error")) conditionMessage(control) else "success",
        result = if (inherits(result, "error")) conditionMessage(result) else "success",
        direct_rows = nrow(direct),
        control_rows = if (inherits(control, "error")) NA_integer_ else nrow(control),
        actual_rows = if (inherits(result, "error")) NA_integer_ else nrow(result)
      )
    }
  }
}
out <- do.call(rbind, rows)
write.csv(out, "/private/tmp/expand-grouping-20261031-boolean-matrix.csv", row.names = FALSE)
print(table(out$control, out$result))
DBI::dbDisconnect(con)
