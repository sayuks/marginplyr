# Investigated SHA: 1680498dbfc37296d0d20ea4352606c48441a16e
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A nested tidyr verb's column sharing an earlier summary's name is refused as a share-source dependency (#1844).
suppressMessages(pkgload::load_all(".", quiet = TRUE))
if (!requireNamespace("tidyr", quietly = TRUE)) {
  cat("tidyr is not installed; nothing was run.\n")
  quit(status = 0)
}
input <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
nested_t <- tibble::tibble(total = c(1, NA, 3), k = c("x", "y", "y"))
outcome <- function(call) {
  tryCatch(suppressWarnings(paste("x =", paste(eval(call)$x, collapse = ","))),
    marginplyr_error = function(e) {
      paste("Package refusal:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 96))
    }, error = function(e) paste("Error:", substr(conditionMessage(e), 1, 80)))
}
sources <- c(
  "nrow(dplyr::filter(nested_t, !is.na(total)))",
  "nrow(tidyr::uncount(nested_t, ifelse(is.na(total), 0, total)))",
  "nrow(tidyr::drop_na(nested_t, total))", "nrow(tidyr::fill(nested_t, total))",
  "nrow(tidyr::expand(nested_t, total))", "nrow(tidyr::complete(nested_t, total, k))",
  "ncol(tidyr::nest(nested_t, data = total))", "nrow(tidyr::pivot_longer(nested_t, total))",
  "nrow(tidyr::unite(nested_t, 'z', total, k))",
  "sum(tidyr::replace_na(nested_t, list(total = 0))$total)"
)
cat("Expected by the native control: each source evaluates against nested_t's own column.\n\n")
for (text in sources) {
  margin <- parse(text = sprintf(paste0(
    "summarize_with_margins(input, total = sum(v), x = %s, p = share_of_total(x), ",
    ".grouping = rollup(g))"
  ), text))[[1]]
  native <- parse(text = sprintf(
    "dplyr::summarise(dplyr::group_by(input, g), total = sum(v), x = %s)", text
  ))[[1]]
  cat(sprintf("%s\n    marginplyr: %s\n    native:     %s\n", text, outcome(margin), outcome(native)))
}
