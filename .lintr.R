# A roxygen line that is nothing but a markdown reference definition,
# `#' [label]: <url>`, leaves `line_length_linter()`'s limit; every other line,
# and every other linter on that one, is measured as before. ADR 0023's
# amendment *a markdown reference definition leaves the line limit* decides it.
linters <- local({
  reference_definition <- "^\\s*#' +\\[[^]]+\\]: +https://\\S+$"
  line_length <- lintr::line_length_linter()
  lintr::linters_with_defaults(
    line_length_linter = lintr::Linter(
      function(source_expression) {
        Filter(
          function(lint) !grepl(reference_definition, lint$line, perl = TRUE),
          line_length(source_expression)
        )
      },
      linter_level = "file"
    )
  )
})
