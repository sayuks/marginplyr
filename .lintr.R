# A markdown reference definition in roxygen, `#' [label]: <url>`, holds a URL
# that cannot wrap and a `# nolint` that roxygen would copy into the help page,
# so a link to a section of a site page has no spelling within 80 characters.
# Only a line that is nothing but such a definition leaves the limit, as ADR
# 0023's amendment for a markdown reference definition decides.
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
