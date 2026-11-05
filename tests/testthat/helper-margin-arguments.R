# Build character arguments whose coercion differs from their stored values.
local_displayed_margin_arg <- function(frame = parent.frame()) {
  rlang::local_bindings(
    as.character.margin_argument_display = function(x, ...) {
      attr(x, "display", exact = TRUE)
    },
    .env = asNamespace("base")$.__S3MethodsTable__.,
    .frame = frame
  )
  function(value, display = "shown") {
    structure(value, class = "margin_argument_display", display = display)
  }
}
