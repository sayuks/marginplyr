# The same call with transparent parentheses around its head, its whole, or
# both. Callers supply the head and leave its arguments unevaluated.
selection_call_spellings <- function(head, ...) {
  call <- rlang::call2(head, ...)
  wrapped <- rlang::call2(rlang::call2("(", head), ...)
  list(call, rlang::call2("(", call), wrapped,
       rlang::call2("(", rlang::call2("(", wrapped)))
}
