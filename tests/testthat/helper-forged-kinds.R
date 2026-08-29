# A Grouping specification kind carrying `kind` under a class whose methods are
# `methods`, so that a reader putting a question to the object rather than to
# its stripped value reaches one. Every reader of a kind nothing has validated
# is asserted against one -- the two guards, the nested-name check, and the
# printed line -- and what each varies is the method and not the kind
# underneath: `set` is a kind all four accept, so the answer each gives for
# `set` is what says no method was reached.
#
# Shared because the hazard is in the registration and not in the fixture.
# `registerS3method()` into base's namespace is what puts a method on a
# primitive, nothing takes it off again, and the class is what a method is
# looked up by -- so two sites that spelled one class name would each get
# whichever method was registered last, in whichever order testthat ran their
# files. `purpose` is what keeps them apart, and the generics are in the name
# so that one site can forge two objects answering different questions.
kind_answering <- function(methods, purpose, kind = "set") {
  class_name <- paste0(
    "marginplyr_kind_",
    purpose,
    "_",
    paste(sub(".", "_", names(methods), fixed = TRUE), collapse = "_")
  )
  for (generic in names(methods)) {
    registerS3method(
      generic,
      class_name,
      methods[[generic]],
      envir = asNamespace("base")
    )
  }
  structure(kind, class = class_name)
}

# The method every site but one registers: a kind that raises rather than
# answer, so that a reader reaching it fails visibly instead of agreeing with
# the reader that does not. Nothing asserts the message, since a reader that
# reaches this method is a reader whose test has already failed.
raising_kind_method <- function(x, ...) {
  rlang::abort("classifying this kind raises")
}
