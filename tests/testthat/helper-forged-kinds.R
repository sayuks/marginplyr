# A Grouping specification kind carrying `kind` under a class whose methods are
# `methods`, so that classifying it reaches a method of the object's own rather
# than R's reading of a character vector. Every reader of a kind nothing has
# validated is asserted against one -- the two guards, the nested-name check,
# and the printed line -- and what each varies is the method and not the kind
# underneath: `set` is a kind all four accept, so a refusal, a declined reading,
# or an empty printed name is the classification's answer and not the name's.
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

# The method every site but one registers: a kind that raises instead of
# answering the question being put to it.
raising_kind_method <- function(x, ...) {
  rlang::abort("classifying this kind raises")
}
