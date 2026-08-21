# The two readings every structural gate in this suite is built out of: the
# names of the functions a namespace binds, and the recursion over one parsed
# body.
#
# A structural gate asserts a property of every call site rather than of any one
# call -- that no function unquotes inside the `.data` pronoun, that only
# `compile_grouping_spec()` reaches the plan compiler, that every
# `abort_marginplyr()` template is authored in the source, that every walk over
# a call's arguments reads them by subscript, that every pluralizing diagnostic
# is pinned. Each of those reads the loaded namespace, because `R/` is not
# installed beside the tests and a scan of the shipped package would find no
# source to read; each therefore needs the package loaded through
# `pkgload::load_all()` or an installed dev build. Why a gate reads the
# namespace at all is the gate's own to state, and each of them states it.
#
# They live here rather than in whichever gate was written first because
# testthat gives each test file its own environment: a second gate cannot see a
# reader the first one defined, so the only two options were a helper or a copy.
# `test-namespace-walk.R` is what keeps the copy from coming back.

# The names of every function bound in a namespace.
#
# Names rather than the functions themselves, because `body()` is all a gate
# reads and `get()` is one call away, while two shapes would be two things to
# keep in step. The default is the package's own namespace; a gate proving its
# matcher works passes a synthetic one instead.
namespace_functions <- function(ns = asNamespace("marginplyr")) {
  Filter(
    function(binding) is.function(get(binding, envir = ns)),
    ls(ns, all.names = TRUE)
  )
}

# `fn` applied to every call below `expr`, including `expr` itself and including
# each call's head -- a head that is not a symbol is a call like any other, and
# a gate reading call targets has to see it.
#
# A parsed call can hold the missing-argument placeholder as one of its own
# elements: a `switch()` fallthrough branch such as `switch(x, a = , b = f())`
# and a subsetting call such as `x[i, ]` both produce one. Reading that
# placeholder through an ordinary `for` loop raises "argument is missing, with
# no default" the moment the loop variable is looked up, even though nothing
# about the walk is actually missing an argument: a `for` loop's own assignment
# preserves the internal missing flag, and normal symbol lookup checks for it.
# Passing the same element as a function argument does not carry the flag
# forward, which is why the recursion goes through `lapply()` and binds no
# element of a call to a name. Reading an element by subscript is safe too, so
# `fn` may take one apart however it needs to; it is the binding that is the
# hazard, and this is the one place the recursion has to avoid it.
visit_calls <- function(expr, fn) {
  if (!is.call(expr)) {
    return(invisible(NULL))
  }
  fn(expr)
  lapply(as.list(expr), visit_calls, fn = fn)
  invisible(NULL)
}
