# Every exported verb taking the argument named, derived from the signatures so
# that a seventh arrives at a caller as a wrapper a list is missing rather than
# as a position nothing covers. The six it answers today are the same six for
# `.by`, `.grouping`, and any other Margin argument, `summarise_with_margins()`
# included: the derivation reads exports rather than function objects, so a
# synonym that stopped being one would show as an entry the list no longer
# covers.
#
# Here rather than in whichever test file wanted it first, because testthat
# gives each file its own environment: `test-grouping-plan.R` builds one
# forwarding wrapper per verb from it, and `test-sent-queries.R` reads the
# entry points out of it, and a second copy is what would drift. That is the
# argument `helper-namespace-walk.R`'s header makes for the two readings it
# holds; this is a third reading and not one of those, being about a
# signature rather than about a body.
verbs_taking <- function(arg) {
  Filter(
    function(name) {
      object <- getExportedValue("marginplyr", name)
      is.function(object) && arg %in% names(formals(object))
    },
    getNamespaceExports("marginplyr")
  )
}
