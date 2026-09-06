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

# One wrapper per verb, forwarding both arguments with `{{ }}`, so that a test
# putting one expression through every verb writes it once. Each supplies
# whatever else its own signature requires and nothing more, which is why the
# two summary verbs carry a `total` and the other four take the data alone.
#
# Here rather than in the file that wanted it first, by the argument this
# file's header already makes: `test-grouping-plan.R` puts empty arguments
# through it and `test-grouping-backends.R` a refused input, and a per-file
# copy is what could disagree with `verbs_taking()` above while the other still
# matched it. Every caller holds the two to each other, so a seventh verb fails
# where it is used rather than arriving uncovered.
forwarded_verbs <- list(
  summarize_with_margins = function(data, by, grouping) {
    summarize_with_margins(
      data,
      total = sum(value),
      .by = {{ by }},
      .grouping = {{ grouping }}
    )
  },
  summarise_with_margins = function(data, by, grouping) {
    summarise_with_margins(
      data,
      total = sum(value),
      .by = {{ by }},
      .grouping = {{ grouping }}
    )
  },
  expand_with_margins = function(data, by, grouping) {
    expand_with_margins(data, .by = {{ by }}, .grouping = {{ grouping }})
  },
  nest_with_margins = function(data, by, grouping) {
    nest_with_margins(data, .by = {{ by }}, .grouping = {{ grouping }})
  },
  nest_by_with_margins = function(data, by, grouping) {
    nest_by_with_margins(data, .by = {{ by }}, .grouping = {{ grouping }})
  },
  inspect_grouping = function(data, by, grouping) {
    inspect_grouping(data, .by = {{ by }}, .grouping = {{ grouping }})
  }
)
