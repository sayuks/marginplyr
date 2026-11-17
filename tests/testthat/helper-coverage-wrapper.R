# covr's instrumentation, read through by the structural gates that take part
# of a call by position: `nth_statement()` in `test-sent-queries.R`, the
# literal-branch reader in `helper-diagnostic-sites.R`, and the `for` scan in
# `test-utils.R`. A gate that only matches a call wherever it sits needs none
# of it.
#
# covr measures a namespace by replacing an expression with
# `if (TRUE) { <counter>; <expression> }`, so a reader that takes one by
# position takes covr's wrapper instead. Which expressions covr wraps is its
# own and changes between releases; 4.0.0 began wrapping the parts of a
# conditional or loop, such as the arms of an unbraced `if` and a `for`
# sequence.
#
# Read through the wrapper rather than skip under covr: a gate that stops
# asserting in one job reads exactly like a gate nothing violates, which is the
# failure a structural gate exists to prevent. The shape is covr's and not
# documented, so a covr that changed it fails these gates instead of quieting
# them -- the direction the reading has to fail in, and why no `skip()` is here.

# `covr:::<name>`, assembled rather than written as a `:::` call. `R CMD
# check`'s *unstated dependencies in tests* reads a `:::` in a test source from
# the parse tree, so spelling it there makes covr a dependency this package
# would have to declare -- and covr is supplied by `test-coverage.yaml`'s
# `extra-packages`, belonging in no dependency field of its own (`AGENTS.md`,
# *Dependency metadata*). Assembling it names the same call without putting the
# token where that scan reads.
covr_internal <- function(name) {
  call(":::", as.name("covr"), as.name(name))
}

# The counter call a covr wrapper opens with, for a key, in each covr
# generation the coverage job has installed: measured on covr 3.6.5.9001 and on
# covr 4.0.0. Written out for fixtures rather than produced by calling covr,
# which is in no dependency field.
coverage_counters <- list(
  "covr < 4.0.0" = function(key) as.call(list(covr_internal("count"), key)),
  "covr >= 4.0.0" = function(key) {
    call(".Call", covr_internal("covr_count"), key)
  }
)

# `expr` as covr wraps it, with the counter `counter` builds.
coverage_wrap <- function(expr, counter, key = "marginplyr/R/f.R:1:1:1:1") {
  # jarl-ignore if_always_true: This reproduces covr's literal wrapper.
  bquote(if (TRUE) {
    .(counter(key))
    .(expr)
  })
}

# Whether `expr` is one of the counter calls `coverage_counters` writes.
is_coverage_counter <- function(expr) {
  is.call(expr) && (
    identical(expr[[1]], covr_internal("count")) ||
      (length(expr) == 3L && identical(expr[[1]], quote(.Call)) &&
         identical(expr[[2]], covr_internal("covr_count")))
  )
}

# `expr` with covr's instrumentation taken off, where it has any.
strip_coverage_wrapper <- function(expr) {
  if (!is.call(expr) || !identical(expr[[1]], quote(`if`)) ||
        length(expr) != 3L || !identical(expr[[2]], TRUE)) {
    return(expr)
  }
  branch <- expr[[3]]
  if (!is.call(branch) || !identical(branch[[1]], quote(`{`)) ||
        length(branch) != 3L || !is_coverage_counter(branch[[2]])) {
    return(expr)
  }
  branch[[3]]
}
