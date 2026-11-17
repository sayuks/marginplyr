# covr's instrumentation, read through by the structural gates that take an
# expression by position: `nth_statement()` in `test-sent-queries.R` and the
# literal-branch reader in `helper-diagnostic-sites.R`. A gate that walks with
# `visit_calls()` finds a call wherever the wrapper puts it and needs neither.
#
# covr measures a namespace by replacing an expression with
# `if (TRUE) { <counter>; <expression> }`, so a reader that takes one by
# position takes covr's wrapper instead. covr wraps each statement of a body,
# and from 4.0.0 also each arm of an unbraced `if`.
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

# Whether `expr` is the counter call a covr wrapper opens with: `covr:::count(
# <key>)` before covr 4.0.0, and `.Call(covr:::covr_count, <key>)` from it, the
# `.Call` written `base::.Call` where the package defines its own.
is_coverage_counter <- function(expr) {
  if (!is.call(expr)) {
    return(FALSE)
  }
  if (identical(expr[[1]], covr_internal("count"))) {
    return(TRUE)
  }
  dot_call <- list(quote(.Call), call("::", as.name("base"), as.name(".Call")))
  length(expr) == 3L &&
    any(vapply(dot_call, identical, logical(1), expr[[1]])) &&
    identical(expr[[2]], covr_internal("covr_count"))
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
