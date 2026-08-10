# rlang soft-deprecated unquoting inside the `.data` pronoun, and signals a
# lifecycle condition when `expr()` sees it — twice per Margin operation, into
# whatever condition handler the caller has installed. Building the call
# directly produces the identical expression, `.data[["name"]]`, without the
# signal. The name must stay a literal: a bare symbol would be resolved
# against the data mask when the generated expression runs.
margin_column_pronoun <- function(name) {
  # An invariant, not a Package condition (ADR-0015): every call site passes a
  # column name it already holds as a string, so no rewrite of a public call
  # reaches this. A symbol arriving here would build the mask-resolving
  # reference the comment above warns about, silently.
  stopifnot(is.character(name), length(name) == 1L, !is.na(name))
  rlang::call2("[[", rlang::sym(".data"), name)
}

assert_logical_scalar <- function(x) {
  nm <- deparse(substitute(x))
  if (!(isTRUE(x) || isFALSE(x))) {
    abort_marginplyr(
      sprintf("`%s` must be a logical scalar (`TRUE` or `FALSE`).", nm)
    )
  }
}

# NA_character_ is allowed
assert_string_scalar <- function(x) {
  nm <- deparse(substitute(x))
  if (!(is.character(x) && length(x) == 1)) {
    abort_marginplyr(
      sprintf("`%s` must be a character vector of length 1.", nm)
    )
  }
}

assert_nest_possible <- function(x) {
  nm <- deparse(substitute(x))
  valid_classes <- c("data.frame", "dtplyr_step")
  if (!inherits(x, valid_classes)) {
    abort_marginplyr(
      sprintf(
        "`%s` must be one of the following classes, which can be nested: %s",
        nm,
        toString(valid_classes)
      )
    )
  }
}

# Admission is duck-typed rather than a class whitelist: marginplyr supports
# any input the dplyr verbs it calls can handle, and a whitelist would reject
# a backend that works. `group_vars()` is the first dplyr generic every Margin
# operation reaches, so an input without a method for it cannot proceed, and
# saying so here keeps the diagnostic in the caller's terms instead of
# surfacing "no applicable method for 'group_vars'".
#
# Admissibility is decided by looking for a method rather than by calling the
# generic and catching whatever comes back. A backend's `group_vars()` can
# fail for its own reasons, and catching every error would report that failure
# as the caller having supplied the wrong kind of object. dplyr registers no
# default method, so an object no registered method matches is exactly the
# object the generic cannot dispatch on.
assert_margin_input <- function(x) {
  nm <- deparse(substitute(x))
  dispatches <- any(vapply(
    class(x),
    function(cls) {
      # `envir` names the namespace that owns the generic. The default is the
      # caller's frame, where `group_vars` is not visible from inside this
      # package, so every class would look unsupported.
      !is.null(utils::getS3method(
        "group_vars",
        cls,
        optional = TRUE,
        envir = asNamespace("dplyr")
      ))
    },
    logical(1L)
  ))
  if (dispatches) {
    return(invisible(NULL))
  }

  abort_marginplyr(
    sprintf(
      paste0(
        "`%s` must be a data frame or a lazy table that supports dplyr ",
        "verbs; %s was supplied. Convert it with `as.data.frame()` or ",
        "`dplyr::tbl()` first."
      ),
      nm,
      if (is.null(x)) "`NULL`" else sprintf("a <%s>", class(x)[[1L]])
    )
  )
}

assert_lazy_table <- function(x) {
  nm <- deparse(substitute(x))
  invalid_lazy_table_names <- "RecordBatchReader"
  if (inherits(x, invalid_lazy_table_names)) {
    abort_marginplyr(
      sprintf(
        "`%s` must not be an object of the following classes: %s",
        nm,
        toString(invalid_lazy_table_names)
      )
    )
  }
}

# The name and namespace a static analysis reads from a call it has already
# recognized as one with `rlang::is_call()`.
#
# `rlang::call_name()` and `rlang::call_ns()` do not answer a formula as the
# call to `~` that it is: both unwrap a one-sided formula to its right-hand
# side. When that side is a bare symbol they raise "`call` must be a defused
# call, not a symbol" -- an untyped condition of the class ADR-0015
# separates -- and when it is not they answer a different call, so
# `~ .data$share` reads as a `$` call and the analysis enters a branch written
# for another shape, carrying the formula as the expression that branch then
# reads `[[2L]]` of. `rlang::call_args()` unwraps the same way, which is how a
# formula reached the direct-share path and was computed as the share it is
# not (#163).
#
# Every analysis in this package recognizes a call by its name, none of them
# recognizes `~`, and each already has an answer for a name it does not know:
# walk the call's parts, or report the shape as one it does not handle. A
# formula is therefore a call with no name, and the analysis treats it as the
# `~` call it is rather than as its right-hand side.
static_call_name <- function(expr) {
  if (rlang::is_call(expr, "~")) {
    return(NULL)
  }
  rlang::call_name(expr)
}

static_call_ns <- function(expr) {
  if (rlang::is_call(expr, "~")) {
    return(NULL)
  }
  rlang::call_ns(expr)
}

# Required because dtplyr is an optional Suggest rather than an Import: it
# brings data.table, which reads this flag from the calling namespace.
# data.table fixes the spelling of this name, so it cannot follow the package's
# object naming convention.
.datatable.aware <- TRUE # nolint: object_name_linter

utils::globalVariables(".data")
