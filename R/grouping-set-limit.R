# The number of grouping-set occurrences a Grouping plan may hold unless
# `marginplyr.max_grouping_sets` says otherwise. ADR 0046 holds why it is this
# number and why it is one number for every backend and verb.
default_grouping_set_limit <- 4096

# The limit for the call being prepared: the option's value, or the default
# when the option is unset or `NULL`. Read wherever expansion is about to
# materialize occurrences, so no caller has to carry it; every entry point reads
# it once before expansion too, so an invalid value is refused at the same
# point whatever the plan.
grouping_set_limit <- function() {
  limit <- getOption("marginplyr.max_grouping_sets")
  if (is.null(limit)) {
    return(default_grouping_set_limit)
  }
  valid <- is.numeric(limit) && length(limit) == 1L && !is.na(limit) &&
    limit >= 1 && (is.infinite(limit) || limit == trunc(limit))
  if (!valid) {
    abort_marginplyr(c(
      paste0(
        "{.code marginplyr.max_grouping_sets} must be a whole number of at ",
        "least 1, {.code Inf}, or {.code NULL}."
      ),
      i = "It is {.obj_type_friendly {limit}}."
    ))
  }
  as.numeric(limit)
}

# Refuse a plan known to hold more than `limit` occurrences. Without
# `constructor`, `count` is the plan's own count after the duplicate policy.
# With it, `count` is what a family that constructor wrote expands to, a lower
# bound on the plan's, and `NULL` says only that the family passes the limit.
# A count too large for a double is the same as none.
check_grouping_set_count <- function(count, constructor = NULL,
                                     limit = grouping_set_limit()) {
  if (!is.null(count) && count <= limit) {
    return(invisible(NULL))
  }
  abort_grouping_set_limit(limit, count, constructor)
}

abort_grouping_set_limit <- function(limit, count = NULL, constructor = NULL) {
  # Read only from the cli templates below, which codetools cannot see.
  # nolint start: object_usage_linter.
  limit_text <- format_grouping_set_count(limit)
  count_text <- if (!is.null(count) && is.finite(count)) {
    format_grouping_set_count(count)
  }
  # nolint end
  if (is.null(constructor) && is.null(count_text)) {
    abort_marginplyr(c(
      paste0(
        "The Grouping plan holds more grouping sets than the limit of ",
        "{limit_text}."
      ),
      i = "Use fewer Margin dimensions or a narrower Grouping specification.",
      i = paste0(
        "Or raise the limit with ",
        "{.code options(marginplyr.max_grouping_sets = )}, which takes a ",
        "whole number or {.code Inf}."
      )
    ))
  }
  if (is.null(constructor)) {
    abort_marginplyr(c(
      paste0(
        "The Grouping plan holds {count_text} grouping sets, more than the ",
        "limit of {limit_text}."
      ),
      i = "Use fewer Margin dimensions or a narrower Grouping specification.",
      i = paste0(
        "Or raise the limit with ",
        "{.code options(marginplyr.max_grouping_sets = )}, which takes a ",
        "whole number or {.code Inf}."
      )
    ))
  }
  if (is.null(count_text)) {
    abort_marginplyr(c(
      paste0(
        "A {.fun {constructor}} in the Grouping specification expands to ",
        "more grouping sets than the limit of {limit_text}."
      ),
      i = "Use fewer Margin dimensions or a narrower Grouping specification.",
      i = paste0(
        "Or raise the limit with ",
        "{.code options(marginplyr.max_grouping_sets = )}, which takes a ",
        "whole number or {.code Inf}."
      )
    ))
  }
  abort_marginplyr(c(
    paste0(
      "A {.fun {constructor}} in the Grouping specification expands to ",
      "{count_text} grouping sets, more than the limit of {limit_text}."
    ),
    i = "Use fewer Margin dimensions or a narrower Grouping specification.",
    i = paste0(
      "Or raise the limit with ",
      "{.code options(marginplyr.max_grouping_sets = )}, which takes a ",
      "whole number or {.code Inf}."
    )
  ))
}

format_grouping_set_count <- function(count) {
  format(count, big.mark = ",", scientific = FALSE, trim = TRUE)
}
