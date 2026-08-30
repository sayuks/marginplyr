# Every spelling a Margin verb recognizes as a Contextual helper or as one of
# the three families read alongside them, keyed by the family that decides what
# recognition does. It is the one place those names and the namespaces they may
# carry are written down, and the one place their namespace test is
# implemented. ADR 0019's *The recognized spellings live in one registry* is
# authoritative for why one place, for what it replaced, and for the
# language-capture primitives it keeps out.
#
# A family names its own spellings only where no other table already owns them.
# `share_kind_rules()` is the authoritative description of a contextual share
# and `grouping_kind_rules()` of a specification kind, so those two families
# derive; a third share helper or a sixth constructor appears here without
# being written down a second time. Read the other way, a family listing names
# outright is a family whose names exist nowhere else.
#
# Each entry is a function rather than a value, so asking about one family
# builds that family and no other. That is what keeps this module's dependency
# running the way `design/architecture.md` requires: built eagerly, a
# `grouping_helper_name()` lookup would evaluate `share_kind_rules()` and make
# the grouping-context rewrite reach into the contextual-share module for a
# fact that is not about shares, which is exactly what #179 separated. Nothing
# but the list of family names is available without naming a family, and
# `test-contextual-helpers.R` asserts the laziness rather than trusting it.
#
# `contextual` is what ADR 0019's criterion decides, and it is not a synonym
# for "read statically": three of the four families here are read statically
# and are still ordinary names. That ADR's *What the criterion decides* is
# authoritative for which family the field is true of and why.
static_spelling_rules <- function() {
  list(
    grouping = function() {
      list(
        namespaces = "marginplyr",
        contextual = TRUE,
        names = c("grouping_bit", "grouping_id")
      )
    },
    share = function() {
      list(
        namespaces = "marginplyr",
        contextual = TRUE,
        names = vapply(share_kind_rules(), `[[`, character(1), "helper")
      )
    },
    selection = function() {
      list(
        namespaces = "dplyr",
        contextual = TRUE,
        names = c("across", "if_any", "if_all", "pick")
      )
    },
    # tidyselect exports `where()` and dplyr re-exports it, so both qualifiers
    # name the same function. Which owners a spelling has is a property of the
    # spelling, and this is the one place it is recorded. tidyselect leads
    # because it defines the function, which is the rule the canonical
    # qualifier below reads.
    #
    # Only `qualify_static_spelling()` writes a qualifier back, and only for
    # the `selection` family; the marginplyr-owned families are rewritten away
    # entirely and a refused spelling never runs, so three of the five need no
    # qualifier either. What is peculiar to `where` is what happens in the
    # positions this family does *not* decide.
    #
    # It is read here only to refuse it -- a contextual share's `across()`
    # selects among preceding summaries by name, and a type predicate has
    # nothing to test there -- and every other `where()` a caller writes is
    # left to execute, reaching `tidyselect::eval_select()`, which binds the
    # name in the selection mask itself. So a caller binding loses to
    # tidyselect rather than to anything written here, which makes it the one
    # entry in this table whose promise another package's code keeps.
    # `test-contextual-helpers.R` asserts it for that reason.
    predicate = function() {
      list(
        namespaces = c("tidyselect", "dplyr"),
        contextual = TRUE,
        names = "where"
      )
    },
    refused = function() {
      list(
        namespaces = "dplyr",
        contextual = TRUE,
        names = c(
          "cur_group",
          "cur_group_id",
          "cur_group_rows",
          "cur_data",
          "cur_data_all"
        )
      )
    },
    grouping_constructor = function() {
      list(
        namespaces = "marginplyr",
        contextual = FALSE,
        names = grouping_constructor_names()
      )
    },
    tibble_frame = function() {
      list(
        namespaces = "tibble",
        contextual = FALSE,
        names = c("tibble", "data_frame")
      )
    },
    base_frame = function() {
      list(
        namespaces = "base",
        contextual = FALSE,
        names = "data.frame"
      )
    }
  )
}

# The family names, which is everything available without naming one. Reading
# them costs no domain module, which is the property the laziness above exists
# to give.
static_spelling_families <- function() {
  names(static_spelling_rules())
}

# An invariant rather than a Package condition (ADR 0015): a family name is
# written in this package's own source, so an unknown one is a typo in a call
# no caller can reach and not something a caller can rewrite.
static_spelling_rule <- function(family) {
  build <- static_spelling_rules()[[family]]
  if (is.null(build)) {
    stop("Unknown static-spelling family: ", family, call. = FALSE)
  }
  build()
}

static_spelling_names <- function(family) {
  unname(static_spelling_rule(family)$names)
}

static_spelling_namespaces <- function(family) {
  static_spelling_rule(family)$namespaces
}

# The one qualifier a rewrite writes back, where a family has more than one
# owner. It is the first, and the order is the table's rather than incidental:
# a family lists the package that defines the function first and its
# re-exporters after, so `where` is written back as `tidyselect::where` and not
# as `dplyr::where`. Nothing but `qualify_static_spelling()` needs this --
# recognition accepts every owner -- and naming it is what stops the choice
# living as an unexplained `[[1L]]` at the one site that makes it.
static_spelling_qualifier <- function(family) {
  static_spelling_namespaces(family)[[1L]]
}

# The families whose spellings are Contextual helpers. Tests derive from this
# rather than from the whole table, because the assertion that separates the
# two is exactly that a caller binding cannot win: for a constructor it can,
# and asserting otherwise would assert the opposite of ADR 0019.
contextual_helper_families <- function() {
  families <- static_spelling_families()
  contextual <- vapply(
    families,
    function(family) static_spelling_rule(family)$contextual,
    logical(1)
  )
  unname(families[contextual])
}

# The spelling this call is recognized as within one family, or `NULL` where it
# is not one. Recognition is the name matching and the namespace being absent
# or an owner of that name; any other qualifier is another package's function
# and passes through to ordinary evaluation, where R answers it.
#
# Asked of any expression, not only of a call: a node that is no call has no
# name, and no name matches.
static_spelling_name <- function(expr, family) {
  name <- static_call_name(expr)
  if (is.null(name) || !name %in% static_spelling_names(family)) {
    return(NULL)
  }
  namespace <- static_call_ns(expr)
  owners <- static_spelling_namespaces(family)
  if (!is.null(namespace) && !namespace %in% owners) {
    return(NULL)
  }
  name
}

is_static_spelling_call <- function(expr, family, name) {
  identical(static_spelling_name(expr, family), name)
}

# Whether a call is recognized in any of several families, for the one reader
# that treats them alike: a data-frame-valued summary's output names are
# predicted the same way whether tibble or base owns the constructor.
is_any_static_spelling_call <- function(expr, families) {
  any(vapply(
    families,
    function(family) !is.null(static_spelling_name(expr, family)),
    logical(1)
  ))
}

# The name a function *reference* is recognized as, for the one position that
# takes a helper by value rather than by call: an `across()` `.fns` argument
# written as `share_of_total` or as `marginplyr::share_of_total`. The namespace
# rule is the same one calls follow, and reading it from the same table is what
# keeps a reference position from drifting away from the call position.
#
# Redundant parentheses are read through here for the same reason they are read
# through in a call head: `(share_of_total)` is the value `share_of_total` is,
# so refusing it while accepting the call spelling would leave one position out
# of the rule the rest of the registry follows (#178).
#
# By recursion rather than by rebinding `expr`, which is an argument the caller
# may have left empty -- an `across()` written `across(units, )` reaches here
# with R's missing marker, and assigning that to a local raises
# `missingArgError` on the next read of it (#174).
static_spelling_reference_name <- function(expr, family) {
  if (is_redundant_parens(expr)) {
    return(static_spelling_reference_name(
      unparenthesized_value(expr),
      family
    ))
  }
  if (rlang::is_symbol(expr)) {
    name <- rlang::as_name(expr)
  } else if (
    rlang::is_call(expr, "::") &&
      length(expr) == 3L &&
      rlang::is_symbol(expr[[2L]]) &&
      rlang::as_name(expr[[2L]]) %in% static_spelling_namespaces(family) &&
      rlang::is_symbol(expr[[3L]])
  ) {
    name <- rlang::as_name(expr[[3L]])
  } else {
    return(NULL)
  }
  if (!name %in% static_spelling_names(family)) {
    return(NULL)
  }
  name
}

# The recognized call written so that nothing but the owning package's function
# can run it. ADR 0019's rule has a consequence beyond naming: a spelling read
# statically has to execute as the code that was read, or a rule is checked
# against an expression that never runs -- which is what a shadowed `across()`
# did, evaluating the caller's function underneath a grouping-column exclusion
# check that had already passed on dplyr's (#172).
#
# Qualifying the head is what closes it, and it closes every position at once.
# `dplyr:::summarise_cols()` expands `pick()` syntactically wherever it appears
# but expands `across()` only in an unnamed top-level dot, so every summary a
# Margin verb stages -- all of them named -- reached the data mask with its head
# resolved by ordinary lexical lookup. A qualified head is out of reach of any
# binding there, and neither static expansion notices the difference: both
# accept an absent or `dplyr` qualifier, as does dbplyr's `partial_eval()`,
# which matches these names without examining the qualifier at all.
#
# The rebuild goes through `rebuild_static_call()` rather than through
# `expr[[1L]] <-` for the reason that function's own comment gives: the two
# spellings a walk reaches for are the ones rlang soft-deprecated on a quosure.
# No quosure reaches here -- `static_call_name()` answers a `~` call as no name,
# so recognition never fires on one -- but routing every rebuild through one
# function is what makes that rule checkable rather than remembered.
qualify_static_spelling <- function(expr, family, name) {
  rebuild_static_call(
    expr,
    static_call_args(expr),
    head = rlang::call2(
      "::",
      rlang::sym(static_spelling_qualifier(family)),
      rlang::sym(name)
    )
  )
}
