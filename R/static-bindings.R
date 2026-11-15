# Static binding and mutation proofs for summary naming, expression rewriting
# and the contextual share dependency walk.

# Names visible to a loop body. R evaluates the sequence before binding the
# index, and binds the index even for an empty sequence.
static_loop_bound_names <- function(expr, bound, lookup) {
  union(
    static_statement_bound_names(expr[[3L]], bound, lookup),
    assignment_target_names(expr[[2L]])
  )
}

# Definite local bindings after a statement in the current lexical frame.
# A possibly evaluated argument may remove a binding but cannot establish one.
static_statement_bound_names <- function(expr, bound, lookup) {
  if (!rlang::is_call(expr)) return(bound)
  if (rlang::is_quosure(expr)) {
    lookup$env <- rlang::quo_get_env(expr)
  }
  expr <- unparenthesized_value(expr)
  name <- static_call_name(expr)
  if (rlang::is_call(expr, "{")) {
    return(Reduce(function(visible, part) {
      static_statement_bound_names(part, visible, lookup)
    }, static_call_args(expr), init = bound))
  }
  if (rlang::is_call(expr, c("<-", "="), n = 2L) && is_name_part(expr[[2L]])) {
    return(union(
      static_statement_bound_names(expr[[3L]], bound, lookup),
      assignment_target_names(expr[[2L]])
    ))
  }
  if (rlang::is_call(expr, "for", n = 3L) && is_name_part(expr[[2L]])) {
    bound <- static_loop_bound_names(expr, bound, lookup)
    after_body <- static_statement_bound_names(expr[[4L]], bound, lookup)
    return(intersect(bound, after_body))
  }
  arm <- literal_if_parts(expr, name, bound, lookup)
  if (identical(name, "switch") && length(expr) >= 3L &&
        !is_parenthesized(static_call_head(expr)) &&
        is_known_package_call(expr, name, bound, lookup)) {
    arm <- literal_switch_parts(expr)
  }
  if (length(arm) == 1L && !rlang::is_missing(arm[[1L]])) {
    return(static_statement_bound_names(arm[[1L]], bound, lookup))
  }
  if (identical(name, "force") && length(expr) == 2L &&
        is_known_package_call(expr, name, bound, lookup)) {
    return(static_statement_bound_names(expr[[2L]], bound, lookup))
  }
  if (length(bound) == 0L || identical(name, "function")) return(bound)
  if (identical(name, "local") &&
        is_known_package_call(expr, name, bound, lookup) &&
        length(base_call_argument(expr, "envir")) == 0L) return(bound)

  mutation <- if (!any(name %in% bound) || !is.null(static_call_ns(expr))) {
    scope_mutation_call(expr, bound, lookup)
  }
  if (any(static_call_name(mutation) %in% c("rm", "remove"))) {
    target <- base_call_argument(mutation, c("envir", "pos"))
    if (length(target) > 0L && !is.null(
      lookup_environment_value(target[[1L]], bound, lookup)
    )) return(bound)
    args <- static_call_args(mutation)
    removed <- literal_removal_names(
      args[!argument_names(args) %in% c("envir", "pos", "inherits")],
      is_concat = function(part) {
        identical(static_call_name(part), "c") &&
          is_known_package_call(part, "c", bound, lookup)
      }
    )
    return(if (is.null(removed)) character() else setdiff(bound, removed))
  }
  parts <- c(
    evaluated_call_head(expr),
    scope_call_args(expr, name, bound, lookup),
    evaluated_language_parts(expr)
  )
  Reduce(function(visible, part) {
    intersect(visible, static_statement_bound_names(part, visible, lookup))
  }, parts, init = bound)
}

# The reachable arm of an if with a literal condition, or NULL when both
# arms may run. An empty list represents an if with no reachable arm.
literal_if_parts <- function(expr, name, bound, lookup) {
  if (identical(name, "if") && length(expr) >= 3L &&
        is_scope_control_primitive(expr, name, bound, lookup)) {
    condition <- unparenthesized_value(expr[[2L]])
    if (identical(condition, FALSE)) {
      return(if (length(expr) >= 4L) list(expr[[4L]]) else list())
    }
    if (identical(condition, TRUE)) {
      return(list(expr[[3L]]))
    }
  }
  NULL
}

# Standalone syntax has no captured caller. With a caller, short-circuiting
# requires the actual base callee.
is_scope_control_primitive <- function(expr, name, bound, lookup) {
  if (is.null(lookup$env) && is.null(static_call_ns(expr))) {
    return(!name %in% bound)
  }
  is_known_package_call(expr, name, bound, lookup)
}

# Call arguments that the read and removal walks may visit.
scope_call_args <- function(expr, name, bound, lookup) {
  if (!is.null(name) && name %in% c("&&", "||") && length(expr) == 3L &&
        identical(unparenthesized_value(expr[[2L]]), identical(name, "||")) &&
        is_scope_control_primitive(expr, name, bound, lookup)) {
    return(list())
  }
  literal_if_parts(expr, name, bound, lookup) %||%
    evaluated_call_args(expr, bound = bound)
}

# The written arm selected by a verified base switch() call.
literal_switch_parts <- function(expr) {
  if (rlang::is_missing(expr[[2L]])) {
    return(NULL)
  }
  key <- unparenthesized_value(expr[[2L]])
  if (!is.object(key) && typeof(key) %in% c("integer", "double") &&
        length(key) == 1L && is.finite(key) && key >= 1 &&
        key < length(expr) - 1L) {
    return(as.list(expr)[as.integer(key) + 2L])
  }
  if (rlang::is_string(key) && !is.object(key)) {
    index <- match(key, names(expr))
    if (!is.na(index) && index >= 3L) {
      return(as.list(expr)[index])
    }
  }
  NULL
}

# Read a function-position name without forcing a caller promise or invoking an
# active binding. R skips non-function values in a function position; an
# unresolved binding leaves its eventual callee unknown to prediction. Known
# package owners may load their namespace and attached export promises.
# `absent` distinguishes exhausted lookup from an unresolved binding.
ordinary_function_binding <- function(name, env, owners = character(),
                                      absent = NULL) {
  allowed <- c(
    list(baseenv(), asNamespace("base")),
    lapply(owners, asNamespace),
    lapply(intersect(paste0("package:", owners), search()), as.environment)
  )
  current <- env
  while (!identical(current, emptyenv())) {
    if (exists(name, envir = current, inherits = FALSE)) {
      if (bindingIsActive(name, current) ||
            (!any(vapply(allowed, identical, logical(1), y = current)) &&
               rlang::env_binding_are_lazy(current, name))) {
        return(NULL)
      }
      binding <- get(name, envir = current, inherits = FALSE)
      if (is.function(binding)) {
        return(binding)
      }
    }
    current <- parent.env(current)
  }
  absent
}

# Names that may be rebound while the expression runs. Unlike the walk's
# definite `bound` set, conditional assignments also prevent a lexical
# environment value from proving an external lookup. An unresolved reflective
# assignment reports the lookup marker, excluding every lexical binding.
# Literal list2env setter keys invalidate only the bindings they name;
# unresolved setter keys invalidate every external binding proof.
# `visible_removals` also invalidates finite removals in existing environments.
lookup_rebound_names <- function(expr, env = emptyenv(), mask = character(),
                                 function_lookup = FALSE,
                                 visible_removals = FALSE) {
  rebound <- expression_rebound_names(expr)
  written <- if (function_lookup) {
    expression_rebound_names(expr, function_lookup = TRUE)
  } else {
    rebound
  }
  unique(c(written, expression_rebound_names(expr, list(
    env = env, mask = mask, rebound = rebound,
    function_rebound = expression_rebound_names(expr, function_lookup = TRUE),
    visible_removals = visible_removals
  ), function_lookup = function_lookup)))
}

# Rebindings visible in evaluated syntax. The first pass supplies ordinary
# assignments before the second pass resolves mutation callees against them.
# Function lookup includes replacement roots and skips plain assignments of
# literal non-function values. Parent mutations are tracked separately by
# lookup_reparented_environments().
expression_rebound_names <- function(expr, lookup = NULL,
                                     function_lookup = FALSE) {
  if (rlang::is_quosure(expr) && !is.null(lookup)) {
    lookup$env <- rlang::quo_get_env(expr)
  }
  if (!rlang::is_call(expr)) {
    return(character())
  }
  if (is_parenthesized(expr)) {
    return(expression_rebound_names(
      unparenthesized_value(expr), lookup, function_lookup
    ))
  }
  assigned <- lookup_assignment_names(expr, replacement = function_lookup)
  if (function_lookup && rlang::is_call(expr, c("<-", "=")) &&
        length(expr) >= 3L && is_name_part(expr[[2L]]) &&
        identical(scope_literal_kind(expr[[3L]]), "value")) {
    assigned <- character()
  }
  if (!is.null(lookup)) {
    mutation <- scope_mutation_call(expr, character(), lookup)
    mutation_name <- static_call_name(mutation)
    if (identical(mutation_name, "list2env")) {
      assigned <- c(assigned, list2env_setter_names(mutation, lookup))
    }
    if (isTRUE(lookup$visible_removals) &&
          any(mutation_name %in% c("rm", "remove"))) {
      assigned <- c(assigned, visible_removal_binding_names(mutation, lookup))
    }
  }
  parts <- c(evaluated_call_args(expr), evaluated_language_parts(expr))
  head <- static_call_head(expr)
  if (!rlang::is_symbol(head)) {
    parts <- c(list(head), parts)
  }
  unique(c(assigned, unlist(lapply(
    parts, expression_rebound_names, lookup = lookup,
    function_lookup = function_lookup
  ), use.names = FALSE)))
}

# Binding names replaced by a recognized base list2env call. Omitted/NULL
# targets construct an environment; unreadable setter keys remain unresolved.
list2env_setter_names <- function(expr, lookup = NULL) {
  target <- base_call_argument(expr, "envir")
  if (length(target) == 0L ||
        is.null(unparenthesized_value(target[[1L]]))) {
    return(character())
  }
  values <- base_call_argument(expr, "x")
  if (length(values) > 0L &&
        identical(static_call_name(values[[1L]]), "list") &&
        is_known_package_call(values[[1L]], "list", character(), lookup)) {
    return(names(static_call_args(values[[1L]])))
  }
  unresolved_lookup_name()
}

# Finite removals with a statically visible target can change a later
# lexical lookup. An implicit target belongs to the current evaluation frame.
visible_removal_binding_names <- function(expr, lookup) {
  args <- static_matched_arguments(expr, args(base::rm))
  target <- intersect(c("envir", "pos"), names(args))
  if (
    length(target) == 0L || rlang::is_missing(args[[target[[1L]]]]) ||
      is.null(lookup_environment_value(
        args[[target[[1L]]]], character(), lookup
      ))
  ) {
    return(character())
  }
  literal_removal_names(
    args[!argument_names(args) %in% c("envir", "pos", "inherits")],
    is_concat = function(part) {
      identical(static_call_name(part), "c") &&
        is_known_package_call(part, "c", character(), lookup)
    }
  )
}

# The root binding written by an assignment target, including replacement calls.
assignment_target_names <- function(expr) {
  if (is_name_part(expr) || rlang::is_string(expr)) {
    return(rlang::as_name(expr))
  }
  if (rlang::is_call(expr) && length(expr) >= 2L) {
    return(assignment_target_names(expr[[2L]]))
  }
  character()
}

# Names assigned by this call itself, or the unresolved marker when a reflective
# assignment's target is not statically known. The caller supplies a call, and
# a concatenation check as `static_character_value()` takes one.
lookup_assignment_names <- function(expr, replacement = TRUE,
                                    is_concat = NULL) {
  name <- resolved_callee_name(expr)
  assigned <- if (!is.null(name) && name %in% c("<-", "=", "<<-", "for") &&
                    length(expr) >= 3L &&
                    (replacement || is_name_part(expr[[2L]]) ||
                       rlang::is_string(expr[[2L]]))) {
    assignment_target_names(expr[[2L]])
  } else {
    character()
  }
  if (!is.null(name) &&
        name %in% c("assign", "delayedAssign", "makeActiveBinding")) {
    formal <- if (name == "makeActiveBinding") "sym" else "x"
    assigned <- unlist(lapply(
      call_formal_argument(expr, formal), static_character_value,
      is_concat = is_concat
    ), use.names = FALSE)
    if (is.null(assigned)) {
      assigned <- unresolved_lookup_name()
    }
  }
  assigned
}

# Identify literal function or value syntax without evaluating it.
# NULL leaves the kind unknown.
scope_literal_kind <- function(value) {
  value <- unparenthesized_value(value)
  if (is.function(value) || rlang::is_call(value, "function")) {
    "function"
  } else if (!is.language(value)) {
    "value"
  }
}

# Whether a standard package function is the callee, without invoking it.
# The caller supplies its static name; unresolved bindings prove no exemption.
is_known_package_call <- function(expr, name, bound, lookup, owner = "base") {
  namespace <- static_call_ns(expr)
  if (!is.null(namespace)) {
    return(identical(namespace, owner))
  }
  rebound <- lookup$function_rebound %||% lookup$rebound
  if (is.null(lookup$env) || name %in% c(bound, rebound) ||
        unresolved_lookup_name() %in% rebound) {
    return(FALSE)
  }
  identical(
    ordinary_function_binding(name, lookup$env, owners = owner),
    get(name, envir = asNamespace(owner), inherits = FALSE)
  )
}

# A recognized scope mutation in direct-call form, without evaluating its head
# or arguments. An unreadable do.call() argument list retains unknown targets.
scope_mutation_call <- function(expr, bound, lookup) {
  name <- resolved_callee_name(expr)
  if (is.null(name) ||
        !name %in% c("rm", "remove", "parent.env<-", "list2env", "do.call")) {
    return(NULL)
  }
  head <- static_call_head(expr)
  callee_bound <- bound
  callee_lookup <- lookup
  if (identical(name, "do.call")) {
    if (scope_callee_is_shadowed(head, name, bound, lookup)) {
      return(NULL)
    }
    target <- base_call_argument(expr, "what")
    if (length(target) == 0L) {
      return(NULL)
    }
    name <- static_callee_name(target[[1L]])
    head <- target[[1L]]
    environment <- base_call_argument(expr, "envir")
    # String targets use envir; function values are evaluated in the caller.
    if (length(static_character_value(head)) == 1L &&
          length(environment) > 0L) {
      callee_lookup$env <- lookup_environment_value(
        environment[[1L]], bound, lookup
      )
      callee_lookup$mask <- character()
      callee_bound <- character()
    }
    arguments <- base_call_argument(expr, "args")
    parts <- if (
      length(arguments) > 0L &&
        identical(static_call_name(arguments[[1L]]), "list") &&
        is_known_package_call(arguments[[1L]], "list", bound, lookup)
    ) {
      static_call_args(arguments[[1L]])
    } else if (identical(name, "parent.env<-")) {
      alist(env = NULL, value = NULL)
    } else {
      alist(list = NULL)
    }
  } else {
    parts <- static_call_args(expr)
  }
  if (is.null(name) ||
        !name %in% c("rm", "remove", "parent.env<-", "list2env")) {
    return(NULL)
  }
  if (scope_callee_is_shadowed(head, name, callee_bound, callee_lookup)) {
    return(NULL)
  }
  rlang::call2(name, !!!parts)
}

# Whether ordinary lookup proves a named scope callee is caller-defined.
# Unknown bindings remain conservative; function-naming resolvers are inspected
# only for literal targets and existing environments, never invoked.
scope_callee_is_shadowed <- function(callee, name, bound, lookup) {
  if (is_parenthesized(callee)) {
    return(scope_callee_is_shadowed(
      unparenthesized_value(callee), name, bound, lookup
    ))
  }
  if (rlang::is_call(callee, c("::", ":::"))) {
    return(!identical(rlang::as_string(callee[[2L]]), "base"))
  }
  if (rlang::is_symbol(callee) && name %in% lookup$local_functions) {
    return(TRUE)
  }
  if (is.null(lookup$env) || name %in% c(bound, lookup$rebound) ||
        unresolved_lookup_name() %in% lookup$rebound) {
    return(FALSE)
  }
  env <- lookup$env
  own_lookup <- exclude_generic <- FALSE
  if (rlang::is_call(callee)) {
    resolver <- static_call_name(callee)
    if (is.null(resolver) || is.null(function_naming_formal(resolver))) {
      return(FALSE)
    }
    if (resolver %in% c(bound, lookup$rebound)) {
      return(FALSE)
    }
    owner <- if (identical(resolver, "getFunction")) "methods" else "base"
    namespace <- static_call_ns(callee)
    binding <- if (is.null(namespace)) {
      ordinary_function_binding(resolver, env, owners = owner)
    } else if (identical(namespace, owner)) {
      get(resolver, envir = asNamespace(owner))
    } else {
      return(TRUE)
    }
    if (!is.null(binding) &&
          !identical(binding, get(resolver, envir = asNamespace(owner)))) {
      return(TRUE)
    }
    if (is.null(binding)) {
      return(FALSE)
    }
    if (resolver %in% c("get", "get0")) {
      inherited <- base_call_argument(callee, "inherits", definition = binding)
      own_lookup <- length(inherited) > 0L &&
        !identical(unparenthesized_value(inherited[[1L]]), TRUE)
    }
    if (identical(resolver, "getFunction")) {
      generic <- base_call_argument(callee, "generic", definition = binding)
      exclude_generic <- length(generic) > 0L &&
        !identical(unparenthesized_value(generic[[1L]]), TRUE)
    }
    environment <- base_call_argument(
      callee, c("envir", "pos", "where"), definition = binding
    )
    if (length(environment) > 0L) {
      env <- lookup_environment_value(environment[[1L]], bound, lookup)
      if (is.null(env)) {
        return(FALSE)
      }
    } else if (resolver %in% c("match.fun", "getFunction")) {
      # These resolvers can start outside the captured lexical frame. A shadow
      # confined to that frame cannot establish the function they will find.
      if (identical(resolver, "match.fun")) {
        lexical <- ordinary_function_binding(name, env)
        if (!is.function(lexical) ||
              identical(lexical, get(name, envir = baseenv()))) {
          return(FALSE)
        }
      }
      env <- topenv(env)
    } else if (own_lookup || name %in% lookup$mask) {
      return(FALSE)
    }
  }
  if (!lookup_environment_is_external(
    env, bound, lookup, inherits = !own_lookup, safe_parents = FALSE
  )) {
    return(FALSE)
  }
  binding <- if (own_lookup) {
    if (!exists(name, envir = env, inherits = FALSE) ||
          bindingIsActive(name, env) ||
          rlang::env_binding_are_lazy(env, name)) {
      return(FALSE)
    }
    get(name, envir = env, inherits = FALSE)
  } else {
    ordinary_function_binding(name, env)
  }
  if (exclude_generic && inherits(binding, "genericFunction")) {
    return(FALSE)
  }
  is.function(binding) && !identical(binding, get(name, envir = baseenv()))
}

# Names requested by a removal call. NULL denotes an unreadable name.
# A caller with lexical scope can verify concatenation in a `list` argument.
literal_removal_names <- function(args, is_concat = function(expr) FALSE) {
  arg_names <- argument_names(args)
  removed <- character()
  for (i in seq_along(args)) {
    name <- arg_names[[i]]
    # An empty argument is R's missing marker, which cannot be read by name.
    if (rlang::is_missing(args[[i]])) {
      return(NULL)
    }
    arg <- args[[i]]
    literal <- if (identical(name, "list")) {
      static_character_value(arg, is_concat)
    } else if (!nzchar(name) && is_name_part(arg)) {
      rlang::as_name(arg)
    } else if (!nzchar(name) && is.character(arg) && !anyNA(arg)) {
      arg
    }
    if (is.null(literal)) {
      return(NULL)
    }
    removed <- c(removed, literal)
  }
  removed
}

# The first supplied formal in priority order, or an empty list for defaults.
# Match without evaluating caller expressions. A non-base resolver supplies its
# verified definition; other callers use the recognized base primitive.
base_call_argument <- function(expr, formals, definition = NULL) {
  callee <- resolved_callee_name(expr)
  args <- static_matched_arguments(
    rlang::call2(callee, !!!static_call_args(expr)),
    if (is.null(definition)) {
      args(get(callee, envir = baseenv()))
    } else {
      args(definition)
    }
  )
  for (formal in formals) {
    if (formal %in% names(args)) {
      return(args[formal])
    }
  }
  list()
}

# Existing environment values without forcing caller bindings, or NULL.
# Unknown expressions, promises, active bindings, and names shadowed by the
# mask or an expression-local binding stay conservative without running code.
lookup_environment_value <- function(expr, bound, lookup) {
  if (is_parenthesized(expr)) {
    return(lookup_environment_value(
      unparenthesized_value(expr), bound, lookup
    ))
  }
  if (is.environment(expr)) {
    return(expr)
  }
  if (unresolved_lookup_name() %in% lookup$rebound) {
    return(NULL)
  }
  if (rlang::is_call(expr, c("$", "[["), n = 2L) &&
        rlang::is_symbol(expr[[2L]], ".env") &&
        !".env" %in% c(bound, lookup$rebound)) {
    name <- if (rlang::is_call(expr, "$") && is_name_part(expr[[3L]])) {
      rlang::as_name(expr[[3L]])
    } else {
      static_character_value(expr[[3L]])
    }
    if (!is.null(name) && length(name) == 1L && !is.null(lookup)) {
      return(lookup_environment_value(
        rlang::sym(name), character(),
        list(env = lookup$env, mask = character(), rebound = lookup$rebound)
      ))
    }
  }
  if (!is_name_part(expr)) {
    return(NULL)
  }
  name <- rlang::as_name(expr)
  if (is.null(lookup) || name %in% c(bound, lookup$mask, lookup$rebound)) {
    return(NULL)
  }
  current <- lookup$env
  while (!identical(current, emptyenv())) {
    if (exists(name, envir = current, inherits = FALSE)) {
      if (bindingIsActive(name, current) ||
            rlang::env_binding_are_lazy(current, name)) {
        return(NULL)
      }
      value <- get(name, envir = current, inherits = FALSE)
      return(if (is.environment(value)) value else NULL)
    }
    current <- parent.env(current)
  }
  NULL
}

# Whether a lookup stays outside the mask after visible parent replacements.
# Non-inheriting reads cannot reach a changed parent. A TRUE target value
# denotes an unresolved mutation target, which invalidates every proof.
# Safe parents exclude mask reads, but can still change function lookup.
lookup_environment_is_external <- function(expr, bound, lookup,
                                           inherits = TRUE,
                                           safe_parents = TRUE) {
  value <- lookup_environment_value(expr, bound, lookup)
  if (is.null(value)) {
    return(FALSE)
  }
  if (!inherits) {
    return(TRUE)
  }
  while (!identical(value, emptyenv())) {
    if (any(vapply(lookup$reparented, function(target) {
      !(safe_parents && target$safe_parent) &&
        (isTRUE(target$value) || identical(target$value, value))
    }, logical(1)))) {
      return(FALSE)
    }
    value <- parent.env(value)
  }
  TRUE
}

# The name the walk reports for a read it could not resolve.
#
# #130 fixed the walk as two-valued -- a name is read or it is not -- and this
# keeps that shape rather than adding a third answer to it: the marker is a
# name, carried in the vector every branch already unions, and the one site
# that reads it is `expression_alias_dependencies()`, which turns it into a
# read of every alias in scope. Returning a record instead, as
# `statement_reads_and_scope()` does for the two answers a binding statement
# has, would rewrite every branch of the walk to carry a second field that
# only three of them can set.
#
# The alternatives are the two #130 rules out. Reporting nothing is the silence
# the whole walk exists to prevent, and refusing the call outright would reject
# `get(name)` wherever a summary precedes it, which is legal code.
#
# The `..marginplyr` prefix is the one this package's internal names carry. A
# column of this name would be read as unresolvable rather than as itself,
# which is the over-reporting direction, so a collision costs a diagnostic and
# never a silent miss.
unresolved_lookup_name <- function() {
  "..marginplyr_unresolved_lookup"
}
