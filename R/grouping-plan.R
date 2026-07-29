validate_grouping_spec_early <- function(grouping_spec) {
  if (is.null(grouping_spec)) {
    return(invisible(NULL))
  }
  if (!inherits(grouping_spec, "margin_grouping_spec")) {
    stop(
      "`.grouping` must be created with ",
      format_grouping_constructors(),
      ".",
      call. = FALSE
    )
  }

  kind <- grouping_spec$type
  args <- grouping_spec$args
  if (
    !is.character(kind) ||
      length(kind) != 1L ||
      !is.list(args)
  ) {
    abort_invalid_grouping_spec()
  }

  rule <- find_grouping_kind_rule(kind)
  if (is.null(rule)) {
    abort_invalid_grouping_spec()
  }
  rule$validate_empty(grouping_spec)

  invisible(NULL)
}

abort_invalid_grouping_spec <- function() {
  stop("Invalid grouping specification.", call. = FALSE)
}

abort_empty_grouping_units <- function(kind) {
  stop(
    sprintf("`%s()` requires at least one dimension.", kind),
    call. = FALSE
  )
}

abort_empty_composite <- function() {
  stop(
    "An empty `grouping_set()` cannot be a composite dimension.",
    call. = FALSE
  )
}

allow_empty_grouping <- function(spec) {
  invisible(NULL)
}

validate_empty_grouping_sets <- function(spec) {
  if (length(spec$args) == 0L) {
    stop(
      "`grouping_sets()` requires at least one set. Use `grouping_set()` ",
      "for the empty grouping set.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

validate_empty_grouping_units <- function(spec) {
  if (length(spec$args) == 0L) {
    abort_empty_grouping_units(spec$type)
  }
  invisible(NULL)
}

reject_nested_in_set <- function(parent, nested) {
  stop(
    "A `grouping_set()` can contain columns, not another ",
    "grouping family.",
    call. = FALSE
  )
}

allow_nested_grouping <- function(parent, nested) {
  invisible(NULL)
}

validate_nested_grouping_units <- function(parent, nested) {
  if (!identical(nested$type, "set")) {
    stop(
      sprintf(
        paste0(
          "`%s()` only accepts columns or `grouping_set()` ",
          "composite dimensions."
        ),
        parent$type
      ),
      call. = FALSE
    )
  }
  if (length(nested$args) == 0L) {
    abort_empty_composite()
  }

  invisible(NULL)
}

is_name_only_expr <- function(expr, env, data_vars) {
  if (is.symbol(expr)) {
    name <- as.character(expr)
    return(
      name %in% data_vars ||
        !rlang::env_has(
          env,
          name,
          inherit = TRUE
        )
    )
  }
  if (!is.language(expr)) {
    return(is.atomic(expr))
  }
  if (!rlang::is_call(expr)) {
    return(FALSE)
  }

  call_name <- rlang::call_name(expr)
  if (is.null(call_name)) {
    return(FALSE)
  }
  leaf_helpers <- c(
    "all_of", "any_of", "starts_with", "ends_with", "contains",
    "matches", "num_range", "everything", "last_col"
  )
  if (call_name %in% leaf_helpers) {
    return(TRUE)
  }
  if (!call_name %in% c("c", ":", "!", "-", "|", "&", "(")) {
    return(FALSE)
  }

  args <- rlang::call_args(expr)
  all(vapply(
    args,
    is_name_only_expr,
    logical(1),
    env = env,
    data_vars = data_vars
  ))
}

is_name_only_selection <- function(arg, data_vars) {
  is_name_only_expr(
    rlang::quo_get_expr(arg),
    env = rlang::quo_get_env(arg),
    data_vars = data_vars
  )
}

grouping_name_proxy <- function(data_vars) {
  stats::setNames(as.list(seq_along(data_vars)), data_vars)
}

preflight_grouping_spec <- function(grouping_spec, data_vars) {
  stopifnot(is.character(data_vars), !anyNA(data_vars))
  validate_grouping_spec_early(grouping_spec)
  if (is.null(grouping_spec)) {
    return(list(spec = NULL, name_only = TRUE))
  }

  rule <- find_grouping_kind_rule(grouping_spec$type)
  stopifnot(!is.null(rule))
  name_only <- TRUE
  for (arg in grouping_spec$args) {
    nested <- grouping_arg_spec(arg, data_vars)
    if (is.null(nested)) {
      name_only <- name_only && is_name_only_selection(arg, data_vars)
      next
    }

    nested_preflight <- preflight_grouping_spec(nested, data_vars)
    rule$validate_nested(grouping_spec, nested_preflight$spec)
    name_only <- name_only && nested_preflight$name_only
  }
  list(spec = grouping_spec, name_only = name_only)
}

compile_grouping_spec <- function(.grouping,
                                  data_vars,
                                  data_proxy = NULL,
                                  .by = character(),
                                  .duplicates = c("error", "drop", "keep")) {
  .duplicates <- match.arg(.duplicates)
  preflight <- preflight_grouping_spec(.grouping, data_vars)
  compile_grouping_spec_impl(
    preflight$spec,
    data_vars = data_vars,
    data_proxy = data_proxy,
    .by = .by,
    .duplicates = .duplicates
  )
}

compile_grouping_spec_impl <- function(.grouping,
                                       data_vars,
                                       data_proxy,
                                       .by,
                                       .duplicates) {
  stopifnot(is.character(.by), !anyNA(.by))
  stopifnot(.duplicates %in% c("error", "drop", "keep"))
  if (is.null(data_proxy)) {
    data_proxy <- grouping_name_proxy(data_vars)
  }

  unknown_by <- setdiff(.by, data_vars)
  if (length(unknown_by) > 0L) {
    stop(
      "Unknown `.by` column", if (length(unknown_by) == 1L) " " else "s ",
      paste0("`", unknown_by, "`", collapse = ", "), ".",
      call. = FALSE
    )
  }

  if (is.null(.grouping)) {
    .grouping <- new_grouping_spec("set", list())
  }

  expanded <- unname(
    expand_grouping_family(.grouping, data_vars, data_proxy)
  )
  dimensions <- unique(unlist(expanded, use.names = FALSE))

  overlap <- intersect(.by, dimensions)
  if (length(overlap) > 0L) {
    stop(
      "Columns cannot appear in both `.by` and `.grouping`: ",
      paste0("`", overlap, "`", collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  normalized <- lapply(
    expanded,
    function(set) c(.by, dimensions[dimensions %in% set])
  )
  keys <- vapply(
    normalized,
    function(set) paste(as.integer(dimensions %in% set), collapse = ""),
    character(1)
  )
  duplicate_keys <- duplicated(keys) | duplicated(keys, fromLast = TRUE)

  if (any(duplicate_keys) && identical(.duplicates, "error")) {
    groups <- split(which(duplicate_keys), keys[duplicate_keys])
    positions <- vapply(groups, paste, collapse = ", ", character(1))
    stop(
      "Duplicate grouping sets were produced at position",
      if (length(groups) == 1L) "s " else " groups ",
      paste(positions, collapse = "; "),
      ". Use `.duplicates = \"drop\"` or `\"keep\"`.",
      call. = FALSE
    )
  }

  if (identical(.duplicates, "drop")) {
    keep <- !duplicated(keys)
    normalized <- normalized[keep]
    keys <- keys[keep]
  }

  masks <- matrix(
    0L,
    nrow = length(normalized),
    ncol = length(dimensions),
    dimnames = list(NULL, dimensions)
  )
  if (length(dimensions) > 0L) {
    for (i in seq_along(normalized)) {
      masks[i, ] <- as.integer(!dimensions %in% normalized[[i]])
    }
  }

  structure(
    list(
      by = unique(.by),
      dimensions = dimensions,
      sets = normalized,
      grouping_masks = masks,
      duplicates = .duplicates
    ),
    class = "margin_grouping_plan"
  )
}

expand_grouping_family <- function(spec, data_vars, data_proxy) {
  rule <- find_grouping_kind_rule(spec$type)
  if (is.null(rule)) {
    stop("Unknown grouping specification type.", call. = FALSE)
  }
  rule$expand(spec, data_vars, data_proxy)
}

expand_single_grouping_set <- function(spec, data_vars, data_proxy) {
  list(resolve_grouping_set(spec, data_vars, data_proxy))
}

resolve_grouping_set <- function(spec, data_vars, data_proxy) {
  if (length(spec$args) == 0L) {
    return(character())
  }

  cols <- unlist(
    lapply(
      spec$args,
      function(arg) {
        nested <- grouping_arg_spec(arg, data_vars)
        stopifnot(is.null(nested))
        resolve_grouping_selection(arg, data_proxy)
      }
    ),
    use.names = FALSE
  )
  unique(cols)
}

expand_grouping_sets <- function(spec, data_vars, data_proxy) {
  unlist(
    lapply(
      spec$args,
      function(arg) {
        nested <- grouping_arg_spec(arg, data_vars)
        if (is.null(nested)) {
          return(list(resolve_grouping_selection(arg, data_proxy)))
        }
        expand_grouping_family(nested, data_vars, data_proxy)
      }
    ),
    recursive = FALSE
  )
}

resolve_grouping_units <- function(spec, data_vars, data_proxy) {
  units <- unlist(
    lapply(
      spec$args,
      function(arg) {
        nested <- grouping_arg_spec(arg, data_vars)
        if (is.null(nested)) {
          cols <- resolve_grouping_selection(arg, data_proxy)
          return(lapply(cols, function(col) col))
        }
        stopifnot(identical(nested$type, "set"))
        cols <- resolve_grouping_set(nested, data_vars, data_proxy)
        if (length(cols) == 0L) {
          abort_empty_composite()
        }
        list(cols)
      }
    ),
    recursive = FALSE
  )

  if (length(units) == 0L) {
    abort_empty_grouping_units(spec$type)
  }
  units
}

expand_rollup <- function(spec, data_vars, data_proxy) {
  units <- resolve_grouping_units(spec, data_vars, data_proxy)
  lapply(
    rev(seq.int(0L, length(units))),
    function(n) {
      if (n == 0L) character() else unique(unlist(units[seq_len(n)]))
    }
  )
}

expand_cube <- function(spec, data_vars, data_proxy) {
  units <- resolve_grouping_units(spec, data_vars, data_proxy)
  n <- length(units)
  indices <- unlist(
    lapply(
      rev(seq.int(0L, n)),
      function(size) utils::combn(seq_len(n), size, simplify = FALSE)
    ),
    recursive = FALSE
  )

  lapply(
    indices,
    function(index) {
      if (length(index) == 0L) character() else unique(unlist(units[index]))
    }
  )
}

expand_grouping_product <- function(spec, data_vars, data_proxy) {
  product <- list(character())
  if (length(spec$args) == 0L) {
    return(product)
  }

  for (arg in spec$args) {
    nested <- grouping_arg_spec(arg, data_vars)
    family <- if (is.null(nested)) {
      list(resolve_grouping_selection(arg, data_proxy))
    } else {
      expand_grouping_family(nested, data_vars, data_proxy)
    }

    product <- unlist(
      lapply(
        product,
        function(left) lapply(family, function(right) unique(c(left, right)))
      ),
      recursive = FALSE
    )
  }
  product
}

grouping_kind_rules <- local({
  rules <- NULL

  function() {
    if (is.null(rules)) {
      rules <<- list(
        set = list(
          constructor = "grouping_set",
          validate_empty = allow_empty_grouping,
          validate_nested = reject_nested_in_set,
          expand = expand_single_grouping_set
        ),
        sets = list(
          constructor = "grouping_sets",
          validate_empty = validate_empty_grouping_sets,
          validate_nested = allow_nested_grouping,
          expand = expand_grouping_sets
        ),
        rollup = list(
          constructor = "rollup",
          validate_empty = validate_empty_grouping_units,
          validate_nested = validate_nested_grouping_units,
          expand = expand_rollup
        ),
        cube = list(
          constructor = "cube",
          validate_empty = validate_empty_grouping_units,
          validate_nested = validate_nested_grouping_units,
          expand = expand_cube
        ),
        product = list(
          constructor = "grouping_spec",
          validate_empty = allow_empty_grouping,
          validate_nested = allow_nested_grouping,
          expand = expand_grouping_product
        )
      )
    }
    rules
  }
})

find_grouping_kind_rule <- function(kind) {
  if (
    !is.character(kind) ||
      length(kind) != 1L ||
      is.na(kind)
  ) {
    return(NULL)
  }
  grouping_kind_rules()[[kind]]
}

grouping_constructor_names <- function() {
  unname(vapply(
    grouping_kind_rules(),
    function(rule) rule$constructor,
    character(1)
  ))
}

format_grouping_constructors <- function() {
  constructors <- paste0("`", grouping_constructor_names(), "()`")
  last <- length(constructors)
  paste0(
    paste(constructors[-last], collapse = ", "),
    ", or ",
    constructors[[last]]
  )
}

grouping_arg_spec <- function(arg, data_vars) {
  expr <- rlang::quo_get_expr(arg)
  if (is.symbol(expr) && as.character(expr) %in% data_vars) {
    return(NULL)
  }

  constructors <- grouping_constructor_names()
  call_name <- if (rlang::is_call(expr)) rlang::call_name(expr) else NULL
  call_ns <- if (rlang::is_call(expr)) rlang::call_ns(expr) else NULL
  is_constructor_call <-
    !is.null(call_name) &&
    call_name %in% constructors &&
    (is.null(call_ns) || identical(call_ns, "marginplyr"))

  should_evaluate <-
    is_constructor_call ||
    is.symbol(expr) ||
    !is.language(expr)
  if (!should_evaluate) {
    return(NULL)
  }

  value <- tryCatch(
    rlang::eval_tidy(arg),
    error = function(cnd) structure(list(), class = "grouping_eval_failed")
  )
  if (inherits(value, "margin_grouping_spec")) {
    return(value)
  }
  NULL
}

resolve_grouping_selection <- function(arg, data_proxy) {
  selected <- tryCatch(
    tidyselect::eval_select(arg, data = data_proxy, strict = TRUE),
    error = function(cnd) {
      stop(
        "Invalid grouping column selection: ", conditionMessage(cnd),
        call. = FALSE
      )
    }
  )
  names(selected)
}
