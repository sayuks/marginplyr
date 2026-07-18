compile_grouping_spec <- function(.grouping,
                                  data_vars,
                                  data_proxy = NULL,
                                  .by = character(),
                                  .duplicates = c("error", "drop", "keep")) {
  .duplicates <- match.arg(.duplicates)
  stopifnot(is.character(data_vars), !anyNA(data_vars))
  stopifnot(is.character(.by), !anyNA(.by))
  if (is.null(data_proxy)) {
    data_proxy <- stats::setNames(as.list(seq_along(data_vars)), data_vars)
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
  if (!inherits(.grouping, "margin_grouping_spec")) {
    stop(
      "`.grouping` must be created with `grouping_set()`, ",
      "`grouping_sets()`, `rollup()`, `cube()`, or `grouping_spec()`.",
      call. = FALSE
    )
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
  switch(
    spec$type,
    set = list(resolve_grouping_set(spec, data_vars, data_proxy)),
    sets = expand_grouping_sets(spec, data_vars, data_proxy),
    rollup = expand_rollup(spec, data_vars, data_proxy),
    cube = expand_cube(spec, data_vars, data_proxy),
    product = expand_grouping_product(spec, data_vars, data_proxy),
    stop("Unknown grouping specification type.", call. = FALSE)
  )
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
        if (!is.null(nested)) {
          stop(
            "A `grouping_set()` can contain columns, not another ",
            "grouping family.",
            call. = FALSE
          )
        }
        resolve_grouping_selection(arg, data_proxy)
      }
    ),
    use.names = FALSE
  )
  unique(cols)
}

expand_grouping_sets <- function(spec, data_vars, data_proxy) {
  if (length(spec$args) == 0L) {
    stop(
      "`grouping_sets()` requires at least one set. Use `grouping_set()` ",
      "for the empty grouping set.",
      call. = FALSE
    )
  }

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
        if (!identical(nested$type, "set")) {
          stop(
            sprintf(
              paste0(
                "`%s()` only accepts columns or `grouping_set()` ",
                "composite dimensions."
              ),
              spec$type
            ),
            call. = FALSE
          )
        }
        cols <- resolve_grouping_set(nested, data_vars, data_proxy)
        if (length(cols) == 0L) {
          stop(
            "An empty `grouping_set()` cannot be a composite dimension.",
            call. = FALSE
          )
        }
        list(cols)
      }
    ),
    recursive = FALSE
  )

  if (length(units) == 0L) {
    stop(
      sprintf("`%s()` requires at least one dimension.", spec$type),
      call. = FALSE
    )
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

grouping_arg_spec <- function(arg, data_vars) {
  expr <- rlang::quo_get_expr(arg)
  if (is.symbol(expr) && as.character(expr) %in% data_vars) {
    return(NULL)
  }

  constructors <- c(
    "grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec"
  )
  is_constructor_call <- FALSE
  if (rlang::is_call(expr)) {
    fn <- expr[[1]]
    if (is.symbol(fn)) {
      is_constructor_call <- as.character(fn) %in% constructors
    } else if (
      rlang::is_call(fn, c("::", ":::")) &&
        length(fn) == 3L &&
        identical(as.character(fn[[2]]), "marginplyr")
    ) {
      is_constructor_call <- as.character(fn[[3]]) %in% constructors
    }
  }

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

grouping_selection_proxy <- function(.data) {
  needs_local_proxy <- inherits(
    .data,
    c(
      "dtplyr_step",
      "arrow_dplyr_query",
      "ArrowTabular",
      "Dataset",
      "tbl_duckdb_connection"
    )
  )
  if (needs_local_proxy) {
    return(dplyr::collect(utils::head(.data, n = 0L)))
  }
  .data
}
