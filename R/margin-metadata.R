# Read standard variable labels from the operation's existing schema snapshot.
margin_variable_labels <- function(columns) {
  labels <- lapply(columns, attr, which = "label", exact = TRUE)
  labels[vapply(labels, function(label) {
    is.character(label) && !is.object(label) && length(label) == 1L
  }, logical(1))]
}

# Attach one captured label without changing the vector's native metadata.
set_margin_variable_label <- function(x, label) {
  attr(x, "label") <- label
  x
}

# Restore captured source labels or agreed native summary labels.
# Callers supply their evidence; this never infers a label from a name.
restore_margin_variable_labels <- function(data, labels) {
  labels <- labels[intersect(names(labels), margin_col_names(data))]
  if (length(labels) == 0L) {
    return(data)
  }
  if (is.data.frame(data)) {
    for (column in names(labels)) {
      data[[column]] <- set_margin_variable_label(
        data[[column]], labels[[column]]
      )
    }
    return(data)
  }
  if (inherits(data, "dtplyr_step")) {
    return(margin_safe_column_reads(
      data, names(labels), function(source, safe_name) {
        expressions <- lapply(names(labels), function(column) {
          rlang::call2(
            marginplyr_private_call("set_margin_variable_label"),
            margin_column_pronoun(safe_name(column)), labels[[column]]
          )
        })
        names(expressions) <- vapply(names(labels), safe_name, character(1))
        dplyr::mutate(source, !!!expressions)
      }
    ))
  }
  attr(data, "marginplyr_label_boundary") <- data
  attr(data, "marginplyr_variable_labels") <- labels
  class(data) <- c("marginplyr_labelled_result", class(data))
  data
}

# Combine labels on already evaluated native branches. Conflicts and unusual
# representations retain the native vector-combination answer (ADR 0042).
combine_margin_column_labels <- function(branches) {
  columns <- names(branches[[1L]])
  labels <- lapply(columns, function(column) {
    values <- lapply(branches, function(branch) {
      attr(branch[[column]], "label", exact = TRUE)
    })
    standard <- vapply(values, function(value) {
      is.null(value) ||
        (is.character(value) && !is.object(value) && length(value) == 1L)
    }, logical(1))
    if (!all(standard)) {
      return(NULL)
    }
    present <- unique(unlist(values, use.names = FALSE))
    nonmissing <- present[!is.na(present)]
    if (length(nonmissing) == 1L) {
      return(nonmissing)
    }
    if (length(nonmissing) == 0L && anyNA(present)) {
      return(NA_character_)
    }
    NULL
  })
  names(labels) <- columns
  labels[!vapply(labels, is.null, logical(1))]
}

# Standard frame classes covered by the native dataset-reference contract.
# Extra frame classes retain their existing S3/vctrs behavior.
standard_margin_frame <- function(data) {
  classes <- class(data)
  identical(classes, "data.frame") ||
    identical(class(data), c("tbl_df", "tbl", "data.frame")) ||
    identical(class(data), c("data.table", "data.frame"))
}

# Dataset metadata excludes fields describing the actual frame structure.
margin_frame_metadata <- function(data) {
  attributes(data)[setdiff(names(attributes(data)), c(
    "names", "row.names", "class", "groups", ".internal.selfref",
    "sorted", "index"
  ))]
}

# Apply native inner-frame metadata to a generated cell, never its structure.
restore_margin_frame_metadata <- function(data, metadata) {
  for (name in names(metadata)) attr(data, name) <- metadata[[name]]
  data
}

# Native tidyr cells retain metadata except direct data.table cells. Local
# nest_by retains it only for keyless sets; dtplyr uses that local reference.
margin_nest_frame_metadata <- function(operation, rowwise) {
  proxy <- operation$data_proxy
  metadata <- if (standard_margin_frame(proxy)) {
    margin_frame_metadata(proxy)
  } else {
    list()
  }
  lapply(operation$plan$sets, function(set) {
    if (rowwise) {
      if (length(c(operation$plan$by, set)) == 0L) metadata else list()
    } else if (identical(operation$backend$kind, "local") &&
                 inherits(proxy, "data.table")) {
      list()
    } else {
      metadata
    }
  })
}

# A generated cell knows its grouping-set occurrence, even with no payload.
restore_margin_cell_metadata <- function(data, metadata, set_id) {
  # summarise also evaluates an empty group to infer its output type.
  if (length(set_id) == 0L) return(data)
  restore_margin_frame_metadata(data, metadata[[set_id[[1L]]]])
}


# A deferred constant for dtplyr, whose expression walker rejects list literals.
# The closure owns only the captured metadata and never evaluates caller code.
margin_metadata_literal <- function(value) {
  force(value)
  rlang::call2(rlang::call2(base::`(`, function() value))
}

# Native label evidence retained between generated union nodes, without rows.
margin_label_evidence <- function(data) {
  state <- attr(data, "marginplyr_native_label_state", exact = TRUE)
  if (inherits(state, "marginplyr_label_evidence")) return(state)
  labels <- lapply(data, function(column) {
    label <- attr(column, "label", exact = TRUE)
    valid <- is.null(label) ||
      (is.character(label) && !is.object(label) && length(label) == 1L)
    values <- if (valid && !is.null(label)) {
      attributes(label) <- NULL
      label
    } else {
      character()
    }
    list(valid = valid, values = values)
  })
  structure(labels, class = "marginplyr_label_evidence")
}

# Preserve the native union call and its failure/evaluation order. Intermediate
# evidence is consumed at the final node and never reaches a returned frame.
union_dtplyr_margin_labels <- function(x, y, final = FALSE) {
  result <- data.table::funion(x, y, all = TRUE)
  left <- margin_label_evidence(x)
  right <- margin_label_evidence(y)
  state <- lapply(names(left), function(column) {
    list(
      valid = left[[column]]$valid && right[[column]]$valid,
      values = unique(c(left[[column]]$values, right[[column]]$values))
    )
  })
  names(state) <- names(left)
  if (!final) {
    attr(result, "marginplyr_native_label_state") <- structure(
      state, class = "marginplyr_label_evidence"
    )
    return(result)
  }
  labels <- lapply(state, function(entry) {
    if (!entry$valid) return(NULL)
    values <- entry$values[!is.na(entry$values)]
    if (length(values) == 1L) return(values)
    if (length(values) == 0L && anyNA(entry$values)) return(NA_character_)
    NULL
  })
  labels <- labels[!vapply(labels, is.null, logical(1))]
  restore_margin_variable_labels(result, labels)
}

# The balanced native source graph owns sources and locals. Its generated call
# keeps the same native unions, adding only scalar label evidence (ADR 0042).
#' @exportS3Method dtplyr::dt_call
#' @noRd
dt_call.marginplyr_label_union <- function(x, needs_copy = x$needs_copy) {
  dt_call <- utils::getFromNamespace("dt_call", "dtplyr")
  branches <- x[["marginplyr_branches"]]
  calls <- lapply(seq_along(branches), function(i) {
    if (i == 1L) dt_call(branches[[i]], needs_copy) else dt_call(branches[[i]])
  })
  while (length(calls) > 1L) {
    n <- length(calls)
    pairs <- lapply(seq_len(n %/% 2L), function(i) {
      rlang::call2(
        marginplyr_private_call("union_dtplyr_margin_labels"),
        calls[[2L * i - 1L]], calls[[2L * i]]
      )
    })
    calls <- if (n %% 2L == 1L) c(pairs, calls[n]) else pairs
  }
  calls[[1L]][["final"]] <- TRUE
  calls[[1L]]
}


# Restore construction-time labels after the backend's direct collection.
#' @exportS3Method dplyr::collect
#' @noRd
collect.marginplyr_labelled_result <- function(x, ...) {
  if (!direct_margin_label_result(x)) return(NextMethod())
  labels <- attr(x, "marginplyr_variable_labels", exact = TRUE)
  result <- NextMethod()
  if (is.data.frame(result)) {
    return(restore_margin_variable_labels(result, labels))
  }
  if (inherits(result, "ArrowTabular")) {
    # Clone the R6 wrapper before replacing schema metadata on its pointer.
    result <- result$clone()
    metadata <- result$metadata
    for (column in intersect(names(labels), result$ColumnNames())) {
      metadata$r$columns[[column]]$attributes$label <- labels[[column]]
    }
    result$metadata <- metadata
  }
  result
}

# A companion belongs to the exact result returned by the Margin verb. Later
# ordinary operations cannot reuse captured labels for newly computed columns.
direct_margin_label_result <- function(x) {
  boundary <- attr(x, "marginplyr_label_boundary", exact = TRUE)
  if (is.null(boundary)) return(FALSE)
  class(x) <- setdiff(class(x), "marginplyr_labelled_result")
  attr(x, "marginplyr_variable_labels") <- NULL
  attr(x, "marginplyr_label_boundary") <- NULL
  identical(x, boundary)
}

# Numeric storage and generated scalar calls establish safe native candidates.
# Object columns additionally require proven native optimization at execution.
native_margin_scalar_proof <- function(expr, schema, grouped) {
  if (!rlang::is_call(expr) || !rlang::is_symbol(expr[[1L]])) return(NULL)
  name <- rlang::as_name(expr[[1L]])
  owners <- c(mean = "base", sum = "base", min = "base", max = "base",
              prod = "base", median = "stats", var = "stats", sd = "stats",
              length = "base", head = "utils", tail = "utils",
              first = "data.table", last = "data.table")
  if (!name %in% names(owners)) return(NULL)
  args <- as.list(expr)[-1L]
  if (name %in% c("first", "last") && !grouped) return(NULL)
  if (length(args) == 0L || !rlang::is_symbol(args[[1L]])) return(NULL)
  column <- rlang::as_name(args[[1L]])
  if (!column %in% names(schema)) return(NULL)
  value <- schema[[column]]
  if (!typeof(value) %in% c("integer", "double") ||
        !is.null(attr(value, "dim", exact = TRUE))) return(NULL)
  if (name %in% c("head", "tail")) {
    if (!grouped || length(args) != 2L ||
          !identical(unname(args[[2L]]), 1L)) return(NULL)
  } else if (length(args) > 1L) {
    if (length(args) != 2L || !identical(names(args)[[2L]], "na.rm") ||
          !(identical(args[[2L]], TRUE) || identical(args[[2L]], FALSE))) {
      return(NULL)
    }
  }
  list(name = name, owner = unname(owners[[name]]), object = is.object(value),
       gforce_only = name %in% c("first", "last"))
}

# Recheck lexical function bindings at execution without forcing caller code.
# A rebound, active or promised callee chooses the original checked expression.
native_margin_scalar_bindings <- function(proofs, env) {
  all(vapply(proofs, function(proof) {
    missing <- new.env(parent = emptyenv())
    binding <- ordinary_function_binding(
      proof$name, env, owners = proof$owner, absent = missing
    )
    known <- identical(binding, getExportedValue(proof$owner, proof$name)) ||
      (isTRUE(proof$gforce_only) && identical(binding, missing))
    known && native_margin_default_dispatch(proof, env)
  }, logical(1)))
}

# Retain checked/native alternatives as expressions on the same lazy step.
# No summary is constructed or evaluated twice and the input schema is reused.
native_margin_scalar_step <- function(step, schema, share_outputs) {
  if (is.null(schema)) return(step)
  if (inherits(step, "dtplyr_step_group")) {
    step$parent <- native_margin_scalar_step(
      step$parent, schema, share_outputs
    )
    return(step)
  }
  if (!inherits(step, "dtplyr_step_subset") || !rlang::is_call(step$j)) {
    return(step)
  }
  validators <- lapply(c(
    "check_dtplyr_share_source", "check_dtplyr_share_scalar",
    "check_dtplyr_share_ordinary"
  ), dtplyr_share_private_call)
  expressions <- as.list(step$j)[-1L]
  proofs <- list()
  changed <- FALSE
  positions <- integer()
  checks <- list()
  for (i in seq_along(expressions)) {
    expression <- expressions[[i]]
    if (!rlang::is_call(expression) || !any(vapply(
      validators, identical, logical(1), y = expression[[1L]]
    ))) next
    value <- expression[[2L]]
    proof <- if (identical(value, quote(.N))) {
      list()
    } else {
      native_margin_scalar_proof(value, schema, length(step$groups) > 0L)
    }
    if (is.null(proof)) next
    if (length(proof) > 0L) proofs[[as.character(i)]] <- proof
    checks[[as.character(i)]] <- expression
    positions <- c(positions, i)
    expressions[[i]] <- value
    changed <- TRUE
  }
  if (!changed) return(step)
  placeholders <- intersect(names(expressions), share_outputs)
  placeholders <- placeholders[vapply(expressions[placeholders],
                                      identical, logical(1), y = NA_real_)]
  keep <- !names(expressions) %in% placeholders
  native_expressions <- expressions[keep]
  all_gforce <- length(step$groups) > 0L && all(vapply(
    native_expressions, function(expression) {
      identical(expression, quote(.N)) || !is.null(
        native_margin_scalar_proof(expression, schema, grouped = TRUE)
      )
    }, logical(1)
  ))
  object_positions <- as.integer(names(proofs)[vapply(
    proofs, function(proof) isTRUE(proof$object), logical(1)
  )])
  guarded <- object_positions[!vapply(object_positions, function(i) {
    (identical(proofs[[as.character(i)]]$name, "mean") &&
       length(step$groups) > 0L) || all_gforce
  }, logical(1))]
  gforce_only <- as.integer(names(proofs)[vapply(
    proofs, function(proof) isTRUE(proof$gforce_only), logical(1)
  )])
  if (!all_gforce) guarded <- union(guarded, gforce_only)
  for (i in guarded) expressions[[i]] <- as.list(step$j)[[i + 1L]]
  proofs <- proofs[setdiff(names(proofs), as.character(guarded))]
  checks <- checks[setdiff(names(checks), as.character(guarded))]
  positions <- setdiff(positions, guarded)
  object_positions <- setdiff(object_positions, guarded)
  if (length(positions) == 0L) return(step)
  expressions <- expressions[keep]
  names(checks) <- names(as.list(step$j)[-1L])[as.integer(names(checks))]
  checks <- checks[!vapply(checks, function(check) {
    identical(check[[1L]], dtplyr_share_private_call(
      "check_dtplyr_share_ordinary"
    ))
  }, logical(1))]
  step[["marginplyr_native_checks"]] <- checks
  object_functions <- vapply(proofs[as.character(object_positions)],
                             `[[`, character(1), "name")
  step[["marginplyr_scalar_min_opt"]] <- if (length(object_functions) == 0L) {
    0L
  } else if (all(object_functions == "mean")) {
    1L
  } else {
    2L
  }
  scalar_functions <- vapply(proofs, `[[`, character(1), "name")
  if (length(setdiff(gforce_only, guarded)) > 0L) {
    step[["marginplyr_scalar_min_opt"]] <- 2L
  }
  step[["marginplyr_scalar_needs_rows"]] <- any(object_functions != "mean") ||
    any(scalar_functions %in% c("head", "tail", "first", "last"))
  step[["marginplyr_share_placeholders"]] <- placeholders
  step[["marginplyr_native_j"]] <- rlang::call2(step$j[[1L]], !!!expressions)
  step[["marginplyr_scalar_proofs"]] <- proofs
  class(step) <- c("marginplyr_native_scalars", class(step))
  step
}

# Choose one summary call only after native callee bindings are established.
#' @exportS3Method dtplyr::dt_call
#' @noRd
dt_call.marginplyr_native_scalars <- function(x, needs_copy = x$needs_copy) {
  checked <- NextMethod()
  native <- checked
  native[[4L]] <- x[["marginplyr_native_j"]]
  if (length(x[["marginplyr_share_placeholders"]]) > 0L) {
    native <- rlang::call2(
      marginplyr_private_call("native_margin_share_columns"), native,
      x[["marginplyr_share_placeholders"]], x$vars,
      margin_metadata_literal(x[["marginplyr_native_checks"]])
    )
  }
  condition <- rlang::call2(
    marginplyr_private_call("native_margin_scalar_bindings"),
    margin_metadata_literal(x[["marginplyr_scalar_proofs"]]),
    margin_metadata_literal(x$env)
  )
  minimum <- x[["marginplyr_scalar_min_opt"]]
  if (minimum > 0L) {
    condition <- rlang::call2(
      base::`&&`, condition,
      rlang::call2(base::`>=`,
                   rlang::call2(base::getOption, "datatable.optimize"), minimum)
    )
  }
  source_name <- new_margin_internal_names(
    1L, unique(c(x$vars, all.names(checked), names(x$locals))),
    prefix = "..marginplyr_scalar_input_"
  )
  source <- rlang::sym(source_name)
  parent <- checked[[2L]]
  checked[[2L]] <- source
  if (length(x[["marginplyr_share_placeholders"]]) > 0L) {
    native[[2L]][[2L]] <- source
  } else {
    native[[2L]] <- source
  }
  if (isTRUE(x[["marginplyr_scalar_needs_rows"]])) {
    condition <- rlang::call2(
      base::`&&`, condition,
      rlang::call2(base::`>`, rlang::call2(base::nrow, source), 0L)
    )
  }
  body <- rlang::call2(base::`if`, condition, native, checked)
  bind <- rlang::call2(base::`<-`, source, parent)
  rlang::call2(base::`{`, bind, body)

}

# Generated share placeholders must not disable native aggregate optimization.
# Add them on the freshly summarized table, preserving its public column order.
native_margin_share_columns <- function(data, columns, column_order, checks) {
  if (nrow(data) > 0L) {
    for (column in names(checks)) {
      check <- checks[[column]]
      value <- data[[column]]
      checker <- if (identical(check[[1L]], dtplyr_share_private_call(
        "check_dtplyr_share_source"
      ))) {
        "check_native_margin_source"
      } else {
        "check_native_margin_scalar"
      }
      check[[1L]] <- marginplyr_private_call(checker)
      # Only the generated validator remains; its caller expression has run.
      check[[2L]] <- rlang::call2(base::quote, value)
      rlang::eval_bare(check, baseenv())
    }
  }
  for (column in columns) {
    data.table::set(data, j = column, value = rep(NA_real_, nrow(data)))
  }
  data.table::setcolorder(data, column_order)
  data
}

# Native scalar proofs establish cardinality; inspect the actual native column
# type without slicing it through a caller-owned vector method.
check_native_margin_source <- function(value, share_output, source_summary,
                                       share_kind, call_text) {
  if (!is_share_source_type(value)) {
    abort_share_source_type(value, share_output, source_summary, share_kind,
                            call = parse_call_text(call_text))
  }
  invisible(NULL)
}

# The across validator retains its generated input-to-output mapping.
check_native_margin_scalar <- function(value, input, mapping, call_text) {
  position <- match(input, mapping$inputs)
  if (is.na(position)) return(invisible(NULL))
  check_native_margin_source(
    value, mapping$share_outputs[[position]],
    mapping$source_summaries[[position]], mapping$share_kinds[[position]],
    call_text
  )
}

# Bare numeric vectors can also acquire implicit S3 methods. Unknown method
# bindings keep the original guard, without forcing a method or calling it.
native_margin_default_dispatch <- function(proof, env) {
  if (!proof$name %in% c("mean", "median")) return(TRUE)
  missing <- new.env(parent = emptyenv())
  owner <- asNamespace(proof$owner)
  registry <- get(".__S3MethodsTable__.", envir = owner)
  for (class in c("integer", "numeric")) {
    method <- paste(proof$name, class, sep = ".")
    if (!identical(ordinary_function_binding(method, env, absent = missing),
                   missing) ||
          exists(method, envir = registry, inherits = FALSE)) {
      return(FALSE)
    }
  }
  method <- paste0(proof$name, ".default")
  expected <- get(method, envir = owner, inherits = FALSE)
  identical(ordinary_function_binding(method, env, owners = proof$owner),
            expected) && dtplyr_registered_method(
    method, expected, envir = owner, home = owner, absent = TRUE
  )
}
