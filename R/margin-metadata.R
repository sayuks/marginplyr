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
    margin_label_consensus(
      all(standard), unique(unlist(values, use.names = FALSE))
    )
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
  structural <- c("names", "row.names", "class")
  if (inherits(data, c("grouped_df", "rowwise_df"))) {
    structural <- c(structural, "groups")
  }
  if (inherits(data, "data.table")) {
    structural <- c(structural, ".internal.selfref", "sorted", "index")
  }
  attributes(data)[setdiff(names(attributes(data)), structural)]
}

# Apply native inner-frame metadata to a generated cell, never its structure.
restore_margin_frame_metadata <- function(data, metadata) {
  for (name in names(metadata)) attr(data, name) <- metadata[[name]]
  data
}

# Native tidyr drops metadata for direct data.table or empty dtplyr cells.
# Local nest_by retains metadata only for keyless sets, also under dtplyr.
margin_nest_frame_metadata <- function(proxy, data_vars, plan, kind,
                                       rowwise, keep) {
  metadata <- if (standard_margin_frame(proxy)) {
    margin_frame_metadata(proxy)
  } else {
    list()
  }
  payload <- if (keep) {
    data_vars
  } else {
    setdiff(data_vars,
            c(plan$by, plan$dimensions))
  }
  empty_dtplyr <- identical(kind, "dtplyr") &&
    length(payload) == 0L
  direct_dt <- identical(kind, "local") &&
    inherits(proxy, "data.table")
  lapply(plan$sets, function(set) {
    if (rowwise) {
      if (length(c(plan$by, set)) == 0L) metadata else list()
    } else if (empty_dtplyr || direct_dt) {
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
  left <- margin_label_evidence(x)
  right <- margin_label_evidence(y)
  aligned <- align_dtplyr_union_types(x, y)
  result <- data.table::funion(aligned$x, aligned$y, all = TRUE)
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
    margin_label_consensus(entry$valid, entry$values)
  })
  labels <- labels[!vapply(labels, is.null, logical(1))]
  # The data frame `[[<-` copies the table and leaves a stale self-reference;
  # reallocating restores the valid data.table the native union returns.
  data.table::setalloccol(restore_margin_variable_labels(result, labels))
}

# Give two evaluated branches the common type a local data frame's vctrs
# combination gives, for each column `funion()` would refuse. `funion()`
# compares the first class, treating factor as character and integer as
# numeric; columns it accepts reach it unchanged. Branches are fresh summary
# results, so only a branch with a refused column is copied.
align_dtplyr_union_types <- function(x, y) {
  union_class <- function(column) {
    class <- class(column)[[1L]]
    switch(class, factor = "character", integer = "numeric", class)
  }
  refused <- names(x)[vapply(names(x), function(name) {
    !identical(union_class(x[[name]]), union_class(y[[name]]))
  }, logical(1))]
  if (length(refused) == 0L) {
    return(list(x = x, y = y))
  }
  x <- data.table::copy(x)
  y <- data.table::copy(y)
  for (name in refused) {
    common <- tryCatch(
      vctrs::vec_ptype2(x[[name]], y[[name]]),
      vctrs_error_incompatible_type = function(cnd) {
        abort_marginplyr(c(
          "Summary {.var {name}} has no common type across grouping sets.",
          i = paste0(
            "It is {.cls {vctrs::vec_ptype_full(x[[name]])}} in one grouping ",
            "set and {.cls {vctrs::vec_ptype_full(y[[name]])}} in another."
          )
        ), call = NULL)
      }
    )
    data.table::set(x, j = name, value = vctrs::vec_cast(x[[name]], common))
    data.table::set(y, j = name, value = vctrs::vec_cast(y[[name]], common))
  }
  list(x = x, y = y)
}

# The balanced native source graph owns sources and locals. Its generated call
# keeps the same native unions, adding only scalar label evidence (ADR 0042).
#' @exportS3Method dtplyr::dt_call
#' @noRd
dt_call.marginplyr_label_union <- function(x, needs_copy = x$needs_copy) {
  native <- NextMethod()
  rlang::call2(
    marginplyr_private_call("union_dtplyr_margin_labels"),
    native[[2L]], native[[3L]],
    final = isTRUE(x[["marginplyr_final_union"]])
  )
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

# Callee evidence names functions only; native data.table owns argument grammar.
# Unknown bindings retain the guarded expression without forcing caller code.
native_margin_scalar_proof <- function(expr) {
  if (!rlang::is_call(expr)) return(NULL)
  head <- expr[[1L]]
  if (!rlang::is_symbol(head) && !rlang::is_call(head, "::", n = 2L)) {
    return(NULL)
  }
  name <- static_call_name(expr)
  namespace <- static_call_ns(expr)
  owners <- c(mean = "base", sum = "base", min = "base", max = "base",
              prod = "base", median = "stats", var = "stats", sd = "stats",
              length = "base", head = "utils", tail = "utils",
              first = "data.table", last = "data.table", shift = "data.table",
              weighted.mean = "stats", `[` = "base", `[[` = "base")
  if (!name %in% names(owners)) return(NULL)
  if (!is.null(namespace) && !identical(namespace, owners[[name]])) return(NULL)
  list(name = name, owner = unname(owners[[name]]), namespace = namespace,
       gforce_only = name %in% c("first", "last", "shift"))
}

# Recheck lexical function bindings at execution without forcing caller code.
# A rebound, active or promised callee chooses the original checked expression.
native_margin_scalar_bindings <- function(proofs, env) {
  all(vapply(proofs, function(proof) {
    missing <- new.env(parent = emptyenv())
    binding <- ordinary_function_binding(
      proof$name, env, owners = proof$owner, absent = missing
    )
    known <- !is.null(proof$namespace) ||
      identical(binding, getExportedValue(proof$owner, proof$name)) ||
      (isTRUE(proof$gforce_only) && identical(binding, missing))
    known && native_margin_default_dispatch(proof, env)
  }, logical(1)))
}

# Retain checked/native alternatives on the same grouped query node. Native
# optimization precedes validation; both paths evaluate the parent only once.
native_margin_scalar_step <- function(step, schema, share_outputs) {
  if (is.null(schema)) return(step)
  if (inherits(step, "dtplyr_step_group")) {
    step$parent <- native_margin_scalar_step(step$parent, schema, share_outputs)
    return(step)
  }
  if (!inherits(step, "dtplyr_step_subset") || !rlang::is_call(step$j) ||
        length(step$groups) == 0L) return(step)
  validators <- lapply(c(
    "check_dtplyr_share_source", "check_dtplyr_share_scalar",
    "check_dtplyr_share_ordinary"
  ), dtplyr_share_private_call)
  expressions <- as.list(step$j)[-1L]
  checks <- vector("list", length(expressions))
  guarded <- rep(FALSE, length(expressions))
  proofs <- list()
  for (i in seq_along(expressions)) {
    expression <- expressions[[i]]
    if (!rlang::is_call(expression) || !any(vapply(
      validators, identical, logical(1), y = expression[[1L]]
    ))) next
    checks[[i]] <- expression
    # Native .get_gcall reads q[[2]]; zero-argument calls have no GForce form.
    value <- expression[[2L]]
    guarded[[i]] <- rlang::is_call(value) && length(value) == 1L
    if (!guarded[[i]]) expressions[[i]] <- value
    proof <- native_margin_scalar_proof(expression[[2L]])
    if (!is.null(proof)) proofs[[length(proofs) + 1L]] <- proof
  }
  if (all(vapply(checks, is.null, logical(1)))) return(step)
  placeholders <- intersect(names(expressions), share_outputs)
  placeholders <- placeholders[vapply(expressions[placeholders], identical,
                                      logical(1), y = NA_real_)]
  keep <- !names(expressions) %in% placeholders
  checks <- checks[keep]
  attr(checks, "marginplyr_native_guarded") <- guarded[keep]
  step[["marginplyr_native_checks"]] <- checks
  step[["marginplyr_share_placeholders"]] <- placeholders
  step[["marginplyr_native_j"]] <- rlang::call2(
    step$j[[1L]], !!!expressions[keep]
  )
  step[["marginplyr_scalar_proofs"]] <- proofs
  step[["marginplyr_checked_scalar"]] <- list(
    j = step$j, vars = step$vars, groups = step$groups
  )
  class(step) <- c("marginplyr_native_scalars", class(step))
  step
}

# Preserve the native query node's ownership boundary after ordinary edits.
#' @exportS3Method dtplyr::dt_call
#' @noRd
dt_call.marginplyr_native_scalars <- function(x, needs_copy = x$needs_copy) {
  boundary <- list(j = x$j, vars = x$vars, groups = x$groups)
  if (!identical(boundary, x[["marginplyr_checked_scalar"]])) {
    return(NextMethod())
  }
  checked <- NextMethod()
  native <- checked
  native[[1L]] <- rlang::call2(
    marginplyr_private_call("native_margin_subset"),
    margin_metadata_literal(x[["marginplyr_native_checks"]])
  )
  native[[4L]] <- x[["marginplyr_native_j"]]
  private_names <- new_margin_internal_names(
    2L, unique(c(x$vars, all.names(checked), names(x$locals))),
    prefix = "..marginplyr_scalar_input_"
  )
  source <- rlang::sym(private_names[[1L]])
  state <- rlang::sym(private_names[[2L]])
  native[[1L]][[3L]] <- state
  parent <- checked[[2L]]
  checked[[2L]] <- source
  native[[2L]] <- source
  native <- rlang::call2(
    marginplyr_private_call("native_margin_share_columns"), native,
    x[["marginplyr_share_placeholders"]], x$vars, state
  )
  native <- rlang::call2(
    base::`{`,
    rlang::call2(base::`<-`, state, rlang::call2(
      base::new.env, parent = rlang::call2(base::emptyenv)
    )),
    native
  )
  condition <- rlang::call2(
    base::`&&`,
    rlang::call2(
      marginplyr_private_call("native_margin_scalar_bindings"),
      margin_metadata_literal(x[["marginplyr_scalar_proofs"]]),
      margin_metadata_literal(x$env)
    ),
    rlang::call2(base::`>`, rlang::call2(base::nrow, source), 0L)
  )
  rlang::call2(
    base::`{`, rlang::call2(base::`<-`, source, parent),
    rlang::call2(base::`if`, condition, native, checked)
  )
}

# A local function environment intercepts data.table's grouped evaluation after
# optimization: Cdogroups receives guarded jsub; gforce guards each primitive.
# The native function body and namespace remain untouched. Unknown shapes stop.
native_margin_subset <- function(checks, state) {
  state$validated <- FALSE
  native <- utils::getFromNamespace("[.data.table", "data.table")
  owner <- environment(native)
  dogroups <- get("Cdogroups", envir = owner)
  gforce <- get("gforce", envir = owner)
  shim <- new.env(parent = owner)
  shim$.Call <- function(native_symbol, ...) {
    if (identical(native_symbol, dogroups)) {
      frame <- parent.frame()
      stopifnot("Native grouped summary has no expression to validate." =
                  exists("jsub", frame, inherits = FALSE))
      j <- get("jsub", frame, inherits = FALSE)
      assign("jsub", guard_native_margin_j(j, checks), envir = frame)
      state$validated <- TRUE
    }
    base::.Call(native_symbol, ...)
  }
  shim$gforce <- function(env, jsub, o, f, l, rows) {
    check_native_margin_j_shape(jsub, checks)
    evaluated <- jsub
    for (i in seq_along(checks)) {
      expression <- jsub[[i + 1L]]
      if (identical(expression, quote(.N))) next
      stopifnot("Unknown native optimized summary call." =
                  rlang::is_call(expression) &&
                  rlang::is_symbol(expression[[1L]]))
      evaluated[[i + 1L]][[1L]] <- native_margin_primitive(
        rlang::as_name(expression[[1L]]), checks[[i]], l, owner
      )
    }
    result <- gforce(env, evaluated, o, f, l, rows)
    stopifnot("Unknown native grouped summary result shape." =
                is.list(result) && !is.object(result) &&
                length(result) == length(checks))
    state$validated <- TRUE
    result
  }
  environment(native) <- shim
  native
}

# Guard one optimized primitive while its native GForce context is active.
# Cghead/Cgtail/Cgshift supply already-coerced indices without another dispatch.
native_margin_primitive <- function(name, check, groups, owner) {
  force(check)
  force(groups)
  scalar <- name %in% c(
    "gfirst", "glast", "g[", "g[[", "gsum", "gmean", "gprod", "gmedian",
    "gmin", "gmax", "gvar", "gsd", "gweighted.mean"
  )
  variable <- name %in% c("ghead", "gtail", "gshift")
  stopifnot("Unknown native optimized summary primitive." = scalar || variable)
  native <- get(name, envir = owner)
  indices <- NULL
  if (variable) {
    symbol <- get(paste0("C", name), envir = owner)
    shim <- new.env(parent = environment(native))
    shim$.Call <- function(native_symbol, ...) {
      args <- list(...)
      stopifnot("Unknown native indexed summary evaluation shape." =
                  identical(native_symbol, symbol) &&
                  length(args) == if (name == "gshift") 4L else 2L)
      value <- do.call(base::.Call, c(list(native_symbol), args), quote = TRUE)
      stopifnot("Unknown native summary index representation." =
                  typeof(args[[2L]]) == "integer" && !is.object(args[[2L]]))
      indices <<- args[[2L]]
      value
    }
    environment(native) <- shim
  }
  function(...) {
    value <- native(...)
    sizes <- if (scalar) {
      rep.int(1L, length(groups))
    } else if (name == "gshift") {
      if (length(indices) == 1L) {
        groups
      } else {
        rep.int(length(indices), length(groups))
      }
    } else {
      stopifnot("Unknown native head/tail index shape." = length(indices) == 1L)
      pmin.int(groups, indices)
    }
    size <- if (any(sizes != 1L)) sizes[which(sizes != 1L)[[1L]]] else 1L
    validate_native_margin_value(value, size, check)
    value
  }
}

# Native optimization may rewrite calls, but each output must retain its slot.
check_native_margin_j_shape <- function(j, checks) {
  stopifnot("Unknown native grouped summary expression shape." =
              rlang::is_call(j, "list") && length(j) == length(checks) + 1L)
  invisible(NULL)
}

# Apply the original check to the optimized expression before native padding.
guard_native_margin_j <- function(j, checks) {
  check_native_margin_j_shape(j, checks)
  guarded <- attr(checks, "marginplyr_native_guarded", exact = TRUE)
  stopifnot("Unknown native summary guard ownership." =
              is.logical(guarded) && length(guarded) == length(checks))
  for (i in seq_along(checks)) {
    if (is.null(checks[[i]])) next
    check <- checks[[i]]
    if (guarded[[i]]) {
      stopifnot("Native summary lost its original validator." =
                  rlang::is_call(j[[i + 1L]]) &&
                  identical(j[[i + 1L]][[1L]], check[[1L]]))
      next
    }
    check[[2L]] <- j[[i + 1L]]
    j[[i + 1L]] <- check
  }
  j
}

# Optimized primitive cardinality comes from its raw result and native groups.
# Only cardinality failures use a plain witness; type checks use actual columns.
validate_native_margin_value <- function(value, size, check) {
  if (is.null(check)) return(invisible(NULL))
  ordinary <- identical(check[[1L]], dtplyr_share_private_call(
    "check_dtplyr_share_ordinary"
  ))
  if (size != 1L) {
    witness <- rep.int(NA_real_, if (size == 0L) 0L else 2L)
    check[[2L]] <- rlang::call2(base::quote, witness)
    rlang::eval_bare(check, baseenv())
  }
  if (!ordinary) {
    check[[1L]] <- marginplyr_private_call(if (identical(
      check[[1L]], dtplyr_share_private_call("check_dtplyr_share_source")
    )) "check_native_margin_source" else "check_native_margin_scalar")
    check[[2L]] <- rlang::call2(base::quote, value)
    rlang::eval_bare(check, baseenv())
  }
  invisible(NULL)
}

# Generated share placeholders must not disable native aggregate optimization.
native_margin_share_columns <- function(data, columns, column_order, state) {
  force(data)
  stopifnot("Native grouped summary bypassed source validation." =
              isTRUE(state$validated))
  for (column in columns) {
    data.table::set(data, j = column, value = rep(NA_real_, nrow(data)))
  }
  data.table::setcolorder(data, column_order)
  data
}

# Inspect the actual native column type without slicing it through a
# caller-owned vector method.
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
  if (!proof$name %in% c("mean", "median", "weighted.mean")) return(TRUE)
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
  binding <- ordinary_function_binding(method, env, owners = proof$owner,
                                       absent = missing)
  (identical(binding, missing) || identical(binding, expected)) &&
    dtplyr_registered_method(
      method, expected, envir = owner, home = owner, absent = TRUE
    )
}

# Resolve already collected label evidence; NULL leaves native combination.
margin_label_consensus <- function(valid, values) {
  if (!valid) return(NULL)
  present <- unique(values[!is.na(values)])
  if (length(present) == 1L) return(present)
  if (length(present) == 0L && anyNA(values)) return(NA_character_)
  NULL
}
