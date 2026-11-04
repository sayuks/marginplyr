# Finite value/membership oracle for a factor with a noncanonical display.
pkgload::load_all(".", quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
plain_factor <- function(x) {
  structure(unclass(x), levels = levels(x),
            class = if (is.ordered(x)) c("ordered", "factor") else "factor")
}
as.character.hunt_display_factor <- function(x, ...) {
  value <- as.character(plain_factor(x))
  mode <- attr(x, "display_mode", exact = TRUE)
  if (identical(mode, "suffix")) {
    value[!is.na(value)] <- paste0(value[!is.na(value)], "!")
  } else if (identical(mode, "swap")) {
    value <- unname(c(A = "B", B = "A")[value])
  } else if (identical(mode, "label")) {
    value[!is.na(value)] <- "Total"
  }
  value
}
registerS3method("vec_ptype2", "hunt_display_factor.hunt_display_factor",
                 function(x, y, ...) vctrs::vec_slice(x, integer()),
                 envir = asNamespace("vctrs"))
registerS3method("vec_cast", "hunt_display_factor.hunt_display_factor",
                 function(x, to, ...) x, envir = asNamespace("vctrs"))
registerS3method("as.character", "hunt_display_factor",
                 as.character.hunt_display_factor, envir = asNamespace("base"))

ledger <- list()
record <- function(...) ledger[[length(ledger) + 1L]] <<- data.frame(...)
verdicts <- c("correct", "wrong_outer_values")
for (mode in c("suffix", "swap", "label")) {
  for (ordered in c(FALSE, TRUE)) for (na_level in c(FALSE, TRUE)) {
    codes <- c(2L, 1L, 2L, NA_integer_, 1L, if (na_level) 3L else NA_integer_)
    g <- structure(codes, levels = c("A", "B", if (na_level) NA_character_),
                   display_mode = mode,
                   class = c("hunt_display_factor",
                             if (ordered) "ordered", "factor"))
    input <- tibble::tibble(g = g, h = c("x", "y", "x", "x", "y", "x"),
                            row_id = seq_along(g))
    for (empty in c(FALSE, TRUE)) {
      original <- input[if (empty) integer() else seq_len(nrow(input)), ]
      for (representation in c("tibble", "frame")) {
        source <- if (representation == "frame") as.data.frame(original) else original
        before <- serialize(source, NULL)
        ordinary <- dplyr::union_all(source, source)
        stopifnot(identical(as.integer(ordinary$g), rep(as.integer(source$g), 2L)))
        for (role in c("dimension", "fixed", "payload")) {
          for (plan_name in c("single", "rollup")) {
            selected <- if (role == "dimension") "g" else "h"
            groups <- if (plan_name == "single") list(selected) else list(selected, character())
            fixed <- if (role == "fixed") "g" else character()
            expected <- lapply(groups, function(set) {
              loc <- vctrs::vec_group_loc(source[unique(c(fixed, set))])$loc
              lapply(loc, function(i) source$row_id[i])
            })
            for (label in list("Total", NULL)) for (keep in c(FALSE, TRUE)) {
              for (sort in c("none", "first", "last")) {
                for (verb_name in c("nest_with_margins", "nest_by_with_margins")) {
                  verb <- get(verb_name, asNamespace("marginplyr"))
                  grouping <- if (selected == "g") {
                    if (plan_name == "single") grouping_set(g) else rollup(g)
                  } else if (plan_name == "single") grouping_set(h) else rollup(h)
                  out <- if (role == "fixed") {
                    verb(source, .by = g, .grouping = grouping, .margin_label = label,
                         .id = "set", .keep = keep, .sort = sort)
                  } else {
                    verb(source, .grouping = grouping, .margin_label = label,
                         .id = "set", .keep = keep, .sort = sort)
                  }
                  values_ok <- TRUE
                  for (set_index in seq_along(groups)) {
                    cells <- out$data[out$set == set_index]
                    actual <- lapply(cells, function(z) z$row_id)
                    canonical <- function(x) sort(vapply(x, paste, character(1), collapse = ","))
                    stopifnot(identical(canonical(actual), canonical(expected[[set_index]])))
                  }
                  for (i in seq_len(nrow(out))) {
                    cell <- out$data[[i]]
                    ids <- cell$row_id
                    key_columns <- unique(c(fixed, selected))
                    expected_names <- c(if (keep) key_columns,
                                        setdiff(names(source), key_columns))
                    stopifnot(identical(names(cell), expected_names))
                    for (column in names(cell)) {
                      if (column == "g") {
                        stopifnot(identical(as.integer(cell$g), as.integer(source$g[ids])))
                      } else stopifnot(identical(cell[[column]], source[[column]][ids]))
                    }
                    if (role == "dimension" && out$set[[i]] == 1L) {
                      expected_value <- as.character(plain_factor(source$g[ids[[1L]]]))
                      actual_value <- as.character(plain_factor(out$g[i]))
                      values_ok <- values_ok && identical(actual_value, expected_value)
                    } else if (role == "fixed") {
                      stopifnot(identical(as.integer(out$g[i]), as.integer(source$g[ids[[1L]]])))
                    }
                  }
                  stopifnot(identical(serialize(source, NULL), before))
                  record(mode = mode, ordered = ordered, na_level = na_level,
                         empty = empty, source = representation, role = role,
                         plan = plan_name, label = if (is.null(label)) "NULL" else label,
                         keep = keep, sort = sort, verb = verb_name,
                         verdict = verdicts[[if (values_ok) 1L else 2L]])
                }
              }
            }
          }
        }
      }
    }
  }
}
result <- do.call(rbind, ledger)
cat("Completed calls:", nrow(result), "\n")
print(table(result$verdict))
print(with(result, table(mode, role, verdict)))
stopifnot(all((result$verdict == "wrong_outer_values") ==
                (!result$empty & result$role == "dimension" & result$label == "Total")))
cat("Every source-group membership, cell projection, fixed factor value and serialized source matched.\n")
cat("R:", R.version.string, "; platform:", R.version$platform, "\n")
for (pkg in c("marginplyr", "dplyr", "vctrs", "tibble")) {
  cat(pkg, as.character(utils::packageVersion(pkg)), "\n")
}
