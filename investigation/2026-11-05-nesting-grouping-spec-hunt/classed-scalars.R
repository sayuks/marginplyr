pkgload::load_all(quiet = TRUE)

as.character.hunt_display_scalar <- function(x, ...) {
  rep(attr(x, "display"), length(x))
}
displayed <- function(value, display) {
  structure(value, class = "hunt_display_scalar", display = display)
}
package_error <- function(expr) {
  inherits(tryCatch(expr, error = identity), "marginplyr_error")
}

arguments <- commandArgs(trailingOnly = TRUE)
desired <- "--assert" %in% arguments
tracks <- setdiff(arguments, "--assert")
if (length(tracks) == 0L) tracks <- c("key", "id", "choice")
stopifnot(all(tracks %in% c("key", "id", "choice")))

source <- data.frame(v = 5L)
verbs <- list(nest = nest_with_margins, rowwise = nest_by_with_margins)
for (track in tracks) {
  if (track == "key") {
    key <- displayed("payload", "shown")
    stopifnot(rlang::is_string(key))
    for (input in list(source, source[0L, , drop = FALSE])) {
      stopifnot(identical(names(tidyr::nest(input, .key = key)), "payload"))
      stopifnot(identical(names(dplyr::nest_by(input, .key = key)), "payload"))
    }
    regular <- nest_with_margins(source, .key = key)
    rowwise <- nest_by_with_margins(source, .key = key)
    empty <- nest_by_with_margins(source[0L, , drop = FALSE], .key = key)
    cat(".key populated columns:", names(regular), names(rowwise), "\n")
    cat(".key populated row-wise identifiers:", dplyr::group_vars(rowwise), "\n")
    cat(".key empty row-wise columns:", names(empty), "\n")
    stopifnot(nrow(empty) == 1L, identical(names(empty), "payload"))
    if (desired) {
      stopifnot(identical(regular, nest_with_margins(source, .key = "payload")))
      stopifnot(identical(rowwise, nest_by_with_margins(source, .key = "payload")))
    } else {
      stopifnot(identical(names(regular), "shown"),
                identical(names(rowwise), "shown"),
                identical(dplyr::group_vars(rowwise), "shown"))
    }
  }
  if (track == "id") {
    colliding <- displayed("v", "shown")
    noncolliding <- displayed("set", "v")
    key_collision <- displayed("data", "shown")
    stopifnot(rlang::is_string(colliding), rlang::is_string(noncolliding))
    for (verb_name in names(verbs)) {
      verb <- verbs[[verb_name]]
      stopifnot(package_error(verb(source, .id = "v")))
      stopifnot(package_error(verb(source, .id = "data")))
      stopifnot(identical(verb(source, .id = "set")$data[[1L]]$v, 5L))
      if (desired) {
        stopifnot(package_error(verb(source, .id = colliding)))
        stopifnot(package_error(verb(source, .id = key_collision)))
        stopifnot(identical(verb(source, .id = noncolliding),
                            verb(source, .id = "set")))
      } else {
        result <- verb(source, .id = colliding)
        cat(".id source collision", verb_name, "outer v:", result$v,
            "cell column count:", ncol(result$data[[1L]]), "\n")
        stopifnot(identical(result$v, 1L), nrow(result$data[[1L]]) == 1L,
                  ncol(result$data[[1L]]) == 0L)
        stopifnot(identical(names(verb(source, .id = key_collision)), "data"))
        stopifnot(package_error(verb(source, .id = noncolliding)))
      }
    }
    stopifnot(package_error(expand_with_margins(source, .id = "v")))
    if (desired) {
      stopifnot(package_error(expand_with_margins(source, .id = colliding)))
    } else {
      stopifnot(identical(expand_with_margins(source, .id = colliding)$v, 1L))
    }
    stopifnot(identical(source$v, 5L))
  }
  if (track == "choice") {
    first <- displayed("first", "last")
    abbreviation <- displayed("f", "first")
    keep <- displayed("keep", "drop")
    exact <- displayed("list", "text")
    repeated <- grouping_sets(grouping_set(v), grouping_set(v))
    stopifnot(rlang::is_string(first), rlang::is_string(abbreviation),
              rlang::is_string(keep), rlang::is_string(exact))
    stopifnot(is.list(inspect_grouping(source, .grouping = rollup(v), .format = "list")$included))
    stopifnot(is.character(inspect_grouping(source, .grouping = rollup(v), .format = "text")$included))
    plan <- inspect_grouping(source, .grouping = rollup(v), .format = exact)
    for (verb_name in names(verbs)) {
      verb <- verbs[[verb_name]]
      run <- function(option) verb(source, .grouping = rollup(v), .sort = option)
      stopifnot(identical(run("first")$v, c("Total", "5")))
      stopifnot(identical(run("last")$v, c("5", "Total")))
      stopifnot(package_error(run("f")))
      stopifnot(package_error(verb(source, .grouping = repeated,
                                   .duplicates = "keep")))
      result <- run(first)
      cat(".sort", verb_name, "stored first/displayed last:", result$v, "\n")
      if (desired) {
        stopifnot(identical(result, run("first")), package_error(run(abbreviation)))
        stopifnot(package_error(verb(source, .grouping = repeated,
                                     .duplicates = keep)))
      } else {
        stopifnot(identical(result, run("last")), identical(run(abbreviation), run("first")))
        stopifnot(nrow(verb(source, .grouping = repeated, .duplicates = keep)) == 1L)
      }
    }
    cat(".format stored list/displayed text included type:", typeof(plan$included), "\n")
    if (desired) stopifnot(is.list(plan$included)) else stopifnot(is.character(plan$included))
  }
}
cat("R", as.character(getRversion()), "on", R.version$platform, "\n")
for (package in c("dplyr", "tidyr", "tibble", "rlang", "vctrs")) {
  cat(package, as.character(utils::packageVersion(package)), "\n")
}
