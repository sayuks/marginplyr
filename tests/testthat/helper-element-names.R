# Attach names after frame conversion so fixtures reach the public seam.
element_name_input <- function(convert = identity, size = 3L,
                               element_names = c("same", "", NA_character_)[
                                 seq_len(size)
                               ]) {
  input <- convert(data.frame(k = c("a", "a", "b")[seq_len(size)],
                              g = c("x", "y", "x")[seq_len(size)],
                              z = c(3, 1, 2)[seq_len(size)]))
  columns <- lapply(input, function(column) {
    names(column) <- element_names
    column
  })
  attributes(columns) <- attributes(input)
  columns
}

# The independent oracle performs the specified native operations, then
# combines in plan order. It does no Margin processing.
element_name_reference <- function(input, included, verb, label = "Total",
                                   keep = FALSE) {
  branches <- lapply(seq_along(included), function(i) {
    keys <- c("k", "g"[included[[i]]])
    branch <- switch(verb,
      summary = dplyr::summarise(input, first = .data$z[1L],
                                 z = mean(.data$z), .by = dplyr::all_of(keys)),
      expand = input,
      nest = if (inherits(input, "dtplyr_step")) {
        element_name_dt_nest(input, keys, keep)
      } else {
        tidyr::nest(input, data = dplyr::all_of(
          if (keep) c("k", "g", "z") else "z"
        ), .by = dplyr::all_of(keys))
      },
      rowwise = dplyr::ungroup(dplyr::nest_by(
        dplyr::select(input, dplyr::all_of(
          if (keep) c("k", "g", "z") else c(keys, "z")
        )), !!!rlang::syms(keys), .keep = keep
      ))
    )
    if (!included[[i]]) branch <- dplyr::mutate(branch, g = label)
    branch <- dplyr::relocate(branch, dplyr::all_of(c("k", "g")))
    dplyr::mutate(branch, set = i)
  })
  Reduce(dplyr::union_all, branches)
}

element_name_result <- function(input, included, verb, label = "Total",
                                keep = FALSE, sort = "none") {
  spec <- grouping_sets(!!!lapply(included, function(present) {
    if (present) grouping_set(dplyr::all_of("g")) else grouping_set()
  }))
  args <- list(.data = input, .by = rlang::expr(dplyr::all_of("k")),
               .grouping = spec,
               .margin_label = label, .id = "set", .sort = sort)
  if (verb %in% c("summary", "alias", "expand")) args$.duplicates <- "keep"
  if (verb %in% c("nest", "rowwise")) args$.keep <- keep
  if (verb %in% c("summary", "alias")) {
    args$first <- rlang::expr(.data$z[1L])
    args$z <- rlang::expr(mean(.data$z))
  }
  fun <- switch(verb, summary = summarize_with_margins,
                alias = summarise_with_margins, expand = expand_with_margins,
                nest = nest_with_margins, rowwise = nest_by_with_margins)
  rlang::inject(fun(!!!args))
}

# Compare each column explicitly; generated container classes have their own
# contract, while original columns inside cells retain the native names.
expect_element_names <- function(result, control, info = "") {
  columns <- setdiff(names(control), "data")
  for (column in columns) {
    expect_identical(names(result[[column]]), names(control[[column]]),
                     info = paste(info, column))
    expect_identical(unname(result[[column]]), unname(control[[column]]),
                     info = paste(info, column, "values"))
  }
  if ("data" %in% names(control)) {
    expect_identical(length(result$data), length(control$data), info = info)
    for (i in seq_along(control$data)) {
      for (column in names(control$data[[i]])) {
        expect_identical(names(result$data[[i]][[column]]),
                         names(control$data[[i]][[column]]),
                         info = paste(info, i, column))
        expect_identical(unname(result$data[[i]][[column]]),
                         unname(control$data[[i]][[column]]),
                         info = paste(info, i, column, "values"))
      }
    }
  }
}

# dtplyr's supported nest method selects payload columns without `.by`.
# Separate native aliases retain outer keys when those keys are also payload.
element_name_dt_nest <- function(input, keys, keep) {
  if (!keep) {
    input <- dplyr::select(input, dplyr::all_of(c(keys, "z")))
    return(tidyr::nest(input, data = dplyr::all_of("z")))
  }
  aliases <- paste0("outer_", keys)
  copies <- rlang::set_names(rlang::syms(keys), aliases)
  input <- dplyr::mutate(input, !!!copies)
  result <- tidyr::nest(input, data = dplyr::all_of(c("k", "g", "z")))
  dplyr::rename(result, !!!stats::setNames(aliases, keys))
}

# Observe native collection diagnostics without pinning a version's warnings.
element_name_collect <- function(input) {
  warnings <- list()
  result <- withCallingHandlers(dplyr::collect(input), warning = function(cnd) {
    warnings[[length(warnings) + 1L]] <<- list(class = class(cnd),
                                               message = conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  list(result = result, warnings = warnings)
}

# Establish named keys and payload immediately before each public call.
expect_element_name_input <- function(input, expected) {
  for (column in c("k", "g", "z")) {
    expect_identical(names(input[[column]]), expected, info = column)
  }
}
