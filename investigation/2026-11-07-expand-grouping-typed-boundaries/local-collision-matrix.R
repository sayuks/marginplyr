# Run with Rscript from the package source root.
# The default mode verifies the observed baseline and its controls.
# --assert-observed and --assert-declared assert the public contracts.
pkgload::load_all(".", quiet = TRUE)

character_storage <- function(x) {
  value <- sprintf("key:%d", unclass(x))
  value[is.na(unclass(x))] <- NA_character_
  value
}

as.character.strict_probe_key <- function(x, ...) {
  structure(character_storage(x), class = "strict_probe_display")
}

Ops.strict_probe_display <- function(e1, e2) {
  if (.Generic != "==") {
    return(NextMethod())
  }
  inherits(e2, "strict_probe_display") & (unclass(e1) == unclass(e2))
}

as.character.folded_probe_key <- function(x, ...) {
  structure(character_storage(x), class = "folded_probe_display")
}

Ops.folded_probe_display <- function(e1, e2) {
  if (.Generic != "==") {
    return(NextMethod())
  }
  tolower(unclass(e1)) == tolower(unclass(e2))
}

as.character.vctrs_probe_key <- function(x, ...) {
  vctrs::new_vctr(character_storage(x), class = "vctrs_probe_display")
}

levels.domain_probe_factor <- function(x) {
  structure(NextMethod(), class = "domain_probe_levels")
}

as.character.domain_probe_levels <- function(x, ...) {
  toupper(unclass(x))
}

as_input <- function(source, backend) {
  if (backend == "dtplyr") dtplyr::lazy_dt(source) else source
}

capture_expand <- function(input, plan, label, checked, position = "last",
                           margin_order = "none") {
  tryCatch(
    dplyr::collect(expand_with_margins(
      input, .grouping = plan, .margin_label = label,
      .check_margin_label = checked, .margin_label_position = position,
      .sort = margin_order, .id = "set"
    )),
    error = identity
  )
}

check_frame <- function(out, keys, occurrences, payload) {
  if (inherits(out, "error")) {
    return(FALSE)
  }
  expected_order <- order(occurrences, payload)
  observed_order <- order(out$set, out$payload)
  identical(names(out), c("k", "set", "payload")) &&
    identical(out$k[observed_order], keys[expected_order]) &&
    identical(out$set[observed_order], occurrences[expected_order]) &&
    identical(out$payload[observed_order], payload[expected_order])
}

is_label_condition <- function(out, fragment) {
  inherits(out, "marginplyr_error") &&
    grepl(fragment, conditionMessage(out), fixed = TRUE)
}

observed_case <- function(backend, kind, rows, plan_name, label, checked) {
  values <- c(2L, NA_integer_, 1L)[seq_len(rows)]
  display <- sprintf("key:%d", values)
  display[is.na(values)] <- NA_character_
  keys <- if (kind == "plain") {
    display
  } else {
    structure(values, class = paste0(kind, "_probe_key"))
  }
  payload <- 17L + seq_len(rows)
  source <- tibble::tibble(k = keys, payload = payload)
  source_bytes <- serialize(source, NULL)
  input <- as_input(source, backend)
  plan <- if (plan_name == "included") grouping_set(k) else rollup(k)
  out <- capture_expand(input, plan, label, checked)
  omitted <- plan_name == "rollup"
  exact_collision <- checked && label %in% display
  expected_keys <- if (omitted) c(display, rep(label, rows)) else display
  expected_ids <- if (omitted) {
    rep(c(1L, 2L), each = rows)
  } else {
    rep(1L, rows)
  }
  expected_payload <- if (omitted) rep(payload, 2L) else payload
  stopifnot(identical(serialize(source, NULL), source_bytes))

  if (is_label_condition(out, "is already present in grouping")) {
    return(if (exact_collision) "required refusal" else "false collision")
  }
  if (inherits(out, "error")) {
    stopifnot(grepl("Can't combine", conditionMessage(out), fixed = TRUE))
    return("wrong diagnostic")
  }
  stopifnot(check_frame(out, expected_keys, expected_ids, expected_payload))
  if (exact_collision) "missed collision" else "exact result"
}

declared_case <- function(backend, kind, rows, plan_name, label, checked,
                          position) {
  domain <- c("south", "north", "unused")
  codes <- c(2L, NA_integer_, 1L)[seq_len(rows)]
  factor_class <- c("ordered", "factor")
  key <- structure(
    codes, levels = domain,
    class = c(if (kind == "classed") "domain_probe_factor", factor_class)
  )
  payload <- 41L + seq_len(rows)
  source <- tibble::tibble(k = key, payload = payload)
  source_bytes <- serialize(source, NULL)
  input <- as_input(source, backend)
  plan <- if (plan_name == "included") grouping_set(k) else rollup(k)
  out <- capture_expand(input, plan, label, checked, position)
  exact_collision <- !is.null(label) && label %in% domain
  stopifnot(identical(serialize(source, NULL), source_bytes))

  if (is_label_condition(out, "is already a factor level")) {
    return(if (exact_collision) "required refusal" else "false collision")
  }
  stopifnot(!inherits(out, "error"), is.ordered(out$k))
  if (exact_collision) {
    stopifnot(anyDuplicated(attr(out$k, "levels", exact = TRUE)) > 0L)
    return("missed collision with duplicate levels")
  }

  omit <- plan_name == "rollup"
  label_domain <- if (is.null(label)) {
    domain
  } else if (position == "first") {
    c(label, domain)
  } else {
    c(domain, label)
  }
  display <- domain[codes]
  if (omit) {
    display <- c(display, rep(if (is.null(label)) NA_character_ else label, rows))
  }
  expected <- structure(match(display, label_domain), levels = label_domain,
                        class = factor_class)
  ids <- if (omit) rep(c(1L, 2L), each = rows) else rep(1L, rows)
  payload <- if (omit) rep(payload, 2L) else payload
  class(out$k) <- factor_class
  stopifnot(check_frame(out, expected, ids, payload))
  "exact result"
}

run_matrix <- function(cases, probe) {
  values <- lapply(seq_len(nrow(cases)), function(row) {
    args <- lapply(cases, function(column) column[[row]])
    do.call(probe, args)
  })
  table(unlist(values, use.names = FALSE))
}

observed_cases <- expand.grid(
  backend = c("local", "dtplyr"),
  kind = c("plain", "strict", "folded", "vctrs"),
  rows = c(0L, 1L, 3L),
  plan_name = c("included", "rollup"),
  label = c("key:2", "KEY:2", "Overall"),
  checked = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

declared_cases <- expand.grid(
  backend = c("local", "dtplyr"),
  kind = c("plain", "classed"),
  rows = c(0L, 1L, 3L),
  plan_name = c("included", "rollup"),
  checked = c(FALSE, TRUE),
  position = c("first", "last"),
  stringsAsFactors = FALSE
)
declared_cases <- lapply(list(NULL, "south", "unused", "SOUTH", "Overall"),
                         function(label) {
  cases <- declared_cases
  cases$label <- rep(list(label), nrow(cases))
  cases
})
declared_cases <- do.call(rbind, declared_cases)

mode <- commandArgs(trailingOnly = TRUE)
if (identical(mode, "--assert-observed")) {
  stopifnot(identical(
    observed_case("local", "strict", 1L, "rollup", "key:2", TRUE),
    "required refusal"
  ))
} else if (identical(mode, "--assert-declared")) {
  stopifnot(identical(
    declared_case("local", "classed", 1L, "rollup", "unused", FALSE, "last"),
    "required refusal"
  ))
} else {
  observed <- run_matrix(observed_cases, observed_case)
  declared <- run_matrix(declared_cases, declared_case)
  print(observed)
  print(declared)
  stopifnot(
    sum(observed) == nrow(observed_cases),
    sum(declared) == nrow(declared_cases),
    observed[["missed collision"]] == 8L,
    observed[["false collision"]] == 8L,
    observed[["wrong diagnostic"]] == 36L,
    declared[["missed collision with duplicate levels"]] == 96L,
    declared[["false collision"]] == 48L
  )
}
