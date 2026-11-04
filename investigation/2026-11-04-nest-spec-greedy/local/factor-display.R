# Replay from the investigated checkout root. --assert requires desired behavior.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
output_dir <- Sys.getenv("HUNT_OUTPUT_DIR", tempdir())
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
as.character.hunt_decorated_factor <- function(x, ...) {
  value <- as.character.factor(x)
  present <- !is.na(value)
  value[present] <- paste0("[", value[present], "]")
  value
}
registerS3method("vec_ptype2", "hunt_decorated_factor.hunt_decorated_factor",
                function(x, y, ...) vctrs::vec_slice(x, integer()),
                envir = asNamespace("vctrs"))
registerS3method("vec_cast", "hunt_decorated_factor.hunt_decorated_factor",
                function(x, to, ...) x, envir = asNamespace("vctrs"))
make_factor <- function(na_level) {
  x <- factor(c("a", "b"), levels = c("a", "b", if (na_level) NA_character_),
              exclude = NULL)
  class(x) <- c("hunt_decorated_factor", "factor")
  x
}
# Native controls establish admitted values independently of subclass restoration.
for (na_level in c(FALSE, TRUE)) {
  input <- tibble::tibble(g = make_factor(na_level), h = "x", row = 1:2)
  native <- tidyr::nest(input, .by = g)
  stopifnot(identical(as.integer(native$g), 1:2))
  stopifnot(identical(vctrs::vec_group_loc(input$g)$loc, list(1L, 2L)))
  stopifnot(identical(as.integer(dplyr::union_all(input, input)$g), rep(1:2, 2)))
  lazy <- dtplyr::lazy_dt(input)
  stopifnot(identical(as.integer(dplyr::collect(lazy)$g), 1:2))
  ordinary <- dplyr::collect(dplyr::summarise(lazy, rows = list(row), .by = g))
  stopifnot(identical(as.integer(ordinary$g), 1:2))
  stopifnot(identical(ordinary$rows, list(1L, 2L)))
  stopifnot(identical(as.integer(dplyr::collect(dplyr::union_all(lazy, lazy))$g),
                      rep(1:2, 2)))
}
cat("Native source, grouping, local nesting and union controls passed.\n")
rows <- list(); i <- 0L
for (backend in c("tibble", "frame", "dtplyr"))
for (na_level in c(FALSE, TRUE)) for (empty in c(FALSE, TRUE))
for (role in c("dimension", "fixed", "payload"))
for (labelled in c(FALSE, TRUE)) for (keep in c(FALSE, TRUE))
for (verb_name in c("nest", "nest_by")) {
  i <- i + 1L
  input <- tibble::tibble(g = make_factor(na_level), h = "x", row = 1:2)
  if (empty) input <- input[0, ]
  source <- switch(backend, tibble = input, frame = as.data.frame(input),
                   dtplyr = dtplyr::lazy_dt(input))
  before <- serialize(input, NULL)
  verb <- if (verb_name == "nest") nest_with_margins else nest_by_with_margins
  call <- rlang::expr(verb(source, .keep = keep, .id = "set",
                           .margin_label = if (labelled) "Total" else NULL,
                           .sort = c("none", "first", "last")[(i %% 3L) + 1L]))
  if (role == "dimension") call$.grouping <- rlang::expr(rollup(g))
  if (role == "fixed") {
    call$.by <- quote(g)
    call$.grouping <- rlang::expr(rollup(h))
  }
  if (role == "payload") call$.grouping <- rlang::expr(rollup(h))
  result <- dplyr::collect(eval(call))
  stopifnot(identical(serialize(input, NULL), before))
  outer_ok <- TRUE; cell_ok <- TRUE
  for (j in seq_len(nrow(result))) {
    cell <- result$data[[j]]
    ids <- cell$row
    if (role != "payload") {
      omitted <- role == "dimension" && result$set[[j]] == 2L
      want <- if (omitted) { if (labelled) "Total" else NA_character_ } else {
        as.character.factor(input$g)[ids[[1L]]]
      }
      outer_ok <- outer_ok && identical(as.character.factor(result$g)[[j]], want)
    }
    if ("g" %in% names(cell)) {
      cell_ok <- cell_ok && identical(as.character.factor(cell$g),
                                      as.character.factor(input$g)[ids])
    }
  }
  if (empty) stopifnot(nrow(result) == 0L)
  status <- if (outer_ok && cell_ok) "matched" else "corrupted"
  rows[[i]] <- data.frame(backend, na_level, empty, role, labelled, keep,
                          verb = verb_name, outer_ok, cell_ok, status)
}
results <- do.call(rbind, rows)
write.csv(results, file.path(output_dir, "factor-display.csv"), row.names = FALSE)
cat("Nesting matrix calls:", nrow(results), "\n")
print(as.data.frame(table(results$status, results$backend)))
print(subset(results, status == "corrupted", select = -empty))
# A single included-only set reaches all four verbs without a branch union.
companion <- list(); k <- 0L
for (backend in c("tibble", "dtplyr")) for (verb_name in c("nest", "nest_by", "expand", "summary")) {
  input <- tibble::tibble(g = make_factor(FALSE), row = 1:2)
  source <- if (backend == "dtplyr") dtplyr::lazy_dt(input) else input
  result <- switch(verb_name,
    nest = nest_with_margins(source, .grouping = grouping_set(g)),
    nest_by = nest_by_with_margins(source, .grouping = grouping_set(g)),
    expand = expand_with_margins(source, .grouping = grouping_set(g)),
    summary = summarize_with_margins(source, n = dplyr::n(), .grouping = grouping_set(g)))
  result <- dplyr::collect(result)
  same <- identical(as.character.factor(result$g), c("a", "b"))
  k <- k + 1L
  companion[[k]] <- data.frame(backend, verb = verb_name, matched = same)
}
companions <- do.call(rbind, companion)
cat("Included-only companion panel:\n"); print(companions)
write.csv(companions, file.path(output_dir, "factor-display-companions.csv"), row.names = FALSE)
cat("Scope of the confirmed defect: package factor encoding/reconstruction, not subclass restoration.\n")
print(sessionInfo())
if ("--assert" %in% commandArgs(trailingOnly = TRUE) &&
      (any(results$status != "matched") || !all(companions$matched))) {
  stop("Desired behavior failed: factor values changed during package encoding/reconstruction.")
}
