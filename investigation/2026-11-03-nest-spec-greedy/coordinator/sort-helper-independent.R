# Desired-behavior mode is deliberately red for the investigated baseline.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
assert <- "--assert" %in% commandArgs(TRUE)
in_mode <- "--in" %in% commandArgs(TRUE)
e <- new.env(parent = globalenv())
e$x <- tibble::tibble(g = if (in_mode) c("b", "a") else c("a", NA_character_), row_id = 1:2)
src <- evalq(dtplyr::lazy_dt(x), e)
before <- serialize(e$x, NULL)
spec <- if (in_mode) rollup(g) else grouping_set(g)
lab <- if (in_mode) "Total" else NULL
wanted <- if (in_mode) c("a", "b", "Total") else c("a", NA_character_)
builders <- list(
  nest = function() nest_with_margins(src, .grouping = spec, .margin_label = lab, .sort = "last", .keep = TRUE),
  nest_by = function() nest_by_with_margins(src, .grouping = spec, .margin_label = lab, .sort = "last", .keep = TRUE),
  expand = function() expand_with_margins(src, .grouping = spec, .margin_label = lab, .sort = "last"),
  summarize = function() summarize_with_margins(src, .grouping = spec, value = dplyr::n(), .margin_label = lab, .sort = "last")
)
queries <- lapply(builders, function(f) f())
baseline <- lapply(queries, dplyr::collect)
expected_for <- function(name) if (in_mode && name == "expand") c("a", "b", "Total", "Total") else wanted
stopifnot(all(vapply(names(baseline), function(name) identical(baseline[[name]]$g, expected_for(name)), logical(1))))
e$calls <- 0L
helper <- if (in_mode) "%in%" else "is.na"
e[[helper]] <- if (in_mode) function(x, table) { e$calls <- e$calls + 1L; !base::`%in%`(x, table) } else function(x) { e$calls <- e$calls + 1L; !base::is.na(x) }
masked <- lapply(queries, dplyr::collect)
masked$nest_by <- dplyr::collect(builders$nest_by())
print(lapply(masked, function(x) x$g))
cat("caller", helper, "calls:", e$calls, "\n")
stopifnot(identical(serialize(e$x, NULL), before))
stopifnot(identical(dplyr::collect(src), e$x))
native <- dplyr::collect(dplyr::arrange(src, base::is.na(g), g))
stopifnot(identical(native$g, sort(e$x$g, na.last = TRUE)))
e[[helper]] <- get(helper, envir = baseenv())
restored <- lapply(queries, dplyr::collect)
stopifnot(identical(restored, baseline))
if (assert) {
  stopifnot(all(vapply(names(masked), function(name) identical(masked[[name]]$g, expected_for(name)), logical(1))))
}
cat("Observed helper-dependent order with unchanged source; restoring helper restored every result.\n")
print(sessionInfo())
