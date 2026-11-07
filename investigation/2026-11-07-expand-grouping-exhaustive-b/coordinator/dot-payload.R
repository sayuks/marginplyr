Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "/private/tmp/marginplyr-exhaustive-b-root/source"), quiet = TRUE)
for (kind in c("frame", "tibble")) for (name in c("...", "..1", "..01", "normal")) {
  dat <- data.frame(fixed = 1L, dimension = "A", payload = 13L)
  names(dat)[3L] <- name
  if (kind == "tibble") dat <- tibble::as_tibble(dat, .name_repair = "minimal")
  run <- function(expr) tryCatch(expr, error = identity)
  ordinary <- run(dplyr::relocate(dplyr::union_all(dplyr::mutate(dat, set_id = 1L), dplyr::mutate(dat, dimension = "Total", set_id = 2L)), fixed, dimension, set_id))
  inspected <- run(inspect_grouping(dat, .by = fixed, .grouping = rollup(dimension)))
  expanded <- run(expand_with_margins(dat, .by = fixed, .grouping = rollup(dimension), .id = "set_id"))
  cat(kind, name, "ordinary:", if (inherits(ordinary, "error")) conditionMessage(ordinary) else "OK", "; inspect:", if (inherits(inspected, "error")) conditionMessage(inspected) else "OK", "; expand:", if (inherits(expanded, "error")) conditionMessage(expanded) else "OK", "\n")
  if (inherits(expanded, "error") && !inherits(ordinary, "error")) print(expanded)
}
