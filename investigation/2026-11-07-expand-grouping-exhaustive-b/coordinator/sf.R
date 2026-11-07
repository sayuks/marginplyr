Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "/private/tmp/marginplyr-exhaustive-b-root/source"), quiet = TRUE)
out_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", "/private/tmp/marginplyr-expand-grouping-exhaustive-20261107-b/investigation/2026-11-07-expand-grouping-exhaustive-b/coordinator")
make_sf <- function(n, active, special) {
  dat <- sf::st_sf(
    fixed = c(2L, 1L, NA_integer_)[seq_len(n)],
    dimension = c("B", "A", NA_character_)[seq_len(n)],
    geom_a = sf::st_sfc(lapply(seq_len(n), function(i) sf::st_point(c(i, i + 1))), crs = 4326),
    geom_b = sf::st_sfc(lapply(seq_len(n), function(i) sf::st_point(c(i + 10, i + 11))), crs = 4326),
    sf_column_name = active
  )
  if (special) {
    names(dat)[names(dat) == "dimension"] <- "..01"
  }
  dat
}
results <- list()
i <- 0L
for (n in c(0L, 1L, 3L)) for (active in c("geom_a", "geom_b")) {
  for (role in c("payload", "fixed", "dimension")) for (special in c(FALSE, TRUE)) {
    for (sort in c("none", "first", "last")) for (persistent in c(FALSE, TRUE)) {
      i <- i + 1L
      dat <- make_sf(n, active, special)
      dimension <- if (special) "..01" else "dimension"
      fixed <- "fixed"
      if (role == "fixed") fixed <- active
      if (role == "dimension") dimension <- active
      input <- if (persistent) dplyr::group_by(dat, !!!rlang::syms(fixed)) else dat
      spec <- rollup(tidyselect::all_of(dimension))
      actual <- tryCatch(if (persistent) {
        expand_with_margins(input, .grouping = spec, .margin_label = NULL, .sort = sort, .id = "set_id")
      } else {
        expand_with_margins(input, .by = tidyselect::all_of(fixed), .grouping = spec, .margin_label = NULL, .sort = sort, .id = "set_id")
      }, error = identity)
      control <- tryCatch({
        a <- dplyr::mutate(dat, set_id = 1L)
        missing_value <- vctrs::vec_init(dat[[dimension]], n)
        b <- dplyr::mutate(dat, !!dimension := !!missing_value, set_id = 2L)
        dplyr::union_all(a, b)
      }, error = identity)
      status <- if (inherits(control, "error")) "ordinary-exclusion" else if (!identical(sort(names(control)), sort(c(names(dat), "set_id")))) "ordinary-name-repair" else if (inherits(actual, "error")) {
        if (role == "dimension" && !all(vctrs::vec_detect_missing(vctrs::vec_init(dat[[dimension]], 1L))) && inherits(actual, "marginplyr_error") && grepl("cannot be represented for:", conditionMessage(actual), fixed = TRUE)) "required-missing-refusal" else "candidate-error"
      } else {
        # Canonicalize full rows by occurrence/fixed for unordered results.
        a <- tibble::as_tibble(actual, .name_repair = "minimal")
        b <- tibble::as_tibble(control, .name_repair = "minimal")
        canon <- function(x) {
          y <- x[order(x$set_id, x$fixed), ]
          stats::setNames(lapply(sort(names(y)), function(name) y[[name]]), sort(names(y)))
        }
        leading <- unique(c(fixed, dimension, "set_id"))
        if (identical(canon(a), canon(b)) && identical(names(a), c(leading, setdiff(names(dat), leading))) && identical(dplyr::group_vars(actual), character())) "match" else "candidate-mismatch"
      }
      results[[i]] <- data.frame(n, active, role, special, sort, persistent, status, symptom = if (inherits(actual, "error")) conditionMessage(actual) else "", control_symptom = if (inherits(control, "error")) conditionMessage(control) else "")
    }
  }
}
results <- do.call(rbind, results)
write.csv(results, file.path(out_dir, "sf.csv"), row.names = FALSE)
print(table(results$status))
print(results[grepl("candidate", results$status), ], row.names = FALSE)
