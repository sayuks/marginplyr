# Dated investigation evidence, investigated 2026-10-26 at 6b91254016dbcce60cc2cc69071f01f69eb8084e.
# Run from the repository root, or pass its path as the first argument.
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1L]] else "."
pkgload::load_all(repo, quiet = TRUE)
observations <- list()
for (backend in c("local", "dtplyr")) {
  for (verb in c("nest_with_margins", "nest_by_with_margins")) {
    for (role in c("id", "fixed", "dimension", "key", "payload")) {
      for (keep in c(FALSE, TRUE)) for (rows in c(0L, 2L)) {
        for (payload in c(FALSE, TRUE)) {
          input <- tibble::tibble(g = c("a", "b"))
          if (payload) input$value <- 1:2
          if (role %in% c("fixed", "dimension")) names(input)[1L] <- ".rows"
          if (role == "payload") input$.rows <- 1:2
          input <- input[seq_len(rows), ]
          if (backend == "dtplyr") input <- dtplyr::lazy_dt(input)
          call_args <- list(.keep = keep, .id = "set")
          if (role == "id") call_args$.id <- ".rows"
          if (role == "key") call_args$.key <- ".rows"
          if (role == "fixed") call_args$.by <- quote(.rows)
          else call_args$.grouping <- if (role == "dimension") rollup(.rows) else rollup(g)
          actual <- tryCatch(
            dplyr::collect(eval(rlang::call2(verb, quote(input), !!!call_args))),
            error = identity
          )
          failed <- inherits(actual, "error")
          if (role %in% c("key", "payload")) stopifnot(!failed)
          observations[[length(observations) + 1L]] <- data.frame(
            backend = backend, verb = verb, role = role, keep = keep,
            rows = rows, payload = payload, failed = failed
          )
        }
      }
    }
  }
}
result <- do.call(rbind, observations)
print(stats::aggregate(failed ~ backend + verb + role, result, sum), row.names = FALSE)
cat("Observed", nrow(result), "calls; list-column-name and payload controls all passed.\n")
