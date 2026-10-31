# Dated evidence matrix. Run from the repository root.
devtools::load_all(quiet = TRUE)
output <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempfile("prototype-matrix-"))
dir.create(output, recursive = TRUE, showWarnings = FALSE)
builders <- list(
  dist = function() stats::dist(matrix(c(0, 1, 4), ncol = 1L)),
  logLik = function() stats::logLik(stats::lm(mpg ~ wt, data = mtcars))
)
plans <- list(
  included_first = list("key", character()),
  omitted_first = list(character(), "key"),
  duplicate = list("key", character(), "key")
)
rows <- list()
i <- 0L
for (family in names(builders)) {
  original <- builders[[family]]()
  for (n in c(0L, 1L, 3L)) for (packed in c(FALSE, TRUE)) {
    index <- if (family == "dist") seq_len(n) else rep(1L, n)
    key <- vctrs::vec_slice(original, index)
    if (packed) key <- tibble::tibble(inner = key)
    base <- tibble::tibble(key = key, h = rep("a", n), row = seq_len(n))
    for (container in c("tibble", "data.frame", "data.table")) {
      input <- switch(container, tibble = base, data.frame = as.data.frame(base),
                      data.table = data.table::as.data.table(base))
      untouched <- serialize(input, NULL)
      for (role in c("dimension", "fixed", "payload")) {
        for (label_kind in c("NULL", "NA")) for (plan_name in names(plans)) {
          sets <- if (role == "dimension") plans[[plan_name]] else {
            lapply(plans[[plan_name]], function(s) if (length(s)) "h" else character())
          }
          dimensions <- if (role == "dimension") "key" else "h"
          fixed <- if (role == "fixed") "key" else character()
          specs <- lapply(sets, function(s) grouping_set(dplyr::all_of(s)))
          specification <- rlang::inject(grouping_sets(!!!specs))
          label <- if (label_kind == "NULL") NULL else NA_character_
          control <- tryCatch({
            branches <- lapply(seq_along(sets), function(j) {
              out <- input
              for (name in setdiff(dimensions, sets[[j]])) {
                value <- vctrs::vec_init(input[[name]], n)
                out <- dplyr::mutate(out, "{name}" := !!value)
              }
              dplyr::mutate(out, occurrence = j)
            })
            result <- Reduce(dplyr::union_all, branches)
            dplyr::relocate(result, dplyr::all_of(c(fixed, dimensions, "occurrence")))
          }, error = identity)
          for (sort in c("none", "last", "first")) {
            actual <- tryCatch(expand_with_margins(
              input, .by = dplyr::all_of(fixed), .grouping = specification,
              .margin_label = label, .duplicates = "keep",
              .id = "occurrence", .sort = sort
            ), error = identity)
            status <- if (inherits(actual, "error")) {
              if (inherits(control, "error")) "shared_error" else "package_error"
            } else if (inherits(control, "error")) "control_error" else {
              ordered_actual <- dplyr::arrange(actual, .data$occurrence, .data$row)
              ordered_control <- dplyr::arrange(control, .data$occurrence, .data$row)
              if (isTRUE(all.equal(ordered_actual, ordered_control))) "match" else "difference"
            }
            i <- i + 1L
            rows[[i]] <- data.frame(
              family, n, packed_requested = packed, packed_actual = is.data.frame(input$key),
              container, role, label_kind, plan_name, sort, status,
              source_unchanged = identical(untouched, serialize(input, NULL)),
              detail = if (inherits(actual, "error")) conditionMessage(actual) else ""
            )
          }
        }
      }
    }
  }
}
results <- do.call(rbind, rows)
write.csv(results, file.path(output, "prototype-results.csv"), row.names = FALSE)
print(with(results, table(family, status)))
print(with(results, table(packed_actual, role, status)))
stopifnot(all(results$source_unchanged))
cat("Output:", output, "\n")
