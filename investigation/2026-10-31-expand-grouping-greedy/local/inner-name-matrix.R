# Dated evidence matrix. Run from the repository root.
devtools::load_all(quiet = TRUE)
output <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempfile("inner-name-matrix-"))
dir.create(output, recursive = TRUE, showWarnings = FALSE)
rows <- list()
i <- 0L
for (n in c(0L, 1L, 3L, 6L)) for (packed in c(FALSE, TRUE)) {
  key <- vctrs::vec_slice(summary(c(1, 2, 3)), seq_len(n))
  if (packed) key <- tibble::tibble(inner = key)
  base <- tibble::tibble(key = key, h = rep("a", n), row = seq_len(n))
  for (container in c("tibble", "data.frame", "data.table")) {
    input <- switch(container, tibble = base, data.frame = as.data.frame(base),
                    data.table = data.table::as.data.table(base))
    untouched <- serialize(input, NULL)
    for (role in c("dimension", "fixed", "payload")) {
      dimensions <- if (role == "dimension") "key" else "h"
      fixed <- if (role == "fixed") "key" else character()
      for (label_kind in c("NULL", "NA", "Total")) {
        if (packed && role == "dimension" && label_kind == "Total") next
        label <- switch(label_kind, "NULL" = NULL, "NA" = NA_character_, "Total" = "Total")
        for (plan_name in c("included_first", "omitted_first", "duplicate")) {
          sets <- switch(plan_name, included_first = list(dimensions, character()),
                         omitted_first = list(character(), dimensions),
                         duplicate = list(dimensions, character(), dimensions))
          specs <- lapply(sets, function(s) grouping_set(dplyr::all_of(s)))
          specification <- rlang::inject(grouping_sets(!!!specs))
          control <- tryCatch({
            branches <- lapply(seq_along(sets), function(j) {
              out <- input
              for (name in dimensions) {
                value <- if (label_kind == "Total") {
                  if (name %in% sets[[j]]) as.character(input[[name]]) else rep(label, n)
                } else {
                  if (name %in% sets[[j]]) input[[name]] else vctrs::vec_init(input[[name]], n)
                }
                out <- dplyr::mutate(out, "{name}" := !!value)
              }
              dplyr::mutate(out, occurrence = j)
            })
            result <- suppressMessages(dplyr::bind_rows(branches))
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
              n, packed_requested = packed, packed_actual = is.data.frame(input$key),
              container, role, label_kind, plan_name, sort, status,
              source_unchanged = identical(untouched, serialize(input, NULL)),
              detail = if (inherits(actual, "error")) conditionMessage(actual) else "",
              control_detail = if (inherits(control, "error")) conditionMessage(control) else ""
            )
          }
        }
      }
    }
  }
}
results <- do.call(rbind, rows)
write.csv(results, file.path(output, "inner-name-results.csv"), row.names = FALSE)
print(with(results, table(status)))
print(with(results, table(n, packed_actual, status)))
print(with(results, table(role, label_kind, status)))
stopifnot(all(results$source_unchanged))
cat("Output:", output, "\n")
