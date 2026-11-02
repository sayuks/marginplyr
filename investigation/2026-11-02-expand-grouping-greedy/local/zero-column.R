# Zero-column cardinality oracle from the documented constructor algebra.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
out_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempdir())
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
capture <- function(expr) tryCatch(force(expr), error = identity)
plans <- list(
  null = list(spec = NULL, count = 1L),
  set = list(spec = grouping_set(), count = 1L),
  identity_product = list(spec = grouping_spec(), count = 1L),
  set_empty_selection = list(spec = grouping_set(dplyr::everything()), count = 1L),
  one_union = list(spec = grouping_sets(grouping_set()), count = 1L),
  repeated_union = list(spec = grouping_sets(grouping_set(), grouping_set()), count = 2L),
  product = list(spec = grouping_spec(grouping_spec(), grouping_sets(grouping_set(), grouping_set())), count = 2L),
  empty_union = list(spec = grouping_sets(), count = NA_integer_),
  empty_rollup = list(spec = rollup(), count = NA_integer_),
  empty_cube = list(spec = cube(), count = NA_integer_),
  empty_rollup_selection = list(spec = rollup(dplyr::everything()), count = NA_integer_),
  empty_cube_selection = list(spec = cube(dplyr::everything()), count = NA_integer_)
)
rows <- list()
for (container in c("data.frame", "tibble")) {
  for (n in c(0L, 1L, 4L)) {
    source <- if (container == "tibble") tibble::new_tibble(list(), nrow = n) else data.frame(row.names = seq_len(n))
    before <- serialize(source, NULL)
    for (plan_name in names(plans)) {
      plan <- plans[[plan_name]]
      for (duplicates in c("error", "drop", "keep")) {
        count <- if (duplicates == "drop") min(plan$count, 1L) else plan$count
        required_error <- is.na(plan$count) || (duplicates == "error" && plan$count > 1L)
        for (id in list(NULL, "occurrence", "..1")) {
          for (sort in c("none", "first", "last")) {
            got <- capture(expand_with_margins(
              source, .grouping = plan$spec, .duplicates = duplicates,
              .id = id, .sort = sort
            ))
            status <- if (required_error) {
              if (inherits(got, "marginplyr_error")) "required_refusal" else "difference"
            } else if (inherits(got, "error")) "unexpected_error" else {
              equal <- nrow(got) == n * count && ncol(got) == as.integer(!is.null(id)) &&
                identical(names(got), if (is.null(id)) character() else id) &&
                (is.null(id) || identical(sort(got[[id]]), rep(seq_len(count), each = n))) &&
                (is.null(id) || is.integer(got[[id]]))
              if (equal) "match" else "difference"
            }
            rows[[length(rows) + 1L]] <- data.frame(
              container, n, plan_name, duplicates, id = if (is.null(id)) "<absent>" else id,
              sort, status, detail = if (inherits(got, "error")) conditionMessage(got) else ""
            )
            stopifnot(identical(serialize(source, NULL), before))
          }
        }
      }
    }
  }
}
results <- dplyr::bind_rows(rows)
write.csv(results, file.path(out_dir, "zero-column.csv"), row.names = FALSE)
print(table(results$status))
print(results[!results$status %in% c("match", "required_refusal"), ], row.names = FALSE)
