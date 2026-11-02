# Independent integer-code oracle for factor levels, missingness, and Margin order.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
out_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempdir())
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
capture <- function(expr) tryCatch(force(expr), error = identity)
levels_cases <- list(
  c("z", "a"), c("", "a", NA_character_),
  c("..marginplyr_missing_factor_code", "..marginplyr_missing_factor_code_", NA_character_),
  c("日本語", "é", "e\u0301", NA_character_)
)
plans <- list(
  included = list(spec = grouping_set(g), bits = 0L),
  rollup = list(spec = rollup(g), bits = c(0L, 1L)),
  duplicate = list(
    spec = grouping_sets(grouping_set(), grouping_set(g), grouping_set()),
    bits = c(1L, 0L, 1L)
  )
)
rows <- list()
for (level_id in seq_along(levels_cases)) {
  original_levels <- levels_cases[[level_id]]
  for (ordered in c(FALSE, TRUE)) {
    class <- if (ordered) c("ordered", "factor") else "factor"
    for (n in c(0L, 1L, 7L)) {
      codes <- rep(c(seq_along(original_levels), NA_integer_), length.out = n)
      g <- structure(codes, levels = original_levels, class = class)
      fixed <- structure(rev(codes), levels = original_levels, class = class)
      dat <- tibble(g = g, fixed = fixed, row = seq_len(n))
      for (persistent in c(FALSE, TRUE)) {
        source <- if (persistent) group_by(dat, fixed) else dat
        by_quo <- if (persistent) rlang::quo(NULL) else rlang::quo(fixed)
        for (plan_name in names(plans)) {
          plan <- plans[[plan_name]]
          for (label_name in c("null", "na", "text")) {
            if (label_name == "na" && anyNA(original_levels)) next
            label <- switch(label_name, null = NULL, na = NA_character_, text = "Margin")
            for (position in c("first", "last")) {
              new_levels <- if (is.null(label) || is.na(label)) original_levels else {
                if (position == "first") c(label, original_levels) else c(original_levels, label)
              }
              for (sort in c("none", "first", "last")) {
                expected <- bind_rows(lapply(seq_along(plan$bits), function(k) {
                  bit <- plan$bits[[k]]
                  new_codes <- if (bit == 1L) {
                    if (is.null(label) || is.na(label)) rep(NA_integer_, n) else rep(match(label, new_levels), n)
                  } else {
                    match(original_levels[codes], new_levels)
                  }
                  # match(NA, levels) selects the declared NA level; source missing stays missing.
                  if (bit == 0L) new_codes[is.na(codes)] <- NA_integer_
                  tibble(
                    fixed = fixed,
                    g = structure(new_codes, levels = new_levels, class = class),
                    row = seq_len(n), occurrence = k, bit = bit
                  )
                }))
                if (sort != "none") {
                  expected <- arrange(
                    expected, is.na(fixed), fixed,
                    if (sort == "first") -bit else bit,
                    is.na(g), g, occurrence
                  )
                }
                got <- capture(expand_with_margins(
                  source, .by = !!by_quo,
                  .grouping = plan$spec, .margin_label = label,
                  .margin_label_position = position, .duplicates = "keep",
                  .id = "occurrence", .sort = sort
                ))
                status <- if (inherits(got, "error")) "error" else {
                  got_aligned <- arrange(got, occurrence, row)
                  expected_aligned <- arrange(expected, occurrence, row)
                  equal <- identical(as.integer(got_aligned$g), as.integer(expected_aligned$g)) &&
                    identical(levels(got$g), new_levels) && identical(class(got$g), class) &&
                    identical(as.integer(got_aligned$fixed), as.integer(expected_aligned$fixed)) &&
                    identical(levels(got$fixed), original_levels) && !is_grouped_df(got)
                  order_equal <- sort == "none" || identical(
                    as.data.frame(transmute(got, fixed = as.integer(fixed), g = as.integer(g), occurrence)),
                    as.data.frame(transmute(expected, fixed = as.integer(fixed), g = as.integer(g), occurrence))
                  )
                  if (equal && order_equal) "match" else "difference"
                }
                detail <- if (inherits(got, "error")) conditionMessage(got) else ""
                rows[[length(rows) + 1L]] <- data.frame(
                  level_id, ordered, n, persistent, plan_name, label_name, position, sort, status, detail
                )
              }
            }
          }
        }
      }
    }
  }
}
results <- bind_rows(rows)
write.csv(results, file.path(out_dir, "factor-order.csv"), row.names = FALSE)
print(table(results$status))
print(head(results[results$status != "match", ], 20L), row.names = FALSE)
