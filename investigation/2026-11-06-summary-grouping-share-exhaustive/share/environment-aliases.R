# Dated public-interface research harness, without implementation changes.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
require_contract <- "--assert" %in% args
controls_only <- "--controls-only" %in% args
external <- new.env(parent = emptyenv())
external$prior <- 7
witnesses <- alist(
  member = { mask <- parent.env(environment()); sum(mask$prior) },
  index = { mask <- parent.env(environment()); sum(mask[["prior"]]) },
  scalar_accessor = { mask <- parent.env(environment()); sum(rlang::env_get(mask, "prior")) },
  frame_accessor = { frame <- environment(); sum(rlang::env_get(frame, "prior", inherit = TRUE)) },
  bulk_accessor = { mask <- parent.env(environment()); sum(rlang::env_get_list(mask, "prior")$prior) },
  materialization = { mask <- parent.env(environment()); sum(as.list(mask)$prior) }
)
refusals <- alist(
  literal_member = sum(parent.env(environment())$prior),
  literal_scalar_accessor = sum(rlang::env_get(parent.env(environment()), "prior")),
  literal_bulk_accessor = sum(rlang::env_get_list(parent.env(environment()), "prior")$prior),
  literal_materialization = sum(as.list(parent.env(environment()))$prior),
  alias_base_get = { mask <- parent.env(environment()); sum(get("prior", envir = mask, inherits = FALSE)) }
)
legal <- alist(
  external_member = { mask <- external; sum(mask$prior) },
  external_index = { mask <- external; sum(mask[["prior"]]) },
  external_scalar_accessor = { mask <- external; sum(rlang::env_get(mask, "prior")) },
  external_bulk_accessor = { mask <- external; sum(rlang::env_get_list(mask, "prior")$prior) },
  external_materialization = { mask <- external; sum(as.list(mask)$prior) },
  bound_frame_accessor = (function(prior) {
    frame <- environment()
    sum(rlang::env_get(frame, "prior", inherit = TRUE))
  })(7)
)
cases <- c(witnesses, refusals, legal)
expected <- c(rep("missing-refusal", length(witnesses)), rep("refusal", length(refusals)), rep("legal", length(legal)))
names(expected) <- names(cases)
if (controls_only) cases <- cases[expected != "missing-refusal"]
d <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
inputs <- list(data.frame = d, tibble = tibble::as_tibble(d),
               data.table = data.table::as.data.table(d))
rows <- list()
for (input_name in names(inputs)) {
  data <- inputs[[input_name]]
  before <- serialize(data, NULL)
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(source))
    for (direction in c("forward", "reverse")) {
      for (nm in names(cases)) {
        result <- tryCatch({
          expr <- cases[[nm]]
          value <- if (direction == "forward") {
            summarize_with_margins(data, source = sum(x), prior = !!helper,
                                   observed = !!expr, .grouping = rollup(g))
          } else {
            summarize_with_margins(data, prior = sum(x), source = !!expr,
                                   ratio = !!helper, .grouping = rollup(g))
          }
          if (input_name == "dtplyr") value <- dplyr::collect(value)
          value
        }, error = identity)
        refused <- inherits(result, "marginplyr_error") && grepl(
          if (direction == "forward") "Ordinary summaries cannot use an earlier" else "depends on earlier summary alias",
          conditionMessage(result), fixed = TRUE
        )
        outcome <- expected[[nm]]
        if (outcome == "refusal" || (outcome == "missing-refusal" && require_contract)) {
          stopifnot(refused)
        } else {
          if (inherits(result, "error")) stop(paste(input_name, kind, direction, nm, conditionMessage(result)))
          observed <- as.numeric(if (direction == "forward") result$observed else result$source)
          target <- if (outcome == "legal") c(7, 7, 7) else if (direction == "forward" || input_name == "dtplyr") c(100, 300, 400) else c(1, 3, 4)
          if (!identical(observed, target)) stop(paste(input_name, kind, direction, nm, paste(observed, collapse = ",")))
        }
        rows[[length(rows) + 1L]] <- data.frame(input = input_name, kind = kind,
          direction = direction, case = nm, expected = outcome, refused = refused)
      }
    }
  }
  stopifnot(identical(serialize(data, NULL), before))
  cat("Completed", input_name, "\n")
}
results <- do.call(rbind, rows)
print(with(results, table(expected, input)))
cat("Comparisons:", nrow(results), "\n")
if (!require_contract && !controls_only) {
  script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))
  write.csv(results, file.path(dirname(script), "environment-aliases-results.csv"), row.names = FALSE)
}
