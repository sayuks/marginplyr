# Run from the archived package root; --assert requires the desired refusal.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
contract <- "--assert" %in% commandArgs(trailingOnly = TRUE)
data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
inputs <- list(data.frame = data, tibble = tibble::as_tibble(data),
               data.table = data.table::as.data.table(data))
field <- "prior"
external <- new.env(parent = baseenv())
external$prior <- 7
witnesses <- alist(
  computed_name = sum(rlang::env_get(environment(), paste0("pri", "or"),
                                     inherit = TRUE)),
  lexical_name = sum(rlang::env_get(environment(), field, inherit = TRUE)),
  pronoun_name = sum(rlang::env_get(environment(), .env$field, inherit = TRUE)),
  local_name = { field <- "prior"; sum(rlang::env_get(environment(), field,
                                                    inherit = TRUE)) }
)
controls <- alist(
  literal_name = sum(rlang::env_get(environment(), "prior", inherit = TRUE)),
  base_dynamic = sum(base::get(paste0("pri", "or"), envir = environment())),
  external_dynamic = sum(rlang::env_get(external, field, inherit = TRUE)),
  local_binding = (function(prior) {
    sum(rlang::env_get(environment(), "prior", inherit = FALSE))
  })(1)
)
run <- function(input, expr, helper, direction) {
  share <- rlang::call2(helper, rlang::sym(if (direction == "forward") {
    "source"
  } else {
    "observed"
  }))
  tryCatch(if (direction == "forward") {
    rlang::inject(summarize_with_margins(
      input, source = sum(x), prior = !!share, observed = !!expr,
      .grouping = rollup(g), .sort = "last"
    ))
  } else {
    rlang::inject(summarize_with_margins(
      input, prior = sum(x), observed = !!expr, ratio = !!share,
      .grouping = rollup(g), .sort = "last"
    ))
  }, error = identity)
}
rows <- list()
for (input_name in names(inputs)) {
  for (helper in c("share_of_total", "share_of_parent")) {
    for (direction in c("forward", "reverse")) {
      for (case in c(names(witnesses), names(controls))) {
        expr <- c(witnesses, controls)[[case]]
        value <- run(inputs[[input_name]], expr, helper, direction)
        error <- inherits(value, "marginplyr_error")
        if (error) {
          fragment <- if (direction == "forward") {
            "Ordinary summaries cannot use an earlier"
          } else {
            "depends on earlier summary alias"
          }
          message <- gsub("[[:space:]]+", " ", conditionMessage(value))
          error <- grepl(fragment, message, fixed = TRUE)
        }
        witness <- case %in% names(witnesses)
        should_refuse <- witness || case %in% c("literal_name", "base_dynamic")
        if (contract && should_refuse) {
          stopifnot(error)
        } else if (witness) {
          stopifnot(!inherits(value, "condition"))
          expected <- if (direction == "forward") c(100, 300, 400) else c(1, 3, 4)
          stopifnot(identical(as.double(value$observed), expected))
        } else if (should_refuse) {
          stopifnot(error)
        } else {
          expected <- if (case == "external_dynamic") 7 else 1
          stopifnot(!inherits(value, "condition"), all(value$observed == expected))
        }
        rows[[length(rows) + 1L]] <- data.frame(
          input = input_name, helper = helper, direction = direction,
          case = case, expected = if (should_refuse) "refusal" else "valid",
          observed = if (error) "refusal" else "success"
        )
      }
    }
  }
}
results <- do.call(rbind, rows)
print(results, row.names = FALSE)
print(with(results, table(expected, observed)))
cat("\nMinimal forward witness:\n")
print(run(inputs[[1L]], witnesses[[1L]], "share_of_total", "forward"))
cat("\nEnvironment:\n")
cat(R.version.string, "\n", R.version$platform, "\n")
for (package in c("dplyr", "rlang", "pkgload", "tibble", "data.table")) {
  cat(package, as.character(utils::packageVersion(package)), "\n")
}
