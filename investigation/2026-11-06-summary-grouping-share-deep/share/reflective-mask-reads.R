# Run from an archived source root. Observation mode pins the dated failure;
# --assert requires the dependency contract and exits nonzero on that source.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
arguments <- commandArgs(trailingOnly = TRUE)
contract <- "--assert" %in% arguments
only_controls <- "--controls-only" %in% arguments
modes <- setdiff(arguments, c("--assert", "--controls-only"))
if (!length(modes)) modes <- c("index", "options", "bulk")
stopifnot(all(modes %in% c("index", "options", "bulk")))
data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
inputs <- list(data.frame = data, tibble = tibble::as_tibble(data),
               data.table = data.table::as.data.table(data))
external <- list2env(list(prior = 7), parent = baseenv())
target <- "prior"
expressions <- list(
  index = alist(
    concatenate = sum(parent.env(environment())[[paste0("pri", "or")]]),
    lexical_key = sum(parent.env(environment())[[target]]),
    local_key = { key <- "prior"; sum(parent.env(environment())[[key]]) },
    exact = sum(parent.env(environment())[[paste0("pri", "or"), exact = TRUE]]),
    element = sum(getElement(parent.env(environment()), paste0("pri", "or"))),
    subset = sum(.subset2(parent.env(environment()), paste0("pri", "or"))),
    literal_control = sum(parent.env(environment())[["prior"]]),
    base_control = sum(get(paste0("pri", "or"), envir = parent.env(environment()))),
    rlang_control = sum(rlang::env_get(environment(), paste0("pri", "or"),
                                     inherit = TRUE)),
    external_control = sum(external[[paste0("pri", "or")]])
  ),
  options = alist(
    inheritance = sum(rlang::env_get(environment(), "prior", inherit = isTRUE(TRUE))),
    global_binding = sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                                       last = .GlobalEnv)),
    stop_identity = sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                                      last = identity(globalenv()))),
    literal_control = sum(rlang::env_get(environment(), "prior", inherit = TRUE)),
    global_call_control = sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                                            last = globalenv())),
    base_control = sum(get("prior", envir = environment(), inherits = isTRUE(TRUE))),
    external_control = sum(rlang::env_get(external, "prior", inherit = isTRUE(TRUE))),
    local_control = (function(prior) {
      sum(rlang::env_get(environment(), "prior", inherit = isTRUE(FALSE)))
    })(1),
    stopped_control = sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                                        last = environment(), default = 7))
  ),
  bulk = alist(
    selected = sum(rlang::env_get_list(environment(), "prior", inherit = TRUE)$prior),
    selected_pair = sum(rlang::env_get_list(environment(), c("prior", "x"),
                                           inherit = TRUE)$prior),
    explicit_mask = sum(rlang::env_get_list(parent.env(environment()), "prior")$prior),
    materialized = sum(as.list(parent.env(environment()))$prior),
    materialized_index = sum(as.list(parent.env(environment()))[["prior"]]),
    scalar_control = sum(rlang::env_get(environment(), "prior", inherit = TRUE)),
    base_control = sum(mget("prior", envir = environment(), inherits = TRUE)$prior),
    external_control = sum(rlang::env_get_list(external, "prior", inherit = TRUE)$prior),
    external_list_control = sum(as.list(external)$prior),
    local_control = (function(prior) {
      sum(rlang::env_get_list(environment(), "prior", inherit = TRUE)$prior)
    })(1),
    local_list_control = (function(prior) sum(as.list(environment())$prior))(1)
  )
)
legal <- list(index = c(external_control = 7),
              options = c(external_control = 7, local_control = 1, stopped_control = 7),
              bulk = c(external_control = 7, external_list_control = 7,
                       local_control = 1, local_list_control = 1))
run <- function(input, expr, helper, direction) {
  share <- rlang::call2(helper, rlang::sym(if (direction == "forward") "source" else "observed"))
  tryCatch(if (direction == "forward") {
    summarize_with_margins(input, source = sum(x), prior = !!share, observed = !!expr,
                           .grouping = rollup(g), .sort = "last")
  } else {
    summarize_with_margins(input, prior = sum(x), observed = !!expr, ratio = !!share,
                           .grouping = rollup(g), .sort = "last")
  }, error = identity)
}
rows <- list()
for (mode in modes) for (input_name in names(inputs)) {
  before <- serialize(as.data.frame(inputs[[input_name]]), NULL)
  for (helper in c("share_of_total", "share_of_parent")) for (direction in c("forward", "reverse")) {
    cases <- names(expressions[[mode]])
    if (only_controls) cases <- cases[grepl("control$", cases)]
    for (case in cases) {
      value <- run(inputs[[input_name]], expressions[[mode]][[case]], helper, direction)
      is_legal <- case %in% names(legal[[mode]])
      witness <- !grepl("control$", case)
      refusal <- inherits(value, "marginplyr_error") && grepl(
        if (direction == "forward") "Ordinary summaries cannot use an earlier" else "depends on earlier summary alias",
        gsub("[[:space:]]+", " ", conditionMessage(value)), fixed = TRUE
      )
      if (is_legal) {
        stopifnot(!inherits(value, "condition"), all(value$observed == legal[[mode]][[case]]))
      } else if (contract || !witness) {
        stopifnot(refusal)
      } else {
        stopifnot(!inherits(value, "condition"))
        expected <- if (direction == "forward") c(100, 300, 400) else c(1, 3, 4)
        stopifnot(identical(as.double(value$observed), expected))
      }
      rows[[length(rows) + 1L]] <- data.frame(mode, input = input_name, helper,
        direction, case, expected = if (is_legal) "valid" else "refusal",
        observed = if (refusal) "refusal" else "success")
    }
  }
  stopifnot(identical(before, serialize(as.data.frame(inputs[[input_name]]), NULL)))
}
results <- do.call(rbind, rows)
print(with(results, table(mode, expected, observed)))
cat("Total comparisons:", nrow(results), "\n")
for (mode in if (only_controls) character() else modes) {
  cat("\nMinimal", mode, "forward witness:\n")
  print(run(inputs[[1]], expressions[[mode]][[1]], "share_of_total", "forward"))
}
cat("\nEnvironment:\n", R.version.string, "\n", R.version$platform, "\n")
for (package in c("dplyr", "rlang", "pkgload", "tibble", "data.table")) {
  cat(package, as.character(utils::packageVersion(package)), "\n")
}
