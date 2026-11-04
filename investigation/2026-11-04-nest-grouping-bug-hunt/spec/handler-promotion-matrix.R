source_dir <- Sys.getenv("MARGINPLYR_HUNT_SOURCE", unset = getwd())
pkgload::load_all(source_dir, quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")

run <- function() {
  previous <- options(warn = 0L)
  on.exit(options(previous), add = TRUE)
  source <- tibble::tibble(a = 1L, b = "x")
  safe_step <- dtplyr::lazy_dt(source)
  unsafe_step <- safe_step |> dplyr::mutate(b = "z")
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(con, source, "handler_promotion_matrix")
  observed <- new.env(parent = emptyenv())
  observed$queries <- character()
  suppressMessages(do.call(trace, list(
    what = "dbSendQuery", signature = c("duckdb_connection", "character"),
    where = asNamespace("DBI"), print = FALSE,
    tracer = function() {
      observed$queries <- c(observed$queries, get("statement", parent.frame()))
    }
  )))
  on.exit(suppressMessages(untrace(
    "dbSendQuery", signature = c("duckdb_connection", "character"),
    where = asNamespace("DBI")
  )), add = TRUE, after = FALSE)
  promoted <- structure(list(message = "caller promoted selector warning", call = quote(user_call()),
                             token = 41104L, payload = list("original", c(a = 1L))),
                        class = c("caller_selector_failure", "error", "condition"))
  constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
  verbs <- list(
    inspect = function(input, spec) inspect_grouping(input, .grouping = spec, .duplicates = "drop"),
    nest = function(input, spec) nest_with_margins(input, .grouping = spec, .duplicates = "drop"),
    nest_by = function(input, spec) nest_by_with_margins(input, .grouping = spec, .duplicates = "drop"),
    expand = function(input, spec) expand_with_margins(input, .grouping = spec, .duplicates = "drop"),
    summarize = function(input, spec) summarize_with_margins(input, n = dplyr::n(),
                                                          .grouping = spec, .duplicates = "drop")
  )
  rows <- list()
  predicates <- 0L
  numeric_column <- function(x) {
    predicates <<- predicates + 1L
    is.numeric(x)
  }
  for (warn in c(0L, 1L, 2L)) {
    options(warn = warn)
    for (constructor in constructors) {
      spec <- do.call(get(constructor, asNamespace("marginplyr")),
                      list(rlang::quo(one_of("absent")), rlang::quo(where(numeric_column))))
      for (verb_name in names(verbs)) {
        for (mode in c("stop", "muffle")) {
          inputs <- if (mode == "stop") {
            list(local = source, dtplyr = unsafe_step, duckdb = remote)
          } else list(local = source, dtplyr = safe_step, duckdb = remote)
          for (backend in names(inputs)) {
            if (backend == "duckdb" && verb_name %in% c("nest", "nest_by")) next
            predicates <- 0L
            warnings <- list()
            observed$queries <- character()
            result <- tryCatch(withCallingHandlers(
              verbs[[verb_name]](inputs[[backend]], spec),
              warning = function(cnd) {
                warnings[[length(warnings) + 1L]] <<- cnd
                if (mode == "stop") stop(promoted)
                invokeRestart("muffleWarning")
              }
            ), error = identity)
            row <- data.frame(warn, constructor, verb = verb_name, mode, backend,
                              warnings = length(warnings), predicates,
                              queries = length(observed$queries),
                              exact_error = identical(result, promoted),
                              error_class = if (inherits(result, "error")) class(result)[[1L]] else "none")
            rows[[length(rows) + 1L]] <- row
          }
        }
      }
    }
  }
  rows <- do.call(rbind, rows)
  stopping <- rows[rows$mode == "stop", ]
  controls <- rows[rows$mode == "muffle", ]
  output <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", unset = "/private/tmp")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(rows, file.path(output, "handler-promotion-matrix.csv"), row.names = FALSE)
  stopifnot(nrow(rows) == 390L, all(stopping$predicates == 0L),
            all(stopping$exact_error[stopping$backend == "local"]),
            all(stopping$queries[stopping$warn == 2L] == 0L),
            all(stopping$exact_error[stopping$warn == 2L]),
            all(stopping$queries[stopping$backend == "duckdb" & stopping$warn < 2L] == 1L),
            all(stopping$error_class[stopping$backend == "dtplyr" & stopping$warn < 2L] == "marginplyr_error"),
            all(stopping$warnings[stopping$backend == "dtplyr" & stopping$warn < 2L] == 0L),
            all(controls$error_class == "none"), all(controls$warnings == 1L),
            all(controls$predicates == 2L), all(controls$queries[controls$backend == "duckdb"] == 1L))
  cat("Baseline: 78ebbc36200bdc5cef70838dd4e772c5c4a1401c\n")
  cat("Cases:", nrow(rows), "; stopping:", nrow(stopping), "; nonstopping controls:", nrow(controls), "\n")
  print(xtabs(~warn + backend + exact_error, stopping))
  print(xtabs(~warn + backend + queries, stopping))
  cat("All muffle controls succeeded, delivered one warning and evaluated two typed predicates.\n")
  sessionInfo()
}
run()
