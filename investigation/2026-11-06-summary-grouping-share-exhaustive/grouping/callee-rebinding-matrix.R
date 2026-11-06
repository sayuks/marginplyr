Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
output <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempdir())
dir.create(output, recursive = TRUE, showWarnings = FALSE)
input <- tibble::tibble(a = c("east", "west"), b = c(1L, 1L), value = c(2, 3))
seen <- character()
makeActiveBinding("hunt_pattern_source", function() {
  kind <- typeof(tidyselect::peek_data()$a)
  seen <<- c(seen, kind)
  if (kind == "character") "^a$" else "^b$"
}, globalenv())
caller <- rlang::env(globalenv(), paste0 = "ordinary nonfunction binding")
pattern <- eval(quote(function() {
  paste0 <- as.vector(as.vector("hunt_pattern_source", "symbol"), "function")
  paste0()
}), caller)
inputs <- list(
  tibble = input,
  data_frame = as.data.frame(input),
  data_table = data.table::as.data.table(input),
  dtplyr = dtplyr::lazy_dt(input)
)
original <- lapply(inputs, serialize, connection = NULL)
constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
records <- list()
run <- function(data, consumer, constructor, position, expr) {
  spec <- if (position == "fixed") {
    marginplyr::grouping_set(b)
  } else {
    eval(rlang::call2(getExportedValue("marginplyr", constructor), expr))
  }
  by <- if (position == "fixed") expr else if (position == "dimension") quote(b) else NULL
  if (consumer == "inspect") {
    call <- rlang::call2(marginplyr::inspect_grouping,
      .data = data, .by = by, .grouping = spec, .format = "list")
  } else {
    call <- rlang::call2(marginplyr::summarize_with_margins,
      .data = data, n = quote(dplyr::n()), .by = by,
      .grouping = spec, .margin_label = NULL, .id = "set")
  }
  seen <<- character()
  result <- tryCatch(eval(call), error = identity)
  if (inherits(result, "dtplyr_step")) result <- tibble::as_tibble(result)
  list(value = result, metadata = paste(seen, collapse = ","))
}
for (backend in names(inputs)) {
  data <- inputs[[backend]]
  for (position in c("dimension", "fixed", "unfixed")) {
    kinds <- if (position == "fixed") "grouping_set" else constructors
    consumers <- if (position == "unfixed") "inspect" else c("inspect", "summary")
    for (constructor in kinds) for (consumer in consumers) {
      plain <- run(data, consumer, constructor, position, quote(matches(pattern())))
      control <- run(data, consumer, constructor, position, quote(matches(base::identity(pattern()))))
      literal <- run(data, consumer, constructor, position, quote(all_of("a")))
      control_match <- identical(control$value, literal$value)
      if (!control_match) {
        print(list(backend = backend, position = position, constructor = constructor,
          control = control$value, literal = literal$value))
        stop("Typed control differs from literal plan")
      }
      outcome <- if (inherits(plain$value, "error")) {
        "false-refusal"
      } else if (!identical(plain$value, literal$value)) {
        "wrong-plan"
      } else "final-plan-matches"
      records[[length(records) + 1L]] <- data.frame(
        backend, position, constructor, consumer, outcome,
        control_match, plain_metadata = plain$metadata,
        control_metadata = control$metadata,
        message = if (inherits(plain$value, "error")) conditionMessage(plain$value) else "",
        stringsAsFactors = FALSE
      )
    }
  }
}
results <- do.call(rbind, records)
stopifnot(nrow(results) == 68L,
  sum(results$outcome == "false-refusal") == 48L,
  sum(results$outcome == "wrong-plan") == 5L,
  sum(results$outcome == "final-plan-matches") == 15L,
  all(results$control_match),
  identical(original, lapply(inputs, serialize, connection = NULL)))
write.csv(results, file.path(output, "callee-rebinding-matrix.csv"), row.names = FALSE)
print(table(results$outcome))
cat("Typed-wrapper/literal exact matches:", sum(results$control_match), "\n")
cat("Caller input serializations unchanged\n")
for (pkg in c("dplyr", "tidyselect", "rlang", "vctrs", "dtplyr", "data.table", "pkgload")) {
  cat(pkg, as.character(utils::packageVersion(pkg)), "\n")
}
cat(R.version.string, "\n", R.version$platform, "\n")
