Sys.setenv(TESTTHAT_PARALLEL = "false")
source_root <- Sys.getenv("MARGINPLYR_HUNT_SOURCE")
if (!nzchar(source_root)) stop("Set MARGINPLYR_HUNT_SOURCE.")
pkgload::load_all(source_root, quiet = TRUE)
out <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT")
if (!nzchar(out) || !dir.exists(out)) stop("Set an existing MARGINPLYR_HUNT_OUTPUT.")
data <- tibble::tibble(a = "one", b = 2L, c = TRUE)
forms <- list(
  bare = function(x) x,
  syntactic = function(x) rlang::call2("(", x),
  deep_syntactic = function(x) Reduce(function(y, z) rlang::call2("(", y), seq_len(5), init = x),
  public = function(x) rlang::call2(quote(base::`(`), x),
  public_head = function(x) rlang::call2(quote((base::`(`)), x),
  mixed = function(x) rlang::call2(quote(base::`(`), rlang::call2("(", x))
)
constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
results <- list()
failures <- list()
for (kind in c("character", "integer", "predicate", "specification")) {
  for (constructor in constructors) {
    if (kind == "specification" && constructor == "grouping_set") next
    for (form in names(forms)) for (occurrences in seq_len(4L)) {
      state <- new.env(parent = emptyenv())
      state$reads <- 0L
      state$expected <- character()
      caller <- rlang::env()
      makeActiveBinding("selection", function(value) {
        if (!missing(value)) stop("The caller binding must not be changed.")
        state$reads <- state$reads + 1L
        index <- (state$reads - 1L) %% 3L + 1L
        column <- c("a", "b", "c")[[index]]
        state$expected <- c(state$expected, column)
        switch(kind,
          character = column,
          integer = index,
          predicate = local({
            wanted <- c("character", "integer", "logical")[[index]]
            function(x) typeof(x) == wanted
          }),
          specification = grouping_set(tidyselect::all_of(!!column))
        )
      }, caller)
      expression <- rlang::call2(constructor, !!!rep(list(forms[[form]](quote(selection))), occurrences))
      spec <- eval(expression, caller)
      for (call_id in seq_len(5L)) {
        previous_reads <- state$reads
        state$expected <- character()
        actual <- tryCatch(suppressWarnings(inspect_grouping(data, .grouping = spec, .duplicates = "keep", .format = "list")), error = identity)
        reads <- state$reads - previous_reads
        expected_spec <- eval(rlang::call2(constructor, !!!lapply(state$expected, function(column) {
          if (kind == "specification") grouping_set(tidyselect::all_of(!!column)) else column
        })))
        expected <- inspect_grouping(data, .grouping = expected_spec, .duplicates = "keep", .format = "list")
        native_refusal <- kind == "predicate" && form %in% c("bare", "syntactic", "deep_syntactic")
        grammar_refusal <- kind == "specification" && form == "public_head"
        outcome <- if (native_refusal) "native-predicate-refusal" else if (grammar_refusal) "nested-spelling-refusal" else "plan"
        same <- if (native_refusal) {
          # A bare function-valued binding is not a tidyselect predicate call.
          native_env <- rlang::env(selection = function(x) is.character(x))
          native <- tryCatch(tidyselect::eval_select(rlang::new_quosure(forms[[form]](quote(selection)), native_env), data), error = identity)
          inherits(actual, "error") && inherits(native, "error") &&
            identical(class(actual), class(native)) &&
            identical(conditionMessage(actual), conditionMessage(native)) &&
            identical(reads, occurrences)
        } else if (grammar_refusal) {
          # A parenthesized qualified callee is an ordinary selection call;
          # only a written redundant pair admits a specification payload.
          inherits(actual, "marginplyr_error") &&
            grepl("is a Grouping specification", conditionMessage(actual), fixed = TRUE) &&
            identical(reads, 1L)
        } else !inherits(actual, "error") && identical(actual, expected) && identical(reads, occurrences)
        results[[length(results) + 1L]] <- data.frame(kind, constructor, form, occurrences, call_id, reads, outcome, same)
        if (!same) failures[[length(failures) + 1L]] <- list(record = results[[length(results)]], expected = expected, actual = if (inherits(actual, "error")) conditionMessage(actual) else actual)
      }
    }
  }
}
results <- do.call(rbind, results)
utils::write.csv(results, file.path(out, "retained-occurrences.csv"), row.names = FALSE)
dput(failures, file = file.path(out, "retained-occurrences-failures.R"))
cat("Retained occurrence comparisons:", nrow(results), "failures:", length(failures), "binding reads:", sum(results$reads), "\n")
print(table(results$kind, results$same))
if (length(failures)) stop("Retained occurrence comparison failed.")
