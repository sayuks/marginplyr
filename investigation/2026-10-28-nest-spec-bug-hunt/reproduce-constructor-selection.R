pkgload::load_all(quiet = TRUE)
input <- tibble::tibble(a = c("x", "y"), b = c(1L, 1L), row = 1:2)
constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
verbs <- list(nest_with_margins = marginplyr::nest_with_margins,
              nest_by_with_margins = marginplyr::nest_by_with_margins)
forms <- list(
 plain = identity,
 whole = function(expr) rlang::call2("(", rlang::call2("(", expr)),
 head = function(expr) rlang::call2(rlang::call2("(", expr[[1L]]))
)
observe <- function(callee, form, mode, verb, alias = FALSE) {
 state <- rlang::env(reads = 0L, mode = mode)
 fn <- eval(quote(function() {
   reads <<- reads + 1L
   if (mode == "warning") warning("caller selection warning", call. = FALSE)
   if (mode == "error" && reads > 1L) stop("unexpected second evaluation")
   if (mode == "alternating" && reads > 1L) "b" else "a"
 }), state)
 rlang::env_bind(state, ordinary = fn)
 assign(callee, fn, state)
 expr <- form(rlang::call2(if (alias) "ordinary" else callee))
 spec <- eval(rlang::call2("grouping_sets", expr, .ns = "marginplyr"), state)
 messages <- character()
 result <- tryCatch(withCallingHandlers(
   verb(input, .grouping = spec, .margin_label = NULL),
   warning = function(cnd) {
     messages <<- c(messages, conditionMessage(cnd))
     invokeRestart("muffleWarning")
   }), error = identity)
 # Independent source-membership oracle: selecting a yields x -> row 1,
 # y -> row 2. No plan compiled by marginplyr is used to derive this answer.
 correct <- !inherits(result, "error") &&
   identical(names(result), c("a", "data")) &&
   identical(result$a, c("x", "y")) &&
   identical(lapply(result$data, function(cell) cell$row), list(1L, 2L))
 list(correct = correct, reads = state$reads,
      warnings = length(messages), error = inherits(result, "error"))
}
failures <- 0L
controls <- 0L
checks <- 0L
for (callee in constructors) for (form_name in names(forms)) {
 for (mode in c("alternating", "warning", "error")) for (verb_name in names(verbs)) {
   actual <- observe(callee, forms[[form_name]], mode, verbs[[verb_name]])
   control <- observe(callee, forms[[form_name]], mode, verbs[[verb_name]], TRUE)
   expected_warnings <- as.integer(mode == "warning")
   control_ok <- control$correct && control$reads == 1L &&
     control$warnings == expected_warnings && !control$error
   stopifnot(control_ok)
   controls <- controls + 1L
   ok <- actual$correct && actual$reads == 1L &&
     actual$warnings == expected_warnings && !actual$error
   checks <- checks + 1L
   if (!ok) {
     failures <- failures + 1L
     cat(callee, form_name, mode, verb_name,
         "correct=", actual$correct, "reads=", actual$reads,
         "warnings=", actual$warnings, "error=", actual$error, "\n")
   }
 }
}
cat("checks", checks, "failures", failures, "alias controls passed", controls, "\n")
quit(status = as.integer(failures > 0L))
