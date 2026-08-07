# Summary dots whose output names the static predictor cannot see.
#
# The adapters re-check the names a summary really produced, and every test of
# those checks depends on the pre-execution check in `execute_margin_summary()`
# letting the call through first. When the predictor can name the output, that
# earlier check raises the same condition and the test passes without the
# adapter guard ever running -- green, and proving something else.
#
# What produces the case the guards exist for is `.fns` bound to a variable:
# the function-name component then lives in the value rather than in the
# expression, so `known_across_function_names()` cannot read it and falls back
# to a positional placeholder. That shape is load-bearing rather than
# incidental, and it used to be spelled out in each test beside a comment
# saying so -- where an edit that inlined `.fns` back into a literal `list()`
# would have left every test passing and none of them still reaching a guard
# (#126).
#
# The assertion is what makes the precondition structural instead of
# remembered. It asks the predictor directly and refuses to hand back dots the
# predictor could see through, so a future predictor that closed this gap fails
# here, naming this helper, rather than quietly retargeting its callers at the
# pre-execution check.
#
# Splice the result into a Margin verb with `rlang::inject()`, as the suite's
# other generated-argument tests do.
#
# `.names` takes the two shapes the callers need and no others, because the
# expected output names are derived from it rather than resolved through glue:
# `"{.fn}"` names the output for the function alone, and `NULL` leaves
# `across()`'s own `{.col}_{.fn}`.
#
# `predict` is a parameter for the same reason `backend_available()` takes
# `known`: this helper's own tests need to drive both outcomes, and the shape
# that reaches the failing one is the shape this helper exists to avoid
# building. Every other call site takes the default.
unpredictable_summary_dots <- function(fn,
                                       cols = "value",
                                       .names = "{.fn}",
                                       predict = known_summary_output_names) {
  stopifnot(
    is.character(fn),
    length(fn) == 1L,
    is.character(cols),
    length(cols) >= 1L,
    is.null(.names) || identical(.names, "{.fn}")
  )

  # `fns` is a symbol the quosure resolves from the environment attached
  # below, which codetools cannot follow.
  # nolint start: object_usage_linter.
  expr <- if (is.null(.names)) {
    rlang::expr(dplyr::across(dplyr::all_of(cols), fns))
  } else {
    rlang::expr(dplyr::across(dplyr::all_of(cols), fns, .names = !!.names))
  }
  # nolint end
  dots <- list(rlang::new_quosure(
    expr,
    env = rlang::env(
      fns = stats::setNames(list(sum), fn),
      cols = cols
    )
  ))

  outputs <- if (is.null(.names)) paste0(cols, "_", fn) else fn
  # `.cols` resolves to `cols` and nothing else, so a proxy carrying just those
  # columns answers the predictor exactly as the real selection proxy would.
  proxy <- as.data.frame(stats::setNames(
    rep(list(double()), length(cols)),
    cols
  ))
  seen <- intersect(outputs, predict(dots, proxy))
  if (length(seen) > 0L) {
    stop(sprintf(
      paste0(
        "`known_summary_output_names()` predicts %s, so a call built from ",
        "these dots is rejected before any adapter runs and cannot reach an ",
        "adapter's own result-name check. Find a summary shape the predictor ",
        "still cannot name, or retire the callers that need one."
      ),
      paste0("`", seen, "`", collapse = ", ")
    ))
  }

  dots
}
