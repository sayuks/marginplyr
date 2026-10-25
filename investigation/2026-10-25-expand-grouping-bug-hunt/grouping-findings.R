# Evidence gathered on 2026-10-25. Run from the repository root:
# Rscript investigation/2026-10-25-expand-grouping-bug-hunt/grouping-findings.R
# Add --assert to fail unless every desired public-contract check passes.
args <- commandArgs(trailingOnly = TRUE)

repo <- normalizePath(".", mustWork = TRUE)

pkgload::load_all(repo, quiet = TRUE)

capture_result <- function(expr) {
    tryCatch(expr, error = identity)
}

checks <- logical()

record <- function(name, passed) {
    checks[[name]] <<- isTRUE(passed)
}

data <- tibble::tibble(label = c("a", "b"), amount = c(1, 1))

selection <- rlang::quo(tidyselect::all_of(names(Filter(is.numeric, tidyselect::peek_data()))))

ordinary_columns <- tidyselect::eval_select(selection, data)

record("control_tidyselect_selected_amount", identical(names(ordinary_columns), "amount"))

fixed_plan <- inspect_grouping(data, .by = !!selection, .format = "list")

record("typed_leaf_fixed_only_amount", identical(fixed_plan$fixed, list("amount")))

margin_summary <- summarize_with_margins(data, n = dplyr::n(), .by = !!selection)

ordinary_summary <- dplyr::summarise(data, n = dplyr::n(), .by = !!selection)

record("typed_leaf_summary_one_group", identical(margin_summary, ordinary_summary))

character_plan <- capture_result(inspect_grouping(data, .grouping = rollup(tidyselect::all_of(names(Filter(is.character,
    tidyselect::peek_data())))), .format = "list"))

record("typed_leaf_character_rollup_was_accepted", !inherits(character_plan, "error") &&
    identical(character_plan$included, list("label", character())))

optional_guard <- new.env(parent = baseenv())

sys.source(file.path(repo, "inst", "suggests", "guard.R"), envir = optional_guard)

suggests <- read.dcf(file.path(repo, "DESCRIPTION"))[[1L, "Suggests"]]

if (optional_guard$marginplyr_suggest_available("dtplyr", suggests = suggests)) {
    dt_plan <- inspect_grouping(dtplyr::lazy_dt(data), .grouping = grouping_set(!!selection),
        .format = "list")
    record("typed_leaf_dtplyr_only_amount", identical(dt_plan$included, list("amount")))
} else {
    cat("Skipped the additional dtplyr observation: compatible dtplyr was unavailable.\n")
}

quoted_result <- capture_result(summarize_with_margins(data, quoted_bit = deparse1(base::bquote(grouping_bit(label))),
    quoted_id = deparse1(rlang::expr(grouping_id(label))), evaluated_bit = deparse1(base::bquote(f(.(grouping_bit(label))))),
    quote_control = deparse1(base::quote(grouping_bit(label))), .grouping = rollup(label)))

quoted_ok <- !inherits(quoted_result, "error")

record("capture_bquote_preserved_language", quoted_ok && identical(unique(quoted_result$quoted_bit),
    "grouping_bit(label)"))

record("capture_rlang_expr_preserved_language", quoted_ok && identical(unique(quoted_result$quoted_id),
    "grouping_id(label)"))

record("control_quote_preserved_language", quoted_ok && identical(unique(quoted_result$quote_control),
    "grouping_bit(label)"))

record("control_bquote_dot_evaluated_helper", quoted_ok && identical(quoted_result$evaluated_bit,
    c("f(0L)", "f(0L)", "f(1L)")))

cat("Observed fixed keys:\n")

dput(fixed_plan$fixed)

cat("Observed Margin summary:\n")

print(margin_summary)

cat("Ordinary dplyr control:\n")

print(ordinary_summary)

cat("Observed character-rollup result:\n")

if (inherits(character_plan, "error")) {
    print(conditionMessage(character_plan))
} else {
    print(character_plan)
}

cat("Observed captured-language summaries:\n")

if (quoted_ok) {
    print(quoted_result)
} else {
    print(conditionMessage(quoted_result))
}

cat("Desired public-contract checks (FALSE exposed a finding):\n")

print(data.frame(check = names(checks), passed = unname(checks)), row.names = FALSE)

cat("R:", as.character(getRversion()), "\n")

for (package in c("marginplyr", "dplyr", "tidyselect", "rlang")) {
    cat(package, ": ", as.character(utils::packageVersion(package)), "\n", sep = "")
}

if ("--assert" %in% args) {
    stopifnot(all(checks))
}
