pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
options(lifecycle_verbosity = "warning")
capture <- function(expr) {
  w <- character()
  val <- withCallingHandlers(tryCatch(expr, error = identity), warning = function(cnd) {
    w <<- c(w, paste(class(cnd)[1L], conditionMessage(cnd), sep = ": "))
    invokeRestart("muffleWarning")
  })
  list(value = val, warnings = w)
}
dataset <- tibble::tibble(
  ax01 = c("a", "b"), ax02 = 1:2, bx01 = c(FALSE, TRUE),
  code = factor(c("Q", "R")), w = c(20, 30)
)
cases <- c(
  starts_with = 'starts_with(c("bx", "ax"))',
  ends_with = 'ends_with(c("02", "01"))',
  contains = 'contains(c("x02", "x01"))',
  matches = 'matches("^a|01$")',
  num_range = 'num_range("ax", 2:1, width = 2)',
  one_of = 'one_of("w", NULL, "ax01", "absent")',
  everything = 'everything()',
  last_col = 'last_col(1)',
  all_of = 'all_of(c("w", "ax02", "ax01"))',
  any_of = 'any_of(c("code", "absent", "ax01"))',
  where = 'where(~ is.character(.x) || is.logical(.x))'
)
edge <- c(
  name='ax01', range='ax02:code', range_reverse='code:ax02',
  pos='3:1', negative_pos='c(-2, -4)', external='cols',
  pronoun='c(.data$code, .data[["ax02"]])',
  env='all_of(.env$cols)', string='c("w", "ax01")',
  none='any_of("absent")', not_all='!everything()',
  minus_all='-everything()', zero='0L',
  rename='all_of(c(alias = "ax01"))',
  self='all_of(c(ax01 = "ax01"))',
  missing='all_of("absent")', bad_num='last_col(offset = 5)',
  vctrs_bad='one_of(1)', bad_range='ax01:absent',
  s3='matches(stringr::regex("^ax", ignore_case = TRUE))',
  peek_vars='all_of(rev(tidyselect::peek_vars())[1:2])',
  peek_data_names='all_of(names(tidyselect::peek_data())[1:2])',
  explicit_vars='starts_with("x", vars = c("b", "x", "c", "xz"))',
  bang_rhs='c(ax01, !ax02)', minus_rhs='c(ax01, -ax02)',
  bang_lhs='c(!ax02, ax01)', minus_lhs='c(-ax02, ax01)',
  mixed='c(-ax02, !code, -bx01, ax02)',
  namespace_data='c((tidyselect::everything)(), -dplyr::last_col())',
  minus_range='-(ax01:ax02)', bang_range='!(ax01:ax02)',
  double_minus='-(-ax01)', double_bang='!(!ax01)',
  rlang_inject='!!rlang::expr(-ax01)', rlang_splice='c(!!!rlang::exprs(ax01, -ax02))',
  conjunction_double='ax01 && ax02', disjunction_double='ax01 || ax02',
  arithmetic='ax01 + ax02', equals='ax01 == ax02',
  trailing_comma='c(ax01, )', leading_comma='c(, ax01)'
)
cols <- c("w", "ax02")
selection <- function(txt) {
  raw <- str2lang(txt)
  if (txt %in% edge[c("rlang_inject", "rlang_splice")]) raw <- eval(rlang::call2(rlang::expr, raw))
  raw
}
# Ordered cross product and both exclusion spellings under c().
templates <- c(
  'c(%s, %s)', '%s | %s', '%s & %s', '%s / %s',
  'c(%s, -(%s))', 'c(-(%s), %s)', 'c(%s, !(%s))', 'c(!(%s), %s)',
  '!(%s | %s)', '-(%s & %s)'
)
pair <- as.matrix(expand.grid(cases, cases, templates, stringsAsFactors = FALSE))
pairs <- apply(pair, 1L, function(row) sprintf(row[3], row[1], row[2]))
qualified <- unlist(lapply(c("tidyselect", "dplyr"), function(ns) {
  exported <- getNamespaceExports(ns)
  enabled <- names(cases) %in% exported
  paste0(ns, "::", cases[enabled])
}))
allcases <- unique(c(cases, qualified, paste0("-", cases), paste0("!", cases),
                     pairs, edge))
expected_select <- function(txt, proxy) {
  q <- rlang::new_quosure(selection(txt), globalenv())
  z <- capture(tidyselect::eval_select(q, proxy))
  if (!inherits(z$value, "error") && any(names(z$value) != names(tidyselect::tidyselect_data_proxy(proxy))[z$value])) {
    z$value <- simpleError("alias is refused by marginplyr contract")
    class(z$value) <- c("alias", class(z$value))
  }
  z
}
psql_data <- dbplyr::tbl_lazy(dataset, con = dbplyr::simulate_postgres())
backends <- list(
  local = list(input = dataset, oracle = dataset),
  postgres_sim = list(input = psql_data, oracle = psql_data)
)
makers <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec",
            "rollup_composite", "cube_composite", "by")
public_call <- function(input, maker, txt) {
  raw <- selection(txt)
  grouping <- if (maker == "by") {
    quote(NULL)
  } else if (maker %in% c("rollup_composite", "cube_composite")) {
    rlang::call2(sub("_composite", "", maker), rlang::call2("grouping_set", raw))
  } else {
    rlang::call2(maker, raw)
  }
  by <- if (maker == "by") raw else quote(NULL)
  rlang::expr(inspect_grouping(input, .by = !!by, .grouping = !!grouping,
                              .format = "list", .duplicates = "keep"))
}
expected_sets <- function(maker, cols) {
  if (maker == "by") return(list(character()))
  if (maker %in% c("rollup", "cube", "rollup_composite", "cube_composite") &&
      !length(cols)) return(NULL)
  if (maker %in% c("rollup_composite", "cube_composite")) return(list(cols, character()))
  if (maker == "rollup") {
    return(lapply(length(cols):0L, function(n) cols[seq_len(n)]))
  }
  if (maker == "cube") {
    return(unlist(lapply(length(cols):0L, function(n) {
      utils::combn(cols, n, simplify = FALSE)
    }), recursive = FALSE))
  }
  list(cols)
}
error_kind <- function(err) {
  if (inherits(err, "alias")) return("alias")
  if (is.null(err)) return("empty")
  message <- conditionMessage(err)
  if (grepl("rename|renaming", message, ignore.case = TRUE)) return("alias")
  if (grepl("at least one|composite.*empty|empty.*composite", message, ignore.case = TRUE)) {
    return("empty")
  }
  if (grepl("predicate", message, ignore.case = TRUE)) return("predicate")
  class(err)[1L]
}
run_inspection <- function(subset = NULL) {
  rows <- list()
  for (backend in c("local", "postgres_sim")) {
    input <- backends[[backend]]$input
    oracle <- backends[[backend]]$oracle
    selected_cases <- if (backend == "local") allcases else unique(c(cases, qualified, edge,
      paste0("-", cases), paste0("!", cases), pairs[seq(1L, length(pairs), 11L)]))
    if (!is.null(subset)) selected_cases <- head(selected_cases, subset)

    for (txt in selected_cases) {
      target <- expected_select(txt, oracle)
      ok <- !inherits(target$value, "error")
      cols <- if (ok) names(target$value) else character()
      if (is.null(cols)) cols <- character()
      for (maker in makers) {
        observed <- capture(eval(public_call(input, maker, txt)))
        obs_ok <- !inherits(observed$value, "error")
        sets <- if (ok) expected_sets(maker, cols) else NULL
        equal <- if (obs_ok) {
          !is.null(sets) && identical(observed$value$included, sets) &&
            (maker != "by" || identical(observed$value$fixed, list(cols)))
        } else {
          is.null(sets) &&
            error_kind(observed$value) == error_kind(if (ok) NULL else target$value)
        }
        warning_equal <- identical(observed$warnings, target$warnings)
        rows[[length(rows) + 1L]] <- data.frame(
          backend = backend, maker = maker, txt = txt, equal = equal, warnings = warning_equal,
          expected = if (is.null(sets)) error_kind(if (ok) NULL else target$value) else
            paste(paste0("(", vapply(sets, paste, "", collapse = ","), ")"), collapse = ","),
          observed = if (obs_ok) {
            paste(paste0("(", vapply(observed$value$included, paste, "", collapse = ","), ")"),
                  collapse = ",")
          } else {
            conditionMessage(observed$value)
          },
          oracle_warning = paste(target$warnings, collapse = "|"),
          actual_warning = paste(observed$warnings, collapse = "|")
        )
      }
    }
    current <- do.call(rbind, rows)
    current <- current[current$backend == backend, , drop = FALSE]
    mismatches <- current[!current$equal | !current$warnings, , drop = FALSE]
    cat(backend, length(selected_cases), "expressions,", nrow(current),
        "comparisons,", nrow(mismatches), "mismatches\n")
    if (nrow(mismatches)) print(mismatches, row.names = FALSE)
    stopifnot(nrow(mismatches) == 0L)
  }
}
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args)) run_inspection(as.integer(args[[1L]])) else run_inspection()
}
