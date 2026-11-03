# Finite public-capture differential; no package implementation changes.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", unset = "."), quiet = TRUE)
options(lifecycle_verbosity = "quiet")
out_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", unset = tempfile("spec-frontier-"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
capture <- function(expr) tryCatch(list(ok = TRUE, value = expr), error = function(e) list(ok = FALSE, value = e))
family <- function(cols, kind) {
  if (kind %in% c("rollup_composite", "cube_composite")) return(list(cols, character()))
  if (kind == "rollup") return(lapply(rev(seq.int(0L, length(cols))), function(n) head(cols, n)))
  if (kind == "cube") return(unlist(lapply(rev(seq.int(0L,length(cols))), function(n) {
    if (n == 0L) return(list(character()))
    utils::combn(cols, n, simplify = FALSE)
  }), recursive = FALSE))
  list(cols)
}
expressions <- alist(
  all_of(c(...)), all_of(unlist(list(...))), all_of(..1), all_of(..2),
  all_of((function(...) c(...))(..1, ..2)),
  all_of((function(x = ..1) x)()), all_of((function(x = ..2) x)()),
  all_of((function(x = ..1, y = x) y)()),
  all_of((function(x = c(...)) x)()),
  all_of((function(...) c(...))()),
  all_of((function(x = character()) x)()),
  all_of((function(x = names(tidyselect::peek_data())) x)()),
  all_of(names(tidyselect::peek_data())[vapply(tidyselect::peek_data(), is.numeric, logical(1))]),
  all_of(setdiff(tidyselect::peek_vars(), "b")),
  all_of(sort(tidyselect::peek_vars())),
  all_of(rev(tidyselect::peek_vars())),
  all_of(tidyselect::peek_vars()[seq_along(tidyselect::peek_vars()) %% 2L == 1L]),
  all_of((function(x = tidyselect::peek_vars()) x[c(1L, length(x))])()),
  (all_of)(c("a", "b")), (tidyselect::all_of)(c("a", "b")),
  all_of(c("a", NA_character_)), all_of(c(1L, 2L)), all_of(c(TRUE, FALSE)),
  all_of(matrix(c("a", "b"), nrow = 1L)),
  all_of(structure(c("a", "b"), dim = c(1L, 2L))),
  any_of(c("absent", "a")), any_of(character()),
  c(a = a), c(z = a), c("a", "b"), c(1L, 3L), -c(1L, 3L),
  .data$a, .data[["a"]], .data[[..1]], (.data)$a,
  where(is.numeric), where(is.factor), where(is.list),
  c(where(is.numeric), starts_with("b")),
  { a }, identity("a"), identity(character()),
  quote(a), expression(a), list("a"), NULL, TRUE, NA_integer_,
  (function(x = "a") x)(),
  ((function(x = "a") x))(),
  (function(x = "a", y = x) y)(),
  c(, a), all_of(c(, "a")), all_of(...elt(1L)),
  all_of((function(...) ..1)("a")),
  all_of((function(...) ..2)("a", "b"))
)
data <- list(
  tibble::tibble(a = c(1L, 2L, 1L), b = c("x", "y", "x"), c = factor(c("q", "p", "q"))),
  tibble::tibble(a = integer(), b = character(), c = factor(levels = c("q", "p"))),
  tibble::tibble(a = list(1L, 2L, 1L), b = as.Date(c("2026-01-01", "2026-01-02", "2026-01-01")), c = c(TRUE, FALSE, TRUE))
)
rows <- list()
run <- function(expr, dat, position, ...) {
  env <- environment()
  q <- rlang::new_quosure(expr, env)
  oracle <- function(selection) tidyselect::eval_select(rlang::enquo(selection), dat)
  direct <- capture(rlang::eval_tidy(rlang::expr(oracle(!!q)), env = env))
  call <- if (position == ".by") rlang::expr(inspect_grouping(dat, .by = !!q, .format = "list")) else {
    spec <- if (grepl("_composite$", position)) {
      rlang::call2(sub("_composite$", "", position), rlang::call2("grouping_set", q))
    } else rlang::call2(position, q)
    rlang::expr(inspect_grouping(dat, .grouping = !!spec, .format = "list"))
  }
  actual <- capture(rlang::eval_tidy(call, env = env))
  status <- if (direct$ok) {
    selected <- names(dat)[unname(direct$value)]
    renames <- any(names(direct$value) != selected)
    if (renames) {
      if (!actual$ok && inherits(actual$value, "marginplyr_error")) "renaming-refusal" else "MISMATCH-renaming"
    } else if (!length(selected) && position %in% c("rollup", "cube", "rollup_composite", "cube_composite")) {
      if (!actual$ok && inherits(actual$value, "marginplyr_error")) "empty-unit-refusal" else "MISMATCH-empty"
    } else if (!actual$ok) "MISMATCH-unexpected-error" else {
      got <- if (position == ".by") actual$value$fixed else actual$value$included
      expected <- if (position == ".by") list(selected) else family(selected, position)
      if (identical(got, expected)) "match" else "MISMATCH-selected"
    }
  } else if (!actual$ok && identical(class(direct$value), class(actual$value))) "external-condition-match" else "MISMATCH-condition"
  list(status = status, direct = if (direct$ok) paste(names(direct$value), collapse = "|") else conditionMessage(direct$value),
       actual = if (actual$ok) paste(vapply(actual$value$included, paste, character(1), collapse = "|"), collapse = ";") else conditionMessage(actual$value))
}
for (di in seq_along(data)) for (ei in seq_along(expressions)) for (position in c(".by", constructors, "rollup_composite", "cube_composite")) {
  item <- run(expressions[[ei]], data[[di]], position, "a", "b")
  rows[[length(rows) + 1L]] <- data.frame(data = di, expression = ei, position = position, status = item$status, direct = item$direct, actual = item$actual)
}
result <- do.call(rbind, rows)
utils::write.csv(result, file.path(out_dir,"selection-frontier.csv"), row.names = FALSE)
print(table(result$status))
if (any(grepl("MISMATCH",result$status))) print(result[grepl("MISMATCH",result$status),])
print(sessionInfo())
stopifnot(!any(grepl("MISMATCH",result$status)))
