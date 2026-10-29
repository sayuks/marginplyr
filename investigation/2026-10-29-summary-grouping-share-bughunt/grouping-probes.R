suppressPackageStartupMessages(pkgload::load_all(quiet = TRUE))
options(lifecycle_verbosity = "quiet", width = 120)
state <- new.env(parent = emptyenv())
state$checks <- 0L
state$signals <- list()
record <- function(label, test, detail = NULL) {
  state$checks <- state$checks + 1L
  if (!isTRUE(test)) state$signals[[label]] <- detail
}
observe <- function(expr) tryCatch(force(expr), error = identity)
key_class <- function(x) class(x)[[1L]]
selection_exprs <- list(
  quote(NULL), quote(integer()), quote(character()), quote(0L), quote(-0L),
  quote(c()), quote(c(1L, 1L)), quote(c(1L, 2L, 0L)), quote(-c(1L, 2L)),
  quote(c(1L, 3L) / 3L), quote(!(1L:2L)), quote(1L | 3L), quote(1L & 2L),
  quote(c(1L, 3L) / c(2L, 3L)), quote(c(NA_integer_, 1L)),
  quote(c(TRUE, FALSE)), quote(c(1L, -2L)), quote(99L), quote(-99L),
  quote(dplyr::everything()), quote(tidyselect::last_col(offset = 1L)),
  quote(tidyselect::all_of(names(tidyselect::peek_data()))),
  quote(tidyselect::all_of(tidyselect::peek_vars()[c(3L, 1L)])),
  quote(tidyselect::any_of(c("absent", "a"))),
  quote(tidyselect::one_of("absent")),
  quote(dplyr::where(is.numeric)), quote(dplyr::where(is.character)),
  quote(dplyr::where(~ is.factor(.x))), quote(tidyselect::matches("[a-z]")),
  quote(tidyselect::starts_with("a")), quote(.data$a), quote(.data[["a"]]),
  quote(.data[[names(tidyselect::peek_data())[[1L]]]]),
  quote(.data[[1L]]), quote((.data)$a),
  rlang::call2("c", 1L, rlang::missing_arg()),
  rlang::call2("c", rlang::missing_arg(), 1L),
  rlang::call2(":", 1L, rlang::missing_arg()),
  rlang::call2("-", rlang::missing_arg()),
  quote(1L && 2L), quote(1L || 2L), quote(1L * 2L),
  quote(1L ^ 2L), quote(~ 1L), quote(as.name("a")),
  quote(identity(c(1L, 3L))), quote(base::c(1L, 3L)), quote((c)(1L, 3L)),
  quote(c(a = 1L)), quote(tidyselect::all_of(c(a = "a"))),
  quote(tidyselect::all_of(c(area = "a")))
)
name_pool <- c(
  "a", "b", "c", "1", "-", ".", ".data", ".env", "NA", "NULL", "TRUE",
  "FALSE", "Inf", "NaN", "if", "..0", "..-1", "a,b", "a=b", "a`b",
  "a\nb", "a\rb", "a\tb", "日本語", "é", "é"
)
set.seed(68291L)
selection_checks <- 0L
for (layout in seq_len(70L)) {
  nms <- c("a", sample(setdiff(name_pool, "a"), 3L))
  data <- stats::setNames(data.frame(
    x = c("u", "v"), y = 1:2, z = factor(c("u", "v")), w = c(TRUE, FALSE)
  ), nms)
  for (j in seq_along(selection_exprs)) {
    q <- rlang::new_quosure(selection_exprs[[j]], environment())
    expected <- suppressWarnings(observe(tidyselect::eval_select(q, data)))
    for (role in c("by", "set")) {
      actual <- suppressWarnings(observe(if (role == "by") {
        rlang::inject(inspect_grouping(data, .by = !!q, .format = "list"))
      } else {
        rlang::inject(inspect_grouping(
          data, .grouping = grouping_set(!!q), .format = "list"
        ))
      }))
      label <- paste("selection", layout, j, role)
      if (inherits(expected, "error")) {
        equal <- inherits(actual, "error") &&
          identical(key_class(actual), key_class(expected))
        record(label, equal, list(
          expr = deparse(selection_exprs[[j]]), names = nms,
          expected = expected, actual = actual
        ))
      } else {
        source <- nms[unname(expected)]
        renamed <- !names(expected) == source
        if (any(renamed)) {
          record(label, inherits(actual, "marginplyr_error"),
                 list(expected = source, actual = actual))
        } else {
          field <- if (role == "by") "fixed" else "included"
          got <- if (inherits(actual, "error")) {
            actual
          } else {
            actual[[field]][[1L]]
          }
          record(label, identical(got, unname(source)), list(
            expr = deparse(selection_exprs[[j]]), names = nms,
            expected = source, actual = actual
          ))
        }
      }
      selection_checks <- selection_checks + 1L
    }
  }
}
cat("Selection boundary comparisons:", selection_checks, "\n")
cat("Raw signals:", length(state$signals), "\n")
for (label in names(state$signals)) {
  cat("\nSIGNAL", label, "\n")
  print(state$signals[[label]])
}
cat("\nIdentifier dimensional boundaries\n")
for (dims in c(0L, 1L, 2L, 30L, 31L, 32L, 33L)) {
  data <- if (dims == 0L) {
    data.frame(value = 1)
  } else {
    as.data.frame(stats::setNames(
      rep(list("a"), dims), paste0("g", seq_len(dims))
    ))
  }
  spec <- if (dims == 0L) {
    grouping_set()
  } else {
    rlang::inject(rollup(!!!rlang::syms(names(data))))
  }
  plan <- inspect_grouping(data, .grouping = spec, .format = "list")
  ordinary <- summarize_with_margins(
    data, n = dplyr::n(), .grouping = spec, .id = "set", .sort = "none"
  )
  record(paste("dimension occurrences", dims),
         identical(sort(unique(ordinary$set)), plan$set_id))
  if (dims <= 31L) {
    result <- summarize_with_margins(
      data, gid = grouping_id(), .grouping = spec, .id = "set", .sort = "none"
    )
    record(paste("dimension identifier", dims),
           identical(as.integer(result$gid[order(result$set)]),
                     plan$grouping_id))
    cat("dims", dims, "sets", nrow(plan),
        "max id", max(plan$grouping_id), "PASS\n")
  } else {
    result <- observe(summarize_with_margins(
      data, gid = grouping_id(), .grouping = spec
    ))
    record(paste("dimension cap", dims),
           all(is.na(plan$grouping_id)) &&
             inherits(result, "marginplyr_error"))
    cat("dims", dims, "sets", nrow(plan),
        "inspection NA and helper cap refusal",
        inherits(result, "marginplyr_error"), "\n")
  }
}
cat("\nTyped dtplyr inspection and caller-state probes\n")
source_data <- data.table::data.table(
  txt = c("u", "v"), int = 1:2, dbl = c(1.5, 2.5),
  fac = factor(c("u", "v"), levels = c("v", "u", "z")),
  day = as.Date(c("2026-01-01", "2026-01-02")), flag = c(TRUE, FALSE)
)
typed_predicates <- list(
  quote(is.character), quote(is.numeric), quote(is.integer),
  quote(is.factor), quote(is.logical), quote(~ inherits(.x, "Date"))
)
typed_checks <- 0L
for (empty in c(FALSE, TRUE)) {
  parent <- if (empty) source_data[0] else data.table::copy(source_data)
  saved <- data.table::copy(parent)
  inputs <- list(
    identity,
    function(x) dplyr::select(x, dplyr::everything()),
    function(x) dplyr::select(x, flag, day, fac, dbl, int, txt),
    function(x) dplyr::rename(x, text = txt),
    function(x) dplyr::filter(x, int > 1L),
    function(x) dplyr::mutate(x, more = int + dbl),
    function(x) dplyr::group_by(x, txt)
  )
  for (k in seq_along(inputs)) {
    lazy <- inputs[[k]](dtplyr::lazy_dt(parent, immutable = FALSE))
    control <- inputs[[k]](dtplyr::lazy_dt(
      data.table::copy(parent), immutable = TRUE
    ))
    local <- dplyr::collect(control)
    for (pred in typed_predicates) {
      q <- rlang::new_quosure(rlang::expr(dplyr::where(!!pred)), environment())
      for (role in c("by", "set")) {
        if (k == 7L && role == "by") next
        run <- function(data) {
          if (role == "by") {
            rlang::inject(inspect_grouping(data, .by = !!q, .format = "list"))
          } else {
            rlang::inject(inspect_grouping(
              data, .grouping = grouping_set(!!q), .format = "list"
            ))
          }
        }
        expected <- observe(run(local))
        actual <- observe(run(lazy))
        label <- paste("typed dtplyr", empty, k, deparse(pred), role)
        equal <- if (inherits(expected, "error")) {
          inherits(actual, "error") &&
            identical(key_class(actual), key_class(expected))
        } else {
          identical(actual, expected)
        }
        record(label, equal, list(expected = expected, actual = actual))
        record(paste(label, "source unchanged"), identical(parent, saved))
        typed_checks <- typed_checks + 1L
      }
    }
  }
}
cat("Typed dtplyr comparisons:", typed_checks, "\n")
cat("\nUnrecognized nested factories and valid top-level controls\n")
data <- data.frame(g = c("u", "v"), x = 1:2)
factory_calls <- list(
  quote((function(...) rollup(g))()),
  quote(get("rollup")(g)), quote(base::get("rollup")(g)),
  quote(rlang::eval_bare(quote(rollup(g)))),
  quote(identity(rollup(g)))
)
for (expr in factory_calls) {
  direct <- rlang::inject(inspect_grouping(data, .grouping = !!expr))
  expected <- inspect_grouping(data, .grouping = rollup(g))
  record(paste("factory top-level", deparse(expr)), identical(direct, expected))
  nested <- observe(rlang::inject(inspect_grouping(
    data, .grouping = grouping_sets(!!expr)
  )))
  record(paste("factory nested", deparse(expr)),
         inherits(nested, "marginplyr_error") &&
           grepl("nested position recognizes",
                 conditionMessage(nested), fixed = TRUE))
}
cat("Factory boundary assertions:", 2L * length(factory_calls), "\n")

for (label in names(state$signals)) {
  cat("\nFINAL SIGNAL", label, "\n")
  print(state$signals[[label]])
}
cat("\nFinal checks:", state$checks, "signals:", length(state$signals), "\n")
if (length(state$signals)) quit(status = 1L)
