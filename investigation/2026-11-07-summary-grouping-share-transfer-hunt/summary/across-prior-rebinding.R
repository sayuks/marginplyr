args <- commandArgs(trailingOnly = TRUE)
root <- args[!startsWith(args, "--")][1L]
mode <- if ("--assert" %in% args) "contract" else if (
  "--controls-only" %in% args
) "controls" else "observation"
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(root, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))

capture <- function(expr) tryCatch(force(expr), error = identity)
summary_call <- function(input, margin, role, entrance, direction,
                         control = FALSE, actual_collision = FALSE) {
  final <- if (direction == "forward") base::list(s = sum, m = max) else sum
  fns <- if (direction == "forward") sum else base::list(s = sum, m = max)
  list <- base::list
  base_list <- base::list
  `::` <- base::`::`
  unpack <- FALSE
  frame_fn <- function(x) tibble::tibble(z = sum(x))
  if (entrance == "binding") {
    preceding <- quote({fns <<- final; 0L})
    expression <- quote(across(x, fns, .names = "answer_{.fn}"))
    stale <- if (direction == "forward") "answer_1" else "nonexistent"
  } else if (entrance == "list-call") {
    preceding <- quote({list <<- function(...) base_list(s = sum, m = max); 0L})
    expression <- quote(across(x, list(fake = sum), .names = "answer_{.fn}"))
    stale <- "answer_fake"
  } else if (entrance == "unpack") {
    preceding <- quote({unpack <<- TRUE; 0L})
    expression <- quote(across(x, frame_fn, .names = "answer", .unpack = unpack))
    stale <- "answer"
  } else {
    preceding <- quote({
      `::` <<- function(pkg, name) {
        if (identical(substitute(pkg), quote(base)) &&
            identical(substitute(name), quote(sum))) {
          return(base_list(s = sum, m = max))
        }
        getExportedValue(as.character(substitute(pkg)),
                         as.character(substitute(name)))
      }
      0L
    })
    expression <- quote(across(x, base::sum, .names = "answer_{.fn}"))
    stale <- "answer_1"
  }
  if (control) expression <- if (entrance == "unpack") {
    quote(across(x, frame_fn, .names = "answer", .unpack = TRUE))
  } else if (direction == "reverse") {
    quote(across(x, sum, .names = "answer_{.fn}"))
  } else quote(across(x, base::list(s = sum, m = max),
                      .names = "answer_{.fn}"))
  key <- if (actual_collision) {
    if (entrance == "unpack") "answer_z" else "answer_s"
  } else stale
  if (role != "identifier") {
    names(input)[names(input) == "key"] <- key
  } else input <- select(input, -key)
  dots <- rlang::quos(ready = !!preceding, !!expression)
  if (!margin) return(if (role == "identifier") {
    summarise(input, !!!dots)
  } else summarise(input, !!!dots, .by = all_of(key)))
  if (role == "identifier") {
    summarize_with_margins(input, !!!dots, .id = key)
  } else if (role == "fixed") {
    summarize_with_margins(input, !!!dots, .by = all_of(key))
  } else {
    summarize_with_margins(input, !!!dots,
                           .grouping = grouping_set(all_of(key)))
  }
}

classes <- base::list(
  tibble = tibble(key = c("a", "a", "b"), x = c(1, 3, 2)),
  data.frame = data.frame(key = c("a", "a", "b"), x = c(1, 3, 2)),
  data.table = data.table::as.data.table(
    data.frame(key = c("a", "a", "b"), x = c(1, 3, 2))
  )
)
records <- base::list()
for (class_name in names(classes)) for (role in c("identifier", "fixed", "variable")) {
  for (entrance in c("binding", "list-call", "namespace", "unpack")) {
    input <- classes[[class_name]]
    before <- serialize(input, NULL)
    expected <- summary_call(input, FALSE, role, entrance, "forward")
    actual <- capture(summary_call(input, TRUE, role, entrance, "forward"))
    literal <- summary_call(input, TRUE, role, entrance, "forward", control = TRUE)
    ordinary_cols <- names(expected)
    stopifnot(identical(select(literal, all_of(ordinary_cols)), expected),
              identical(serialize(input, NULL), before))
    false_refusal <- inherits(actual, "marginplyr_error") &&
      grepl("conflicts with a summary output|overwrite grouping column",
            conditionMessage(actual))
    if (mode == "observation" && !false_refusal) {
      print(base::list(class = class_name, role = role, entrance = entrance,
                       actual = actual))
    }
    records[[length(records)+1L]] <- data.frame(
      class=class_name,role=role,entrance=entrance,
      observed=if(inherits(actual,"error"))conditionMessage(actual) else "accepted",
      false_refusal=false_refusal, literal_matches=TRUE
    )
    if (mode == "contract") stopifnot(!inherits(actual,"error"),
      identical(select(actual, all_of(ordinary_cols)), expected))
    if (mode == "observation") stopifnot(false_refusal)
    # A real collision after rebinding must remain refused.
    collision <- capture(summary_call(input, TRUE, role, entrance, "forward",
                                     actual_collision = TRUE))
    stopifnot(inherits(collision, "marginplyr_error"))
  }
  # An initially unknown named list changing to a single function is retained.
  expected <- summary_call(classes[[class_name]], FALSE, role, "binding", "reverse")
  actual <- summary_call(classes[[class_name]], TRUE, role, "binding", "reverse")
  stopifnot(identical(select(actual, all_of(names(expected))), expected))
}
results <- do.call(rbind, records)
print(results, row.names=FALSE)
cat("36 forward observations, 36 literal controls, 36 genuine-collision controls, 9 reverse controls\n")
cat("mode:",mode,"\n")
if (nzchar(Sys.getenv("BUGHUNT_OUTPUT", unset = ""))) {
  write.csv(results, Sys.getenv("BUGHUNT_OUTPUT"), row.names = FALSE)
}
