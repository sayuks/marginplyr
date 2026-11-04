args <- commandArgs(trailingOnly = TRUE)
assert_desired <- "--assert" %in% args
source_dir <- Sys.getenv("MARGINPLYR_HUNT_SOURCE", ".")
output_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempfile("numbered-dots-"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(source_dir, quiet = TRUE)

capture <- function(expr) {
  tryCatch(list(ok = TRUE, value = expr),
           error = function(cnd) list(ok = FALSE, value = cnd))
}
same <- function(actual, expected) {
  if (!identical(actual$ok, expected$ok)) return(FALSE)
  if (actual$ok) return(identical(actual$value, expected$value))
  identical(class(actual$value), class(expected$value)) &&
    identical(conditionMessage(actual$value), conditionMessage(expected$value))
}
construct <- function(parent, child, env) {
  eval(rlang::call2(parent, child), env)
}
consume <- function(consumer, dat, specification) {
  if (consumer == "inspect_grouping") {
    return(inspect_grouping(dat, .grouping = specification, .format = "list"))
  }
  fn <- get(consumer, asNamespace("marginplyr"))
  if (consumer == "summarize_with_margins") {
    return(fn(dat, n = dplyr::n(), .grouping = specification,
              .margin_label = NULL, .id = "set"))
  }
  fn(dat, .grouping = specification, .margin_label = NULL, .id = "set")
}
make_wrapper <- function(consumer, parent, spelling, parens, env) {
  payload <- as.name(spelling)
  for (i in seq_len(parens)) payload <- rlang::call2("(", payload)
  specification <- rlang::call2(parent, payload)
  call <- if (consumer == "inspect_grouping") {
    rlang::call2(consumer, quote(dat), .grouping = specification,
                 .format = "list")
  } else if (consumer == "summarize_with_margins") {
    rlang::call2(consumer, quote(dat), n = quote(dplyr::n()),
                 .grouping = specification, .margin_label = NULL, .id = "set")
  } else {
    rlang::call2(consumer, quote(dat), .grouping = specification,
                 .margin_label = NULL, .id = "set")
  }
  rlang::new_function(alist(... = ), call, env = env)
}

consumers <- c("inspect_grouping", "summarize_with_margins",
               "expand_with_margins", "nest_with_margins",
               "nest_by_with_margins")
parents <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
records <- list()
for (shape in c("ordinary", "padded", "padded-collision")) {
  collision <- shape == "padded-collision"
  dat <- tibble::tibble(a = c("x", "y"), value = 1:2)
  if (collision) {
    dat[["..01"]] <- rep("wrong", 2L)
    dat[["..02"]] <- rep("other", 2L)
  }
  env <- rlang::env(globalenv(), dat = dat)
  child <- grouping_set(a)
  env$child <- child
  for (parent in parents) {
    reference <- construct(parent, quote(child), env)
    expected_plan <- capture(inspect_grouping(
      dat, .grouping = reference, .format = "list"
    ))
    if (parent == "grouping_set") {
      stopifnot(!expected_plan$ok,
                inherits(expected_plan$value, "marginplyr_error"))
    } else {
      included <- if (parent %in% c("rollup", "cube")) {
        list("a", character())
      } else list("a")
      stopifnot(identical(expected_plan$value$included, included))
    }
    for (consumer in consumers) {
      expected <- capture(consume(consumer, dat, reference))
      for (dot in 1:2) {
        spelling <- paste0("..", if (shape == "ordinary") "" else "0", dot)
        supplied <- c(rep(list(NULL), dot - 1L), list(child))
        for (parens in 0:2) {
          wrapper <- make_wrapper(consumer, parent, spelling, parens, env)
          actual <- capture(do.call(wrapper, supplied))
          matches <- same(actual, expected)
          records[[length(records) + 1L]] <- data.frame(
            shape = shape, collision = collision, parent = parent,
            consumer = consumer, spelling = spelling,
            dot = dot, parens = parens, matches = matches,
            ok = actual$ok,
            observation = if (actual$ok) {
              if (consumer == "inspect_grouping") {
                paste(vapply(actual$value$included, paste, collapse = ",",
                             character(1)), collapse = ";")
              } else paste(names(actual$value), collapse = ",")
            } else conditionMessage(actual$value),
            condition_class = if (actual$ok) "" else
              paste(class(actual$value), collapse = "/")
          )
        }
      }
    }
  }
}
results <- do.call(rbind, records)
write.csv(results, file.path(output_dir, "numbered-dots.csv"), row.names = FALSE)
print(with(results, table(parens, collision, matches)))
cat("Total:", nrow(results), "Matches:", sum(results$matches),
    "Mismatches:", sum(!results$matches), "\n")
cat("Parenthesized erroneous successes:",
    sum(results$parens > 0L & results$ok & !results$matches), "\n")
cat("Parenthesized erroneous refusals:",
    sum(results$parens > 0L & !results$ok & !results$matches), "\n")
stopifnot(nrow(results) == 450L,
          all(results$matches[results$parens == 0L]),
          !any(results$matches[results$parens > 0L]))
print(sessionInfo())
if (assert_desired) stopifnot(all(results$matches))
