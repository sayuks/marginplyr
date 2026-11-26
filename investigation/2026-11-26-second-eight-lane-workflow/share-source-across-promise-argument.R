# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Inside a caller's own function, a share of an across() output written after another summary is refused while the argument the across() reads is an unforced promise, and for a literal argument the same line is refused once and accepted after R compiles it.
# The last block replays one candidate answer in memory; nothing in the checkout is edited.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
d <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 5), y = c(10, 20, 30))
run <- function(expr) tryCatch(
  paste("share", paste(round(expr$share, 3), collapse = " ")),
  error = function(e) paste0("<", class(e)[1], "> ", strsplit(conditionMessage(e), "\n")[[1]][1])
)
native <- function(data, measures) summarise(data, n = n(), across(all_of(measures), sum, .names = "{.col}_t"), .by = g)
later <- function(data, measures) summarize_with_margins(
  data, n = n(), across(all_of(measures), sum, .names = "{.col}_t"),
  share = share_of_total(x_t), .grouping = rollup(g))
first <- function(data, measures) summarize_with_margins(
  data, across(all_of(measures), sum, .names = "{.col}_t"), n = n(),
  share = share_of_total(x_t), .grouping = rollup(g))
forced <- function(data, measures) {
  force(measures)
  summarize_with_margins(
    data, n = n(), across(all_of(measures), sum, .names = "{.col}_t"),
    share = share_of_total(x_t), .grouping = rollup(g))
}
cols <- c("x", "y")
cat("native dplyr in the same kind of function, x_t at (g):", native(d, cols)$x_t, "; by hand the share is 0.375 0.625 1\n")
cat("across() after n = n(), argument a variable :", run(later(d, cols)), "\n")
cat("across() after n = n(), computed argument   :", run(later(d, setdiff(names(d), c("g")))), "\n")
cat("across() written first                      :", run(first(d, cols)), "\n")
cat("force(measures) before the call             :", run(forced(d, cols)), "\n")
cat("the same vector read from the global frame  :", run(summarize_with_margins(
  d, n = n(), across(all_of(cols), sum, .names = "{.col}_t"), share = share_of_total(x_t), .grouping = rollup(g))), "\n")
caller <- function() later(d, "x")
for (i in 1:3) cat("one source line, literal argument, call", i, ":", run(caller()), "\n")
reads <- 0L
read_cols <- function() {
  reads <<- reads + 1L
  c("x", "y")
}
pinned <- function() {
  delayedAssign("promise_cols", read_cols())
  run(summarize_with_margins(d, prior = sum(y), across(all_of(promise_cols), sum, .names = "sum_{.col}"),
                             share = share_of_total(sum_x), .grouping = rollup(g)))
}
cat("the delayed binding the tests pin           :", pinned(), "| reads =", reads, "\n")
ns <- asNamespace("marginplyr")
original <- get("known_context_constant", ns)
constant_promise <- function(expr, lookup) {
  if (rlang::is_symbol(expr) && !rlang::as_string(expr) %in% lookup$mask) {
    name <- rlang::as_string(expr)
    current <- lookup$env
    while (!identical(current, emptyenv())) {
      if (exists(name, envir = current, inherits = FALSE)) {
        if (!bindingIsActive(name, current) && rlang::env_binding_are_lazy(current, name)) {
          written <- eval(call("substitute", as.name(name)), current)
          if (is.null(written) || (is.atomic(written) && !is.object(written))) return(list(value = written))
          return(NULL)
        }
        break
      }
      current <- parent.env(current)
    }
  }
  original(expr, lookup)
}
environment(constant_promise) <- environment(original)
unlockBinding("known_context_constant", ns)
assign("known_context_constant", constant_promise, ns)
cat("--- with a promise whose expression is a constant read as that constant (in memory)\n")
caller2 <- function() later(d, "x")
for (i in 1:3) cat("one source line, literal argument, call", i, ":", run(caller2()), "\n")
cat("argument a variable                         :", run(later(d, cols)), "\n")
reads <- 0L
cat("the delayed binding the tests pin           :", pinned(), "| reads =", reads, "\n")
