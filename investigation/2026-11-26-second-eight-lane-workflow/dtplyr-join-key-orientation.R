# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# dtplyr_join_set_types_match() reads dtplyr's `on` in the orientation only a full join stores, so a join on differently named keys is refused and one whose tables carry both key names crosswise is admitted on the wrong pair.
# The last block replays an oriented comparison in memory; nothing in the checkout is edited.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({library(dplyr); library(dtplyr)})
styles <- c("inner", "left", "right", "full", "semi", "anti")
join <- function(style, x, y) get(paste0(style, "_join"))(lazy_dt(x), lazy_dt(y), by = c("a" = "b"))
plan <- function(input) tryCatch(
  paste(inspect_grouping(input, .grouping = rollup(where(is.integer)))$included, collapse = " "),
  marginplyr_error = function(e) "REFUSED"
)
x <- data.table::data.table(a = c(1L, 2L, 3L), b = c("p", "q", "r"), k = c(7L, 8L, 9L))
named <- data.table::data.table(b = c(1L, 2L, 9L), w = c("u", "v", "t"))
cross <- data.table::data.table(b = c(1, 2, 9), a = c("u", "v", "t"))
report <- function() {
  cat("differently named integer keys, by = c(a = b): the step's typed plan, and the collected frame's\n")
  for (style in styles) {
    step <- join(style, x, named)
    cat(sprintf("  %-5s step: %-22s collected: %s\n", style, plan(step), plan(as_tibble(collect(step)))))
  }
  cat("both tables carry both key names; the real key pair is integer against double\n")
  for (style in styles) {
    step <- join(style, x, cross)
    real <- as_tibble(collect(step))
    cat(sprintf("  %-5s step: %-22s collected: %-22s collected a: %s\n", style, plan(step), plan(real), class(real$a)))
  }
}
cat("what dtplyr stores for by = c(a = b):\n")
for (style in styles) {
  step <- join(style, x, named)
  while (!inherits(step, "dtplyr_step_join")) step <- step$parent
  cat(sprintf("  %-5s parent vars: %-8s parent2 vars: %-8s on$x: %s on$y: %s\n", style,
              paste(step$parent$vars, collapse = ","), paste(step$parent2$vars, collapse = ","), step$on$x, step$on$y))
}
report()
cat("native control, the same-name key the tests pin (integer against double, full join): ",
    plan(full_join(lazy_dt(data.table::data.table(g = 1:2)), lazy_dt(data.table::data.table(g = c(1, 2.5))), by = "g")), "\n")
ns <- asNamespace("marginplyr")
original <- get("dtplyr_join_set_types_match", ns)
oriented <- function(step, env = step[["env"]]) {
  if (inherits(step, "dtplyr_step_join") && !identical(step$style, "full")) {
    step$on <- list(x = step$on$y, y = step$on$x)
  }
  original(step, env)
}
environment(oriented) <- environment(original)
unlockBinding("dtplyr_join_set_types_match", ns)
assign("dtplyr_join_set_types_match", oriented, ns)
cat("--- with the key pair read in dtplyr's stored orientation (in memory)\n")
report()
