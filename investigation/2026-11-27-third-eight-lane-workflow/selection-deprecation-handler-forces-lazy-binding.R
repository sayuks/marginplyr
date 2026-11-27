# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# The selection deprecation handler forces a wrapper's lazy argument that tidyselect never evaluates (#1803).
# Candidate 3: the warning handler that withholds tidyselect's external-vector
# deprecation (#1703) reads every name of the selection with rlang::env_get(),
# which forces a wrapper's lazy argument that tidyselect never evaluated
# because the name is a column. A failing argument then fails the call.
# Each part runs in a fresh process, so lifecycle's once-per-session
# deprecation state starts empty in each.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
Sys.setenv(LANGUAGE = "en")
args <- commandArgs(TRUE)
if (length(args) == 0L) {
  self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  for (part in c("native", "marginplyr", "count", "sqlite", "verbosity")) {
    system2(file.path(R.home("bin"), "Rscript"), c(shQuote(self), part))
  }
  quit(status = 0)
}
part <- args[[1]]
sales <- data.frame(region = c("E", "E", "W"), store = c("A", "B", "C"), v = 1:3)
sp <- "region"  # an external character vector written bare (deprecated, still selects)
quietly <- function(expr) withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
show <- function(label, expr) {
  out <- tryCatch({ x <- quietly(force(expr)); paste("RETURNED", paste(format(x), collapse = " ")) },
                  error = function(e) paste0("ERROR <", class(e)[1], "> ", gsub("\n", " | ", conditionMessage(e))))
  cat(sprintf("  %-46s %s\n", label, out))
}
forced <- 0
counting <- function() { forced <<- forced + 1; "unused" }
cat("== part:", part, "\n")
if (part == "native") {
  # Every native call below signals the deprecation, as marginplyr's first does.
  options(lifecycle_verbosity = "warning")
  # The same selection, in the same kind of wrapper, through tidyselect natively.
  sel <- function(data, store) names(select(data, c(sp, store)))
  grp <- function(data, store) group_vars(group_by(data, across(c(sp, store))))
  show("select(), argument never needed:", sel(sales, stop("never needed")))
  show("group_by(across()), argument never needed:", grp(sales, stop("never needed")))
  forced <- 0; show("select(), counting argument:", sel(sales, counting()))
  cat("  times the wrapper's `store` argument was forced:", forced, "\n")
}
mp <- function(data, store) {
  inspect_grouping(data, .grouping = grouping_sets(c(sp, store)))$included
}
mps <- function(data, store) {
  summarize_with_margins(data, n = n(), .grouping = rollup(c(sp, store)))
}
if (part == "marginplyr") {
  cat("  expected: (region, store), the argument not forced, as natively\n")
  show("inspect_grouping(), argument never needed:", mp(sales, stop("never needed")))
}
if (part == "count") {
  forced <- 0; show("inspect_grouping(), counting argument:", mp(sales, counting()))
  cat("  times the wrapper's `store` argument was forced (native: 0):", forced, "\n")
}
if (part == "sqlite") {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  t <- copy_to(con, sales, "sales")
  show("SQLite summarize_with_margins(rollup()):", collect(mps(t, stop("never needed"))))
  show("native SQLite group_by(across()):",
       collect(count((function(data, store) group_by(data, across(c(sp, store))))(t, stop("never needed")))))
}
if (part == "verbosity") {
  # lifecycle signals the deprecation once per session by default; with
  # verbosity "warning" (as testthat sets) every call reaches the handler.
  show("first call, default verbosity:", mp(sales, stop("never needed")))
  show("second call, default verbosity:", mp(sales, stop("never needed")))
  options(lifecycle_verbosity = "warning")
  show("third call, lifecycle_verbosity = 'warning':", mp(sales, stop("never needed")))
  options(lifecycle_verbosity = "quiet")
  show("fourth call, lifecycle_verbosity = 'quiet':", mp(sales, stop("never needed")))
}
