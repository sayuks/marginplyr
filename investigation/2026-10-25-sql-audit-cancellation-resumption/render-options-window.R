# Run this diagnostic in a disposable Rscript process; it loads this checkout.
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
source(file.path(dirname(normalizePath(sub("^--file=", "", script_arg))),
                 "reproduction-setup.R"))
pkgload::load_all(hunt_checkout, quiet = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
remote <- dplyr::copy_to(con, data.frame(g="a", v=1), "source")
options(marginplyr.audit_sql=TRUE,
        rlib_warning_verbosity="default", lifecycle_verbosity="default")
ns <- asNamespace("marginplyr")
# In-memory transition fixture: inject a real R interrupt just after the
# diagnostic options store and before its restoration handler is registered.
# The package source files and backend operation remain unchanged.
trace("render_sent_query_sql", where=ns, at=3,
      tracer=quote(rlang::interrupt()), print=FALSE)
leaf <- tryCatch(
  summarize_with_margins(remote, total=sum(v), .grouping=rollup(g),
                         .check_margin_label=TRUE),
  interrupt=identity
)
untrace("render_sent_query_sql", where=ns)
actual <- c(getOption("rlib_warning_verbosity"), getOption("lifecycle_verbosity"))
rows <- last_sent_queries(FALSE, TRUE)
cat("caught interrupt:", inherits(leaf,"interrupt"), "\n")
cat("expected options: default,default\nactual options:", paste(actual, collapse=","), "\n")
print(rows)
warning_count <- function() {
  n <- 0L
  withCallingHandlers({
    rlang::warn("rlang audit marker", .frequency="once", .frequency_id=tempfile())
    lifecycle::deprecate_warn("0.0.1", "audit_marker()", id=tempfile(), always=TRUE)
  }, warning=function(cnd){n <<- n+1L;invokeRestart("muffleWarning")})
  n
}
leaked_warnings <- warning_count()
options(rlib_warning_verbosity="default", lifecycle_verbosity="default")
restored_warnings <- warning_count()
cat("warning markers with leak:", leaked_warnings,
    "after manual restoration:", restored_warnings,"\n")
saveRDS(list(options=actual, caught=class(leaf), rows=rows,
            warning_counts=c(leaked=leaked_warnings,restored=restored_warnings)),
        hunt_output('render-options-window.rds'))
DBI::dbDisconnect(con)
stopifnot(inherits(leaf, "interrupt"),
          identical(actual, c("quiet", "quiet")),
          identical(leaked_warnings, 0L), identical(restored_warnings, 2L))
quit(status=as.integer(!identical(actual,c("default","default"))), save="no")
