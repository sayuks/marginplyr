script_path <- sub("^--file=", "",
  grep("^--file=", commandArgs(), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script_path)), "support.R"))
args <- commandArgs(TRUE)
if (length(args) != 1L) stop("Supply a fresh output log path explicitly.")
out <- capture_output(args[[1L]])
sink(out, split = TRUE)
load_checkout()

verbs <- list(summarize_with_margins, summarise_with_margins,
              expand_with_margins, nest_with_margins,
              nest_by_with_margins, inspect_grouping)
remote <- dbplyr::lazy_frame(g = "a", v = 1, .name = "lifecycle_outer")
child <- dbplyr::lazy_frame(g = "b", v = 2, .name = "lifecycle_child")
local_input <- data.frame(g = "a", v = 1)
peek <- function(audited) {
  if (audited) return(last_sent_queries(details = TRUE))
  cnd <- tryCatch(last_sent_queries(details = TRUE), error = identity)
  stopifnot(inherits(cnd, "marginplyr_error"),
            grepl("marginplyr.audit_sql", conditionMessage(cnd), fixed = TRUE))
  "inactive"
}
count <- 0L
for (verb in verbs) {
  for (parent_audited in c(FALSE, TRUE)) {
    for (sql_input in c(FALSE, TRUE)) {
      if (sql_input && (identical(verb, nest_with_margins) ||
                        identical(verb, nest_by_with_margins))) next
      for (mode in c("return", "child_error", "child_restart", "outer_restart")) {
        options(marginplyr.audit_sql = parent_audited)
        callback_count <- 0L
        factory <- function() {
          callback_count <<- callback_count + 1L
          before <- peek(parent_audited)
          options(marginplyr.audit_sql = !parent_audited)
          child_factory <- function() {
            initial <- peek(!parent_audited)
            on.exit({
              cleanup_before <- peek(!parent_audited)
              options(marginplyr.audit_sql = parent_audited)
              inspect_grouping(local_input, .grouping = rollup(g))
              stopifnot(identical(peek(!parent_audited), cleanup_before))
            }, add = TRUE)
            inspect_grouping(local_input, .grouping = rollup(g))
            stopifnot(identical(peek(!parent_audited), initial))
            if (mode == "child_error") stop("controlled child error")
            if (mode == "child_restart") invokeRestart("leave_child")
            rollup(g)
          }
          child_value <- tryCatch(withRestarts(
            summarize_with_margins(child, value = sum(v, na.rm = TRUE),
                                   .grouping = child_factory()),
            leave_child = function() "child restart"
          ), error = identity)
          if (mode == "child_error") {
            stopifnot(inherits(child_value, "error"),
                      grepl("controlled child error", conditionMessage(child_value)))
          } else if (mode == "child_restart") {
            stopifnot(identical(child_value, "child restart"))
          }
          stopifnot(identical(peek(parent_audited), before))
          if (mode == "outer_restart") invokeRestart("leave_outer")
          rollup(g)
        }
        result <- withRestarts(
          verb(if (sql_input) remote else local_input, .grouping = factory()),
          leave_outer = function() "outer restart"
        )
        stopifnot(callback_count > 0L)
        rows <- peek(parent_audited)
        if (parent_audited) {
          stopifnot(!any(grepl("lifecycle_child", rows$sql, fixed = TRUE)))
          if (sql_input && !identical(verb, inspect_grouping) &&
              mode != "outer_restart") {
            stopifnot(identical(rows$purpose, "result"),
                      identical(rows$status, "prepared"))
          } else stopifnot(nrow(rows) == 0L)
        }
        options(marginplyr.audit_sql = TRUE)
        fresh <- summarize_with_margins(remote, fresh = sum(v, na.rm = TRUE))
        stopifnot(identical(last_sent_queries()$sql,
                            as.character(dbplyr::sql_render(fresh))))
        count <- count + 1L
      }
    }
  }
}
stopifnot(count == 80L)
cat("PASS", count, "public lifetime cases\n")

sink()
