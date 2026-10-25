script_path <- sub("^--file=", "",
  grep("^--file=", commandArgs(), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script_path)), "support.R"))
args <- commandArgs(TRUE)
if (length(args) != 1L) stop("Supply a fresh output log path explicitly.")
out <- capture_output(args[[1L]])
sink(out, split = TRUE)
load_checkout()

main <- function() {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "storage_source")
  run_check <- function() {
    summarize_with_margins(remote, total = sum(v, na.rm = TRUE),
                           .grouping = rollup(g), .check_margin_label = TRUE)
  }
  create <- function(mode) {
    class_holder <- new.env(parent = emptyenv())
    message_holder <- new.env(parent = emptyenv())
    call_holder <- new.env(parent = emptyenv())
    formatted_holder <- new.env(parent = emptyenv())
    extra_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(extra_con), add = TRUE)
    holders <- list(class_holder, message_holder, call_holder, formatted_holder)
    refs <- lapply(c(holders, list(extra_con@ptr)), rlang::new_weakref)
    class_holder$con <- extra_con
    message_holder$con <- extra_con
    call_holder$con <- extra_con
    formatted_holder$con <- extra_con
    classes <- structure(c("storage_extra_warning", "warning", "condition"),
                         holder = class_holder)
    text <- structure("stored fallback", holder = message_holder,
                      names = "named diagnostic")
    if (mode == "empty_fallback") text <- character()
    leaf <- structure(list(message = text, call = structure(quote(source()),
                     holder = call_holder), extra = message_holder),
                      class = classes, another_holder = call_holder)
    calls <- 0L
    method <- function(cnd) {
      calls <<- calls + 1L
      if (calls == 1L) return("driver warning")
      switch(mode,
        attributed = structure("formatted", holder = formatted_holder,
                               class = "diagnostic_text"),
        zero_length = character(),
        vector = c("one", "two"),
        integer = 1L,
        missing = structure(NA_character_, holder = formatted_holder),
        empty_fallback = stop("formatter failed"),
        error = stop("formatter failed"),
        bare_warning = {
          signalCondition(simpleWarning("secondary diagnostic warning"))
          "unreachable"
        },
        bare_message = {
          signalCondition(simpleMessage("secondary diagnostic message"))
          "unreachable"
        }
      )
    }
    registerS3method("conditionMessage", "storage_extra_warning", method,
                     envir = asNamespace("base"))
    on.exit(rm("conditionMessage.storage_extra_warning",
      envir = asNamespace("base")$.__S3MethodsTable__.), add = TRUE)
    testthat::local_mocked_bindings(collect = function(...) {
      signalCondition(leaf)
      data.frame(g = 0)
    }, .package = "dplyr")
    withCallingHandlers(run_check(), warning = function(cnd) NULL)
    rows <- last_sent_queries(FALSE, TRUE)
    summary <- rows$conditions[[1L]]
    expected <- switch(mode, attributed = "formatted", missing = NA_character_,
                       empty_fallback = NA_character_, "stored fallback")
    stopifnot(identical(summary$message, expected),
              identical(summary$classes[[1L]], unname(as.character(classes))),
              is.null(attributes(summary$message)),
              is.null(attributes(summary$classes[[1L]])))
    list(refs = refs, expected = serialize(last_sent_queries(TRUE, TRUE), NULL))
  }

  modes <- c("attributed", "zero_length", "vector", "integer", "missing",
             "empty_fallback", "error", "bare_warning", "bare_message")
  retained <- list()
  for (mode in modes) {
    fixture <- create(mode)
    retained <- c(retained, fixture$refs)
    for (iteration in seq_len(3L)) invisible(gc())
    stopifnot(all(vapply(fixture$refs, function(ref) {
      is.null(rlang::wref_key(ref))
    }, logical(1))))
    reference <- last_sent_queries(TRUE, TRUE)
    original <- serialize(reference, NULL)
    pristine <- last_sent_queries(TRUE, TRUE)
    default <- last_sent_queries()
    internal <- last_sent_queries(FALSE, TRUE)
    other <- last_sent_queries(TRUE, TRUE)
    reference$status[[1L]] <- "caller status"
    reference$conditions[[1L]]$classes[[1L]][[1L]] <- "caller_class"
    reference$conditions[[1L]]$message[[1L]] <- "caller message"
    attr(reference$conditions[[1L]], "caller") <- new.env(parent = emptyenv())
    internal$conditions[[1L]] <- internal$conditions[[1L]][FALSE, ]
    other$conditions[[1L]]$new_column <- "caller column"
    default$sql[[1L]] <- "caller SQL"
    stopifnot(identical(serialize(last_sent_queries(TRUE, TRUE), NULL), original))
    mutated <- serialize(list(reference, internal, other, default), NULL)
    for (iteration in seq_len(45L)) {
      options(marginplyr.audit_sql = iteration %% 3L == 0L)
      inspect_grouping(data.frame(x = 1))
    }
    stopifnot(identical(serialize(unserialize(fixture$expected), NULL), original))
    stopifnot(identical(serialize(pristine, NULL), original),
              identical(serialize(list(reference, internal, other, default), NULL),
                        mutated))
    options(marginplyr.audit_sql = TRUE)
    rm(fixture)
    for (iteration in seq_len(3L)) invisible(gc())
    stopifnot(all(vapply(retained, function(ref) is.null(rlang::wref_key(ref)),
                        logical(1))))
  }

  inactive <- list(NULL, FALSE, NA, logical(), 0, 1, "TRUE", list(TRUE),
                   c(TRUE, FALSE), c(TRUE, TRUE), factor("TRUE"))
  for (value in inactive) {
    options(marginplyr.audit_sql = value)
    inspect_grouping(data.frame(x = 1))
    options(marginplyr.audit_sql = TRUE)
    caught <- tryCatch(last_sent_queries(TRUE, TRUE), error = identity)
    stopifnot(inherits(caught, "marginplyr_error"),
              grepl("marginplyr.audit_sql", conditionMessage(caught), fixed = TRUE))
  }
  options(marginplyr.audit_sql = TRUE)
  run_check()
  before <- serialize(last_sent_queries(TRUE, TRUE), NULL)
  invalid <- c(inactive[-2L], list(environment(), quote(x), as.raw(1)))
  for (value in invalid) {
    for (argument in c("include_result", "details")) {
      args <- list(include_result = TRUE, details = TRUE)
      args[argument] <- list(value)
      caught <- tryCatch(do.call(last_sent_queries, args, quote = TRUE),
                         error = identity)
      stopifnot(inherits(caught, "marginplyr_error"),
                identical(serialize(last_sent_queries(TRUE, TRUE), NULL), before))
    }
  }
  snapshots <- list()
  testthat::local_mocked_bindings(collect = function(...) {
    signalCondition(simpleWarning("first invocation warning"))
    snapshots[[1L]] <<- last_sent_queries(FALSE, TRUE)
    signalCondition(simpleWarning("second invocation warning"))
    snapshots[[2L]] <<- last_sent_queries(FALSE, TRUE)
    snapshots[[2L]]$conditions[[1L]]$message[[1L]] <<- "caller copy"
    stopifnot(identical(last_sent_queries(FALSE, TRUE)$conditions[[1L]]$message,
                        c("first invocation warning", "second invocation warning")))
    data.frame(g = 0)
  }, .package = "dplyr")
  withCallingHandlers(run_check(), warning = function(cnd) NULL)
  stopifnot(identical(snapshots[[1L]]$status, "invoking"),
            identical(snapshots[[2L]]$status, "invoking"),
            identical(snapshots[[1L]]$conditions[[1L]]$message,
                      "first invocation warning"),
            identical(snapshots[[2L]]$conditions[[1L]]$message,
                      c("caller copy", "second invocation warning")),
            identical(last_sent_queries(FALSE, TRUE)$status, "returned"))
  cat("PASS formatter cases:", length(modes),
      "; cleared weakrefs:", length(retained),
      "; record replacements:", length(modes) * 45L,
      "; inactive settings:", length(inactive),
      "; invalid accessor calls:", length(invalid) * 2L,
      "; active snapshot copies:", length(snapshots), "\n")
}
main()

sink()
