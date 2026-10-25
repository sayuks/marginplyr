# Public API lifecycle matrix. Source package is loaded; no bindings are replaced.
pkgload::load_all('.', quiet = TRUE)
options(marginplyr.audit_sql = TRUE)
local <- data.frame(g = c('a', 'b'), v = c(1, 3))
con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
remote <- dplyr::copy_to(con, local, 'scope_matrix_remote')
inputs <- list(local = local, sql = remote)
verbs <- list(summarize = summarize_with_margins, summarise = summarise_with_margins,
              expand = expand_with_margins, nest = nest_with_margins,
              nest_by = nest_by_with_margins, inspect = inspect_grouping)
read_record <- function(audited) {
  x <- tryCatch(last_sent_queries(details = TRUE), error = identity)
  if (audited) stopifnot(inherits(x, 'tbl_df')) else stopifnot(inherits(x, 'marginplyr_error'))
  if (audited) x else NULL
}
run <- function(verb, input, factory) {
  tryCatch(verb(input, .grouping = factory()), error = function(cnd) {
    if (!inherits(cnd, 'marginplyr_error')) stop(cnd)
    cnd
  })
}
checks <- 0L
callbacks <- 0L
for (aname in names(verbs)) for (atype in names(inputs)) {
  averb <- verbs[[aname]]; ainput <- inputs[[atype]]
  for (aaudit in c(FALSE, TRUE)) {
    options(marginplyr.audit_sql = aaudit)
    baseline <- run(averb, ainput, function() rollup(g))
    expected <- read_record(aaudit)
    for (bname in names(verbs)) for (btype in names(inputs)) for (baudit in c(FALSE, TRUE)) {
      bverb <- verbs[[bname]]; binput <- inputs[[btype]]
      options(marginplyr.audit_sql = aaudit)
      invoked <- FALSE
      factory <- function() {
        invoked <<- TRUE; callbacks <<- callbacks + 1L
        before <- read_record(aaudit)
        options(marginplyr.audit_sql = baudit)
        bfactory <- function() {
          read_record(baudit)
          # A third local, opposite-option call tests recursive ownership.
          options(marginplyr.audit_sql = !baudit)
          inspect_grouping(local, .grouping = {
            read_record(!baudit)
            rollup(g)
          })
          stopifnot(identical(read_record(baudit), read_record(baudit)))
          rollup(g)
        }
        run(bverb, binput, bfactory)
        stopifnot(identical(read_record(aaudit), before))
        rollup(g)
      }
      outcome <- run(averb, ainput, factory)
      if (!inherits(outcome, 'error') && !inherits(baseline, 'error')) {
        if (inherits(outcome, 'tbl_sql')) {
          stopifnot(identical(as.character(dbplyr::sql_render(outcome)), as.character(dbplyr::sql_render(baseline))))
        } else stopifnot(identical(outcome, baseline))
      }
      stopifnot(identical(read_record(aaudit), expected))
      checks <- checks + 1L
    }
  }
}
DBI::dbDisconnect(con)
cat('PASS lifecycle matrix:', checks, 'pairs;', callbacks, 'actual outer callbacks\n')
