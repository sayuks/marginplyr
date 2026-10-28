# Public accessor and recursive ownership controls; no implementation bindings are replaced.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Supply one fresh output-directory path.")
output_dir <- args[[1L]]
if (dir.exists(output_dir)) stop("Output directory already exists: ", output_dir)
if (!dir.create(output_dir, recursive = TRUE)) stop("Could not create output directory.")
repo <- normalizePath(".", mustWork = TRUE)
pkgload::load_all(repo, quiet = TRUE)
options(marginplyr.audit_sql = TRUE)
check_n <- 0L
check <- function(x) { check_n <<- check_n + 1L; stopifnot(x) }
input <- data.frame(g = c('a','b'), v = 1:2)
remote <- dbplyr::lazy_frame(g='a', v=1, con=dbplyr::simulate_postgres())
invisible(summarize_with_margins(remote, total=sum(v), .grouping=rollup(g)))
initial <- last_sent_queries(details=TRUE)
initial_raw <- serialize(initial, NULL)
valid_flags <- list(TRUE, FALSE, c(flag=TRUE), c(flag=FALSE), structure(TRUE, foo='attribute'), structure(FALSE, class='logical_flag'))
for (include in valid_flags) for (details in valid_flags) {
  r <- last_sent_queries(include_result=include, details=details)
  check(identical(names(r), if (as.logical(details)) c('purpose','sql','source','status','conditions') else c('purpose','sql')))
  check(nrow(r) == if (as.logical(include)) 1L else 0L)
  check(identical(serialize(last_sent_queries(details=TRUE), NULL), initial_raw))
}
invalid_flags <- list(NULL, NA, 1L, 'TRUE', c(TRUE,FALSE), logical(), list(TRUE))
for (arg in c('include_result','details')) for (bad in invalid_flags) {
  x <- setNames(list(bad), arg)
  e <- tryCatch(do.call(last_sent_queries, x), error=identity)
  check(inherits(e, 'marginplyr_error'))
  check(identical(serialize(last_sent_queries(details=TRUE), NULL), initial_raw))
}
cat('PASS accessor matrix:', length(valid_flags)^2, 'valid pairs and', length(invalid_flags)*2, 'invalid flags\n')

state <- new.env(parent=emptyenv())
state$depth <- 0L
state$count <- 0L
state$saved <- list()
state$raw <- list()
state$max <- 7L
translate_touch <- function(x) {
  before <- last_sent_queries(details=TRUE)
  raw <- serialize(before, NULL)
  state$count <- state$count + 1L
  state$saved[[state$count]] <- before
  state$raw[[state$count]] <- raw
  if (state$depth < state$max) {
    state$depth <- state$depth + 1L
    nested <- dbplyr::lazy_frame(g='a', v=1, con=dbplyr::simulate_dbi('OwnershipDeepSql'))
    invisible(summarize_with_margins(nested, total=sum(touch(v)), .grouping=rollup(g)))
    state$depth <- state$depth - 1L
    check(identical(last_sent_queries(details=TRUE), before))
    options(marginplyr.audit_sql=FALSE)
    inspect_grouping(input, .grouping=rollup(g))
    check(identical(last_sent_queries(details=TRUE), before))
    options(marginplyr.audit_sql=TRUE)
    e <- tryCatch(inspect_grouping(input, .duplicates='bad'), error=identity)
    check(inherits(e,'marginplyr_error'))
    check(identical(last_sent_queries(details=TRUE), before))
  }
  dbplyr::build_sql('ABS(',x,')')
}
translation <- function(con) dbplyr::sql_variant(dbplyr::sql_translator(.parent=dbplyr::base_scalar, touch=translate_touch), dbplyr::base_agg, dbplyr::base_win)
registerS3method('sql_translation', 'OwnershipDeepSql', translation, envir=asNamespace('dbplyr'))
remote <- dbplyr::lazy_frame(g='a',v=1,con=dbplyr::simulate_dbi('OwnershipDeepSql'))
invisible(summarize_with_margins(remote,total=sum(touch(v)),.grouping=rollup(g)))
final <- last_sent_queries(details=TRUE)
check(nrow(final)==1L && final$purpose=='result' && final$status=='prepared' && !is.na(final$sql))
for (i in seq_along(state$saved)) check(identical(serialize(state$saved[[i]],NULL), state$raw[[i]]))
invisible(inspect_grouping(input))
for (i in seq_along(state$saved)) check(identical(serialize(state$saved[[i]],NULL), state$raw[[i]]))
cat('PASS recursive SQL translator reentry:',state$count,'snapshots; depth',state$max+1L,'; inactive children; caught child refusals; copy stability\n')

cat('PASS total assertions:', check_n, '\n')
saveRDS(list(assertions = check_n, valid_flag_pairs = length(valid_flags)^2,
             invalid_flags = length(invalid_flags)*2L,
             recursive_snapshots = state$count, depth = state$max + 1L,
             session = sessionInfo()), file.path(output_dir, 'summary.rds'))
writeLines(capture.output(sessionInfo()), file.path(output_dir, 'session-info.txt'))
