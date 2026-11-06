Sys.setenv(TESTTHAT_PARALLEL = 'false')
source_root <- Sys.getenv('MARGINPLYR_HUNT_SOURCE')
if (!nzchar(source_root)) stop('Set MARGINPLYR_HUNT_SOURCE to the pinned source checkout.')
source_root <- normalizePath(source_root, mustWork = TRUE)
script_arg <- grep('^--file=', commandArgs(), value = TRUE)
if (length(script_arg) != 1L) stop('Run this script with Rscript.')
script_path <- normalizePath(sub('^--file=', '', script_arg), mustWork = TRUE)
script_dir <- dirname(script_path)
run_name <- tools::file_path_sans_ext(basename(script_path))
output_dir <- Sys.getenv('MARGINPLYR_HUNT_OUTPUT', file.path(script_dir, 'results'))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, mustWork = TRUE)
source_sha <- system2('git', c('-C', shQuote(source_root), 'rev-parse', 'HEAD'), stdout = TRUE)
if (length(source_sha) != 1L || !grepl('^[0-9a-f]{40}$', source_sha)) stop('Cannot establish source SHA.')
pkgload::load_all(source_root, quiet = TRUE)
data <- data.frame(a='x',b=1L,c=1.1)
constructors <- c('grouping_sets','grouping_spec','rollup','cube')
fail <- list(); tested <- 0L
for(op in c('::',':::')) for(depth in 1:5) for(headdepth in 0) for(payload in list(quote(s),quote(grouping_set(a)))) for(returnspec in c(FALSE,TRUE)) {
 state <- new.env(); state$count <- 0L
 env <- rlang::env(s = grouping_set(a))
 env[[op]] <- function(...) {
   state$count <- state$count+1L
   function(x) {
     if(!is.character(tidyselect::peek_data()$a)) stop('wrong metadata')
     if(depth %% 2 == 0) force(x)
     if(returnspec) marginplyr::grouping_set(b) else 'b'
   }
 }
 expr <- payload
 for(i in seq_len(depth)) {
   head <- rlang::call2(op,quote(base),quote(`(`))
   for(j in seq_len(headdepth)) head <- rlang::call2('(',head)
   expr <- rlang::call2(head,expr)
 }
 q <- rlang::new_quosure(expr,env)
 for(cons in constructors) {
   state$count <- 0L
   expected <- list('b')
   spec <- eval(rlang::call2(cons,q),env)
   act <- tryCatch(inspect_grouping(data,.grouping=spec,.duplicates='drop',.format='list'),error=identity)
   exp_n <- if(cons %in% c('rollup','cube')) 2L else 1L
   if(if(op == ':::' && returnspec) !inherits(act,'marginplyr_error') || state$count != if(depth %%2==0)depth else 1L else inherits(act,'error') || !identical(act$included,if(exp_n==2) list('b',character()) else expected) || state$count != if(depth %%2==0)depth else 1L) fail[[length(fail)+1L]] <- list(expr=expr,cons=cons,return=returnspec,count=state$count,actual=act)
   tested <- tested+1L
 }
}
cat('tested', tested, 'fail', length(fail), '\n')
fail_text <- lapply(fail, function(record) {
  if (inherits(record$actual, 'error')) {
    record$actual <- list(class = class(record$actual), message = conditionMessage(record$actual))
  }
  record
})
dput(fail_text, file = file.path(output_dir, paste0(run_name, '-failures.R')))
summary <- data.frame(script = basename(script_path), tested = tested, failures = length(fail),
                      seed = if (exists('hunt_seed')) hunt_seed else NA_integer_,
                      source_sha = source_sha, R_version = as.character(getRversion()))
utils::write.csv(summary, file.path(output_dir, paste0(run_name, '-summary.csv')), row.names = FALSE)
if (length(fail)) stop('The recorded public-API comparisons failed.')
