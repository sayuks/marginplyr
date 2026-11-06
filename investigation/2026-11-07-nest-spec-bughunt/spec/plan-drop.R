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
hunt_seed <- 71107L; set.seed(hunt_seed)
xs <- tibble::tibble(a = 1, b = 2, c = 3, d = 4)
node <- function(kind, args) list(kind=kind, args=args)
leaf <- function(x) list(kind='leaf', args=list(x))
model <- function(x) {
 if (x$kind == 'leaf') return(list(x$args[[1]]))
 families <- lapply(x$args, model)
 if(x$kind == 'set') return(list(unique(unlist(families))))
 if(x$kind == 'sets') return(unlist(families, recursive=FALSE))
 if(x$kind == 'product') return(Reduce(function(p,q) unlist(lapply(p,function(i) lapply(q,function(j) unique(c(i,j)))),recursive=FALSE), families, init=list(character())))
 units <- unlist(lapply(seq_along(x$args), function(i) if(x$args[[i]]$kind == 'leaf') as.list(x$args[[i]]$args[[1]]) else families[[i]]), recursive=FALSE)
 if(x$kind == 'rollup') return(lapply(length(units):0L,function(i) unique(unlist(units[seq_len(i)]))))
 if(x$kind == 'cube') return(unlist(lapply(length(units):0L, function(n) lapply(utils::combn(seq_along(units), n, simplify=FALSE), function(i) unique(unlist(units[i])))), recursive=FALSE))
}
tospec <- function(x) {
 if(x$kind == 'leaf') return(rlang::expr(c(!!!rlang::syms(x$args[[1]]))))
 fn <- c(set='grouping_set',sets='grouping_sets',product='grouping_spec',rollup='rollup',cube='cube')[[x$kind]]
 rlang::call2(fn, !!!lapply(x$args,tospec))
}
gen <- function(depth=0) {
 kind <- if(depth>2) 'set' else sample(c('set','sets','product','rollup','cube'),1)
 n <- sample(if(kind %in% c('sets','rollup','cube')) 1:3 else 0:3,1)
 args <- lapply(seq_len(n),function(i) {
  if(kind %in% c('set','rollup','cube')) {
   vs <- sample(letters[1:4],sample(1:3,1))
   if(kind %in% c('rollup','cube') && length(vs)>1 && sample(c(T,F),1)) node('set',list(leaf(vs))) else leaf(vs)
  } else if(sample(c(T,F),1)) gen(depth+1) else leaf(sample(letters[1:4],sample(0:3,1)))
 })
 node(kind,args)
}
key <- function(xs) vapply(xs,function(x) paste(sort(unique(x)),collapse=','),'')
fail <- list(); tested <- 0L
for(i in 1:1500) {
 x <- gen(); e <- tospec(x); expected <- unique(key(model(x)))
 if(length(expected)>10000) next
 spec <- eval(e)
 keep <- inspect_grouping(xs,.grouping=spec,.duplicates='keep',.format='list')
 actual <- inspect_grouping(xs,.grouping=spec,.duplicates='drop',.format='list')
 expected_plan <- keep[!duplicated(key(keep$included)),]
 expected_plan$set_id <- seq_len(nrow(expected_plan))
 if(!identical(actual,expected_plan)) fail[[length(fail)+1L]] <- list(expr=e,expected=expected_plan,actual=actual)
 tested <- tested + 1L
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
