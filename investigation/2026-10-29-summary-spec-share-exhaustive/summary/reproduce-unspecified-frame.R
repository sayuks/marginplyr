# Run from the repository root. Report mode succeeds; --assert asserts the
# desired behavior and exits nonzero while the bug remains.
pkgload::load_all(quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
root <- 'investigation/2026-10-29-summary-spec-share-exhaustive/summary'
run <- function(expr) tryCatch(suppressWarnings(force(expr)), error = identity)
same <- function(expected, actual) {
  !inherits(actual, 'error') && isTRUE(all.equal(expected, actual, check.attributes = TRUE))
}
convert <- list(data.frame = identity, tibble = tibble::as_tibble,
                data.table = data.table::as.data.table)
observations <- list()
controls <- list()
record <- function(class_name, reversed, rewritten, mode, expected, actual, calls) {
  ok <- same(expected, actual)
  observations[[length(observations) + 1L]] <<- data.frame(
    input = class_name, reversed = reversed, rewritten = rewritten, mode = mode,
    correct = ok, calls = calls,
    observed = if(inherits(actual,'error')) conditionMessage(actual) else paste(names(actual),collapse=' | ')
  )
}
for (class_name in names(convert)) for(reversed in c(FALSE,TRUE)) for(rewritten in c(FALSE,TRUE)) {
  data <- data.frame(fixed = 'f', g = c('a','b','b'), x = 1:3)
  if(reversed) data <- data[c(2L,3L,1L),]
  data <- convert[[class_name]](data)
  calls <- 0L
  value <- function(x) {
    calls <<- calls + 1L
    if(length(x) == 1L) NA else data.frame(z = sum(x))
  }
  for(mode in c('by','included-set','fixed-and-set')) {
    ordinary <- if(rewritten) run(dplyr::summarise(data,value(dplyr::pick(x)$x),.by=c(fixed,g))) else run(dplyr::summarise(data,value(x),.by=c(fixed,g)))
    # The two-key control makes fixed and dimension ordering explicit.
    if(mode != 'fixed-and-set') ordinary <- dplyr::select(ordinary,-fixed)
    calls <- 0L
    actual <- if(mode == 'by') {
      if(rewritten) run(summarize_with_margins(data,value(dplyr::pick(x)$x),.by=g)) else run(summarize_with_margins(data,value(x),.by=g))
    } else if(mode == 'included-set') {
      if(rewritten) run(summarize_with_margins(data,value(dplyr::pick(x)$x),.grouping=grouping_set(g))) else run(summarize_with_margins(data,value(x),.grouping=grouping_set(g)))
    } else {
      if(rewritten) run(summarize_with_margins(data,value(dplyr::pick(x)$x),.by=fixed,.grouping=grouping_set(g))) else run(summarize_with_margins(data,value(x),.by=fixed,.grouping=grouping_set(g)))
    }
    record(class_name,reversed,rewritten,mode,ordinary,actual,calls)
    stopifnot(calls == 2L)
  }
  detail <- if(rewritten) run(dplyr::summarise(data,value(dplyr::pick(x)$x),.by=g)) else run(dplyr::summarise(data,value(x),.by=g))
  total <- if(rewritten) run(dplyr::summarise(data,value(dplyr::pick(x)$x))) else run(dplyr::summarise(data,value(x)))
  total <- dplyr::mutate(total,g='Total',.before=1L)
  expected <- dplyr::bind_rows(detail,total)
  calls <- 0L
  actual <- if(rewritten) run(summarize_with_margins(data,value(dplyr::pick(x)$x),.grouping=rollup(g))) else run(summarize_with_margins(data,value(x),.grouping=rollup(g)))
  record(class_name,reversed,rewritten,'rollup',expected,actual,calls)
  stopifnot(calls == 3L)
  named_ordinary <- run(dplyr::summarise(data,packed=value(x),.by=g))
  named_actual <- run(summarize_with_margins(data,packed=value(x),.by=g))
  stopifnot(same(named_ordinary,named_actual))
  controls[[length(controls)+1L]] <- data.frame(input=class_name,reversed=reversed,rewritten=rewritten,control='explicitly-named',passed=TRUE)
}
for(typed_na in list(NA_integer_,NA_real_,NA_character_)) {
  for(reversed in c(FALSE,TRUE)) {
    data <- data.frame(g=c('a','b','b'),x=1:3)
    if(reversed) data <- data[c(2L,3L,1L),]
    value <- function(x) if(length(x)==1L) typed_na else data.frame(z=sum(x))
    ordinary <- run(dplyr::summarise(data,value(x),.by=g))
    actual <- run(summarize_with_margins(data,value(x),.by=g))
    stopifnot(inherits(ordinary,'error'),inherits(actual,'error'))
    controls[[length(controls)+1L]] <- data.frame(input='data.frame',reversed=reversed,rewritten=FALSE,control=paste0('typed-',typeof(typed_na),'-NA-refusal'),passed=TRUE)
  }
}
for(frame in c(FALSE,TRUE)) {
  data <- data.frame(g=c('a','b','b'),x=1:3)
  value <- if(frame) function(x) data.frame(z=sum(x)) else function(x) NA
  ordinary <- run(dplyr::summarise(data,value(x),.by=g))
  actual <- run(summarize_with_margins(data,value(x),.by=g))
  stopifnot(same(ordinary,actual))
  controls[[length(controls)+1L]] <- data.frame(input='data.frame',reversed=FALSE,rewritten=FALSE,control=if(frame)'homogeneous-frame' else 'homogeneous-unspecified',passed=TRUE)
}
results <- do.call(rbind,observations)
controls <- do.call(rbind,controls)
write.csv(results,file.path(root,'unspecified-frame-results.csv'),row.names=FALSE)
write.csv(controls,file.path(root,'unspecified-frame-controls.csv'),row.names=FALSE)
print(table(results$mode,results$correct))
cat('Affected cases:',sum(!results$correct),'/',nrow(results),'\n')
cat('Passing controls:',sum(controls$passed),'/',nrow(controls),'\n')
cat('Example detail schema:',results$observed[[1L]],'\n')
cat('Example rollup outcome:',results$observed[results$mode=='rollup'][[1L]],'\n')
if('--assert' %in% args) stopifnot(all(results$correct))
