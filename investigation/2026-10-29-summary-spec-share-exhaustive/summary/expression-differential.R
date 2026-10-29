pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
root <- 'investigation/2026-10-29-summary-spec-share-exhaustive/summary'
run <- function(expr) tryCatch(suppressWarnings(force(expr)), error = identity)
compare <- function(a,b) {
  ea <- inherits(a,'error'); eb <- inherits(b,'error')
  if(ea && eb) return('both-refused')
  if(ea != eb) return(if(ea) 'margin-only-success' else 'margin-only-error')
  if(isTRUE(all.equal(a,b,check.attributes=TRUE))) 'equal' else 'value-difference'
}
frame <- function(x) data.frame(lo=min(x), hi=max(x))
frame_fn <- function(x) tibble(lo=min(x), hi=max(x))
unpack_false <- FALSE
unpack_true <- TRUE
unpack_name <- '{outer}_{inner}'
cols <- c('x','y')
fn_factory <- function() mean
name_factory <- function() 'out_{.col}'
exprs <- list(
  quote(sum(x)), quote(sum(dplyr::pick(x)$x)), quote(nrow(dplyr::pick(x))),
  quote(frame(x)), quote(frame(dplyr::pick(x)$x)), quote(frame_fn(x)),
  quote(list(dplyr::pick(x))), quote(list(dplyr::pick(everything()))),
  quote(dplyr::pick(x)), quote(dplyr::pick(c(x,y))), quote(dplyr::pick(where(is.numeric))),
  quote(dplyr::pick(all_of(cols))), quote(dplyr::pick(c(out=x))),
  quote(dplyr::across(x, sum)), quote(dplyr::across(c(x,y), sum)),
  quote(dplyr::across(c(x,y), list(sum=sum, mean=mean))),
  quote(dplyr::across(c(x,y), ~sum(.x))),
  quote(dplyr::across(c(x,y), frame)),
  quote(dplyr::across(c(x,y), frame, .unpack=TRUE)),
  quote(dplyr::across(c(x,y), frame, .unpack=unpack_true)),
  quote(dplyr::across(c(x,y), frame, .unpack=unpack_false)),
  quote(dplyr::across(c(x,y), frame, .unpack=unpack_name)),
  quote(dplyr::across(c(x,y), frame, .names='{.col}_box', .unpack=TRUE)),
  quote(dplyr::across(c(x,y), list(min=min,max=max), .names='{.fn}_{.col}')),
  quote(dplyr::across(c(x,y), list(~min(.x),~max(.x)), .names='{.fn}_{.col}')),
  quote(dplyr::across(where(is.numeric),sum)), quote(dplyr::across(everything(),length)),
  quote(dplyr::across(dplyr::starts_with('t'),identity)),
  quote(dplyr::across(all_of(cols),fn_factory(),.names=name_factory())),
  quote((dplyr::across)(x,sum)), quote((dplyr::pick)(x)),
  quote(as.list(dplyr::pick(x))), quote(unlist(dplyr::pick(x))),
  quote(cbind(lo=min(x),hi=max(x))), quote(I(matrix(sum(x),nrow=1L))),
  quote(tryCatch(stop('probe'),error=function(e) data.frame(z=sum(x)))),
  quote(if(sum(x)>0) data.frame(z=sum(x)) else NA),
  quote(local({a<-sum(x);data.frame(z=a)})),
  quote(tibble(z=sum(x))), quote(data.frame(z=sum(x))),
  quote(tibble(z=sum(x), .name_repair=~paste0('a_',.x))),
  quote(rlang::inject(dplyr::pick(x))),
  quote(dplyr::if_any(c(x,y),~.x>1)), quote(dplyr::if_all(c(x,y),~.x>1)),
  quote(rowSums(dplyr::pick(x,y))), quote(dplyr::n()), quote(dplyr::first(x)),
  quote(sum(.data$x)), quote(sum((.data)[['x']])), quote(sum((.env)[['cols']]=='x'))
)
rows <- list()
for (n in c(0L,1L,4L)) for (prior in c(FALSE,TRUE)) for (named in c(FALSE,TRUE)) for(i in seq_along(exprs)) {
  data <- tibble(g=c('b','a','b','a'),x=1:4,y=5:8)[seq_len(n),]
  dots <- list(rlang::new_quosure(exprs[[i]],environment()))
  names(dots) <- if(named) 'out' else ''
  if(prior) dots <- c(list(tot=rlang::quo(sum(x))),dots)
  a <- run(rlang::inject(dplyr::summarise(data,!!!dots,.by=g)))
  b <- run(rlang::inject(summarize_with_margins(data,!!!dots,.by=g)))
  result <- compare(a,b)
  label <- paste(deparse(exprs[[i]],width.cutoff=500L),collapse=' ')
  rows[[length(rows)+1L]] <- data.frame(n=n,prior=prior,named=named,expr=label,result=result)
  if(!result %in% c('equal','both-refused')) {
    cat('DIFF',n,prior,named,label,result,'\n');print(a);print(b)
  }
}
results <- bind_rows(rows)
write.csv(results,file.path(root,'expression-results.csv'),row.names=FALSE)
print(table(results$result));cat('Expression comparisons:',nrow(results),'\n')
