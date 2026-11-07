args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all(args[[1L]], quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
out <- args[[2L]]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
equal_key <- function(x, value) (is.na(x) & is.na(value)) | (!is.na(x) & !is.na(value) & x == value)
rows <- list()
for (seed in 1:24) {
 set.seed(seed)
 n <- seed %% 12L
 data <- tibble(f = sample(c('east','west',NA_character_),n,TRUE),
                g = sample(c('a','b',NA_character_),n,TRUE),
                h = sample(c('x','y',NA_character_),n,TRUE),
                x = sample(c(-7,-1,0,1,3,7,NA_real_),n,TRUE))
 for (backend in c('local','dtplyr')) for (sort in c('none','last','first')) {
  input <- if(backend=='local') data else dtplyr::lazy_dt(data)
  result <- summarize_with_margins(input,
    value = sum(x,na.rm=TRUE), mean_value = mean(x,na.rm=TRUE), count=n(),
    p=share_of_parent(value), t=share_of_total(value),
    pm=share_of_parent(mean_value), tm=share_of_total(mean_value),
    pn=share_of_parent(count), tn=share_of_total(count),
    .by=f,.grouping=rollup(g,h),.id='set',.margin_label=NULL,.sort=sort)
  if(backend=='dtplyr') result <- as_tibble(result)
  checked <- 0L
  for(i in seq_len(nrow(result))) for(source_name in c('value','mean_value','count')) for(kind in c('parent','total')) {
   candidates <- which(equal_key(result$f,result$f[[i]]))
   root <- result$set[[i]] == 3L
   target_set <- if(kind=='total') 3L else min(result$set[[i]]+1L,3L)
   candidates <- candidates[result$set[candidates] == target_set]
   if(kind=='parent' && target_set == 2L) candidates <- candidates[equal_key(result$g[candidates],result$g[[i]])]
   stopifnot(length(candidates)==1L)
   numerator <- result[[source_name]][[i]]
   denominator <- result[[source_name]][candidates]
   expected <- if(root) 1 else if(is.na(numerator)||is.na(denominator)||denominator==0) NA_real_ else as.double(numerator)/as.double(denominator)
   column <- switch(source_name,value=if(kind=='parent') 'p' else 't',mean_value=if(kind=='parent') 'pm' else 'tm',count=if(kind=='parent') 'pn' else 'tn')
   stopifnot(isTRUE(all.equal(result[[column]][[i]],expected)))
   checked <- checked+1L
  }
  rows[[length(rows)+1L]] <- data.frame(seed,backend,sort,input_rows=n,output_rows=nrow(result),checked)
 }
}
result <- do.call(rbind,rows)
write.csv(result,file.path(out,'results.csv'),row.names=FALSE)
cat('Collections:',nrow(result),'Ratio checks:',sum(result$checked),'\n')
capture.output(sessionInfo(),file=file.path(out,'session-info.txt'))
