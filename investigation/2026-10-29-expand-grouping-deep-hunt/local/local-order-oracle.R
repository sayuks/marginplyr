dir.create('/tmp/marginplyr-root-deep-hunt', showWarnings = FALSE)
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
set.seed(29102026)
values <- list(
  c('b',NA,'a','c'), c(2L,NA_integer_,1L,0L), c(2,NaN,NA,1),
  factor(c('b',NA,'a','c'),levels=c('c','b','a','unused')),
  ordered(c('b',NA,'a','c'),levels=c('c','b','a','unused')),
  list(2L,NULL,NA_integer_,1L), vctrs::list_of(2L,NULL,NA_integer_,1L),
  tibble(a=c(2L,NA_integer_,NA_integer_,1L),b=c('b',NA,'a','c')),
  matrix(c(2L,NA_integer_,NA_integer_,1L,3L,NA_integer_,1L,0L),ncol=2),
  as.Date(c('2020-01-02',NA,'2020-01-01','2020-01-03')),
  bit64::as.integer64(c('9007199254740993',NA,'9007199254740992','0'))
)
ans <- vector('list',1000)
for (i in seq_along(ans)) {
  x <- tibble(rid=1:4)
  for (nm in c('f','a','b','c')) x[[nm]] <- values[[sample.int(length(values),1)]]
  if (i %% 7 == 0L) x <- x[0,]
  fixed <- if(i %% 3 == 0L) 'f' else character()
  sets <- replicate(sample.int(8,1),sample(c('a','b','c'),sample(0:3,1)),simplify=FALSE)
  dims <- unique(unlist(sets))
  if(is.null(dims)) dims <- character()
  calls <- lapply(sets,function(s) rlang::call2('grouping_set',!!!rlang::syms(s)))
  spec <- eval(rlang::call2('grouping_sets',!!!calls))
  sort <- sample(c('none','first','last'),1)
  public_id <- if(i %% 4L == 0L) NULL else 'sid'
  get_result <- function() expand_with_margins(x,.by=all_of(fixed),.grouping=spec,.margin_label=NULL,.duplicates='keep',.sort=sort,.id=public_id)
  got <- tryCatch(get_result(),error=identity)
  expected <- tryCatch({
    rows <- lapply(seq_along(sets),function(j) {
      y <- x
      for(d in setdiff(dims,sets[[j]])) y[[d]] <- vctrs::vec_init(x[[d]],nrow(x))
      y$sid <- rep(j,nrow(x)); y
    })
    y <- do.call(vctrs::vec_rbind,rows)
    key <- list()
    for(d in fixed) {key[[length(key)+1L]] <- as.integer(vctrs::vec_detect_missing(y[[d]]));key[[length(key)+1L]] <- y[[d]]}
    for(d in dims) {
      bit <- vapply(sets,function(s) as.integer(!d %in% s),integer(1))[y$sid]
      if(sort=='first') bit <- -bit
      key[[length(key)+1L]] <- bit
      key[[length(key)+1L]] <- as.integer(vctrs::vec_detect_missing(y[[d]]))
      key[[length(key)+1L]] <- y[[d]]
    }
    if(length(dims)>0L && length(sets)>1L) key[[length(key)+1L]] <- y$sid
    if(sort!='none' && length(key)>0L) {
      names(key) <- paste0('k',seq_along(key))
      y <- vctrs::vec_slice(y,vctrs::vec_order(vctrs::new_data_frame(key,n=nrow(y))))
    }
    if(is.null(public_id)) y$sid <- NULL
    y[,unique(c(fixed,dims,public_id,names(x)))]
  },error=identity)
  verdict <- if(inherits(got,'error')) paste('ERROR',class(got)[1],conditionMessage(got)) else if(inherits(expected,'error')) paste('ORACLE_ERROR',class(expected)[1],conditionMessage(expected)) else if(identical(got,expected)) 'PASS' else paste('DIFF',paste(all.equal(got,expected),collapse=';'))
  ans[[i]] <- list(iteration=i,sort=sort,fixed=fixed,sets=sets,types=vapply(x,function(v) paste(class(v),collapse='/'),character(1)),verdict=verdict)
}
saveRDS(ans,'/tmp/marginplyr-root-deep-hunt/local-order-oracle.rds')
print(table(vapply(ans,function(a) sub('ERROR.*','ERROR',sub('DIFF.*','DIFF',a$verdict)),character(1))))
for(a in ans) if(a$verdict!='PASS') print(a)
