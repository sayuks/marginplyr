args <- commandArgs(TRUE)
pkgload::load_all(if (length(args)) args[[1]] else '.', quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = 'false')
out <- if(length(args)>1) args[[2]] else tempdir()
family <- list(
  raw = as.raw(c(0, 255, 0, 3)),
  complex = c(1+2i, complex(real=NaN), complex(real=NA_real_), 1+2i),
  double_nan = c(NA_real_,NaN,Inf,-Inf),
  double_zero = c(0,-0,0,-0),
  list_vectors = list(c(1,2),c(1,2),c(2,1),NULL),
  list_nested = list(list(a=1),list(b=1),list(a=1),NULL),
  list_frames = list(data.frame(a=1), data.frame(a=1), data.frame(a=2), NULL),
  list_matrix = list(matrix(1,2,2),matrix(1,2,2),matrix(2,2,2),NULL),
  list_exprs = list(quote(a+b),quote(a+b),quote(b+a),NULL),
  list_names = list(c(a=1),c(b=1),c(a=1),NULL),
  packed_zero = tibble::new_tibble(list(),nrow=4),
  packed_nested = tibble::tibble(a=tibble::tibble(b=c(1,1,2,NA))),
  array_1 = array(c(1,1,2,NA),4),
  matrix = matrix(c(1,1,2,NA,2,2,3,NA),4),
  array_3 = array(c(1,1,2,NA,2,2,3,NA),c(4,1,2)),
  factor_na = factor(c('a',NA,'a',NA),exclude=NULL),
  factor_missing_na = structure(c(1L,2L,NA_integer_,1L),levels=c('a',NA),class='factor')
)
results <- list()
for (fam in names(family)) for (empty in c(FALSE,TRUE)) for (by in c(FALSE,TRUE)) for (keep in c(FALSE,TRUE)) for (verb in c('nest_with_margins','nest_by_with_margins')) for (sort in c('none','last','first')) {
  df <- tibble::tibble(f=c('x','y','x','y'),g=family[[fam]],rid=seq_len(4))
  if(empty) df <- vctrs::vec_slice(df,integer())
  serial <- serialize(df,NULL)
  spec <- rollup(g)
  z <- tryCatch(do.call(get(verb), c(list(.data=df,.grouping=spec,.margin_label=NULL,.keep=keep,.id='sid',.sort=sort),if(by)list(.by=rlang::expr(f))else list())),error=identity)
  expected <- list()
  for(sid in 1:2) {
    keys <- c(if(by)'f',if(sid==1)'g')
    groups <- if(length(keys)) vctrs::vec_group_loc(df[keys])$loc else if(nrow(df))list(seq_len(nrow(df)))else list()
    for(rows in groups) expected[[length(expected)+1L]] <- paste(sid,paste(df$rid[rows],collapse=','),sep=':')
  }
  actual <- if(inherits(z,'error')) character()else vapply(seq_len(nrow(z)),function(i)paste(z$sid[[i]],paste(z$data[[i]]$rid,collapse=','),sep=':'),character(1))
  status <- if(inherits(z,'error')) 'error' else if(!identical(sort(as.character(unlist(expected))),sort(actual)))'membership'else 'match'
  if(!identical(serial,serialize(df,NULL))) status <- 'mutation'
  results[[length(results)+1L]] <- data.frame(family=fam,empty,by,keep,verb,sort,status,message=if(inherits(z,'error'))conditionMessage(z)else paste(actual,collapse=';'))
}
r <- do.call(rbind,results)
write.csv(r,file.path(out,'structured-results.csv'),row.names=FALSE)
print(with(r,table(family,status)))
print(unique(r[r$status!='match',c('family','empty','by','status','message')]),row.names=FALSE)
stopifnot(all(r$status[r$family != 'raw']=='match'))
stopifnot(all(r$status[r$family == 'raw']=='error'))
stopifnot(all(grepl('typed-missing',r$message[r$family == 'raw'],fixed=TRUE)))
