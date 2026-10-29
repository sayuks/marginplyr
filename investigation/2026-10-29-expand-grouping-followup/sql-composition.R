pkgload::load_all(quiet = TRUE)
library(dplyr)
options(warn = 1)
conns <- list(sqlite = DBI::dbConnect(RSQLite::SQLite(), ':memory:'), duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE)))
if (nzchar(Sys.getenv('PGHOST'))) conns$postgres <- DBI::dbConnect(RPostgres::Postgres())
failures <- list(); count <- 0L
same_bag <- function(a,b) {
  if(!identical(names(a),names(b)) || nrow(a)!=nrow(b)) return(FALSE)
  a <- as.data.frame(a); b <- as.data.frame(b)
  for(nm in names(a)) { a[[nm]] <- as.character(a[[nm]]); b[[nm]] <- as.character(b[[nm]]) }
  a <- a[do.call(order,c(unname(a),list(na.last=TRUE))),,drop=FALSE]
  b <- b[do.call(order,c(unname(b),list(na.last=TRUE))),,drop=FALSE]
  rownames(a)<-NULL; rownames(b)<-NULL
  identical(a,b)
}
run <- function(nm, expr, expected) {
  count <<- count+1L
  value <- tryCatch(force(expr), error = identity)
  if(inherits(value,'error') || !same_bag(value,expected)) {
    failures[[nm]] <<- list(error=if(inherits(value,'error')) conditionMessage(value) else NULL,value=if(!inherits(value,'error')) value, expected=expected)
    cat('FAIL', nm, if(inherits(value,'error')) conditionMessage(value) else 'VALUE', '\n')
  }
  value
}
namesets <- list(c('g','h','v'), c('.env','TRUE','v'), c('TRUE','.env','v'),c('FALSE','TRUE','.env'),c('rowid','_rowid_','oid'),c('dd1','dots','v'),c('a b','a\nb','v'),c('select','from','v'),c('g','h','..marginplyr_order_1'))
for(backend in names(conns)) for(i in seq_along(namesets)) {
  con <- conns[[backend]]; nms<-namesets[[i]]
  data <- data.frame(x=c('z','a',NA,'a'),y=c('u',NA,'v','v'),z=c(1L,2L,3L,4L)); names(data)<-nms
  source <- copy_to(con,data,paste0('facts',i),temporary=TRUE,overwrite=TRUE)
  for(shape in c('plain','head','arrange','empty','grouped','window','union')) {
    shape_fn <- switch(shape,plain=identity,head=function(x)head(x,2L),arrange=function(x)arrange(x,.data[[nms[[3L]]]]),empty=function(x)filter(x,FALSE),grouped=function(x)group_by(x,across(all_of(nms[[2L]]))),window=function(x)mutate(arrange(x,.data[[nms[[3L]]]]),wn=row_number()),union=function(x)union_all(x,x))
    ld <- shape_fn(data); lazy <- tryCatch(shape_fn(source),error=identity)
    if(inherits(lazy,'error')) next
    native <- tryCatch(collect(lazy),error=identity)
    if(inherits(native,'error') || !same_bag(native,ld)) {cat('UNSUPPORTED',backend,i,shape,'\n');next}
    for(speci in 1:3) for(label in list(NULL,'ALL')) for(sort in c('none','first','last')) {
      s1 <- rlang::sym(nms[[1L]]); s2 <- rlang::sym(nms[[2L]])
      spec <- switch(speci,
        rlang::inject(rollup(!!s1)),
        rlang::inject(grouping_sets(grouping_set(), grouping_set(!!s1))),
        rlang::inject(cube(!!s1, !!s2))
      )
      if(shape=='grouped' && speci==3L) next
      args<-list(.grouping=spec,.margin_label=label,.sort=sort,.id='sid')
      expected <- tryCatch(do.call(expand_with_margins,c(list(ld),args)),error=identity)
      if(inherits(expected,'error')) next
      nm<-paste(backend,i,shape,speci,if(is.null(label))'NULL' else label,sort,sep='/')
      r <- tryCatch(do.call(expand_with_margins,c(list(lazy),args)),error=identity)
      if(inherits(r,'error')) {run(nm,stop(r),expected);next}
      out <- run(nm,collect(r),expected)
      if(i %in% 2:5 && shape %in% c('plain','empty') && speci==1L) {
        run(paste0(nm,'/reexpand'),collect(expand_with_margins(r)),expand_with_margins(expected))
        if(sort!='none' && i!=5L) run(paste0(nm,'/compute'),collect(compute(r,analyze=FALSE)),expected)
      }
    }
  }
}
cat('CASES',count,'FAILURES',length(failures),'\n')
saveRDS(failures,'/tmp/marginplyr-audit-sql-failures.rds')
for(nm in names(conns)) DBI::dbDisconnect(conns[[nm]])
