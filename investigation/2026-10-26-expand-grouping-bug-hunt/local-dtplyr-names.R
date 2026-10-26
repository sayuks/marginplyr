# Investigation evidence, 2026-10-26. Run from the repository root.
suppressPackageStartupMessages(pkgload::load_all('.', quiet=TRUE))
nms <- c('a','b','T','F','NA','TRUE','FALSE','NULL','NaN','Inf','.N','.SD','.I','.GRP','.BY','.EACHI','.data','.env','..marginplyr_set_id','region, store','a\nb','a\rb','a`b','x y','é','日本語')
failures <- list(); total <- 0L
for (nm in nms) for (id in c('sid','.data','.env','.N','TRUE','x y')) {
 if (id==nm) next
 x <- tibble::as_tibble(setNames(list(c('a','b'),c(1L,2L)),c(nm,'value')))
 sp <- rlang::inject(rollup(dplyr::all_of(!!nm)))
 for (backend in c('local','dtplyr')) for (sort in c('none','first','last')) {
  total <- total+1L
  source <- if (backend=='dtplyr') dtplyr::lazy_dt(x) else x
  got <- tryCatch(expand_with_margins(source, .grouping=sp, .id=id,.sort=sort) |> dplyr::collect() |> tibble::as_tibble(),error=identity)
  msg <- if(inherits(got,'error')) conditionMessage(got) else if(nrow(got)!=4L || !identical(sort(got[[id]]),c(1L,1L,2L,2L)) || !identical(sort(got$value),c(1L,1L,2L,2L)) || sum(got[[nm]]=='Total')!=2L) paste(capture.output(print(got)),collapse=' ') else NULL
  if(!is.null(msg)) failures[[length(failures)+1L]] <- list(name=nm,id=id,backend=backend,sort=sort,message=msg)
 }
}
cat('Cases:',total,' Failures:',length(failures),'\n');print(head(failures, 3L))
saveRDS(failures,'/tmp/marginplyr-root-names-failures.rds')
