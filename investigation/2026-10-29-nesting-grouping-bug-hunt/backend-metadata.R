pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
vectors <- list(logical=c(TRUE,FALSE), integer=1:2, double=c(1,2), character=c('a','b'), factor=factor(c('a','b'), levels=c('z','a','b')), date=as.Date(c('2020-01-01','2020-01-02')), posix=as.POSIXct(c('2020-01-01','2020-01-02'),tz='UTC'), list=list(1,2), named=structure(1:2,names=c('a','b')), I=I(1:2))
comparisons <- 0L
for (a in names(vectors)) for (b in names(vectors)) {
  input <- tibble(x=vectors[[a]], y=vectors[[b]],v=1:2)
  source <- dtplyr::lazy_dt(input) |> mutate(x=y)
  px <- tryCatch(utils::getFromNamespace("dtplyr_selection_proxy", "marginplyr")(source),error=identity)
  cx <- tryCatch(collect(source),error=identity)
  if(inherits(px,'error') || inherits(cx,'error')) stop('unexpected probe failure')
  comparisons <- comparisons + 1L
  if (!identical(lapply(as.list(px), vctrs::vec_ptype),lapply(as.list(cx),vctrs::vec_ptype))) cat('METADATA_DIFF',a,b,'\n')
}

cat('Prototype comparisons:',comparisons,'\n')
