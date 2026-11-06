Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all(Sys.getenv('MARGINPLYR_HUNT_SOURCE', '.'), quiet=TRUE)
d <- data.frame(a=1L)
shapes <- list(character='a', symbol=quote(a), call=quote(identity(a)), NULL=NULL, number=1L, environment=globalenv(), list=list('a'), formula=~a, missing=rlang::missing_arg())
for (nm in names(shapes)) {
  s <- structure(list(type='set',args=shapes[nm]),class='margin_grouping_spec')
  e <- tryCatch(inspect_grouping(d,.grouping=s),error=identity)
  cat(nm, paste(class(e),collapse='/'), conditionMessage(e), '\n')
}
