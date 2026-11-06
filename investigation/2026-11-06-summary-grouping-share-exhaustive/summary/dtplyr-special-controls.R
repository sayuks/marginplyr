Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all(quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
columns <- c('.N','.I','.SD','.BY','.GRP','.NGRP','.EACHI','..1','..2','...','key','check.names','keep.rownames','stringsAsFactors','with','by','j','i','on','nomatch','mult','roll','which','.env','.data','n','mean','sum','pick','across')
results <- list(); i <- 0L
for (col in columns) {
  d <- tibble(g=c(1L,1L,2L),v=c(1,2,3)); names(d)[[2L]] <- col
  dt <- dtplyr::lazy_dt(d)
  for (shape in c('pronoun','bare','pick','across')) {
    ex <- switch(shape, pronoun=rlang::expr(sum(.data[[!!col]])), bare=rlang::call2('sum', rlang::sym(col)), pick=rlang::expr(sum(unlist(pick(all_of(!!col))))), across=rlang::expr(across(all_of(!!col),sum)))
    n <- if(shape=='across') '' else 'value'
    dot <- setNames(list(ex),n)
    ordinary <- tryCatch(rlang::eval_tidy(rlang::call2(dplyr::summarise,dt,!!!dot,.by=quote(g))) |> collect() |> as_tibble(),error=identity)
    margin <- tryCatch(rlang::eval_tidy(rlang::call2(summarize_with_margins,dt,!!!dot,.by=quote(g))) |> collect() |> as_tibble(),error=identity)
    matched <- if (inherits(ordinary,'error') || inherits(margin,'error')) inherits(ordinary,'error') && inherits(margin,'error') else isTRUE(all.equal(ordinary,margin,check.attributes=FALSE))
    i <- i+1L; results[[i]] <- data.frame(col,shape,matched,ordinary_error=inherits(ordinary,'error'),margin_error=inherits(margin,'error'))
    if(!matched) {cat('\nDIFF',col,shape,'\n');print(ordinary);print(margin)}
  }
}
results <- do.call(rbind,results)
write.csv(results,'investigation/2026-11-06-summary-grouping-share-exhaustive/summary/dtplyr-special-controls.csv',row.names=FALSE)
cat('SPECIAL',nrow(results),'DIFFS',sum(!results$matched),'\n')
stopifnot(all(results$matched))
