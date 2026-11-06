# Run from the package root; --assert asserts the desired contract and fails on the baseline.
pkgload::load_all('.', quiet = TRUE)
library(dplyr, warn.conflicts = FALSE)
Sys.setenv(TESTTHAT_PARALLEL = 'false')
cat('Baseline:', system2('git', c('rev-parse','HEAD'), stdout=TRUE), '\n')
cat(R.version.string, '\n')
for (pkg in c('dplyr','dtplyr','data.table')) cat(pkg,as.character(packageVersion(pkg)),'\n')
observed <- list(); controls <- 0L; skipped_predicates <- 0L
for (rows in c(0L,1L,3L)) for (shape in c('direct','filtered','arranged','cached')) {
  source <- data.table::data.table(g = rep('a', rows), rid=seq_len(rows))
  before <- data.table::copy(source)
  calls <- 0L
  probe <- function(x) { calls <<- calls+1L; x>0L }
  root <- dtplyr::lazy_dt(source)
  if (shape == 'filtered') root <- filter(root,probe(rid))
  if (shape == 'arranged') root <- arrange(filter(root,probe(rid)),rid)
  if (shape == 'cached') root <- compute(filter(root,probe(rid)),name='empty_cache')
  for (selector in c('empty','negative','helper')) {
    step <- switch(selector,
      empty=select(root), negative=select(root,-everything()),
      helper=select(root,any_of(character())))
    for (family in c('single','repeat')) for (id in list(NULL,'sid')) {
      spec <- if(family=='single') grouping_set() else grouping_sets(grouping_set(),grouping_set())
      got <- tryCatch(expand_with_margins(step,.grouping=spec,.duplicates='keep',.id=id),error=identity)
      observed[[length(observed)+1L]] <- data.frame(rows=rows,shape=shape,selector=selector,family=family,
        id=if(is.null(id))'<NULL>' else id,desired=!inherits(got,'error'),
        actual=if(inherits(got,'error'))conditionMessage(got)else 'accepted')
      stopifnot(calls==0L,identical(source,before))
      # An explicitly created empty root carries precisely this output schema.
      counterpart <- expand_with_margins(dtplyr::lazy_dt(data.frame()),.grouping=spec,.duplicates='keep',.id=id) |> collect()
      stopifnot(nrow(counterpart)==0L,identical(names(counterpart),if(is.null(id))character()else id))
      controls <- controls+1L
    }
    # Type-dependent predicates are never invoked: no columns exist to test.
    pred <- function(x) { skipped_predicates <<- skipped_predicates+1L; is.character(x) }
    got <- tryCatch(inspect_grouping(step,.grouping=grouping_set(where(pred))),error=identity)
    observed[[length(observed)+1L]] <- data.frame(rows=rows,shape=shape,selector=selector,family='typed-inspect',id='<NULL>',
      desired=!inherits(got,'error'),actual=if(inherits(got,'error'))conditionMessage(got)else'accepted')
    stopifnot(skipped_predicates==0L,calls==0L,identical(source,before))
    # Name-only inspection already knows that there are no dimensions.
    stopifnot(identical(inspect_grouping(step,.grouping=grouping_set()),inspect_grouping(tibble(),.grouping=grouping_set())))
    controls <- controls+1L
  }
}
r <- bind_rows(observed)
write.csv(r,'investigation/2026-11-06-expand-grouping-hunt/lazy/dtplyr-empty-projection.csv',row.names=FALSE)
cat('Refused deterministic empty projections:',sum(!r$desired),'of',nrow(r),'\n')
cat('Direct-empty-root and names-only-inspection controls:',controls,'passed\n')
cat('Source predicates evaluated:',calls,'; empty-selection predicates invoked:',skipped_predicates,'\n')
# Explicit materialization does not change the empty projection's schema.
minimal <- select(dtplyr::lazy_dt(data.frame(g='a')))
eager <- collect(minimal)
expected <- expand_with_margins(eager,.id='sid')
stopifnot(nrow(eager)==0L,ncol(eager)==0L,nrow(expected)==0L,identical(expected$sid,integer()))
if('--assert' %in% commandArgs(trailingOnly=TRUE)) stopifnot(all(r$desired))
