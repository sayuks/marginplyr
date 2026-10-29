# Supported-name controls and excluded upstream restriction. Report mode and
# --assert both succeed on the investigated snapshot. Run from the repo root.
pkgload::load_all(quiet=TRUE)
root <- 'investigation/2026-10-29-summary-spec-share-exhaustive/summary'
args <- commandArgs(trailingOnly=TRUE)
run <- function(expr) tryCatch(force(expr),error=identity)
materialize <- function(query) if(inherits(query,'dtplyr_step')) dplyr::collect(query) else tibble::as_tibble(query)
equal <- function(a,b) !inherits(b,'error') && isTRUE(all.equal(
  dplyr::arrange(tibble::as_tibble(a),f,g), dplyr::arrange(tibble::as_tibble(b),f,g),check.attributes=TRUE))
margin <- function(data,role,helper,sort,label,id=NULL) {
  dots <- switch(role,
    ordinary=rlang::quos(.SD=sum(v)),
    source=rlang::quos(.SD=sum(v),p=!!rlang::call2(helper,rlang::sym('.SD'))),
    output=rlang::quos(total=sum(v),.SD=!!rlang::call2(helper,rlang::sym('total'))),
    identifier=rlang::quos(total=sum(v),p=!!rlang::call2(helper,rlang::sym('total'))))
  summarize_with_margins(data,!!!dots,.by=f,.grouping=rollup(g),
                         .sort=sort,.margin_label=label,.id=id)
}
observations <- list(); controls <- 0L
for(size in c(0L,2L)) {
  input <- tibble::tibble(f=c('F','F'),g=c('a','b'),v=c(1,3))[seq_len(size),]
  source <- dtplyr::lazy_dt(input)
  # Without a fixed key, the Grand total itself refuses this name upstream.
  upstream <- run(dplyr::collect(dplyr::summarise(source,.SD=sum(v))))
  margin_empty <- run(dplyr::collect(summarize_with_margins(source,.SD=sum(v))))
  stopifnot(inherits(upstream,'error'),inherits(margin_empty,'error'))
  stopifnot(grepl('A column may not be called .SD',conditionMessage(upstream),fixed=TRUE))
  controls <- controls+2L
  # Both ordinary branch grains are upstream-success controls.
  detail <- dplyr::collect(dplyr::summarise(source,.SD=sum(v),.by=c(f,g)))
  grand <- dplyr::collect(dplyr::summarise(source,.SD=sum(v),.by=f))
  stopifnot(identical(detail$.SD,input$v))
  stopifnot(identical(grand$.SD,if(size)4 else numeric()))
  controls <- controls+2L
  fixed <- dplyr::collect(summarize_with_margins(source,.SD=sum(v),.by=c(f,g)))
  stopifnot(isTRUE(all.equal(fixed,detail)))
  controls <- controls+1L
  before <- dplyr::collect(source)
  for(role in c('ordinary','source','output')) {
    helpers <- if(role=='ordinary')'share_of_total' else c('share_of_parent','share_of_total')
    for(helper in helpers) for(sort in c('none','first','last')) for(label in list('Total',NULL)) {
      expected <- margin(input,role,helper,sort,label)
      query <- run(margin(source,role,helper,sort,label))
      stopifnot(inherits(query,'dtplyr_step'))
      actual <- run(materialize(query))
      observations[[length(observations)+1L]] <- data.frame(
        size=size,role=role,helper=helper,sort=sort,
        label=if(is.null(label))'NULL'else label,correct=equal(expected,actual),
        outcome=if(inherits(actual,'error'))conditionMessage(actual)else'success')
    }
  }
  for(helper in c('share_of_parent','share_of_total')) for(sort in c('none','first','last')) {
    expected <- margin(input,'identifier',helper,sort,'Total',id='.SD')
    actual <- materialize(margin(source,'identifier',helper,sort,'Total',id='.SD'))
    stopifnot(equal(expected,actual)); controls <- controls+1L
  }
  stopifnot(identical(dplyr::collect(source),before)); controls <- controls+1L
}
results <- do.call(rbind,observations)
write.csv(results,file.path(root,'dtplyr-sd-output-results.csv'),row.names=FALSE)
print(table(results$role,results$correct))
cat('Unsupported Margin differences:',sum(!results$correct),'/',nrow(results),'\n')
cat('Passing controls:',controls,'\n')
cat('First outcome:',results$outcome[[1L]],'\n')
if('--assert'%in%args)stopifnot(all(results$correct))
