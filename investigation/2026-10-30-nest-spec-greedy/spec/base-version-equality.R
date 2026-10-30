# Dated observation replay. --assert requires correct public membership.
pkgload::load_all('.',quiet=TRUE)
versions <- package_version(c('1.0','1.0.0'))
stopifnot(isTRUE(versions[[1L]]==versions[[2L]]),
          identical(as.character(versions),c('1.0','1.0.0')))
x <- tibble::tibble(g=versions,row_id=1:2)
stopifnot(identical(vctrs::vec_group_loc(x['g'])$loc,list(1:2)))
keys <- dplyr::mutate(x, display=as.character(g), identity=g)
identity_only <- dplyr::summarise(keys,n=dplyr::n(),.by=identity)
both_keys <- dplyr::summarise(keys,n=dplyr::n(),.by=c(display,identity))
stopifnot(nrow(identity_only)==1L,identical(identity_only$n,2L),
          nrow(both_keys)==2L,identical(both_keys$n,c(1L,1L)))
upstream <- dplyr::nest_by(x,g)
stopifnot(nrow(upstream)==1L,identical(upstream$data[[1L]]$row_id,1:2))
upstream_tidyr <- tidyr::nest(x,.by=g)
stopifnot(nrow(upstream_tidyr)==1L,identical(upstream_tidyr$data[[1L]]$row_id,1:2))
observed <- list()
for (input_kind in c('data.frame','tibble')) for (verb in c('nest_with_margins','nest_by_with_margins')) for (keep in c(FALSE,TRUE)) for (sort in c('none','first','last')) for (spec_kind in c('set','rollup')) for (payload in c(TRUE,FALSE)) {
  data <- if (payload) x else x['g']
  if (input_kind=='data.frame') data <- as.data.frame(data)
  spec <- if (spec_kind=='set') grouping_set(g) else rollup(g)
  expected_rows <- if (spec_kind=='set') 1L else 2L
  apply <- get(verb)
  actual <- apply(data,.grouping=spec,.keep=keep,.sort=sort,.id='set')
  typed <- apply(data,.grouping=spec,.keep=keep,.sort=sort,.id='set',.margin_label=NULL)
  typed_na <- apply(data,.grouping=spec,.keep=keep,.sort=sort,.id='set',.margin_label=NA_character_)
  fixed <- apply(data,.by=g,.keep=keep,.sort=sort)
  summary <- summarize_with_margins(data,n=dplyr::n(),.grouping=spec)
  stopifnot(nrow(typed)==expected_rows,nrow(typed_na)==expected_rows,
            nrow(fixed)==1L,nrow(summary)==expected_rows,
            identical(summary$n,rep(2L,expected_rows)))
  stopifnot(nrow(typed$data[[which(typed$set==1L)]])==2L,
            nrow(typed_na$data[[which(typed_na$set==1L)]])==2L,
            nrow(fixed$data[[1L]])==2L)
  if (payload) {
    stopifnot(identical(typed$data[[which(typed$set==1L)]]$row_id,1:2),
              identical(typed_na$data[[which(typed_na$set==1L)]]$row_id,1:2),
              identical(fixed$data[[1L]]$row_id,1:2))
  }
  detail <- actual$data[actual$set==1L]
  correct <- length(detail)==1L && nrow(detail[[1L]])==2L
  observed[[length(observed)+1L]] <- correct
  stopifnot(nrow(actual)==expected_rows+1L,length(detail)==2L,
            all(vapply(detail,nrow,integer(1))==1L))
  if (payload) {
    membership <- lapply(detail,function(cell)cell$row_id)
    stopifnot(identical(sort(unlist(membership,use.names=FALSE)),1:2))
  }
}
cat('base package_version equal values with distinct display text: upstream vctrs/dplyr/tidyr kept one two-row group.\n')
cat('Wrong-membership observations=',length(observed),' of ',length(observed),
    '; plain frames/tibbles x both nesting verbs x both keep settings x three sorts x grouping_set/rollup x payload/no-payload.\n',sep='')
cat('NULL/NA labels, fixed-key nesting, and Margin summary controls retained one two-row detail group in every combination.\n')
if ('--assert' %in% commandArgs(TRUE)) stopifnot(all(unlist(observed)))
