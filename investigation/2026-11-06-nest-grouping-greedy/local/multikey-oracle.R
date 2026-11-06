args <- commandArgs(TRUE)
mixed <- "--mixed" %in% args
args <- setdiff(args, "--mixed")
pkgload::load_all(if (length(args)) args[[1]] else '.', quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = 'false')
out <- if(length(args)>1) args[[2]] else tempdir()
family <- list(
  character = c('z','a','z',NA,'a','z'),
  integer = c(2L,1L,2L,NA,1L,2L),
  double = c(NA_real_,NaN,Inf,-Inf,0,-0),
  factor = factor(c('z','a','z',NA,'a','z'), levels=c('unused','z','a')),
  factor_na = structure(c(1L,2L,NA_integer_,1L,2L,NA_integer_),levels=c('z',NA),class='factor'),
  ordered = ordered(c('z','a','z',NA,'a','z'),levels=c('z','a')),
  list = list(NULL, NA_integer_, 1L, NULL, 1L, NA_integer_),
  packed = tibble::tibble(a=c(2L,1L,NA,NA,1L,2L), b=c(1L,NA,2L,NA,NA,1L)),
  matrix = matrix(c(2L,1L,NA,NA,1L,2L,1L,NA,2L,NA,NA,1L),6L),
  date = as.Date(c('2026-01-02','2026-01-01',NA,'2026-01-01','2026-01-02',NA)),
  posixct = as.POSIXct(c('2026-01-02','2026-01-01',NA,'2026-01-01','2026-01-02',NA),tz='UTC'),
  difftime = as.difftime(c(2,1,NA,1,2,NA),units='hours')
)
sets <- list(c('g','h'),'g','h',character())
results <- list()
missing <- function(x) vctrs::vec_detect_missing(x)
left <- if(mixed) setdiff(names(family),c("list","packed","matrix")) else names(family)
for (a in left) for (b in names(family)) {
  df <- tibble::tibble(f=c('y','x','x',NA,'y','x'),g=family[[a]],h=family[[b]],rid=seq_len(6),payload=tibble::tibble(f=factor(c('a','b',NA,'a','b',NA),exclude=NULL),x=seq_len(6)))
  serial <- serialize(df,NULL)
  expected <- list()
  for(sid in seq_along(sets)) {
    keys <- c('f',sets[[sid]])
    groups <- vctrs::vec_group_loc(df[keys])$loc
    for(rows in groups) expected[[length(expected)+1L]] <- list(sid=sid,rows=rows)
  }
  for (keep in c(FALSE,TRUE)) for (verb in c('nest_with_margins','nest_by_with_margins')) for (sort in c('none','last','first')) {
    z <- tryCatch(do.call(get(verb),list(.data=df,.by=rlang::expr(f),.grouping=cube(g,h),.margin_label=if(mixed)list(g="Total",h=NULL)else NULL,.keep=keep,.id='sid',.sort=sort)),error=identity)
    # cube enumerates detail, g, h, total on this tested contract.
    status <- 'match'; message <- ''
    if(inherits(z,'error')) { status <- 'error'; message <- conditionMessage(z) }
    else {
      actual <- vapply(seq_len(nrow(z)),function(i)paste(z$sid[[i]],paste(z$data[[i]]$rid,collapse=','),sep=':'),character(1))
      expected_ids <- vapply(expected,function(e)paste(e$sid,paste(df$rid[e$rows],collapse=','),sep=':'),character(1))
      if(!identical(sort(actual),sort(expected_ids))) {status <- 'membership';message <- paste(actual,collapse=';')}
      if(status=='match') for(i in seq_len(nrow(z))) {
        rows <- z$data[[i]]$rid
        cols <- c(if(keep)c('f','g','h'),'rid','payload')
        wanted <- vctrs::vec_slice(df[cols],rows)
        if(!identical(names(z$data[[i]]),cols) || !all(vapply(cols,function(col)vctrs::vec_equal(z$data[[i]][[col]],wanted[[col]],na_equal=TRUE)|>all(),logical(1)))) {status <- 'payload';message <- paste('row',i);break}
      }
      if(status=='match' && sort!='none') {
        # Construct the public ordering key independently, using only source-set
        # membership and visible columns. Whole-row missingness uses vctrs.
        ord <- dplyr::ungroup(z)[c('f','g','h','sid')]
        ord <- tibble::as_tibble(ord)
        ord$.mf <- missing(ord$f)
        ord$.bg <- as.integer(!vapply(ord$sid,function(id)'g'%in%sets[[id]],logical(1)))
        ord$.bh <- as.integer(!vapply(ord$sid,function(id)'h'%in%sets[[id]],logical(1)))
        if(sort=='first'){ord$.bg <- -ord$.bg;ord$.bh <- -ord$.bh}
        ord$.mg <- missing(ord$g);ord$.mh <- missing(ord$h)
        ord$.row <- seq_len(nrow(ord))
        wanted <- dplyr::arrange(ord,.mf,f,.bg,.mg,g,.bh,.mh,h,sid)$.row
        if(!identical(wanted,seq_len(nrow(z)))) {status <- 'order';message <- paste(wanted,collapse=',')}
      }
    }
    if(!identical(serial,serialize(df,NULL))) status <- 'mutation'
    results[[length(results)+1L]] <- data.frame(g=a,h=b,keep,verb,sort,status,message)
  }
}
r <- do.call(rbind,results)
write.csv(r,file.path(out,if(mixed) 'multikey-mixed-results.csv' else 'multikey-results.csv'),row.names=FALSE)
print(table(r$status));print(unique(r[r$status!='match',]),row.names=FALSE)
stopifnot(all(r$status=='match'))
