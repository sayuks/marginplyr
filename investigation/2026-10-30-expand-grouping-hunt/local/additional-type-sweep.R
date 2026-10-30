suppressPackageStartupMessages(pkgload::load_all('.', quiet = TRUE))
probe <- function(expr) tryCatch(force(expr), error = identity)
types <- list(
  posixlt = as.POSIXlt(c('2026-01-02', NA, '2026-01-01'), tz = 'Asia/Tokyo'),
  yearmon = zoo::as.yearmon(c('2026-02', NA, '2026-01')),
  yearqtr = zoo::as.yearqtr(c('2026 Q2', NA, '2026 Q1')),
  labelled = haven::labelled(c(2, NA, 1), labels = c(two = 2, one = 1)),
  labelled_spss = haven::labelled_spss(c(2, NA, 1), labels = c(two = 2), na_values = 1),
  surv = survival::Surv(c(2, NA, 1), c(1, NA, 0)),
  ts = stats::ts(c(2, NA, 1), start = 2000, frequency = 4),
  table = as.table(array(c(2L, NA_integer_, 1L), dim = 3L)),
  complex = c(2 + 1i, NA_complex_, 1 - 3i),
  raw = as.raw(c(2L, 0L, 1L)),
  expression = expression(x + y, NULL, 'a'),
  distribution = distributional::dist_normal(c(2, NA, 1), 1),
  rvar = posterior::rvar(matrix(c(2, NA, 1, 3, NA, 2), nrow = 2)),
  timeDate = timeDate::timeDate(c('2026-02-01', NA, '2026-01-01'))
)
for (kind in c('Date', 'POSIXct', 'difftime', 'units', 'integer64')) {
 x <- switch(kind, Date=as.Date(c('2026-01-02', NA, '2026-01-01')), POSIXct=as.POSIXct(c('2026-01-02', NA, '2026-01-01'),tz='UTC'), difftime=as.difftime(c(2, NA, 1),units='hours'), units=units::set_units(c(2, NA, 1),'m'), integer64=bit64::as.integer64(c('9007199254740995',NA,'9007199254740993')))
 dim(x) <- c(3L, 1L)
 types[[paste0(kind, '_matrix')]] <- x
}
results <- list(); ix <- 0L
for (kind in names(types)) for (size in c(0L, 1L, 3L)) {
 vals <- probe(vctrs::vec_slice(types[[kind]], seq_len(size)))
 if(inherits(vals,'error')) {cat('slice failure:',kind,conditionMessage(vals),'\n');next}
 for (role in c('dimension','fixed','payload')) for (label_name in c('NULL','NA','text')) for (sort in c('none','first','last')) {
  label <- if (label_name == 'NULL') NULL else if (label_name == 'NA') NA_character_ else 'All'
  d <- tibble::tibble(g = c('b',NA,'a')[seq_len(size)], rid = seq_len(size))
  col <- switch(role, dimension='g', fixed='f', payload='v')
  built <- probe({d[[col]] <- vals; d})
  if(inherits(built,'error')) next
  d <- built
  by <- if(role=='fixed') rlang::quo(f) else rlang::quo(NULL)
  actual <- probe(expand_with_margins(d, .by=!!by, .grouping=rollup(g), .margin_label=label, .check_margin_label=FALSE, .id='sid', .sort=sort))
  control <- probe({
    a <- d; b <- d
    if(is.null(label)||is.na(label)) b$g <- vctrs::vec_init(d$g,size) else {a$g<-as.character(d$g);b$g<-rep(label,size)}
    a$sid <- rep(1L,size);b$sid<-rep(2L,size)
    z <- dplyr::union_all(a,b)
    dplyr::relocate(z, dplyr::all_of(c(if(role=='fixed')'f','g','sid')))
  })
  verdict <- if(inherits(control,'error')) 'ordinary_failure' else if(inherits(actual,'marginplyr_error')) 'package_refusal' else if(inherits(actual,'error')) 'package_error' else {
   aa<-actual[order(actual$sid,actual$rid),,drop=FALSE]
   bb<-control[order(control$sid,control$rid),,drop=FALSE]
   if(identical(aa,bb)) 'match' else 'difference'
  }
  ix<-ix+1L
  results[[ix]] <- data.frame(kind,size,role,label_name,sort,verdict,detail=if(inherits(actual,'error'))conditionMessage(actual)else if(verdict=='difference')paste(all.equal(aa,bb),collapse='; ')else'')
 }
}
res<-do.call(rbind,results)
print(with(res, table(kind,verdict)))
print(table(res$verdict))
output <- Sys.getenv('MARGINPLYR_HUNT_OUTPUT', unset = tempdir())
dir.create(output, recursive = TRUE, showWarnings = FALSE)
write.csv(res, file.path(output, 'additional-type-sweep.csv'), row.names = FALSE)
