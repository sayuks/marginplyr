dir.create('/tmp/marginplyr-root-deep-hunt', showWarnings = FALSE)
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
values <- list(
  date = as.Date(c('2020-01-02', NA, '2020-01-01')),
  posixlt = as.POSIXlt(c('2020-01-02', NA, '2020-01-01'), tz = 'Asia/Tokyo'),
  posixct = as.POSIXct(c('2020-01-02', NA, '2020-01-01'), tz = 'Asia/Tokyo'),
  yearmon = zoo::as.yearmon(c(2, NA, 1)), yearqtr = zoo::as.yearqtr(c(2, NA, 1)),
  ts = stats::ts(c(2, NA, 1)),
  surv = survival::Surv(c(2, 3, 1), c(1, 0, 1)),
  difftime = as.difftime(c(2, NA, 1), units = 'hours'), hms = hms::hms(c(2, NA, 1)),
  units = units::set_units(c(2, NA, 1), 'km'),
  labelled = haven::labelled(c(2, haven::tagged_na('a'), 1), c(x = 1, y = 2)),
  ip = structure(c(2L, NA_integer_, 1L), class = 'AsIs'),
  named = c(z = 2, y = NA, x = 1),
  complex = c(2+3i, NA_complex_, 1-2i),
  blob = blob::blob(as.raw(2), NULL, as.raw(1)),
  nested = tibble(a = c(2, NA, 1), b = c('a', NA, 'z')),
  mat = matrix(c(2, NA, 1, 5, NA, 3), ncol = 2),
  arr = array(c(2, NA, 1, 5, NA, 3, 9, NA, 1, 6, NA, 3), dim = c(3,2,2)),
  packed_zero = vctrs::new_data_frame(list(), n = 3L)
)
res <- list(); n <- 0L
for (nm in names(values)) for (empty in c(FALSE, TRUE)) for (lab in list(NULL, NA_character_, 'TOTAL')) for (sort in c('none','first','last')) for (role in c('dim','fixed','payload')) {
  n <- n + 1L
  x <- tibble(g = c('b','a',NA), rid = 1:3); x$key <- values[[nm]]
  if (empty) x <- x[0,]
  oracle <- tryCatch({
    if (role == 'dim') {
      a <- x; b <- x
      if (is.null(lab) || is.na(lab)) b$key <- vctrs::vec_init(x$key, nrow(x)) else { a$key <- as.character(x$key); b$key <- rep(lab, nrow(x)) }
      a$sid <- rep(1L, nrow(x)); b$sid <- rep(2L, nrow(x))
      union_all(a,b)
    } else {
      a <- x; b <- x
      if (is.null(lab) || is.na(lab)) b$g <- rep(NA_character_, nrow(x)) else b$g <- rep(lab, nrow(x))
      a$sid <- rep(1L, nrow(x)); b$sid <- rep(2L, nrow(x))
      union_all(a,b)
    }
  }, error = identity)
  got <- tryCatch({
    if (role == 'dim') expand_with_margins(x, .grouping=rollup(key), .margin_label=lab, .sort=sort, .id='sid')
    else if (role == 'fixed') expand_with_margins(x, .by=key, .grouping=rollup(g), .margin_label=lab, .sort=sort, .id='sid')
    else expand_with_margins(x, .grouping=rollup(g), .margin_label=lab, .sort=sort, .id='sid')
  }, error = identity)
  verdict <- if (inherits(got,'error')) paste('ERROR', class(got)[1], conditionMessage(got)) else if (inherits(oracle,'error')) 'ORACLE_ERROR' else {
    got <- as_tibble(got)[order(got$sid, got$rid),names(oracle)]
    if (isTRUE(all.equal(got, oracle, check.attributes=TRUE))) 'PASS' else paste('DIFF', paste(all.equal(got, oracle), collapse=';'))
  }
  res[[n]] <- data.frame(type=nm, empty=empty, lab=if(is.null(lab)) 'NULL' else lab, sort=sort, role=role, verdict=verdict)
}
out <- as_tibble(bind_rows(res)); saveRDS(out,'/tmp/marginplyr-root-deep-hunt/local-types.rds')
print(count(out, type, sub('ERROR.*', 'ERROR', sub('DIFF.*','DIFF',verdict))), n=Inf)
print(filter(out, startsWith(verdict,'DIFF')), n=Inf)
