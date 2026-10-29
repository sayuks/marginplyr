args <- commandArgs(TRUE)
pkgload::load_all(if(length(args)) args[[1]] else ".", quiet=TRUE)
x <- tibble::tibble(
  fixed=lubridate::interval(lubridate::ymd(c("2026-01-01","2026-02-01",NA)),lubridate::ymd(c("2026-02-01","2026-03-01",NA))),
  g=c("a","b",NA), row_id=1:3)
l <- dtplyr::lazy_dt(x)
sizes <- vapply(list(source=x,
  filtered=dplyr::collect(dplyr::filter(l,row_id==1)),
  unioned=dplyr::collect(dplyr::union_all(l,l))),
  function(z)c(rows=nrow(z),length=length(z$fixed),start=length(z$fixed@start)),integer(3))
print(sizes)
stopifnot(identical(unname(sizes[,2]),c(1L,1L,3L)),identical(unname(sizes[,3]),c(6L,6L,3L)))
y <- tibble::tibble(fixed=lubridate::period(month=c(1,2,NA)),row_id=1:3)
z <- dplyr::collect(dplyr::summarise(dtplyr::lazy_dt(y), ids=list(row_id),.by=fixed))
print(lapply(z$ids,identity))
stopifnot(identical(sort(vapply(z$ids,paste,character(1),collapse=",")),c("1,2","3")))
cat("PASS: Interval attribute corruption and Period grouping collapse reproduce without marginplyr\n")
for (value in list(blob::blob(as.raw(1),as.raw(2),NULL),
                   vctrs::list_of(1L,2L,NA_integer_),
                   I(list(as.Date("2026-01-01"),as.Date("2026-01-02"),NULL)))) {
  q <- dtplyr::lazy_dt(tibble::tibble(key=value,row_id=1:3))
  err <- tryCatch(dplyr::collect(dplyr::summarise(q,n=dplyr::n(),.by=key)),error=identity)
  stopifnot(inherits(err,"error"),grepl("type 'list'",conditionMessage(err),fixed=TRUE))
}
z <- tibble::tibble(value=zoo::zoo(c(1,2,NA)))
err <- tryCatch(dplyr::union_all(z,z),error=identity)
stopifnot(inherits(err,"error"),identical(conditionMessage(err),"indexes overlap"))
cat("PASS: list key refusals and zoo union refusal reproduce without marginplyr\n")
