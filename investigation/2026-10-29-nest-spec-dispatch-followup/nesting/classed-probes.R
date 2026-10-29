args <- commandArgs(TRUE)
pkgload::load_all(if(length(args))args[[1]] else ".",quiet=TRUE)
classes <- list(
  integer64=bit64::as.integer64(c("9007199254740992","9007199254740993",NA)),
  hms=hms::hms(c(1,2,NA)),
  units=units::set_units(c(1,2,NA),"m"),
  blob=blob::blob(as.raw(1),as.raw(2),NULL),
  zoo=zoo::zoo(c(1,2,NA)),
  yearmon=zoo::as.yearmon(c("2026-01","2026-02",NA)),
  yearqtr=zoo::as.yearqtr(c("2026 Q1","2026 Q2",NA)),
  interval=lubridate::interval(lubridate::ymd(c("2026-01-01","2026-02-01",NA)),lubridate::ymd(c("2026-02-01","2026-03-01",NA))),
  period=lubridate::period(month=c(1,2,NA)),
  duration=lubridate::duration(c(1,2,NA)),
  labelled=haven::labelled(c(1,2,NA),c(one=1,two=2)),
  tagged=haven::labelled(c(1,haven::tagged_na("a"),haven::tagged_na("b"))),
  labelled_spss=haven::labelled_spss(c(1,2,99),na_values=99),
  list_of=vctrs::list_of(1L,2L,NA_integer_),
  AsIs=I(c(1,2,NA)),
  list_date=I(list(as.Date("2026-01-01"),as.Date("2026-01-02"),NULL))
)
errors <- list(); differences <- list(); membership_differences <- list(); comparisons <- calls <- 0L
for (nm in names(classes)) for (role in c("fixed","dimension","payload")) {
  value <- classes[[nm]]
  x <- tibble::tibble(fixed=c("x","x",NA), g=c("a","b",NA), payload=1:3,row_id=1:3)
  x[[switch(role,fixed="fixed",dimension="g",payload="payload")]] <- value
  for(keep in c(FALSE,TRUE))for(verb in c("nest_with_margins","nest_by_with_margins"))for(label in list(NULL,"Total")) {
    spec <- rollup(g)
    invoke <- function(data) (get(verb))(data,.by=fixed,.grouping=spec,.margin_label=label,.keep=keep,.id="set")
    calls <- calls+2L
    local <- tryCatch(invoke(x),error=identity)
    dt <- tryCatch(dplyr::collect(invoke(dtplyr::lazy_dt(x))),error=identity)
    if(inherits(local,"error")||inherits(dt,"error")) {
      errors[[length(errors)+1L]] <- list(nm=nm,role=role,keep=keep,verb=verb,label=label,
        local=if(inherits(local,"error"))conditionMessage(local) else "OK",
        dt=if(inherits(dt,"error"))conditionMessage(dt) else "OK")
      next
    }
    # Compare cells by source row ids; set id identifies their plan occurrence.
    canon <- function(z) {
      z <- dplyr::ungroup(z)[c("set","data")]
      keys <- vapply(seq_len(nrow(z)),function(i)paste(z$set[i],paste(z$data[[i]]$row_id,collapse=","),sep=":"),character(1))
      z[order(keys),,drop=FALSE]
    }
    a <- canon(local); b <- canon(dt)
    if(nrow(a)!=nrow(b) || !identical(a$set,b$set) || !identical(lapply(a$data,function(z)z$row_id),lapply(b$data,function(z)z$row_id))) {
      membership_differences[[length(membership_differences)+1L]] <- list(nm=nm,role=role,keep=keep,verb=verb,label=label,a=a,b=b)
      next
    }
    for(i in seq_len(nrow(a))) {
      ac <- as.list(a$data[[i]]); bc <- as.list(b$data[[i]])
      if(!identical(ac,bc)) {
        differences[[length(differences)+1L]] <- list(nm=nm,role=role,keep=keep,verb=verb,label=label,ac=ac,bc=bc)
      }
    }
    comparisons <- comparisons+1L
  }
}
cat("calls",calls,"comparisons",comparisons,"error pairs",length(errors),"\n")
stopifnot(all(vapply(errors,function(z) z$nm %in% c("blob","zoo","list_of","list_date"),logical(1))))
stopifnot(all(vapply(membership_differences,function(z)identical(z$nm,"period"),logical(1))))
stopifnot(all(vapply(differences,function(z)z$nm %in% c("interval","period"),logical(1))))
print(table(vapply(errors,function(z)paste(z$nm,z$role,sep=":"),character(1))))
cat("cell differences",length(differences),"\n")
print(table(vapply(differences,function(z)paste(z$nm,z$role,sep=":"),character(1))))

cat("membership differences",length(membership_differences),"\n")
print(table(vapply(membership_differences,function(z)paste(z$nm,z$role,sep=":"),character(1))))

cat("PASS: no difference outside the separately controlled S4/list/zoo limits\n")
