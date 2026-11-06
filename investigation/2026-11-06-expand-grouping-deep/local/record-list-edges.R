Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
library(dplyr)
new_pair <- function(n) {
  vctrs::new_rcrd(list(
    left = rep(c(1L,NA_integer_,3L),length.out=n),
    right = rep(c(NA_character_,"b","a"),length.out=n)
  ),class="hunt_pair")
}
format.hunt_pair <- function(x, ...) paste(vctrs::field(x,"left"),vctrs::field(x,"right"))
families <- list(
  rcrd=new_pair,
  list_raw=function(n) vctrs::list_of(!!!rep(list(as.raw(0:1),NULL),length.out=n),.ptype=raw()),
  list_frame=function(n) vctrs::list_of(!!!rep(list(tibble(a=1L,b="x"),NULL),length.out=n),.ptype=tibble(a=integer(),b=character())),
  list_matrix=function(n) rep(list(matrix(c(1L,NA),1L),NULL),length.out=n),
  asis_list=function(n) I(rep(list(NULL,NA_integer_,tibble(a=1L)),length.out=n)),
  asis_factor=function(n) I(factor(rep(c("a",NA,"b"),length.out=n),levels=c("b","a",NA),exclude=NULL)),
  objects=function(n) rep(list(quote(x+y),new.env(parent=emptyenv()),function() 1L),length.out=n)
)
records <- list(); index <- 0L
for (family in names(families)) for(n in c(0L,1L,3L)) for(depth in 0:2) for(role in c("dimension","fixed","payload")) for(sort in c("none","first","last")) {
  index <- index + 1L
  x <- tryCatch({
    g <- families[[family]](n)
    for(i in seq_len(depth)) g <- tibble(inner=g,extra=rep(c(NA,1L,2L),length.out=n))
    tibble(g=g,f=rep(c(NA,1L,2L),length.out=n),rid=seq_len(n))
  },error=identity)
  ordinary <- tryCatch({
    if(inherits(x,"error")) stop(x)
    y <- if(role=="dimension") mutate(x,g=!!vctrs::vec_init(x$g,n)) else mutate(x,f=NA_integer_)
    z <- union_all(mutate(x,set=1L),mutate(y,set=2L))
    dims <- if(role=="dimension") "g" else "f"
    fixed <- if(role=="fixed") "g" else character()
    if(sort!="none") {
      terms <- list()
      for(k in fixed) terms <- c(terms,list(vctrs::vec_detect_missing(z[[k]]),z[[k]]))
      terms <- c(terms,list(if(sort=="first") -as.integer(z$set==2L) else as.integer(z$set==2L),vctrs::vec_detect_missing(z[[dims]]),z[[dims]],z$set))
      z <- vctrs::vec_slice(z,vctrs::vec_order(vctrs::new_data_frame(setNames(terms,paste0("k",seq_along(terms))))))
    }
    select(z,all_of(c(fixed,dims,"set",setdiff(names(x),c(fixed,dims)))))
  },error=identity)
  actual <- tryCatch({
    if(inherits(x,"error")) stop(x)
    if(role=="dimension") expand_with_margins(x,.grouping=rollup(g),.margin_label=NULL,.id="set",.sort=sort)
    else if(role=="fixed") expand_with_margins(x,.by=g,.grouping=rollup(f),.margin_label=NULL,.id="set",.sort=sort)
    else expand_with_margins(x,.grouping=rollup(f),.margin_label=NULL,.id="set",.sort=sort)
  },error=identity)
  status <- if(inherits(x,"error")) "fixture-error" else if(inherits(ordinary,"error")) "ordinary-error" else if(inherits(actual,"error")) "package-error" else if(identical(actual,ordinary)) "match" else "difference"
  detail <- if(inherits(actual,"error")) conditionMessage(actual) else if(inherits(ordinary,"error")) conditionMessage(ordinary) else if(status=="difference") paste(capture.output(all.equal(actual,ordinary)),collapse="; ") else ""
  ordinary_message <- if(inherits(ordinary,"error")) conditionMessage(ordinary) else ""
  actual_message <- if(inherits(actual,"error")) conditionMessage(actual) else ""
  records[[index]] <- data.frame(family,n,depth,role,sort,status,detail,ordinary_message,actual_message)
}
results <- bind_rows(records)
path <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT",tempfile("record-list-edges-",fileext=".csv"))
write.csv(results,path,row.names=FALSE)
print(count(results,status)); print(filter(results,status%in%c("package-error","difference")),width=200)
cat("Output:",path,"\n")
sessionInfo()
