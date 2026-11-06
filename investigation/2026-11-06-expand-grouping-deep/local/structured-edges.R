Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
library(dplyr)
out <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempfile("structured-edges-"))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
factories <- list(
  integer = function(n) rep(c(2L, NA_integer_, 1L), length.out=n),
  logical = function(n) rep(c(TRUE, NA, FALSE), length.out=n),
  double = function(n) rep(c(NaN, NA_real_, -Inf, Inf, 1, 0), length.out=n),
  complex = function(n) rep(c(1+2i, NA_complex_, complex(real=NA, imaginary=1)), length.out=n),
  character = function(n) rep(c("b", NA_character_, "a"), length.out=n),
  list = function(n) rep(list(NULL, NA, 2L), length.out=n),
  factor = function(n) factor(rep(c("b", NA, "a"), length.out=n), levels=c("b","a",NA), exclude=NULL),
  ordered = function(n) ordered(rep(c("b", NA, "a"), length.out=n), levels=c("b","a",NA), exclude=NULL),
  date = function(n) as.Date(rep(c(1,NA,2), length.out=n), origin="1970-01-01"),
  datetime = function(n) as.POSIXct(rep(c(1,NA,2), length.out=n), origin="1970-01-01",tz="Pacific/Apia")
)
records <- list(); count <- 0L
for (type in names(factories)) for (n in c(0L,1L,3L)) for (tail in list(integer(), 0L, 1L, 2L, c(0L,2L),c(2L,0L),c(2L,2L))) for (named in c(FALSE,TRUE)) for (packed in 0:2) {
  id <- paste(type,n,paste(tail,collapse="x"),named,packed,sep="/")
  fixture <- tryCatch({
    cells <- n * prod(tail)
    g <- factories[[type]](cells)
    dim(g) <- c(n,tail)
    if (named) dimnames(g) <- lapply(dim(g), function(k) paste0("part",seq_len(k),recycle0=TRUE))
    for (j in seq_len(packed)) g <- tibble(inner=g, keeper=seq_len(n))
    tibble(g=g, f=rep(c(NA,2L,1L),length.out=n), rid=seq_len(n))
  },error=identity)
  for (role in c("dimension","fixed","payload")) for (sort in c("none","first","last")) {
    count <- count + 1L
    if (inherits(fixture,"error")) {
      records[[count]] <- data.frame(id,role,sort,status="fixture-error",detail=conditionMessage(fixture)); next
    }
    x <- fixture
    ordinary <- tryCatch({
      y <- if (role=="dimension") mutate(x,g=!!vctrs::vec_init(x$g,n)) else mutate(x,f=NA_integer_)
      z <- union_all(mutate(x,set=1L),mutate(y,set=2L))
      dims <- if (role=="dimension") "g" else "f"
      fixed <- if (role=="fixed") "g" else character()
      if (sort!="none") {
        terms <- list()
        for (key in fixed) terms <- c(terms,list(vctrs::vec_detect_missing(z[[key]]),z[[key]]))
        terms <- c(terms,list(if(sort=="first") -as.integer(z$set==2L) else as.integer(z$set==2L),vctrs::vec_detect_missing(z[[dims]]),z[[dims]],z$set))
        z <- vctrs::vec_slice(z,vctrs::vec_order(vctrs::new_data_frame(setNames(terms,paste0("k",seq_along(terms))))))
      }
      select(z,all_of(c(fixed,dims,"set",setdiff(names(x),c(fixed,dims)))))
    },error=identity)
    actual <- tryCatch({
      if (role=="dimension") expand_with_margins(x,.grouping=rollup(g),.margin_label=NULL,.id="set",.sort=sort)
      else if (role=="fixed") expand_with_margins(x,.by=g,.grouping=rollup(f),.margin_label=NULL,.id="set",.sort=sort)
      else expand_with_margins(x,.grouping=rollup(f),.margin_label=NULL,.id="set",.sort=sort)
    },error=identity)
    status <- if (inherits(ordinary,"error")) "ordinary-error" else if (inherits(actual,"error")) "package-error" else if (identical(actual,ordinary)) "match" else "difference"
    detail <- if (inherits(actual,"error")) conditionMessage(actual) else if (inherits(ordinary,"error")) conditionMessage(ordinary) else if(status=="difference") paste(capture.output(all.equal(actual,ordinary)),collapse="; ") else ""
    ordinary_message <- if(inherits(ordinary,"error")) conditionMessage(ordinary) else ""
    actual_message <- if(inherits(actual,"error")) conditionMessage(actual) else ""
    records[[count]] <- data.frame(id,role,sort,status,detail,ordinary_message,actual_message)
    if (status%in%c("difference","package-error")) saveRDS(list(input=x,ordinary=ordinary,actual=actual),file.path(out,paste0(count,".rds")))
  }
}
results <- bind_rows(records)
write.csv(results,file.path(out,"results.csv"),row.names=FALSE)
print(count(results,status))
print(head(filter(results,status%in%c("difference","package-error")),30),width=200)
cat("Output:",out,"\n")
sessionInfo()
