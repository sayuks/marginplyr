# Investigation evidence, 2026-10-26. Run from the repository root.
pkgload::load_all(quiet = TRUE)
set.seed(830)
types <- list(
 chr = c("a", NA, "b"), int = c(2L, NA_integer_, 1L), dbl = c(2, NaN, NA_real_),
 log = c(TRUE, NA, FALSE), raw = as.raw(c(0, 1, 255)), cpl = c(2+3i, NA_complex_, 1+1i),
 fct = factor(c("b", NA, "a"), levels=c("unused","a","b")),
 ord = ordered(c("b", NA, "a"), levels=c("unused","a","b")),
 fct_na = factor(c("a", NA, "b"), levels=c("a",NA,"b"), exclude=NULL),
 date = as.Date(c("2026-01-01",NA,"2026-01-02")),
 posixct = as.POSIXct(c("2026-01-01",NA,"2026-01-02"),tz="Asia/Tokyo"),
 difftime = as.difftime(c(2,NA,1),units="hours"),
 list = list(2L, NULL, 1L), list_of = vctrs::list_of(2L,NULL,1L),
 packed = tibble::tibble(a=c(2L, NA, 1L), b=c("x",NA,"y")),
 matrix = matrix(c(2L, NA, 1L, 4L, NA, 3L), ncol=2),
 array = array(c(2L, NA, 1L, 4L, NA, 3L), dim=c(3L,1L,2L))
)
res <- list(); count <- 0L; fails <- list()
for (type in names(types)) for (label_type in c("NULL","string","NA")) for (rows in list(integer(),1L,1:3)) for (sort in c("none","first","last")) {
 x <- vctrs::vec_slice(types[[type]], rows)
 d <- tibble::tibble(fixed=rep(1L,length(rows)),g=x,payload=seq_along(rows))
 label <- switch(label_type,`NULL`=NULL,string="All",`NA`=NA_character_)
 packed <- is.data.frame(x) || length(dim(x)) > 1L
 expected_reject <- (packed && label_type=="string") || (type=="fct_na" && label_type=="NA")
 got <- tryCatch(expand_with_margins(d,.by=fixed,.grouping=rollup(g),.margin_label=label,.id="set",.sort=sort),error=identity)
 count<-count+1L
 key<-paste(type,label_type,paste(rows,collapse=","),sort,sep="/")
 if(inherits(got,"error")) {
  if(!expected_reject) fails[[key]]<-conditionMessage(got)
 } else if(expected_reject) {fails[[key]]<-"expected refusal"} else {
  if(nrow(got)!=2*nrow(d)) fails[[key]]<-"bad nrow"
  detail<-dplyr::arrange(got[got$set==1L,,drop=FALSE],payload)
  omitted<-dplyr::arrange(got[got$set==2L,,drop=FALSE],payload)
  if(!identical(detail$payload,d$payload)||!identical(omitted$payload,d$payload)) fails[[key]]<-"bad payload"
  if(label_type=="string") {
   if(!identical(as.character(detail$g),as.character(d$g))) fails[[key]]<-"bad detail values"
   if(!identical(as.character(omitted$g),rep("All",length(rows)))) fails[[key]]<-"bad omitted values"
  } else {
   if(!identical(detail$g,d$g)) fails[[key]]<-"bad detail type/values"
   if(!identical(omitted$g,vctrs::vec_init(d$g,length(rows)))) fails[[key]]<-"bad omitted type/values"
  }
 }
}
cat("type cases:",count,"failures:",length(fails),"\n"); print(fails)
special_names <- c(".data", ".env", "T", "F", "NA", "TRUE", "FALSE", "..1", "..marginplyr_sort_1", "set_id", "col", "info", "new_levels", "missing_sentinel", "ord", "{x}", "x y", "x`y", "x\ny", "é", "☃")
failnames <- list()
for(nm in special_names) for(role in c("dimension","fixed","payload","id")) for(sort in c("none","last","first")) {
 d<-tibble::tibble(g=factor(c("b","a")),fixed=c(2L,1L),payload=1:2)
 id<-if(role=="id") nm else "set"
 if(role!="id") names(d)[match(role,c("dimension","fixed","payload"))]<-nm
 g<-names(d)[1]; by<-names(d)[2]
 call<-rlang::expr(expand_with_margins(d,.by=dplyr::all_of(!!by),.grouping=rollup(dplyr::all_of(!!g)),.id=!!id,.sort=!!sort))
 got<-tryCatch(rlang::eval_bare(call),error=identity)
 if(inherits(got,"error")) failnames[[paste(nm,role,sort,sep="/")]]<-conditionMessage(got)
}
cat("name cases:",length(special_names)*4*3,"failures:",length(failnames),"\n"); print(failnames)
