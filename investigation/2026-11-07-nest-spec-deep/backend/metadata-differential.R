Sys.setenv(TESTTHAT_PARALLEL = "false")
source_root <- Sys.getenv("MARGINPLYR_HUNT_SOURCE", unset = getwd())
pkgload::load_all(source_root, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
out <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", unset = dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value=TRUE))))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
rows <- list(); k <- 0L
record <- function(root, shape, n, pred, slot, kind, message = "") {
 k <<- k+1L; rows[[k]] <<- data.frame(root,shape,n,pred,slot,kind,message)
}
types <- list(
 integer = 1:4, double = c(1,NA,NaN,Inf), logical = c(TRUE,FALSE,NA,TRUE),
 character = c("a",NA,"b","a"), raw = as.raw(1:4), complex = as.complex(1:4),
 factor = factor(c("a","b",NA,"a"), levels=c("a","b","unused",NA), exclude=NULL),
 ordered = ordered(c("a","b",NA,"a"), levels=c("a","b","unused")),
 date = as.Date("2020-01-01")+0:3,
 posix = as.POSIXct("2020-01-01",tz="Asia/Tokyo")+0:3,
 difftime = as.difftime(1:4,units="hours"),
 attributed = structure(1:4, unit="widgets"),
 named = setNames(1:4, letters[1:4]),
 matrix = matrix(1:8,4,dimnames=list(letters[1:4],c("left","right"))),
 list = list(1L,NULL,"a",list(x=1)),
 packed = data.frame(left=1:4,right=letters[1:4])
)
predicates <- list(character=is.character, integer=is.integer, numeric=is.numeric,
 object=is.object, list=is.list, atomic=is.atomic, factor=is.factor,
 attributed=function(x)!is.null(attr(x,"unit",exact=TRUE)),
 dimensional=function(x)!is.null(dim(x)),
 date=function(x)inherits(x,"Date"),
 empty=function(x)length(x)==0L)
# The empty predicate deliberately uses metadata only for ordinary atomic
# vectors; packed/matrix length differs by shape and is independently compared.
for (nm in names(types)) for (n in c(0L,1L,4L)) {
 x <- data.table::data.table(key_chr=c("a","b","a","b")[seq_len(n)], v=seq_len(n))
 value <- types[[nm]]
 if (is.data.frame(value)) value <- value[seq_len(n),,drop=FALSE]
 else if (!is.null(dim(value))) value <- value[seq_len(n),,drop=FALSE]
 else value <- value[seq_len(n)]
 data.table::set(x,j="typed",value=list(value))
 before <- serialize(x,NULL)
 count <- 0L
 probe <- function(z){count <<- count+length(z); rep(TRUE,length(z))}
 z <- dtplyr::lazy_dt(x, immutable=TRUE)
 builders <- list(root=function(z)z, filter=function(z)filter(z,probe(v)),
 rename=function(z)rename(z,renamed=key_chr), select=function(z)select(z,typed,v,key_chr),
 drop=function(z)select(z,-key_chr), relocate=function(z)relocate(z,typed),
 group=function(z)group_by(z,key_chr), cached=function(z)compute(z,name="hunt_cache"),
 arithmetic=function(z)mutate(z,w=v+1L), scalar_new=function(z)mutate(z,w=1L),
 scalar_alias=function(z)mutate(z,w=1L,u=w),
 overwrite=function(z)mutate(z,v=1L),
 arrange=function(z)arrange(z,key_chr),
 filter_arrange=function(z)arrange(filter(z,probe(v)),key_chr),
 cache_rename=function(z)rename(compute(z,name="hunt_cache"),renamed=key_chr),
 join=function(z)left_join(z,dtplyr::lazy_dt(data.frame(v=seq_len(n),other=rep(1L,n))),by="v"),
 union=function(z)union_all(z,z))
 for (shape in names(builders)) {
  input <- tryCatch(builders[[shape]](z),error=identity)
  if (inherits(input,"error")) {record(nm,shape,n,"-","-","construction_error",conditionMessage(input));next}
  native <- tryCatch(collect(input),error=identity)
  if(inherits(native,"error")){record(nm,shape,n,"-","-","native_error",conditionMessage(native));next}
  for (pn in names(predicates)) for(slot in c("dimension","fixed")) {
   pred <- predicates[[pn]]
   if (pn=="empty") next # source values/length are not the metadata contract
   count <- 0L
   # Native collection supplies column types, not source values, to the oracle.
   oracle_proxy <- native[0,,drop=FALSE]
   selected <- names(oracle_proxy)[vapply(oracle_proxy,pred,logical(1))]
   if (length(group_vars(input)) && slot=="fixed") next
   got <- tryCatch(if(slot=="dimension") inspect_grouping(input,.grouping=grouping_set(where(pred)),.format="list") else inspect_grouping(input,.by=where(pred),.format="list"),error=identity)
   if (inherits(got,"marginplyr_error") && grepl("safely determine column metadata",conditionMessage(got),fixed=TRUE)) {
    record(nm,shape,n,pn,slot,"safe_refusal")
   } else if(inherits(got,"marginplyr_error") && slot=="dimension" &&
             length(intersect(selected,group_vars(input))) &&
             grepl("Columns cannot appear in both",conditionMessage(got),fixed=TRUE)) {
    record(nm,shape,n,pn,slot,"structural_refusal",conditionMessage(got))
   } else if(inherits(got,"error")) record(nm,shape,n,pn,slot,"error",conditionMessage(got))
   else {
    expected <- if(slot=="dimension") setdiff(selected,group_vars(input)) else selected
    actual <- if(slot=="dimension") got$included[[1L]] else got$fixed[[1L]]
    ok <- identical(actual,expected)
    record(nm,shape,n,pn,slot,if(ok)"match" else "mismatch",if(ok)"" else paste("expected",paste(expected,collapse=","),"actual",paste(actual,collapse=",")))
   }
   stopifnot(count==0L, identical(serialize(x,NULL),before))
  }
 }
}
result <- do.call(rbind,rows)
write.csv(result,file.path(out,"metadata-differential.csv"),row.names=FALSE)
print(table(result$kind)); print(subset(result,kind %in% c("mismatch","error")),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(out,"environment.txt"))
stopifnot(!any(result$kind %in% c("mismatch","error","construction_error")))
