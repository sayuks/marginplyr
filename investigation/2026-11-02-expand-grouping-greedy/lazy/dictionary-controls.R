Sys.setenv(TESTTHAT_PARALLEL = "false")
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
a <- arrow::ChunkedArray$create(arrow::Array$create(factor(c("b","a"), levels = c("a","b"))), arrow::Array$create(factor(NA_character_, levels = c("b","a"))))
tbl <- arrow::Table$create(g = a, rid = 1:3, b = c("x","x","y"))
scratch <- tempfile("margin-dictionary-"); dir.create(scratch)
arrow::write_feather(tbl,file.path(scratch,"facts.arrow"))
ds <- arrow::open_dataset(file.path(scratch,"facts.arrow"),format="ipc")
shapes <- list(dataset_union=union_all(ds,filter(tbl,rid==2L)),dataset_join=left_join(tbl,select(ds,rid),by="rid"))
canon <- function(x) { x <- as.data.frame(x); x <- x[order(x$sid,x$rid),]; rownames(x)<-NULL; x }
for(shape in names(shapes)) for(role in c("dimension","payload","fixed")) {
 src <- shapes[[shape]]
 typ <- arrow::schema(src)$GetFieldByName("g")$type
 inc <- mutate(src,sid=1L)
 om <- if(role=="dimension")mutate(src,g=arrow::Scalar$create(NA,type=arrow::null())$cast(typ),sid=2L)else mutate(src,b=NA_character_,sid=2L)
 control <- canon(collect(union_all(inc,om)))
 q <- if(role=="dimension")expand_with_margins(src,.by=b,.grouping=rollup(g),.margin_label=NULL,.id="sid")else if(role=="fixed")expand_with_margins(src,.by=g,.grouping=rollup(b),.margin_label=NULL,.id="sid")else expand_with_margins(src,.grouping=rollup(b),.margin_label=NULL,.id="sid")
 actual <- canon(collect(q))[names(control)]
 cat(shape,role,"\n")
 print(all.equal(actual,control))
 cat("actual levels:",levels(actual$g),"; ordinary levels:",levels(control$g),"\n")
 actual$g <- as.character(actual$g); control$g <- as.character(control$g)
 stopifnot(identical(actual,control))
}
unlink(scratch,recursive=TRUE)
cat("All six normalized value/schema controls passed\n")
