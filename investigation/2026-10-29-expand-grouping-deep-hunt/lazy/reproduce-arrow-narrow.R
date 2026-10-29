# Investigated 2026-10-29 against ee43217 main. No implementation fix.
# Run from a marginplyr repository with: Rscript <this-script> [--assert]
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all('.',quiet=TRUE)
assert <- '--assert' %in% commandArgs(trailingOnly=TRUE)
fixture <- function(part){
path<-tempfile();dir.create(path)
for(j in 1:2){p<-file.path(path,paste0(part,'=',j));dir.create(p);arrow::write_parquet(data.frame(g=10L-j,rid=j),file.path(p,'f.parquet'))}
part_spec<-arrow::hive_partition(!!!setNames(list(arrow::int32()),part))
full<-arrow::open_dataset(path,partitioning=part_spec,factory_options=list(selector_ignore_prefixes=character()))
narrow<-arrow::open_dataset(path,schema=arrow::schema(g=arrow::int32(),rid=arrow::int32()),partitioning=part_spec,factory_options=list(selector_ignore_prefixes=character()))
list(path=path,full=full,narrow=narrow)
}
canon <- function(x){x<-as.data.frame(x);x<-x[order(x$rid,is.na(x$g),x$g),,drop=FALSE];rownames(x)<-NULL;x}
fixtures<-lapply(c('sid','..marginplyr_arrow_dimension_1','..marginplyr_sort_1'),fixture)
all_equal<-logical()
for(i in seq_along(fixtures)){
f<-fixtures[[i]];x<-f$narrow;id<-if(i==3L)NULL else 'sid';sort<-if(i==3L)'last' else 'none'
raw<-collect(x);stopifnot(identical(sort(raw$g),8:9),length(f$full$files)==2L,identical(names(x),c('g','rid')))
expected<-expand_with_margins(raw,.grouping=rollup(g),.margin_label=NULL,.id=id,.sort=sort)
actual<-collect(expand_with_margins(x,.grouping=rollup(g),.margin_label=NULL,.id=id,.sort=sort))
projection_control<-collect(expand_with_margins(select(f$full,g,rid),.grouping=rollup(g),.margin_label=NULL,.id=id,.sort=sort))
computed_control<-collect(expand_with_margins(compute(x),.grouping=rollup(g),.margin_label=NULL,.id=id,.sort=sort))
compare<-function(a,b){identical(canon(a),canon(b))&&(sort=='none'||identical(a$g,b$g))}
stopifnot(compare(projection_control,expected),compare(computed_control,expected))
cat('\nScenario',i,'narrow Dataset:\n');print(raw)
cat('Actual expansion:\n');print(actual)
cat('Expected explicit-collection control:\n');print(expected)
all_equal<-c(all_equal,compare(actual,expected))
}
# An included-only result and ordinary Arrow union over narrow sid both pass.
x<-fixtures[[1L]]$narrow
included<-collect(expand_with_margins(x,.grouping=grouping_set(g),.margin_label=NULL,.id='sid'))
stopifnot(identical(included$sid,rep(1L,2L)))
ordinary<-collect(union_all(mutate(x,g=NA_integer_,sid=2L),mutate(x,sid=1L)))
stopifnot(identical(canon(ordinary)[c('g','sid','rid')],canon(expand_with_margins(collect(x),.grouping=rollup(g),.margin_label=NULL,.id='sid'))))
cat('Projection/materialization/included-only/ordinary-union controls passed.\n')
for(f in fixtures)unlink(f$path,recursive=TRUE)
if(assert)stopifnot(all(all_equal))
