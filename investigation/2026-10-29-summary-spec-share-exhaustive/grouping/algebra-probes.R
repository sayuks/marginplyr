suppressPackageStartupMessages(pkgload::load_all(quiet = TRUE))
options(lifecycle_verbosity = 'quiet', width = 120)
state <- new.env(parent = emptyenv())
state$checks <- 0L
state$plans <- 0L
state$duplicates <- 0L
state$failures <- list()
record <- function(label, truth, detail = NULL) {
  state$checks <- state$checks + 1L
  if (!isTRUE(truth)) state$failures[[label]] <- detail
}
observe <- function(expr) tryCatch(force(expr), error = identity)
columns <- c('a','b','c','d')
data <- stats::setNames(data.frame(v1=1:2,v2=3:4,v3=5:6,v4=7:8),columns)
subsets <- function(n) lapply(0:(2^n-1L),function(mask)columns[as.logical(intToBits(mask)[seq_len(n)])])
normalize <- function(family) {
 dimensions <- unique(unlist(family,use.names=FALSE))
 list(dimensions=dimensions,sets=lapply(family,function(x)dimensions[dimensions %in% x]))
}
inspect <- function(spec, policy='keep') inspect_grouping(data,.grouping=spec,.duplicates=policy,.format='list')
check <- function(spec, family, label) {
 state$plans <- state$plans+1L
 oracle <- normalize(family)
 observed <- observe(inspect(spec))
 if(inherits(observed,'error')) {
  record(paste(label,'accepted'),FALSE,conditionMessage(observed));return(invisible(NULL))
 }
 expected_bits <- lapply(oracle$sets,function(x)stats::setNames(as.integer(!oracle$dimensions %in% x),oracle$dimensions))
 if(!length(oracle$dimensions))expected_bits<-rep(list(integer()),length(family))
 expected_ids <- vapply(expected_bits,function(bits)as.integer(sum(bits*2^rev(seq_along(bits)-1L))),integer(1))
 record(paste(label,'sets'),identical(observed$included,oracle$sets),list(expected=oracle$sets,actual=observed$included))
 record(paste(label,'bits'),identical(observed$grouping_bits,expected_bits),list(expected=expected_bits,actual=observed$grouping_bits))
 record(paste(label,'ids'),identical(observed$grouping_id,expected_ids),list(expected=expected_ids,actual=observed$grouping_id))
 record(paste(label,'occurrences'),identical(observed$set_id,seq_along(family)),observed$set_id)
 keys<-vapply(oracle$sets,paste,collapse='|',character(1))
 if(anyDuplicated(keys)) {
  state$duplicates<-state$duplicates+1L
  dropped<-observe(inspect(spec,'drop'));ref<-observed[!duplicated(keys),];ref$set_id<-seq_len(nrow(ref))
  record(paste(label,'drop'),identical(dropped,ref),list(expected=ref,actual=dropped))
  refused<-observe(inspect(spec,'error'))
  record(paste(label,'error'),inherits(refused,'marginplyr_error') && grepl('Duplicate grouping sets',conditionMessage(refused),fixed=TRUE),refused)
 } else record(paste(label,'no-duplicates'),identical(inspect(spec,'error'),observed),observed)
 invisible(NULL)
}
unit <- function(cols, composite=TRUE) {
 expr<-if(composite)rlang::expr(grouping_set(dplyr::all_of(!!cols)))else rlang::expr(dplyr::all_of(!!cols))
 list(expr=expr,cols=cols)
}
three<-c(lapply(columns[1:3],function(x)unit(x,FALSE)),lapply(subsets(3)[-1L],unit))
# Exhaust every length-1, length-2 and length-3 dimension sequence over three names.
for(n in 1:3) {
 grid<-expand.grid(rep(list(seq_along(three)),n),KEEP.OUT.ATTRS=FALSE)
 for(i in seq_len(nrow(grid))) {
  units<-three[unname(unlist(grid[i,],use.names=FALSE))]
  for(kind in c('rollup','cube')) {
   family<-if(kind=='rollup')lapply(n:0,function(k)if(k)unique(unlist(lapply(units[seq_len(k)],`[[`,'cols'),use.names=FALSE))else character())else {
    masks<-unlist(lapply(n:0,function(k)combn(seq_len(n),k,simplify=FALSE)),recursive=FALSE)
    lapply(masks,function(idx)unique(unlist(lapply(units[idx],`[[`,'cols'),use.names=FALSE)))
   }
   family<-lapply(family,function(x)if(is.null(x))character()else x)
   spec<-eval(rlang::call2(kind,!!!lapply(units,`[[`,'expr')))
   check(spec,family,paste('dimensions',n,i,kind))
  }
 }
}
# Complete four-name subset pairs exercise overlap and all 16 membership patterns.
for(left in subsets(4))for(right in subsets(4)) {
 spec<-grouping_sets(grouping_set(dplyr::all_of(left)),grouping_set(dplyr::all_of(right)))
 check(spec,list(left,right),paste('union-subsets',paste(left,collapse=''),paste(right,collapse='')))
}
# Constructor nesting includes empty products, overlapping cube/rollup and identity families.
leaves<-lapply(subsets(3),function(cols)list(spec=grouping_set(dplyr::all_of(cols)),family=list(cols)))
leaves<-c(leaves,list(list(spec=grouping_spec(),family=list(character()))))
for(cols in subsets(3)[-1L])for(kind in c('rollup','cube')) {
 units<-lapply(cols,function(x)x)
 n<-length(units)
 fam<-if(kind=='rollup')lapply(n:0,function(k)cols[seq_len(k)])else unlist(lapply(n:0,function(k)combn(cols,k,simplify=FALSE)),recursive=FALSE)
 leaves[[length(leaves)+1L]]<-list(spec=eval(rlang::call2(kind,rlang::expr(dplyr::all_of(!!cols)))),family=fam)
}
for(i in seq_along(leaves))for(j in seq_along(leaves)) {
 a<-leaves[[i]];b<-leaves[[j]]
 check(grouping_sets(!!a$spec,!!b$spec),c(a$family,b$family),paste('nested-union',i,j))
 product<-unlist(lapply(a$family,function(x)lapply(b$family,function(y)unique(c(x,y)))),recursive=FALSE)
 check(grouping_spec(!!a$spec,!!b$spec),product,paste('nested-product',i,j))
}
cat('Algebra plans:',state$plans,'duplicate plans:',state$duplicates,'assertions:',state$checks,'failures:',length(state$failures),'\n')
if(length(state$failures)){print(head(state$failures,10L));quit(status=1L)}
