args <- commandArgs(TRUE)
pkgload::load_all(if (length(args)) args[[1L]] else ".", quiet = TRUE)
source("investigation/2026-10-29-nest-spec-exhaustive/local/local-fixtures.R")
failures <- list(); calls <- checks <- grouped_pairs <- 0L; refused <- list(); deviations <- list()
for(kind in kinds)for(n in c(0L,1L,6L))for(p in seq_along(plans))for(fixed in c(FALSE,TRUE)) {
  sets<-plans[[p]]
  x<-tibble::tibble(fixed=make_factor(n),a=make_value(kind,n),b=rep(c("c","b",NA_character_),length.out=n),row_id=seq_len(n),
    payload=tibble::tibble(original=make_value(kind,n),count=seq_len(n)))
  groups<-c(if(fixed)"fixed",unique(unlist(sets)))
  for(keep in c(FALSE,TRUE))for(verb in c("nest_with_margins","nest_by_with_margins")) {
    sort<-sample(c("none","first","last"),1L)
    invoke <- function(data,by) rlang::inject((get(verb))(data,.by=!!by,.grouping=!!expr_sets(sets),.keep=keep,.id="set",.margin_label=NULL,.sort=sort))
    calls<-calls+1L
    before <- serialize(x, NULL, version = 3)
    out<-tryCatch(invoke(x,if(fixed)rlang::sym("fixed") else NULL),error=identity)
    tag<-paste(kind,n,p,fixed,keep,verb,sort,sep="/")
    if(inherits(out,"error")){refused[[tag]]<-conditionMessage(out);next}
    out<-dplyr::ungroup(out)
    if(fixed){
      grouped_pairs<-grouped_pairs+1L;calls<-calls+1L
      g<-tryCatch(invoke(dplyr::group_by(x,fixed),NULL),error=identity)
      if(inherits(g,"error")||!identical(as.list(out),as.list(dplyr::ungroup(g)))) failures[[paste0(tag,"/grouped")]]<-if(inherits(g,"error"))conditionMessage(g) else "grouped differs"
    }
    stopifnot(identical(before, serialize(x, NULL, version = 3)))
    cols<-if(keep)c(groups,setdiff(names(x),groups))else setdiff(names(x),groups)
    for(j in seq_along(sets)){
      keys<-c(if(fixed)"fixed",sets[[j]])
      expected_locs<-if(length(keys))vctrs::vec_group_loc(x[keys])$loc else if(n || (identical(verb,"nest_by_with_margins")&&!length(groups)))list(seq_len(n))else list()
      actual<-out[out$set==j,,drop=FALSE]
      signature<-function(z)paste(z,collapse=",")
      actual_sigs<-vapply(actual$data,function(z)signature(z$row_id),character(1))
      expected_sigs<-vapply(expected_locs,signature,character(1))
      checks<-checks+1L
      if(!identical(sort(actual_sigs),sort(expected_sigs)))failures[[paste0(tag,"/members/",j)]]<-list(expected=expected_sigs,actual=actual_sigs)
      for(cell in actual$data){
        expected<-vctrs::vec_slice(x,cell$row_id)[cols]
        checks<-checks+1L
        if(!identical(as.list(cell),as.list(expected))){
          if(identical(kind,"posixlt"))deviations[[paste0(tag,"/cell/",j)]]<-"POSIXlt converted via ordinary union to POSIXct" else failures[[paste0(tag,"/cell/",j)]]<-list(cell=cell,expected=expected)
        }
      }
    }
  }
}
cat("seed 29102911; kinds",length(kinds),"calls",calls,"checks",checks,"grouped explicit pairs",grouped_pairs,"refusals",length(refused),"failures",length(failures),"delegated POSIXlt differences",length(deviations),"\n")
if(length(refused))print(table(unlist(refused)))
if(length(failures)) {print(utils::head(failures,20));saveRDS(failures,"/tmp/nest-local-failures.rds")}
stopifnot(length(failures)==0L,length(refused)==0L)
cat("PASS: independently grouped source membership, original cells, and grouped-input equivalence\n")
