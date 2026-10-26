# Investigation evidence, 2026-10-26. Run from the repository root.
pkgload::load_all(quiet=TRUE)
set.seed(10830)
# An independent row-copy oracle. It constructs literal grouping sets and
# row copies; it does not call marginplyr's planner or branch builders.
fixtures <- list(
 character=c("z","a",NA,"b"), integer=c(2L,1L,NA,0L),
 factor=factor(c("z","a",NA,"b"),levels=c("b","a","unused","z")),
 ordered=ordered(c("z","a",NA,"b"),levels=c("b","a","unused","z")),
 logical=c(TRUE,FALSE,NA,TRUE),
 Date=as.Date(c("2025-02-01",NA,"2024-01-01","2025-01-01")),
 list=list(2L,NULL,1L,3L),
 packed=tibble::tibble(a=c(2L,1L,NA,3L),b=c("x","a",NA,"b")),
 matrix=matrix(c(2L,1L,NA,3L,3L,2L,NA,4L),ncol=2)
)
names_pool <- c("g1",".data",".env","T",".keep","..marginplyr_sort_1","foo bar","set_id","new_levels")
failures<-list(); runs<-0L
for (seed in seq_len(200L)) {
 n<-sample(0:8,1L); rows<-sample(1:4,n,replace=TRUE)
 type_names<-sample(names(fixtures),3L,replace=TRUE)
 nm<-sample(names_pool,3L)
 d<-tibble::tibble(fixed=sample(c("x","y",NA),n,replace=TRUE), row=seq_len(n))
 for(j in seq_along(nm)) d[[nm[j]]]<-vctrs::vec_slice(fixtures[[type_names[j]]],rows)
 family<-sample(c("cube","rollup","sets"),1L)
 if (family=="cube") {
  masks<-as.matrix(expand.grid(rep(list(c(0L,1L)),3L)))[,3:1,drop=FALSE]
 } else if(family=="rollup") {
  masks<-rbind(c(0L,0L,0L),c(0L,0L,1L),c(0L,1L,1L),c(1L,1L,1L))
 } else {
  masks<-matrix(sample(0:1,15L,replace=TRUE),ncol=3L)
 }
 sets<-lapply(seq_len(nrow(masks)),function(i) nm[masks[i,]==0L])
 # Literal grouping_sets() avoids importing the implementation's expansion order.
 gs<-lapply(sets,function(x) rlang::eval_tidy(rlang::expr(grouping_set(dplyr::all_of(!!x)))))
 spec<-rlang::eval_tidy(rlang::expr(grouping_sets(!!!gs)))
 dims<-unique(unlist(sets,use.names=FALSE))
 labels<-setNames(rep(list(NULL),length(dims)),dims)
 for (j in seq_len(3L)) {
  if (nm[j] %in% dims && !is.data.frame(d[[nm[j]]]) && length(dim(d[[nm[j]]])) < 2L && sample(c(FALSE, TRUE), 1L)) {
   labels[[nm[j]]] <- paste0("ALL", j)
  }
 }
 for(sort in c("none","first","last")) {
  runs<-runs+1L
  got<-tryCatch(expand_with_margins(d,.by=fixed,.grouping=spec,.margin_label=labels,.duplicates="keep",.id="occurrence",.sort=sort),error=identity)
  if(inherits(got,"error")) {failures[[paste(seed,sort)]]<-conditionMessage(got);next}
  branches<-lapply(seq_along(sets),function(i) {
   out<-d
   for(j in seq_along(nm)) {
    name<-nm[j]; source<-d[[name]]; omitted<-name %in% dims && !name %in% sets[[i]]; label<-labels[[name]]
    if(!is.null(label)) {
     value<-if(omitted) rep(label,n) else as.character(source)
     if(is.factor(source)) {
      value<-factor(value,levels=c(levels(source),label),ordered=is.ordered(source))
     }
    } else value<-if(omitted) vctrs::vec_init(source,n) else source
    out[[name]]<-value
   }
   out$occurrence<-rep(as.integer(i),n)
   out
  })
  want<-do.call(vctrs::vec_rbind,branches)
  want<-want[,c("fixed",dims,"occurrence",setdiff(names(d),c("fixed",dims))),drop=FALSE]
  if(sort!="none") {
   # Distinct literal sort-key columns isolate spelling and allocation bugs.
   orderdata<-tibble::tibble(fixedmissing=is.na(want$fixed), fixedvalue=want$fixed)
   for(j in seq_along(dims)) {
    vals<-want[[dims[j]]]
    bits<-masks[want$occurrence,match(dims[j],nm)]
    orderdata[[paste0("b",j)]]<-if(sort=="first") -bits else bits
    orderdata[[paste0("m",j)]]<-if(is.data.frame(vals)||length(dim(vals))>1L) vctrs::vec_detect_missing(vals) else is.na(vals)
    orderdata[[paste0("v",j)]]<-vals
   }
   orderdata$id<-want$occurrence
   want<-vctrs::vec_slice(want,vctrs::vec_order(orderdata))
  }
  if(!identical(got,want)) failures[[paste(seed,sort)]]<-all.equal(got,want)
 }
}
cat("random multidimension runs:",runs,"failures:",length(failures),"\n"); print(head(failures,6L))
if(length(failures)) saveRDS(list(failures=failures),"/tmp/marginplyr-expand-random-failures.rds")
