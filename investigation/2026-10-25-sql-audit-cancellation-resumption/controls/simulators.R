script_path <- sub("^--file=", "",
  grep("^--file=", commandArgs(), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script_path)), "support.R"))
args <- commandArgs(TRUE)
if (length(args) != 2L || !args[[1L]] %in% c("on", "off")) {
  stop("Usage: Rscript simulators.R <on|off> <fresh-output.rds>")
}
out <- capture_output(args[[2L]])
audited <- identical(args[[1L]],'on')
load_checkout()
options(marginplyr.audit_sql=audited)
simulators <- sort(grep('^simulate_',getNamespaceExports('dbplyr'),value=TRUE))
cases <- list(
 summary=quote(summarize_with_margins(src,total=sum(v),.grouping=rollup(g,h))),
 ordered=quote(summarize_with_margins(src,total=sum(v,na.rm=TRUE),.grouping=rollup(g,h),.sort='last')),
 share=quote(summarize_with_margins(src,total=sum(v,na.rm=TRUE),share=share_of_total(total),.grouping=rollup(g,h))),
 unchecked_share=quote(summarize_with_margins(src,total=sum(v,na.rm=TRUE),share=share_of_total(total),.grouping=rollup(g,h),.check_share_source=FALSE)),
 checked=quote(summarize_with_margins(src,total=sum(v,na.rm=TRUE),.grouping=rollup(g,h),.check_margin_label=TRUE)),
 expansion=quote(expand_with_margins(src,.grouping=cube(g,h),.sort='first',.id='setid')),
 inspection=quote(inspect_grouping(src,.grouping=rollup(g,h))),
 typed_inspection=quote(inspect_grouping(src,.grouping=rollup(where(is.character))))
)
res <- list()
for(sim in simulators) {
 con <- getExportedValue('dbplyr',sim)()
 src <- dbplyr::tbl_lazy(data.frame(g='a',h='b',v=1),con=con)
 for(nm in names(cases)) {
  conditions <- list()
  v <- withCallingHandlers(tryCatch(eval(cases[[nm]]),error=identity),condition = function(cnd) {
    if (inherits(cnd, c('warning', 'error', 'interrupt'))) {
      conditions[[length(conditions) + 1L]] <<-
        list(class = class(cnd), message = conditionMessage(cnd))
    }
  })
  record <- if(audited) last_sent_queries(details=TRUE) else NULL
  rendered_conditions <- list()
  rendered <- if(inherits(v,'tbl_lazy')) withCallingHandlers(tryCatch(as.character(dbplyr::sql_render(v)),error=function(cnd) list(class=class(cnd),message=conditionMessage(cnd))), condition = function(cnd) {
    if (inherits(cnd, c('warning', 'error', 'interrupt'))) {
      rendered_conditions[[length(rendered_conditions) + 1L]] <<-
        list(class = class(cnd), message = conditionMessage(cnd))
    }
  }) else NULL
  value <- if(inherits(v,'condition')) list(class=class(v),message=conditionMessage(v)) else if(inherits(v,'tbl_lazy')) list(class=class(v),vars=as.character(dplyr::tbl_vars(v))) else v
  res[[paste(sim,nm,sep='/')]] <- list(value=value,conditions=conditions,rendered=rendered,rendered_conditions=rendered_conditions,record=record)
 }
}
saveRDS(res,out)
cat(length(res),'cases complete\n')
