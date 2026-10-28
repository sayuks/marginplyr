suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(if(length(commandArgs(TRUE)))commandArgs(TRUE)[1L] else getwd(), quiet=TRUE)
data <- data.frame(g=c('a','b'),x=c(1,3))
cases <- list(
 with_binding=quote(with(list(previous=2), previous+sum(x))),
 explicit_base_with=quote(base::with(list(previous=2), previous+sum(x))),
 local_binding=quote(local({previous<-2;previous+sum(x)})),
 lambda_binding=quote((function(previous=2) previous+sum(x))()),
 qualified_bquote=quote(length(base::bquote(previous))+sum(x)),
 substitute=quote(length(substitute(previous))+sum(x)),
 external_environment=quote(sum(x)+get('previous',envir=list2env(list(previous=2)),inherits=FALSE))
)
for(nm in names(cases)) {
  result <- tryCatch(summarize_with_margins(data,previous=sum(x),value=!!cases[[nm]],share=share_of_total(value),.grouping=rollup(g)),error=identity)
  cat(nm,': ')
  if(inherits(result,'error'))cat(class(result)[1],gsub('\n',' ',conditionMessage(result)),'\n') else cat('PASS',paste(result$share,collapse=','),'\n')
}
