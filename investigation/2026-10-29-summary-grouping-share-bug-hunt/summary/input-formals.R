suppressPackageStartupMessages(library(dplyr))
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) normalizePath(args[[1L]]) else getwd()
devtools::load_all(repo, quiet = TRUE)
for(family in c('dplyr_mask','dplyr_count_mask','margin_mask')){
  rule <- getFromNamespace("static_spelling_rule", "marginplyr")(family)
  for(n in rule$names){
    f <- getExportedValue(rule$namespaces[[1]],n)
    cat(family,n,'registered',rule$input,'actual',names(formals(f))[[1]],'\n')
  }
}
