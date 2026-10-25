script_path <- sub("^--file=", "",
  grep("^--file=", commandArgs(), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script_path)), "support.R"))
args <- commandArgs(TRUE)
if (length(args) > 1L) stop("Usage: Rscript compare.R [capture-directory]")
root <- capture_root(args)
count <- 0L
all_ok <- TRUE
for(backend in c('sqlite','duckdb')) {
 off <- readRDS(file.path(root,paste0(backend,'-off.rds')))
 on <- readRDS(file.path(root,paste0(backend,'-on.rds')))
 stopifnot(length(off) == 111L, identical(names(off), names(on)))
 for(nm in names(off)) {
  count <- count + 1L
  a <- off[[nm]]; b <- on[[nm]]
  if(is.data.frame(a$value) && !grepl('sorted|cube|summary_by|summary_share$|factor', nm)) {
   a$value <- dplyr::arrange(a$value,dplyr::across(where(is.atomic)))
   b$value <- dplyr::arrange(b$value,dplyr::across(where(is.atomic)))
  }
  for(field in c('conditions','sql','class','value')) if(!identical(a[[field]], b[[field]])) {
   all_ok <- FALSE
   cat(backend,nm,'DIFFERENCE',field,'\n')
   print(all.equal(a[[field]],b[[field]]))
  }
  rec <- b$record
  internal <- rec[rec$purpose!='result',]
  # A translator failure at invocation can leave an errored row before DBI.
  internal <- internal[!is.na(internal$sql),]
  if(length(b$sql)!=nrow(internal)) {
   all_ok <- FALSE
   cat(backend,nm,'COUNT',length(b$sql),nrow(internal),'\n');print(b$sql);print(internal[c('purpose','sql','status')])
  }
  if(length(b$sql)==nrow(internal) && !identical(unname(b$sql),internal$sql)) {
   all_ok <- FALSE
   cat(backend,nm,'RECORDED SQL DIFFERENCE\n');print(b$sql);print(internal[c('purpose','sql','status')])
  }
 }
 cat(backend,length(off),'cases\n')
}
stopifnot(count == 222L)
if(!all_ok) quit(status=1L)
cat('PASS',count,'SQLite/DuckDB off/on pairs: conditions SQL classes values and internal-record matches\n')
