script_path <- sub("^--file=", "",
  grep("^--file=", commandArgs(), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script_path)), "support.R"))
args <- commandArgs(TRUE)
if (length(args) > 1L) stop("Usage: Rscript compare-simulators.R [capture-directory]")
root <- capture_root(args)
a <- readRDS(file.path(root,'sim-off.rds'));b <- readRDS(file.path(root,'sim-on.rds'))
stopifnot(length(a) == 128L, identical(names(a), names(b)))
ok <- TRUE
for(nm in names(a)) for(field in c('value','conditions','rendered','rendered_conditions')) if(!identical(a[[nm]][[field]],b[[nm]][[field]])) {
 ok <- FALSE;cat(nm,field,'DIFFERENCE\n');print(all.equal(a[[nm]][[field]],b[[nm]][[field]]))
}
if(!ok) quit(status=1L)
cat('PASS',length(a),'simulated dialect cases: construction result/conditions, later rendered SQL/conditions all identical\n')
