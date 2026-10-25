script_path <- sub("^--file=", "",
  grep("^--file=", commandArgs(), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script_path)), "support.R"))
args <- commandArgs(TRUE)
if (length(args) > 1L) stop("Usage: Rscript validate-captures.R [capture-directory]")
root <- capture_root(args)
files <- c("sqlite-off.rds", "sqlite-on.rds", "duckdb-off.rds", "duckdb-on.rds",
           "sim-off.rds", "sim-on.rds", "transaction-failures.rds")
expected <- c(111L, 111L, 111L, 111L, 128L, 128L, 10L)
unsafe <- character()
sensitive <- character()
inspect <- function(value, location) {
  if (typeof(value) %in% c("environment", "externalptr", "closure", "builtin",
                         "special", "weakref")) {
    unsafe <<- c(unsafe, paste(location, typeof(value)))
  }
  if (is.character(value)) {
    found <- grepl("/Users/|/tmp/|password=|postgres(ql)?://|Bearer |BEGIN .*PRIVATE KEY",
                   value, ignore.case = TRUE)
    if (any(found, na.rm = TRUE)) sensitive <<- c(sensitive, location)
  }
  if (is.list(value) || is.pairlist(value)) {
    for (i in seq_along(value)) inspect(value[[i]], paste0(location, "[[", i, "]]"))
  }
  attrs <- attributes(value)
  for (name in names(attrs)) inspect(attrs[[name]], paste0(location, "@", name))
}
for (i in seq_along(files)) {
  value <- readRDS(file.path(root, files[[i]]))
  stopifnot(length(value) == expected[[i]])
  inspect(value, files[[i]])
  cat(files[[i]], length(value), "records inspected\n")
}
stopifnot(!length(unsafe), !length(sensitive))
cat("PASS retained RDS contains no environments, pointers, functions, weakrefs,",
    "or matched path/credential markers\n")
transactions <- readRDS(file.path(root, "transaction-failures.rds"))
stopifnot(length(transactions) == 10L)
for (value in transactions) stopifnot(identical(value$off[1:4], value$on[1:4]))
cat("PASS 10 transaction off/on pairs: conditions, value, live marker, rollback\n")
if (!length(args)) {
  lines <- readLines(file.path(root, "focused.log"), warn = FALSE)
  markers <- lines[grepl("^[a-z-]+: [.]+$", lines)]
  expectations <- sum(nchar(gsub("[^.]", "", markers)))
  stopifnot(length(markers) == 14L, expectations == 1094L)
  cat("PASS historical focused log:", length(markers), "files,", expectations,
      "expectation markers\n")
}
