# Archive the investigated commit and install only into the requested run directory.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L, !file.exists(args[[1L]]))
script_arg <- commandArgs(FALSE)
script <- sub("^--file=", "", script_arg[grepl("^--file=", script_arg)][[1L]])
bundle <- normalizePath(dirname(script))
repo <- normalizePath(file.path(bundle, "..", ".."))
target <- "c14746aed1962f8fda78ed3c05323e73251536b0"

versions <- read.csv(file.path(bundle, "dependency-versions.csv"))
for (i in which(versions$package != "marginplyr")) {
  name <- versions$package[[i]]
  if (as.character(packageVersion(name)) != versions$version[[i]]) {
    stop("Dependency version differs from the investigated environment: ", name)
  }
}
out <- args[[1L]]
dir.create(out, recursive = TRUE)
out <- normalizePath(out)
source <- file.path(out, "source")
lib <- file.path(out, "library")
dir.create(source)
dir.create(lib)
archive <- file.path(out, "source.tar")
status <- system2("git", c(
  "-C", shQuote(repo), "archive", "--format=tar",
  shQuote(paste0("--output=", archive)), target
))
stopifnot(status == 0L)
untar(archive, exdir = source)
writeLines(c(target, unname(tools::md5sum(archive))), file.path(out, "source-identity.txt"))
install_log <- file.path(out, "install.log")
status <- system2(file.path(R.home("bin"), "R"), c(
  "CMD", "INSTALL", "--no-multiarch", shQuote(paste0("--library=", lib)),
  shQuote(source)
), stdout = install_log, stderr = install_log)
stopifnot(status == 0L)
for (mode in c("margin", "arrow-only")) {
  arguments <- c(
    shQuote(file.path(bundle, "reproduce.R")),
    shQuote(file.path(out, mode)), shQuote(lib)
  )
  if (mode == "arrow-only") arguments <- c(arguments, "--arrow-only")
  log <- file.path(out, paste0(mode, ".log"))
  status <- system2(file.path(R.home("bin"), "Rscript"), arguments,
                    stdout = log, stderr = log)
  stopifnot(status == 0L)
}
cat("Saved fresh reproduction:", out, "\n")
