# Observes the registered R environment without loading marginplyr or a suite.
# Unknown startup/build hooks have an undeclared external-input closure.
description <- read.dcf("DESCRIPTION")
home <- path.expand("~")
configuration <- unique(c(
  file.path(R.home("etc"), c("Renviron", "Renviron.site", "Rprofile.site", "Makeconf")),
  file.path(home, c(".Rprofile", ".Renviron", ".R/Makevars", ".R/Makevars.site")),
  file.path(getwd(), c(".Rprofile", ".Renviron"))
))
hooks <- c("R_PROFILE", "R_PROFILE_USER", "R_ENVIRON", "R_ENVIRON_USER",
  "R_MAKEVARS_USER", "R_MAKEVARS_SITE", "R_LIBS", "R_LIBS_USER", "R_LIBS_SITE",
  "QUARTO_DENO", "QUARTO_PATH", "QUARTO_PANDOC", "LD_PRELOAD", "DYLD_INSERT_LIBRARIES")
unknown <- hooks[nzchar(Sys.getenv(hooks))]
# R itself sets its library defaults. Their resolved ordered paths are recorded.
unknown <- setdiff(unknown, c("R_LIBS_USER", "R_LIBS_SITE"))
custom <- configuration[!configuration %in% file.path(R.home("etc"), c("Renviron", "Makeconf"))]
unknown <- c(unknown, custom[file.exists(custom)])
dated <- intersect(c("Date", "Date/Publication"), colnames(description))
unknown <- c(unknown, paste0("DESCRIPTION:", dated))
clock_sources <- c(list.files("R", "[.][Rr]$", full.names = TRUE),
  list.files("tests", "[.](Rin|[Rr])$", full.names = TRUE, recursive = TRUE),
  list.files("vignettes", "[.]qmd$", full.names = TRUE), "README.qmd")
for (file in clock_sources[file.exists(clock_sources)]) {
  if (any(grepl("\\b(Sys[.](Date|time)|date|now|today)[[:space:]]*[(]",
                readLines(file, warn = FALSE), perl = TRUE))) unknown <- c(unknown, file)
}
checksum <- function(name) {
  file <- tempfile("review-ready-r-implementation-")
  on.exit(unlink(file), add = TRUE)
  writeLines(deparse(get(name, asNamespace("tools"))), file)
  unname(tools::md5sum(file))
}
timeout <- get("get_timeout", asNamespace("tools"))
leeway <- timeout(Sys.getenv("_R_CHECK_FUTURE_FILE_TIMESTAMPS_LEEWAY_", "5m"))
if (leeway <= 0) leeway <- 600
quarto <- unname(Sys.which("quarto"))
runtime <- R.home()
if (nzchar(quarto)) runtime <- c(runtime, dirname(dirname(normalizePath(quarto))))
cat(jsonlite::toJSON(list(
  r = as.character(getRversion()), libraries = as.list(unname(.libPaths())),
  runtime_roots = as.list(unique(runtime)),
  executables = list(file.path(R.home("bin"), "exec", "R")),
  configuration = as.list(setdiff(configuration, custom[!file.exists(custom)])),
  locale = Sys.getlocale(), timezone = Sys.timezone(),
  loader_defaults = as.list(Sys.getenv(c("LD_LIBRARY_PATH", "DYLD_FALLBACK_LIBRARY_PATH"))),
  clock = list(r = as.character(getRversion()), checker = checksum(".check_packages"),
    timeout = checksum("get_timeout"), leeway = leeway),
  unregistered = as.list(unname(unknown))
), auto_unbox = TRUE, null = "null"))
