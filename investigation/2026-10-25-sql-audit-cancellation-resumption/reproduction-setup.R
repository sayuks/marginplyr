# Resolves this bundle's checkout and keeps rerun output apart from evidence.
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
stopifnot(length(script_arg) == 1L)
hunt_script <- normalizePath(sub("^--file=", "", script_arg))
hunt_bundle <- dirname(hunt_script)
hunt_checkout <- normalizePath(file.path(hunt_bundle, "..", ".."))
stopifnot(file.exists(file.path(hunt_checkout, "DESCRIPTION")))
hunt_output_dir <- Sys.getenv("HUNT_OUTPUT_DIR")
if (!nzchar(hunt_output_dir)) hunt_output_dir <- tempfile("sql-audit-hunt-")
# Resolve an existing ancestor before creating a directory, including symlinks.
resolve_output <- function(path) {
  if (file.exists(path)) return(normalizePath(path))
  file.path(resolve_output(dirname(path)), basename(path))
}
hunt_output_dir <- resolve_output(hunt_output_dir)
hunt_evidence <- normalizePath(file.path(hunt_bundle, "evidence"))
if (identical(hunt_output_dir, hunt_evidence) ||
    startsWith(hunt_output_dir, paste0(hunt_evidence, .Platform$file.sep))) {
  stop("HUNT_OUTPUT_DIR must be outside the historical evidence directory")
}
dir.create(hunt_output_dir, recursive = TRUE, showWarnings = FALSE)
hunt_output <- function(filename) file.path(hunt_output_dir, filename)
cat("RERUN_OUTPUT", hunt_output_dir, "\n")
