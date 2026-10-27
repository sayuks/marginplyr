# Arrow's Windows C++ fallback reads %USERPROFILE%/Downloads/tzdata when
# R CMD check hides its optional tzdb namespace. Check the timestamp cast
# under that same library restriction before running the full package suite.
stopifnot(.Platform$OS.type == "windows")
profile <- Sys.getenv("USERPROFILE")
if (!nzchar(profile)) {
  stop("USERPROFILE is unavailable for Arrow timezone provisioning.")
}
target <- file.path(profile, "Downloads", "tzdata")
dir.create(target, recursive = TRUE, showWarnings = FALSE)
files <- list.files(tzdb::tzdb_path("text"), full.names = TRUE)
if (length(files) == 0L || !all(file.copy(files, target, overwrite = TRUE))) {
  stop("Could not provision Arrow's Windows timezone database.")
}

Sys.setenv(`_R_CHECK_CRAN_INCOMING_REMOTE_` = "false")
rcmdcheck::rcmdcheck(
  ".github/scripts/fixtures/windows-arrow-timezone",
  args = c("--as-cran", "--no-manual"),
  check_dir = tempfile("arrow-timezone-check-"),
  error_on = "warning"
)
