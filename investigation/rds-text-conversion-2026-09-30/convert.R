# Convert the six evidence RDS files from the recorded source checkout into
# an unused output directory, asserting preservation before returning.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
source_root <- normalizePath(args[[1L]])
output_root <- args[[2L]]
stopifnot(!file.exists(output_root))
dir.create(output_root, recursive = TRUE)

# Decimal digits17 preserves the recorded doubles and integer64 storage bits.
# hexNumeric underflowed the latter when parsed on the measured R build.
controls <- c("keepNA", "keepInteger", "niceNames", "showAttributes", "digits17")
write_object <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  dput(object, path, control = controls)
  writeLines(trimws(readLines(path), which = "right"), path)
  stopifnot(identical(object, dget(path), num.eq = FALSE))
}

# Archived names are relative file paths; contents must be unmodified UTF-8.
write_files <- function(files, directory) {
  stopifnot(!anyDuplicated(names(files)))
  for (name in names(files)) {
    stopifnot(!grepl("^/|(^|/)\\.\\.(/|$)", name))
    bytes <- files[[name]]
    stopifnot(is.raw(bytes), !any(bytes == as.raw(0)))
    stopifnot(!is.na(iconv(rawToChar(bytes), "UTF-8", "UTF-8")))
    path <- file.path(directory, name)
    stopifnot(!file.exists(path))
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeBin(bytes, path)
    stopifnot(identical(bytes, readBin(path, "raw", n = length(bytes) + 1L)))
  }
}

postgres <- "investigation/2026-09-30-postgres-near-floor"
for (case in c("near", "current-r45", "current-r46")) {
  object <- readRDS(file.path(source_root, postgres, case, "values.rds"))
  write_object(object, file.path(output_root, postgres, case, "values.dput"))
  cat(case, ": seven typed objects preserved\n", sep = "")
}

signaling <- "investigation/condition-resignaling-2026-09-30"
archive <- readRDS(file.path(source_root, signaling, "evidence.rds"))
environment_file <- tempfile(fileext = ".rds")
writeBin(archive$files[["environment.rds"]], environment_file)
environment <- readRDS(environment_file)
unlink(environment_file)
write_object(environment, file.path(output_root, signaling, "environment.dput"))
write_object(archive[names(archive) != "files"],
             file.path(output_root, signaling, "archive.dput"))
write_files(archive$files[names(archive$files) != "environment.rds"],
            file.path(output_root, signaling))
cat("signaling: seven original text files and typed environment preserved\n")

sqlite <- "investigation/sqlite-interruption-recovery-2026-09-30"
for (name in c("acceptance", "harness-development")) {
  archive <- readRDS(file.path(source_root, sqlite, paste0(name, ".rds")))
  directory <- file.path(output_root, sqlite, name)
  nonempty <- lengths(archive$files) > 0L
  write_files(archive$files[nonempty], directory)
  empty_paths <- names(archive$files)[!nonempty]
  empty_list <- file.path(directory, "empty-files.txt")
  writeLines(empty_paths, empty_list)
  stopifnot(identical(empty_paths, readLines(empty_list)))
  write_object(archive[names(archive) != "files"],
               file.path(directory, "archive.dput"))
  cat(name, ": ", length(archive$files), " original text files preserved (",
      sum(lengths(archive$files)), " bytes)\n", sep = "")
}
