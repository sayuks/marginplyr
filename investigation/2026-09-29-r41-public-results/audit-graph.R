args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 4L)
mode <- args[[1L]]
manifest_path <- args[[2L]]
source_dir <- args[[3L]]
output_path <- args[[4L]]
stopifnot(mode %in% c("source", "installed"))
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
stopifnot(identical(names(manifest), c("package", "version", "source", "sha256")))

if (mode == "source") {
  desc_dir <- tempfile("descriptions-")
  dir.create(desc_dir)
  for (i in seq_len(nrow(manifest))) {
    p <- manifest$package[[i]]
    archive <- file.path(source_dir, paste0(p, "_", manifest$version[[i]], ".tar.gz"))
    stopifnot(file.exists(archive))
    utils::untar(archive, files = paste0(p, "/DESCRIPTION"), exdir = desc_dir)
  }
  describe <- function(p) read.dcf(file.path(desc_dir, p, "DESCRIPTION"))[1L, ]
} else {
  lib <- normalizePath(source_dir, mustWork = TRUE)
  installed <- installed.packages(lib.loc = lib)
  stopifnot(setequal(installed[, "Package"], manifest$package))
  stopifnot(identical(unname(installed[manifest$package, "Version"]), manifest$version))
  describe <- function(p) read.dcf(file.path(lib, p, "DESCRIPTION"))[1L, ]
}

base <- rownames(installed.packages(lib.loc = .Library))
checks <- list()
for (p in manifest$package) {
  desc <- describe(p)
  for (field in c("Depends", "Imports", "LinkingTo")) {
    raw <- desc[field]
    if (length(raw) == 0L || is.na(raw) || !nzchar(raw)) next
    dependencies <- tools:::.split_dependencies(raw)
    for (q in names(dependencies)) {
      dep <- dependencies[[q]]
      selected <- match(q, manifest$package)
      if (q == "R") {
        actual <- as.character(getRversion())
        location <- R.home()
      } else if (!is.na(selected)) {
        actual <- manifest$version[[selected]]
        location <- if (mode == "source") manifest$source[[selected]] else file.path(lib, q)
      } else if (q %in% base) {
        actual <- as.character(packageVersion(q, lib.loc = .Library))
        location <- find.package(q, lib.loc = .Library)
      } else {
        actual <- NA_character_
        location <- NA_character_
      }
      operator <- if (is.null(dep$op)) "" else as.character(dep$op)
      required <- if (is.null(dep$version)) "" else as.character(dep$version)
      cmp <- if (is.na(actual) || !nzchar(required)) NA_integer_ else utils::compareVersion(actual, required)
      valid <- !is.na(actual) && (operator == "" || switch(
        operator, ">=" = cmp >= 0L, ">" = cmp > 0L,
        "=" = cmp == 0L, "==" = cmp == 0L,
        "<=" = cmp <= 0L, "<" = cmp < 0L, FALSE
      ))
      checks[[length(checks) + 1L]] <- data.frame(
        package = p, field = field, dependency = q, operator = operator,
        required = required, actual = actual, location = location, valid = valid
      )
    }
  }
}
checks <- do.call(rbind, checks)
write.csv(checks, output_path, row.names = FALSE, na = "")
if (mode == "source") unlink(desc_dir, recursive = TRUE)
cat(R.version.string, "\n")
cat(nrow(manifest), "packages;", nrow(checks), "constraints;", sum(!checks$valid), "violations\n")
if (any(!checks$valid)) {
  print(checks[!checks$valid, , drop = FALSE], row.names = FALSE)
  quit(status = 2L)
}
