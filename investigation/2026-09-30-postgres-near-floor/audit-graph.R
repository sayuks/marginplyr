args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
out <- args[[1L]]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
libs <- normalizePath(.libPaths()[1:2])
installed <- installed.packages(lib.loc = libs)
installed <- installed[!duplicated(installed[, "Package"]), , drop = FALSE]
manifest <- data.frame(
  package = installed[, "Package"], version = installed[, "Version"],
  path = file.path(installed[, "LibPath"], installed[, "Package"])
)
manifest <- manifest[order(manifest$package), , drop = FALSE]
utils::write.csv(manifest, file.path(out, "installed-manifest.csv"), row.names = FALSE)

parse_dependencies <- function(raw) {
  if (is.na(raw) || !nzchar(raw)) return(list())
  entries <- strsplit(raw, ",", fixed = TRUE)[[1L]]
  pattern <- "^\\s*([[:alpha:]][[:alnum:].]*)\\s*(?:\\(\\s*(>=|<=|==|=|>|<)\\s*([[:alnum:].-]+)\\s*\\))?\\s*$"
  matches <- regmatches(entries, regexec(pattern, entries, perl = TRUE))
  stopifnot(all(lengths(matches) > 0L))
  lapply(matches, function(x) list(
    name = x[[2L]],
    operator = if (length(x) >= 3L) x[[3L]] else "",
    version = if (length(x) >= 4L) x[[4L]] else ""
  ))
}

checks <- list()
for (i in seq_len(nrow(manifest))) {
  package <- manifest$package[[i]]
  desc <- read.dcf(file.path(manifest$path[[i]], "DESCRIPTION"))[1L, ]
  for (field in c("Depends", "Imports", "LinkingTo")) {
    raw <- desc[field]
    if (length(raw) == 0L) next
    for (dep in parse_dependencies(raw)) {
      j <- match(dep$name, manifest$package)
      if (dep$name == "R") {
        actual <- as.character(getRversion())
        location <- R.home()
      } else if (!is.na(j)) {
        actual <- manifest$version[[j]]
        location <- manifest$path[[j]]
      } else if (dep$name %in% rownames(installed.packages(lib.loc = .Library))) {
        actual <- as.character(packageVersion(dep$name, lib.loc = .Library))
        location <- find.package(dep$name, lib.loc = .Library)
      } else {
        actual <- NA_character_
        location <- NA_character_
      }
      cmp <- if (is.na(actual) || !nzchar(dep$version)) NA_integer_ else
        utils::compareVersion(actual, dep$version)
      valid <- !is.na(actual) && (dep$operator == "" || switch(
        dep$operator, ">=" = cmp >= 0L, ">" = cmp > 0L,
        "=" = cmp == 0L, "==" = cmp == 0L,
        "<=" = cmp <= 0L, "<" = cmp < 0L, FALSE
      ))
      checks[[length(checks) + 1L]] <- data.frame(
        package = package, field = field, dependency = dep$name,
        operator = dep$operator, required = dep$version,
        actual = actual, location = location, valid = valid
      )
    }
  }
}
checks <- do.call(rbind, checks)
utils::write.csv(checks, file.path(out, "dependency-constraints.csv"),
                 row.names = FALSE, na = "")
cat(R.version.string, "\n", nrow(manifest), "packages,", nrow(checks),
    "constraints,", sum(!checks$valid), "violations\n")
stopifnot(all(checks$valid))
