# Reading a file the package ships alongside its code. Both callers --
# `test-package-metadata.R` on the license files and `test-documentation.R` on
# DESCRIPTION -- need the same two answers, so they are given once here rather
# than once per test file.

# Prefer the repository copy, because `system.file()` reads whichever marginplyr
# happens to be installed. `R CMD check` runs the tests from a directory holding
# no repository copy, and there the installed package is the only one -- which
# reaches these files because R installs them, `LICENSE` because the `License`
# field names it. A file whose installation depends on the R version, as
# `README.md`'s does, cannot use this: absence there is a fact about the
# checking machine and not the drift being guarded against.
metadata_path <- function(file) {
  source_copy <- test_path("..", "..", file)
  if (file.exists(source_copy)) {
    return(source_copy)
  }
  installed <- system.file(file, package = "marginplyr")
  if (!nzchar(installed)) {
    stop(sprintf("No copy of %s is available to read.", file), call. = FALSE)
  }
  installed
}

# A field this cannot find is a failure rather than a pass, since a file that
# states no value is exactly the drift being guarded against.
dcf_field <- function(path, field) {
  # `read.dcf()` names the value after the field, which would otherwise turn a
  # mismatch into a report about names.
  value <- unname(read.dcf(path, fields = field)[1L, 1L])
  if (is.na(value)) {
    stop(
      sprintf("%s states no %s field.", basename(path), field),
      call. = FALSE
    )
  }
  value
}
