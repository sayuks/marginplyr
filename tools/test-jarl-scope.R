# Runs the installed jarl with this repository's `jarl.toml` over a disposable
# tree holding the same violation in `R/` and `investigation/`, so the
# configured scope is observed rather than read back from the file.
local({
  jarl <- Sys.which("jarl")
  if (!nzchar(jarl)) stop("jarl scope unresolved: install jarl.", call. = FALSE)
  workspace <- tempfile("jarl-scope-")
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  for (directory in c("R", "investigation")) {
    dir.create(file.path(workspace, directory), recursive = TRUE)
    writeLines("x <- any(is.na(y))", file.path(workspace, directory, "violation.R"))
  }
  stopifnot(file.copy("jarl.toml", workspace))
  result <- processx::run(jarl, c("check", "."), wd = workspace, error_on_status = FALSE)
  output <- paste(result$stdout, result$stderr)
  if (result$status == 0L || !grepl("violation.R", output, fixed = TRUE)) {
    cat(output, "\n")
    stop("jarl no longer reports a violation under R/.", call. = FALSE)
  }
  if (grepl("investigation", output, fixed = TRUE)) {
    cat(output, "\n")
    stop("jarl checks investigation/, which jarl.toml excludes.", call. = FALSE)
  }
  cat("jarl scope excludes investigation/ and still checks R/.\n")
})
