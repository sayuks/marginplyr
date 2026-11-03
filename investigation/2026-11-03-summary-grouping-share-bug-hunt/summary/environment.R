cat("Investigated: 2026-11-03\n")
cat("Source SHA: ", system2("git", c("rev-parse", "HEAD"), stdout = TRUE), "\n", sep = "")
cat("R: ", R.version.string, "\nPlatform: ", R.version$platform, "\n", sep = "")
for (name in c("dplyr", "dbplyr", "dtplyr", "data.table", "arrow", "glue", "rlang", "tidyselect", "vctrs", "pkgload")) {
 cat(name, as.character(utils::packageVersion(name)), "\n")
}
print(sessionInfo())
