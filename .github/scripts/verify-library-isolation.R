# Confirms that a job got the library it declared, before it checks anything.
#
# Which optional backends a job installs is the whole signal of `depends-only`,
# `tarball`, and the `backend` jobs, and the dependency cache is what
# nearly took it away: `setup-r-dependencies@v2` falls back to a `restore-keys`
# prefix, so jobs sharing a `cache-version` share a library (#64). The
# `cache-version` scheme that separates them is recorded in
# `release-matrix.yaml`'s header. A cache key is not self-checking, though --
# a wrong one produces a green run that reads like a correct one -- which is
# what this script is for.
#
# `check-tarball.R` sources it rather than the workflow calling it as its own
# step. A step can be deleted, and deleting it would restore exactly the silent
# regression #64 found; sourcing it here means a job can only skip the
# assertion by no longer checking the tarball at all. Every job that checks one
# is a job that makes a claim about which backends it had.
#
# `MARGINPLYR_REQUIRED_SUGGESTS` is the job's declaration of which optional
# packages it asked for -- the same one `helper-optional-backends.R` reads to
# turn an absent backend into a failure -- so the assertion needs no second
# list to keep in step with the matrix. Every package named there must be
# installed, and every other optional backend must be absent. A job that names
# none, such as `tarball`, is declaring that all of them are absent.

source(".github/scripts/ci-helpers.R")

label <- check_label()
backends <- optional_backends()
declared <- env_list("MARGINPLYR_REQUIRED_SUGGESTS")

# `optional_suggests()` names every package a guard may be written against;
# `optional_backends()` is the subset whose absence a job can claim, and is
# what this script iterates over. A job may declare `DBI`, which is in the
# first and not the second, and makes no absence claim by doing so. Anything in
# neither is a backend added to the matrix without being added to the list,
# which would leave it unchecked in every other job -- silently, since an
# untracked name simply drops out of the intersection below. Failing here is
# what makes that omission visible.
untracked <- setdiff(declared, names(optional_suggests()))
if (length(untracked) > 0L) {
  stop(call. = FALSE, sprintf(
    paste0(
      "%s declares %s in MARGINPLYR_REQUIRED_SUGGESTS, which ",
      "`optional_suggests()` in `tests/testthat/helper-optional-backends.R` ",
      "does not name, so no job asserts it is absent."
    ),
    label,
    paste(untracked, collapse = ", ")
  ))
}

expected <- intersect(declared, backends)
withheld <- setdiff(backends, expected)

# Searches every library on the path, which is the same set `requireNamespace()`
# consults from the tests, so "visible to this job" means the same thing here as
# it does there.
location <- function(package) {
  found <- find.package(package, quiet = TRUE)
  if (length(found) == 0L) NA_character_ else normalizePath(found[1])
}

found <- vapply(backends, location, character(1))
installed <- !is.na(found)

missing <- expected[!installed[expected]]
leaked <- withheld[installed[withheld]]

describe <- function(package) {
  if (is.na(found[[package]])) {
    return("absent")
  }
  sprintf(
    "%s at `%s`",
    as.character(packageVersion(package)),
    found[[package]]
  )
}

verdict <- rep("OK", length(backends))
verdict[backends %in% missing] <- "MISSING"
verdict[backends %in% leaked] <- "LEAKED"

summary_lines <- c(
  sprintf("## %s library isolation", label),
  "",
  sprintf(
    "Requested: %s. Withheld: %s.",
    if (length(expected) == 0L) "none" else paste(expected, collapse = ", "),
    if (length(withheld) == 0L) "none" else paste(withheld, collapse = ", ")
  ),
  "",
  paste0(
    "- ", verdict,
    " **", backends, "** — ",
    vapply(backends, describe, character(1))
  ),
  "",
  "Libraries searched:",
  "",
  paste0("- `", .libPaths(), "`")
)
write_step_summary(summary_lines)

problems <- character()
if (length(leaked) > 0L) {
  problems <- c(problems, sprintf(
    paste0(
      "These optional backends are installed but were not requested: %s. ",
      "This job did not run with them withheld."
    ),
    paste(leaked, collapse = ", ")
  ))
}
if (length(missing) > 0L) {
  problems <- c(problems, sprintf(
    "These requested optional backends are not installed: %s.",
    paste(missing, collapse = ", ")
  ))
}
if (length(problems) > 0L) {
  # `call. = FALSE` because `check-tarball.R` sources this file: without it the
  # message arrives wrapped in `Error in eval(ei, envir)` and a `source ->
  # withVisible` traceback, which buries the one line that names the cause.
  stop(call. = FALSE, sprintf("%s: %s", label, paste(problems, collapse = " ")))
}

message(sprintf(
  "Verified: %s runs with %s installed and %s withheld.",
  label,
  if (length(expected) == 0L) {
    "no optional backend"
  } else {
    paste(expected, collapse = ", ")
  },
  paste(withheld, collapse = ", ")
))
