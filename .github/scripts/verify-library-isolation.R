# Confirms that a job whose signal depends on which optional backends are
# installed really ran with that library.
#
# `r-lib/actions/setup-r-dependencies@v2` restores a cache by prefix --
# `<os>-<R version>-<arch>-<cache-version>-` -- whenever the exact key misses,
# so a job that asks for hard dependencies only still starts from whatever
# library the last cache under that prefix saved, and pak then installs only
# what is still missing on top of it. Every job in this workflow and in
# `R-CMD-check.yaml` once shared `cache-version: 3`, which restored the fully
# provisioned library into the jobs documented as running without it (#64). The
# per-job `cache-version` values keep the prefixes apart; this script is what
# stops that separation from decaying silently, because a cache-key mistake
# produces a green run that looks exactly like a correct one unless somebody
# reads a `Session info` step.
#
# `MARGINPLYR_REQUIRED_SUGGESTS` is the job's declaration of which optional
# packages it asked for. Every one of them must be installed, and every other
# optional backend must be absent. A job that names none, such as `tarball`, is
# declaring that all of them are absent.
#
# This runs before `R CMD check` rather than after it. A leaked backend
# invalidates the run's whole claim, so there is nothing to learn from spending
# another 45 minutes on the check, and the failure names the cause instead of
# the symptom.

source(".github/scripts/ci-helpers.R")

label <- check_label()
backends <- optional_backends()

# The declaration also names `DBI`, which is a driver interface rather than a
# backend and is not tracked by `optional_backends()`. Intersecting keeps the
# two lists independent of each other.
expected <- intersect(env_packages("MARGINPLYR_REQUIRED_SUGGESTS"), backends)
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

verdict <- ifelse(
  backends %in% c(missing, leaked),
  ifelse(backends %in% missing, "MISSING", "LEAKED"),
  "OK"
)

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
      "these optional backends are installed but were not requested: %s, so ",
      "this job did not run with them withheld"
    ),
    paste(leaked, collapse = ", ")
  ))
}
if (length(missing) > 0L) {
  problems <- c(problems, sprintf(
    "these requested optional backends are not installed: %s",
    paste(missing, collapse = ", ")
  ))
}
if (length(problems) > 0L) {
  stop(sprintf("%s: %s.", label, paste(problems, collapse = "; ")))
}

message(sprintf(
  "Verified: %s runs with %s installed and %s withheld.",
  label,
  if (length(expected) == 0L) "no optional backend" else
    paste(expected, collapse = ", "),
  paste(withheld, collapse = ", ")
))
