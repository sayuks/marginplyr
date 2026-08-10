# Whether an optional Suggested package can be used, which DESCRIPTION makes a
# stronger question than whether it is installed.
#
# Several of this package's Suggests carry a version constraint, and
# `requireNamespace()` cannot see one: it answers "installed" and returns TRUE
# for a version whose API the guarded code then calls and does not find. The
# case that motivated this is `duckdb (>= 1.5.5)`, where
# `duckdb::duckdb(shared_home = FALSE)` is a hard error under 1.5.4.x rather
# than a degraded result, so a guard that only asked "installed" turned a clean
# skip into a confusing failure (#123).
#
# `R CMD check` is already safe: it stops at `checking package dependencies`
# with "required and available but unsuitable version" before any test,
# example, or vignette runs. What this file covers is every context that is not
# `R CMD check` -- `devtools::test()`, `pkgload::load_all()` plus
# `testthat::test_local()`, an example run interactively, and the altdoc site
# build, whose `Config/Needs/website` entries carry no versions at all.
#
# DESCRIPTION states each constraint once and this file is the only thing that
# reads one, so there is no second copy to drift. It is sourced rather than
# exported, in the manner of `inst/vignette-hooks/must-error.R`: a vignette, an
# example, and `tests/testthat/helper-optional-backends.R` each reach it in one
# line -- a `source()` call on the `system.file()` path to this file -- and the
# package's API gains nothing. A caller then guards on
# `marginplyr_suggest_available()`, which is the only name here any of them
# uses; the setup chunk of every vignette and the optional-backend section of
# every example show the pair.
#
# `AGENTS.md` is authoritative for which guards must go through here and for
# what asserts that they do.

# DESCRIPTION's `Suggests` field, read from the installed package. Every caller
# that can reach a repository DESCRIPTION passes it instead, which is why each
# function below takes the field as an argument rather than reaching for it.
marginplyr_declared_suggests <- function() {
  suggests <- utils::packageDescription("marginplyr")$Suggests
  if (is.null(suggests) || is.na(suggests)) {
    stop(
      "marginplyr's DESCRIPTION states no Suggests field to read a version ",
      "constraint from.",
      call. = FALSE
    )
  }
  suggests
}

# One entry per Suggested package, split on the commas that separate entries
# rather than on every comma: `pkg (>= 1.0, < 2.0)` is a legal single entry
# whose constraint contains one, and splitting it would produce two entries
# neither of which parses. Depth is counted rather than assumed, so the split
# stays correct whichever form DESCRIPTION happens to use.
marginplyr_suggest_entries <- function(suggests) {
  characters <- strsplit(suggests, "", fixed = TRUE)[[1]]
  depth <- cumsum(characters == "(") - cumsum(characters == ")")
  breaks <- which(characters == "," & depth == 0L)
  starts <- c(1L, breaks + 1L)
  ends <- c(breaks - 1L, length(characters))
  entries <- substring(suggests, starts, ends)
  # A DESCRIPTION field wraps across lines, so an entry arrives with newlines
  # and indentation inside it as readily as around it.
  entries <- trimws(gsub("[[:space:]]+", " ", entries))
  entries[nzchar(entries)]
}

# The shape of one entry, shared by the two functions below so that a package's
# name is extracted the same way it is matched.
marginplyr_entry_pattern <- paste0(
  "^([[:alnum:]._]+)",
  "[[:space:]]*(\\((.*)\\))?$"
)

# The packages DESCRIPTION suggests, in the order it lists them. An entry this
# cannot read halts rather than being passed over: a constraint that stopped
# being understood is one that silently stopped being honored, which is the
# failure this file exists to prevent.
marginplyr_suggest_names <- function(suggests) {
  entries <- marginplyr_suggest_entries(suggests)
  unreadable <- entries[!grepl(marginplyr_entry_pattern, entries)]
  if (length(unreadable) > 0L) {
    stop(
      sprintf(
        "These Suggests entries are not readable as `name (constraint)`: %s.",
        paste(unreadable, collapse = "; ")
      ),
      call. = FALSE
    )
  }
  sub(marginplyr_entry_pattern, "\\1", entries)
}

# The version constraint DESCRIPTION states for `package`, or NULL when it
# states none.
#
# A package DESCRIPTION does not suggest at all is refused rather than answered.
# Every guard reaching this file names a package the tarball declares, so a name
# that is absent from `Suggests` is a typo or a dependency that moved -- and
# answering it would answer the version-blind question this file exists to
# replace, at exactly the call sites that have no other registry. The test
# helpers refuse an unregistered backend for the same reason; a vignette and an
# example have only this.
marginplyr_suggest_requirement <- function(package,
                                           suggests =
                                             marginplyr_declared_suggests()) {
  # Both derived from `marginplyr_suggest_entries()`, so the name at a position
  # and the entry at it describe the same package.
  entries <- marginplyr_suggest_entries(suggests)
  named <- marginplyr_suggest_names(suggests)
  matched <- which(named == package)
  if (length(matched) == 0L) {
    stop(
      sprintf(
        paste0(
          "{%s} is not a Suggested package of marginplyr, so guarding on it ",
          "would ask only whether it is installed. Suggested: %s."
        ),
        package,
        paste(named, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  text <- trimws(sub(
    marginplyr_entry_pattern,
    "\\3",
    entries[matched[[1]]]
  ))
  if (!nzchar(text)) {
    return(NULL)
  }
  comparison_pattern <- "^(<=|>=|==|!=|<|>)[[:space:]]*(.+)$"
  parts <- trimws(strsplit(text, ",", fixed = TRUE)[[1]])
  unreadable <- parts[!grepl(comparison_pattern, parts)]
  if (length(unreadable) > 0L) {
    stop(
      sprintf(
        "The version constraint on {%s} is not readable: %s.",
        package,
        paste(unreadable, collapse = "; ")
      ),
      call. = FALSE
    )
  }
  list(
    text = text,
    comparisons = lapply(parts, function(part) {
      list(
        operator = sub(comparison_pattern, "\\1", part),
        version = package_version(sub(comparison_pattern, "\\2", part))
      )
    })
  )
}

# Everything a caller needs to decide whether to run guarded code and, when it
# does not, to say why. The two unusable cases are worded apart on purpose: a
# too-old package reported as "not installed" would send a reader looking for a
# package that is sitting in their library.
marginplyr_suggest_status <- function(package,
                                      suggests =
                                        marginplyr_declared_suggests()) {
  requirement <- marginplyr_suggest_requirement(package, suggests = suggests)
  if (!requireNamespace(package, quietly = TRUE)) {
    return(list(
      package = package,
      available = FALSE,
      installed = FALSE,
      version = NULL,
      requirement = requirement,
      reason = sprintf("{%s} is not installed", package)
    ))
  }
  version <- utils::packageVersion(package)
  satisfied <- is.null(requirement) || all(vapply(
    requirement$comparisons,
    function(comparison) {
      isTRUE(do.call(
        comparison$operator,
        list(version, comparison$version)
      ))
    },
    logical(1)
  ))
  list(
    package = package,
    available = satisfied,
    installed = TRUE,
    version = version,
    requirement = requirement,
    reason = if (satisfied) {
      ""
    } else {
      sprintf(
        "{%s} %s is installed, but marginplyr requires %s",
        package,
        format(version),
        requirement$text
      )
    }
  )
}

# The guard itself. Reads as `requireNamespace()` did at the call sites it
# replaced, and answers the question those call sites always meant to ask.
marginplyr_suggest_available <- function(package,
                                         suggests =
                                           marginplyr_declared_suggests()) {
  marginplyr_suggest_status(package, suggests = suggests)$available
}
