# Fails when a maintained document names a repository path no file answers.
#
# `AGENTS.md`'s *Code comments* requires a citation to name its target rather
# than a coordinate, because a name fails loudly: a grep for it returns
# nothing. Nothing in this repository ran that grep, so a path a document names
# and that was since renamed or deleted stayed in place reading as though it
# resolved -- which is the failure every gate here exists to refuse, a
# reference that asserts nothing and reports nothing (#417).
#
# What is read, and why each bound is where it is:
#
# * Prose, not code. A path in a string literal is resolved by the interpreter
#   and fails at run time when it is wrong; `verify-verifier-invocation.R`
#   already reports the `source()` case. A path in a comment resolves for
#   nobody, so nothing but this fails on it. Markdown is prose throughout; in
#   `.R` and YAML it is the comment text.
# * Maintained documents. `investigation/` notes are not maintained after their
#   date (`investigation/README.md`), and they describe other packages' source
#   trees, so an `R/`-rooted path in a note is rlang's or arrow's and no scan
#   can tell that from ours. Their own file names are still covered, from the
#   side of every document that cites one. `investigation/README.md` states
#   the convention rather than recording a note, so it is read.
# * Generated files. `README.md` is rendered from `README.Rmd`, which is read
#   in its place; `man/` and `NAMESPACE` are roxygen output and carry no
#   extension this reads.
# * Paths, not bare names. A candidate has to contain a `/`. A bare
#   `CHANGELOG.md` or `cnd-last.R` names a file in whatever tree the sentence
#   is about, and matching it against this repository's base names reports
#   prose about another repository as a dead reference.
# * Files, not directories. Prose spells an alternative `a/b` the way a
#   directory path is spelled, so a trailing-slash match is noise rather than a
#   weaker signal: on this repository it returns `and/or`, `Config/Needs/`, and
#   `filter/summarise/` before it returns a directory. A renamed directory is
#   caught through the files under it that anything cites.
#
# A candidate's extension has to be one some tracked file carries. That is
# derived rather than listed, and it is what excludes the `.html` paths, which
# are the site URLs `R/*.R` roxygen links to and the rendered vignettes
# `cran-comments.md` says the tarball ships: no tracked file is `.html`, so an
# `.html` path never names one.
#
# A candidate resolves against the tracked set -- not the working tree, which
# carries `docs/` and other build output that varies by what was last run --
# either repository-relative or relative to the document naming it, since a
# markdown link resolves the second way. A `*` is matched as a glob.
#
# Run it locally with:
#
#     Rscript .github/scripts/verify-doc-references.R

source(".github/scripts/ci-helpers.R")

# Paths a document names deliberately and that no file answers. Each entry
# states why the reference is correct as written; the check below fails when
# one goes stale, in either direction, so this cannot outlive what it excuses.
exempt <- data.frame(
  file = c(
    "design/agents/code-review.md",
    "design/agents/code-review.md"
  ),
  path = c(
    "design/review-dispositions.md",
    "docs/agents/issue-tracker.md"
  ),
  reason = c(
    paste(
      "past tense: #288 retired the file, and the sentence records that it",
      "has no successor"
    ),
    paste(
      "the left column of a table of what the skill looks for, so the row",
      "exists to say this repository has no such file"
    )
  ),
  stringsAsFactors = FALSE
)

tracked <- system2("git", "ls-files", stdout = TRUE)

if (length(tracked) == 0L) {
  stop(call. = FALSE, paste0(
    "`git ls-files` returned nothing, so every path below would resolve to ",
    "no file and every document would be reported. That is this script ",
    "reading the wrong directory, not a repository of dead references."
  ))
}

# The documents read. Prose lives in these six kinds here; the exclusions are
# the two the header argues for.
prose_kinds <- c("md", "Rmd", "qmd", "R", "yaml", "yml")
documents <- tracked[tools::file_ext(tracked) %in% prose_kinds]
documents <- setdiff(documents, "README.md")
documents <- documents[!startsWith(documents, "_quarto/")]
documents <- documents[
  !startsWith(documents, "investigation/") |
    basename(documents) == "README.md"
]

# A URL's path is not a repository path, and a format conversion's argument is
# not written in prose at all -- `"%s/probe-mentioned.R"` names a path the call
# builds. Both are removed before anything is matched, so neither can be read
# as a citation.
strip_noise <- function(lines) {
  lines <- gsub("(https?|ftp)://[^[:space:])>\"'`]*", " ", lines)
  gsub("%[0-9.*-]*[a-zA-Z]", " ", lines)
}

prose <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (tools::file_ext(path) %in% c("R", "yaml", "yml")) {
    lines <- regmatches(lines, regexpr("#.*$", lines))
  }
  strip_noise(lines)
}

# Longest extension first so the alternation cannot stop at `R` inside `Rmd`;
# `\\b` is what rejects the short match, and the order saves the backtrack.
extensions <- unique(tools::file_ext(tracked))
extensions <- extensions[nzchar(extensions)]
extensions <- extensions[order(-nchar(extensions))]

segment <- "[A-Za-z0-9_.*+-]+"
candidate_pattern <- sprintf(
  "%s(/%s)*\\.(%s)\\b",
  segment, segment, paste(extensions, collapse = "|")
)

candidates <- function(lines) {
  found <- unique(unlist(regmatches(lines, gregexpr(candidate_pattern, lines))))
  found[grepl("/", found, fixed = TRUE)]
}

# Collapses `.` and `..` textually. The path may not exist, which is the whole
# question, so `normalizePath()` is not what answers it.
flatten <- function(path) {
  kept <- character()
  for (part in strsplit(path, "/", fixed = TRUE)[[1]]) {
    if (part == ".." && length(kept) > 0L) {
      kept <- kept[-length(kept)]
    } else if (part != "." && part != "..") {
      kept <- c(kept, part)
    }
  }
  paste(kept, collapse = "/")
}

resolves <- function(candidate, from) {
  here <- dirname(from)
  targets <- unique(c(
    candidate,
    if (here == ".") candidate else flatten(file.path(here, candidate))
  ))
  for (target in targets) {
    if (target %in% tracked) {
      return(TRUE)
    }
    globbed <- grepl("*", target, fixed = TRUE) &&
      any(grepl(utils::glob2rx(target), tracked))
    if (globbed) {
      return(TRUE)
    }
  }
  FALSE
}

# The reader, before the verdict. A reader that stopped matching reports a
# clean repository; one that stopped stripping reports every URL in it. Both
# are answered here, where the diagnosis is this file and not a document.
probe <- strip_noise(c(
  "cites `design/architecture.md` and https://example.org/design/absent.md",
  "sprintf(\"# %s/probe-mentioned.R\", dir)"
))
if (!identical(candidates(probe), "design/architecture.md")) {
  stop(call. = FALSE, paste0(
    "The candidate reader in this script does not separate a cited path from ",
    "a URL and a built one, so every verdict below would be about the ",
    "reader. Fix `strip_noise()` and `candidate_pattern` here before reading ",
    "the repository."
  ))
}

dead <- list()
counted <- 0L

for (document in documents) {
  for (candidate in candidates(prose(document))) {
    counted <- counted + 1L
    if (resolves(candidate, document)) {
      next
    }
    dead[[length(dead) + 1L]] <- c(document, candidate)
  }
}

# The exemptions, held to both directions. One whose path came back is an
# exemption excusing nothing; one its document stopped naming is an exemption
# nobody can check.
named <- vapply(
  seq_len(nrow(exempt)),
  function(i) {
    any(vapply(
      dead,
      function(hit) identical(hit, c(exempt$file[i], exempt$path[i])),
      logical(1)
    ))
  },
  logical(1)
)

if (any(!named)) {
  stop(call. = FALSE, sprintf(
    paste0(
      "These exemptions are no longer needed: %s. Either the path now ",
      "answers to a file, or the document stopped naming it. Delete the ",
      "entry from `exempt` in this script."
    ),
    paste(
      sprintf("`%s` in `%s`", exempt$path[!named], exempt$file[!named]),
      collapse = ", "
    )
  ))
}

dead <- Filter(
  function(hit) {
    !any(exempt$file == hit[[1]] & exempt$path == hit[[2]])
  },
  dead
)

write_step_summary(c(
  "## Document references",
  "",
  sprintf(
    "%d path(s) named across %d maintained document(s), %d exempt.",
    counted, length(documents), nrow(exempt)
  ),
  if (length(dead) > 0L) {
    c("", sprintf(
      "- **%s** — named by `%s`",
      vapply(dead, `[[`, character(1), 2L),
      vapply(dead, `[[`, character(1), 1L)
    ))
  }
))

if (length(dead) > 0L) {
  # The remedy is part of the message, as `verify-suite-coverage.R`'s is: a
  # gate that only refuses invites the reference to be deleted rather than
  # pointed at what replaced it.
  stop(call. = FALSE, paste(
    c(
      paste(
        "These documents name paths no tracked file answers, so each reads",
        "as a citation and resolves to nothing:"
      ),
      sprintf(
        "  %s names `%s`",
        vapply(dead, `[[`, character(1), 1L),
        vapply(dead, `[[`, character(1), 2L)
      ),
      paste(
        "Point each at the file that now holds what it cited, or delete the",
        "sentence with what it named. A reference that is correct as written",
        "-- a retired file named in the past tense, or a path outside this",
        "repository -- goes in `exempt` in this script with the reason it is",
        "correct."
      )
    ),
    collapse = "\n"
  ))
}

message(sprintf(
  paste0(
    "Verified: all %d path(s) named across %d maintained document(s) answer ",
    "to a tracked file."
  ),
  counted, length(documents)
))
