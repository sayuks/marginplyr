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
# * Prose, not code. A path in code is either resolved when the code runs,
#   where a wrong one fails on its own, or it is data the code compares
#   against -- `verify-site.R`'s `docs/*.html` keys are the page names it
#   derives. Neither is a citation, and a citation is what this reads.
#   Markdown is prose throughout. R comments come from the parser, so a `#`
#   inside a string is not one. YAML is read from its first `#`, which is the
#   weaker rule of the two: a `#` inside a quoted scalar would be read as a
#   comment, and no workflow here writes one.
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
# * Files, not directories. On `842f156`, the commit before this file, a
#   trailing-slash match returns 56 references answering to no tracked
#   directory, and not one of them is dead: they are DESCRIPTION fields, build
#   output, repository owners, and prose spelling an alternative the way a
#   path is spelled. Fifty-six exemptions is not a gate. A renamed directory
#   is caught through the files under it that anything cites.
#
# A candidate's extension has to be one some tracked file carries, derived
# rather than listed. It is what excludes the `.html` paths `cran-comments.md`
# names, which are the vignettes the tarball ships rather than anything
# tracked. The site URLs `R/*.R` roxygen links to never reach it, being
# stripped as URLs first.
#
# A candidate resolves against the tracked set -- not the working tree, which
# carries `docs/` and other build output that varies by what was last run --
# either repository-relative or relative to the document naming it, since a
# markdown link resolves the second way. A `*` is matched as a glob that stops
# at a path separator.
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

# The documents read. Prose lives in these six kinds here; the two exclusions
# are the ones the header argues for.
prose_kinds <- c("md", "Rmd", "qmd", "R", "yaml", "yml")
documents <- tracked[tools::file_ext(tracked) %in% prose_kinds]
documents <- setdiff(documents, "README.md")
documents <- documents[
  !startsWith(documents, "investigation/") |
    basename(documents) == "README.md"
]

# A URL's path is not a repository path, and neither is what a `%` spelling
# stands in for: `"%s/probe-mentioned.R"` names a path the call builds, and
# `%in%` names an operator. Both are removed before anything is matched.
strip_noise <- function(lines) {
  lines <- gsub("(https?|ftp)://[^[:space:])>\"'`]*", " ", lines)
  gsub("%[0-9.*-]*[a-zA-Z]", " ", lines)
}

# The prose of one document, by kind. Takes lines rather than a path so that
# the probe below runs the same code the scan does.
prose_of <- function(lines, kind) {
  if (kind == "R") {
    data <- utils::getParseData(parse(text = lines, keep.source = TRUE))
    if (is.null(data)) {
      return(character())
    }
    return(data$text[data$token == "COMMENT"])
  }
  if (kind %in% c("yaml", "yml")) {
    return(regmatches(lines, regexpr("#.*$", lines)))
  }
  lines
}

# Longest extension first so the alternation cannot stop at `R` inside `Rmd`;
# `\\b` is what rejects the short match, and the order saves the backtrack.
extensions <- unique(tools::file_ext(tracked))
extensions <- extensions[nzchar(extensions)]
extensions <- extensions[order(-nchar(extensions))]

segment <- "[A-Za-z0-9_.*-]+"
candidate_pattern <- sprintf(
  "%s(/%s)*\\.(%s)\\b",
  segment, segment, paste(extensions, collapse = "|")
)

candidates <- function(lines) {
  lines <- strip_noise(lines)
  found <- unique(unlist(regmatches(lines, gregexpr(candidate_pattern, lines))))
  found[grepl("/", found, fixed = TRUE)]
}

# Collapses `.` and `..` textually, because the path may not exist and that is
# the whole question, so `normalizePath()` is not what answers it. `NA` when
# `..` walks past the root: a path outside the repository is not a repository
# path, and dropping the `..` silently would resolve it against the wrong tree.
flatten <- function(path) {
  kept <- character()
  for (part in strsplit(path, "/", fixed = TRUE)[[1]]) {
    if (part == "..") {
      if (length(kept) == 0L) {
        return(NA_character_)
      }
      kept <- kept[-length(kept)]
    } else if (part != ".") {
      kept <- c(kept, part)
    }
  }
  paste(kept, collapse = "/")
}

# `utils::glob2rx()` is not what matches a glob here: it renders `*` as `.*`,
# which crosses `/`, so `man/*.Rd` would answer to a file under a directory of
# `man/`. Each literal run is escaped and each `*` becomes `[^/]*`.
glob_pattern <- function(path) {
  parts <- strsplit(path, "*", fixed = TRUE)[[1]]
  if (endsWith(path, "*")) {
    parts <- c(parts, "")
  }
  escape <- function(part) gsub("([][^$.|?*+(){}\\\\])", "\\\\\\1", part)
  quoted <- vapply(parts, escape, character(1))
  sprintf("^%s$", paste(quoted, collapse = "[^/]*"))
}

resolves <- function(candidate, from) {
  here <- dirname(from)
  targets <- unique(c(
    candidate,
    if (here == ".") candidate else flatten(file.path(here, candidate))
  ))
  targets <- targets[!is.na(targets)]
  for (target in targets) {
    if (target %in% tracked) {
      return(TRUE)
    }
    globbed <- grepl("*", target, fixed = TRUE) &&
      any(grepl(glob_pattern(target), tracked))
    if (globbed) {
      return(TRUE)
    }
  }
  FALSE
}

# The reader, before the verdict, in each way it can go quiet. A reader that
# stopped matching reports a clean repository; one that stopped stripping
# reports every URL in it; one that stopped separating prose from code drops
# the 66 candidates that reach this scan through an R or YAML comment, or adds
# every path a string literal holds. Each is answered here, where the
# diagnosis is this file and not a document.
probes <- list(
  list(
    kind = "md",
    lines = "cites `design/architecture.md`, https://example.org/design/x.md",
    want = "design/architecture.md"
  ),
  list(
    kind = "R",
    lines = c(
      "path <- \"# tests/testthat/inside-a-string.R\"",
      "# cites `design/architecture.md`"
    ),
    want = "design/architecture.md"
  ),
  list(
    kind = "yaml",
    lines = c(
      "        run: Rscript .github/scripts/verify-site.R",
      "      # cites `design/architecture.md`"
    ),
    want = "design/architecture.md"
  )
)

for (probe in probes) {
  if (!identical(candidates(prose_of(probe$lines, probe$kind)), probe$want)) {
    stop(call. = FALSE, sprintf(
      paste0(
        "The reader in this script does not separate a cited path from a URL ",
        "and from code, on a `%s` document, so every verdict below would be ",
        "about the reader. Fix `prose_of()`, `strip_noise()`, and ",
        "`candidate_pattern` here before reading the repository."
      ),
      probe$kind
    ))
  }
}

if (length(documents) < 2L) {
  stop(call. = FALSE, sprintf(
    paste0(
      "%d document(s) were selected out of %d tracked file(s), so the scan ",
      "below reads almost nothing and passes on any repository. Fix ",
      "`prose_kinds` and the exclusions in this script before trusting what ",
      "it reports."
    ),
    length(documents), length(tracked)
  ))
}

dead <- data.frame(
  file = character(), path = character(), stringsAsFactors = FALSE
)
counted <- 0L

for (document in documents) {
  lines <- readLines(document, warn = FALSE)
  for (candidate in candidates(prose_of(lines, tools::file_ext(document)))) {
    counted <- counted + 1L
    if (resolves(candidate, document)) {
      next
    }
    dead <- rbind(dead, data.frame(
      file = document, path = candidate, stringsAsFactors = FALSE
    ))
  }
}

# The last way this goes quiet, and the one the probes cannot reach: they read
# three lines this file holds, so they pass on a repository whose documents
# this script never opened.
if (counted == 0L) {
  stop(call. = FALSE, sprintf(
    paste0(
      "No path was found in any of %d document(s), which is a repository ",
      "that cites nothing. Fix the extension derivation and `candidates()` ",
      "in this script before trusting what it reports."
    ),
    length(documents)
  ))
}

key <- function(frame) paste(frame$file, frame$path)

# The exemptions, held to both directions. One whose path came back is an
# exemption excusing nothing; one its document stopped naming is an exemption
# nobody can check.
stale <- !(key(exempt) %in% key(dead))

if (any(stale)) {
  stop(call. = FALSE, sprintf(
    paste0(
      "These exemptions are no longer needed: %s. Either the path now ",
      "answers to a file, or the document stopped naming it. Delete the ",
      "entry from `exempt` in this script."
    ),
    paste(
      sprintf("`%s` in `%s`", exempt$path[stale], exempt$file[stale]),
      collapse = ", "
    )
  ))
}

dead <- dead[!(key(dead) %in% key(exempt)), , drop = FALSE]

write_step_summary(c(
  "## Document references",
  "",
  sprintf(
    "%d path(s) named across %d maintained document(s), %d exempt.",
    counted, length(documents), nrow(exempt)
  ),
  if (nrow(dead) > 0L) {
    c("", sprintf("- **%s** — named by `%s`", dead$path, dead$file))
  }
))

if (nrow(dead) > 0L) {
  # The remedy is part of the message, as `verify-suite-coverage.R`'s is: a
  # gate that only refuses invites the reference to be deleted rather than
  # pointed at what replaced it.
  stop(call. = FALSE, paste(
    c(
      paste(
        "These documents name paths no tracked file answers, so each reads",
        "as a citation and resolves to nothing:"
      ),
      sprintf("  %s names `%s`", dead$file, dead$path),
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
