# The generated function references are the canonical user contract, so the
# rules they state must be reachable from every topic that points at them.
# These tests read the Rd sources rather than any private roxygen structure.

read_documentation <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

# Pages keyed by the file name a failure has to name, which is the only handle
# a reader has on a page read out of two directories and an installed library.
read_pages <- function(paths) {
  stats::setNames(lapply(paths, read_documentation), basename(paths))
}

rd_topics <- function() {
  # Prefer the source `man/` directory, because `tools::Rd_db()` reads whatever
  # marginplyr version happens to be installed and would silently test stale
  # documentation.
  man <- testthat::test_path("..", "..", "man")
  if (dir.exists(man)) {
    files <- list.files(man, pattern = "[.]Rd$", full.names = TRUE)
    return(read_pages(files))
  }

  db <- tryCatch(tools::Rd_db("marginplyr"), error = function(cnd) NULL)
  if (length(db) == 0L) {
    return(NULL)
  }
  lapply(db, function(rd) {
    paste(utils::capture.output(print(rd)), collapse = "\n")
  })
}

rd_section_titles <- function(text) {
  matches <- regmatches(text, gregexpr("\\\\section\\{[^{}]+\\}", text))[[1]]
  gsub("^\\\\section\\{|\\}$", "", matches)
}

# An italicised section reference takes one of two forms. A bare
# `\emph{Title}` names a section in the same topic, while
# `\emph{\link[=topic]{Title}}` names one in `topic`. Both are resolved against
# the topic they actually promise, so a cross-topic reference cannot pass by
# accident.
rd_section_references <- function(text, topic) {
  linked <- "\\\\emph\\{\\\\link\\[=([^][]+)\\]\\{([^{}]+)\\}\\}"
  linked_matches <- regmatches(text, gregexpr(linked, text))[[1]]
  linked_refs <- data.frame(
    # `paste0()` would recycle the suffix into a phantom row when nothing
    # matched, so build the target from the match itself.
    target = sub(linked, "\\1.Rd", linked_matches),
    title = sub(linked, "\\2", linked_matches)
  )

  without_links <- gsub(linked, "", text)
  plain_matches <- regmatches(
    without_links,
    gregexpr("\\\\emph\\{[^{}]+\\}", without_links)
  )[[1]]
  plain_refs <- data.frame(
    target = rep(topic, length(plain_matches)),
    title = gsub("^\\\\emph\\{|\\}$", "", plain_matches)
  )

  rbind(linked_refs, plain_refs)
}

test_that("italicised section references reach the section they promise", {
  topics <- rd_topics()
  skip_if(is.null(topics), "No Rd sources available")

  sections <- lapply(topics, rd_section_titles)
  # Only phrases that actually name a section somewhere in the package are
  # treated as cross-references; ordinary emphasis is left alone.
  known <- unique(unlist(sections, use.names = FALSE))

  unresolved <- unlist(
    lapply(names(topics), function(topic) {
      refs <- rd_section_references(topics[[topic]], topic)
      refs <- refs[refs$title %in% known, , drop = FALSE]
      reached <- mapply(
        function(target, title) title %in% sections[[target]],
        refs$target,
        refs$title
      )
      if (all(reached)) {
        return(NULL)
      }
      broken <- refs[!reached, , drop = FALSE]
      paste0(topic, " -> ", broken$target, ": ", broken$title)
    }),
    use.names = FALSE
  )

  expect_equal(unresolved, NULL)
})

test_that("every documented user-facing topic offers related-topic links", {
  topics <- rd_topics()
  skip_if(is.null(topics), "No Rd sources available")

  navigable <- vapply(
    topics,
    function(text) grepl("\\seealso{", text, fixed = TRUE),
    logical(1)
  )

  expect_equal(names(navigable)[!navigable], character())
})

test_that("the Grouping-identity comparison has exactly one canonical home", {
  topics <- rd_topics()
  skip_if(is.null(topics), "No Rd sources available")

  # The table is recognised by its own header cells rather than by a section
  # title, so a copy pasted into another topic is still detected.
  header <- "Value \\tab Meaning \\tab Duplicate Grouping-set occurrences"
  carries_table <- vapply(
    topics,
    function(text) grepl(header, text, fixed = TRUE),
    logical(1)
  )

  expect_equal(names(topics)[carries_table], "grouping_bit.Rd")
})

# Every guard in the shipped documentation decides whether optional code runs,
# and `requireNamespace()` cannot make that decision correctly: it reports an
# installed-but-too-old package usable, and the guarded code then calls an API
# that version does not have (#123). `inst/suggests/guard.R` is what reads the
# constraint DESCRIPTION states, and these scans are what keep a new guard from
# quietly going back to the version-blind question.
#
# The scan is the gate rather than a list of guard sites, for the reason
# `AGENTS.md` gives for deriving rather than listing elsewhere: a list has to be
# edited when a guard is added, and the failure of the one left behind is
# silence.
#
# Both names the *Release matrix* section forbids are scanned, not only the one
# #123 found. The rlang spelling even takes a version argument of its own,
# which makes it the likelier of the two to arrive looking correct.
version_blind_guards <- c("requireNamespace", "is_installed")

# Both halves of the README are pages: `README.Rmd` is where a claim is
# written, and `README.md` is what a reader on GitHub is shown and what the
# website's home page includes.
#
# A repository run reads both. A check run reads whichever half R installed,
# and that is a question about the R running the check, not about this package:
# `.Rbuildignore` keeps `README.Rmd` out of the tarball, and "Package README.md
# files are now installed and featured in HTML help" is an R 4.6.0 change, while
# `DESCRIPTION` supports 4.1.0. So an older R checking a tarball reaches
# neither, which is a page the scans do not see rather than a failure -- the
# residency the vignette sources already have.
readme_sources <- function() {
  repository <- c(
    testthat::test_path("..", "..", "README.Rmd"),
    testthat::test_path("..", "..", "README.md")
  )
  paths <- repository[file.exists(repository)]
  if (length(paths) == 0L) {
    installed <- system.file("README.md", package = "marginplyr")
    paths <- installed[nzchar(installed)]
  }
  read_pages(paths)
}

# Every shipped page the scans below hold to their rules, whatever it is
# written in. The three sources reach a run from different places, and each
# takes the strongest route open to it:
#
# - Rd topics: `man/` in a repository, `tools::Rd_db()` otherwise.
# - Vignette sources: repository-only, because `R CMD check` unpacks the
#   tarball beside the `.Rcheck` directory rather than inside it.
# - The README: repository copies, else the installed `README.md` where the R
#   running the check is new enough to have installed one.
#
# Each is added when it is reachable rather than skipped for when it is not,
# because a skip naming no withheld backend is what `verify-backend.R` fails a
# job over. Reaching nothing at all is a different thing and stops: every scan
# below iterates over this set, so a set that arrived empty is a set that
# passes.
documentation_sources <- function() {
  pages <- rd_topics()
  vignettes <- testthat::test_path("..", "..", "vignettes")
  if (dir.exists(vignettes)) {
    sources <- list.files(vignettes, pattern = "[.]qmd$", full.names = TRUE)
    pages <- c(pages, read_pages(sources))
  }
  pages <- c(pages, readme_sources())
  if (length(pages) == 0L) {
    stop("No documentation source is reachable to scan.", call. = FALSE)
  }
  pages
}

test_that("no shipped page guards on installation alone", {
  pages <- documentation_sources()

  blind <- vapply(
    pages,
    function(text) {
      any(vapply(
        version_blind_guards,
        grepl,
        logical(1),
        x = text,
        fixed = TRUE
      ))
    },
    logical(1)
  )

  # Named rather than counted, so the failure says which page to fix and what
  # to replace the call with.
  expect_equal(
    names(pages)[blind],
    character(),
    info = paste(
      "Guard with `marginplyr_suggest_available()` from",
      "`inst/suggests/guard.R` instead."
    )
  )
})

# Where a reader can get the package is a fact about the outside world, and no
# page can be read to find it out: a README claiming CRAN availability reads
# exactly the same whether CRAN has published the package or not. DESCRIPTION's
# `Config/marginplyr/cran-status` field is where that fact is recorded, once,
# and it is what the directions below are asserted against. It sits beside
# `Version`, which is the line a release edits anyway.
cran_status_field <- "Config/marginplyr/cran-status"

# Only `unpublished` and `published` can be read. Any other value stops rather
# than being treated as either, for the reason a malformed `must_error` header
# halts a render: a field this cannot read is a field whose assertions silently
# stopped happening. Reading and checking are separate so that the refusal is
# executed by a test today, rather than first attempted on release day.
checked_cran_status <- function(status) {
  if (!isTRUE(status %in% c("unpublished", "published"))) {
    stop(
      "DESCRIPTION's `",
      cran_status_field,
      "` must read `unpublished` or `published`, not `",
      status,
      "`.",
      call. = FALSE
    )
  }
  status
}

cran_status <- function() {
  description <- metadata_path("DESCRIPTION")
  checked_cran_status(dcf_field(description, cran_status_field))
}

# The instruction itself, which is the only one of the three forms below that
# gives a reader something to run.
cran_install_call <- "install.packages(\"marginplyr\")"

# The concrete forms in which a page tells a reader that CRAN has this package.
# Each one names marginplyr, because the README's comparison table links to
# another package's CRAN page and that is not a claim about this one. Prose
# needing to mention the CRAN call has to spell it some other way, as the
# version-blind guard scan above already requires of prose naming those calls.
cran_claims <- c(
  cran_install_call,
  "cranlogs.r-pkg.org/badges/marginplyr",
  "cran.r-project.org/package=marginplyr"
)

# Matched case-insensitively, because the CRAN host is written
# `cran.r-project.org` and `CRAN.R-project.org` about equally often and the
# same claim in the second spelling is still the claim. `\Q...\E` quotes the
# marker so that a call's own parentheses and quotes stay text rather than
# becoming a pattern, as `verify-site.R` quotes a home directory.
holds_any <- function(text, markers) {
  any(vapply(
    markers,
    function(marker) {
      grepl(paste0("\\Q", marker, "\\E"), text, perl = TRUE, ignore.case = TRUE)
    },
    logical(1)
  ))
}

# The pages that disagree with the recorded state, which is what both
# directions reduce to. Taking the state as an argument is what lets the
# fixtures below execute the direction the field does not currently select: a
# `published` branch first evaluated on release day is a branch nothing has
# ever run.
#
# A page disagrees with `unpublished` by claiming CRAN has the package. The
# README disagrees with `published` by dropping either half of what publication
# gives a reader -- the instruction to run, and the badge or link that says
# where it goes. Requiring both is what holds the release to its own steps,
# since a check either half satisfied would pass a README that carries the
# badge and still sends every reader to GitHub to install.
cran_state_disagreements <- function(status, pages) {
  claiming <- names(pages)[vapply(pages, holds_any, logical(1), cran_claims)]
  if (identical(status, "unpublished")) {
    return(claiming)
  }

  readme <- pages[["README.md"]]
  if (is.null(readme)) {
    # No copy of the generated README was reachable, which is the older-R
    # tarball case `readme_sources()` describes. There is nothing to judge
    # here, and the repository run that produced the tarball judged it.
    return(character())
  }

  complete <- holds_any(readme, cran_install_call) &&
    holds_any(readme, setdiff(cran_claims, cran_install_call))
  if (complete) character() else "README.md"
}

test_that("installation instructions follow the recorded CRAN state", {
  pages <- documentation_sources()

  # The generated README is the page the `published` direction is about, and a
  # repository run always has both halves on disk. Asserting that they reached
  # the page set is what stops a broken derivation from reading as a state
  # nothing disagrees with -- which is the one way this gate could go quiet.
  if (file.exists(testthat::test_path("..", "..", "README.md"))) {
    expect_true(all(c("README.Rmd", "README.md") %in% names(pages)))
  }

  disagreeing <- cran_state_disagreements(cran_status(), pages)

  # Named rather than counted, so the failure says which page is out of step
  # with the state, and where the rule it broke is written down.
  expect_equal(
    disagreeing,
    character(),
    info = paste(
      "See *Installation instructions* in `AGENTS.md`:",
      "`Config/marginplyr/cran-status` becomes `published` on the day CRAN",
      "publishes the package, and the README changes with it."
    )
  )
})

test_that("the CRAN-state rule reads both states", {
  github_route <- list("README.md" = "pak::pkg_install(\"sayuks/marginplyr\")")
  # The uppercase spelling of the host, which is the same claim.
  badge <- "https://CRAN.R-project.org/package=marginplyr"
  badge_only <- list("README.md" = badge)
  instruction_only <- list("README.md" = cran_install_call)
  published_readme <- list("README.md" = paste(badge, cran_install_call))

  expect_equal(
    cran_state_disagreements("unpublished", github_route),
    character()
  )
  expect_equal(cran_state_disagreements("unpublished", badge_only), "README.md")
  expect_equal(
    cran_state_disagreements("unpublished", instruction_only),
    "README.md"
  )

  expect_equal(
    cran_state_disagreements("published", published_readme),
    character()
  )
  expect_equal(cran_state_disagreements("published", github_route), "README.md")
  # Half a release is a failure in both halves: a badge is not an instruction,
  # and an instruction is not the link the badge block is supposed to regain.
  expect_equal(cran_state_disagreements("published", badge_only), "README.md")
  expect_equal(
    cran_state_disagreements("published", instruction_only),
    "README.md"
  )

  # An older R checking a tarball, where neither half of the README is
  # readable. The Rd topics are still held to `unpublished`.
  no_readme <- list("summarize_with_margins.Rd" = cran_install_call)
  expect_equal(
    cran_state_disagreements("unpublished", no_readme),
    "summarize_with_margins.Rd"
  )
  expect_equal(cran_state_disagreements("published", no_readme), character())
})

test_that("an unreadable CRAN state stops rather than choosing a direction", {
  expect_equal(checked_cran_status("unpublished"), "unpublished")
  expect_equal(checked_cran_status("published"), "published")
  expect_error(checked_cran_status("soon"), "must read")
  # What `read.dcf()` returns for a field DESCRIPTION does not state.
  expect_error(checked_cran_status(NA_character_), "must read")
})

test_that("every shipped page using the guard also sources it", {
  pages <- documentation_sources()

  # An example is evaluated with marginplyr attached but nothing sourced, and a
  # vignette chunk is knitted the same way, so a guard call without the
  # `source()` line above it is an object-not-found error rather than the
  # withheld content it reads as.
  uses <- grepl("marginplyr_suggest_available", pages, fixed = TRUE)
  sources <- grepl("\"suggests\", \"guard.R\"", pages, fixed = TRUE)

  expect_equal(names(pages)[uses & !sources], character())
  # The other direction, so a `source()` left behind by a deleted guard is
  # reported rather than shipped as an unexplained line.
  expect_equal(names(pages)[sources & !uses], character())
})
