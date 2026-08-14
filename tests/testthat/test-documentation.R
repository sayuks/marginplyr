# The generated function references are the canonical user contract, so the
# rules they state must be reachable from every topic that points at them.
# These tests read the Rd sources rather than any private roxygen structure.

read_documentation <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

rd_topics <- function() {
  # Prefer the source `man/` directory, because `tools::Rd_db()` reads whatever
  # marginplyr version happens to be installed and would silently test stale
  # documentation.
  man <- testthat::test_path("..", "..", "man")
  if (dir.exists(man)) {
    files <- list.files(man, pattern = "[.]Rd$", full.names = TRUE)
    topics <- lapply(files, read_documentation)
    return(stats::setNames(topics, basename(files)))
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
# website's home page includes. `.Rbuildignore` keeps the source out of the
# tarball, so a check run reaches the generated half alone -- but it does reach
# it, and `metadata_path()` is what finds it there, so a run that could reach
# neither copy stops rather than returning a page set with no README in it.
readme_sources <- function() {
  source <- testthat::test_path("..", "..", "README.Rmd")
  paths <- c(source[file.exists(source)], metadata_path("README.md"))
  stats::setNames(lapply(paths, read_documentation), basename(paths))
}

# Every shipped page whose guards this holds to, whatever it is written in.
#
# `.Rbuildignore` keeps no vignette out of the tarball, but `R CMD check`
# unpacks it beside the `.Rcheck` directory rather than inside it, so a vignette
# source is reachable from a repository run only. The Rd topics are reachable
# either way -- from `man/` in a repository, from `tools::Rd_db()` otherwise --
# so a run that finds no vignette still asserts these rules over the examples,
# and adding the pages conditionally rather than skipping keeps
# `verify-backend.R`'s rule that every skip names a backend its job withheld.
documentation_sources <- function() {
  pages <- rd_topics()
  vignettes <- testthat::test_path("..", "..", "vignettes")
  if (dir.exists(vignettes)) {
    sources <- list.files(vignettes, pattern = "[.]qmd$", full.names = TRUE)
    text <- lapply(sources, read_documentation)
    pages <- c(pages, stats::setNames(text, basename(sources)))
  }
  c(pages, readme_sources())
}

test_that("no shipped page guards on installation alone", {
  pages <- documentation_sources()
  skip_if(length(pages) == 0L, "No documentation sources available")

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
# and it is what the two directions below are asserted against.
#
# Only `unpublished` and `published` can be read. Any other value stops the
# test rather than being treated as either, for the reason a malformed
# `must_error` header halts a render: a field this cannot read is a field whose
# assertions silently stopped happening.
cran_status <- function() {
  field <- "Config/marginplyr/cran-status"
  status <- dcf_field(metadata_path("DESCRIPTION"), field)
  if (!isTRUE(status %in% c("unpublished", "published"))) {
    stop(
      "DESCRIPTION's `",
      field,
      "` must read `unpublished` or `published`, not `",
      status,
      "`.",
      call. = FALSE
    )
  }
  status
}

# The instruction itself, which is what a reader installs from. The other two
# forms below claim availability without giving the reader anything to run, so
# only this one can stand for the published direction.
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

test_that("installation instructions follow the recorded CRAN state", {
  # The generated README is the page both directions below are about, and
  # `readme_sources()` stops rather than leaving it out, so neither direction
  # can pass by having nothing to check.
  pages <- documentation_sources()

  claiming <- names(pages)[vapply(
    pages,
    function(text) {
      any(vapply(cran_claims, grepl, logical(1), x = text, fixed = TRUE))
    },
    logical(1)
  )]

  if (identical(cran_status(), "unpublished")) {
    # Named rather than counted, so the failure says which page is telling
    # readers to install from a repository that does not have the package.
    expect_equal(
      claiming,
      character(),
      info = paste(
        "Set `Config/marginplyr/cran-status` to `published` only once CRAN",
        "has published the package; until then no page may claim it."
      )
    )
  } else {
    # The other direction, so flipping the field is what activates the CRAN
    # instruction rather than something a release can forget to write. It asks
    # for the instruction and not merely a claim, because the release also
    # restores a CRAN badge to the same file: a check any claim satisfied
    # would pass a README carrying the badge and still sending every reader to
    # GitHub for the install itself.
    expect_true(grepl(cran_install_call, pages[["README.md"]], fixed = TRUE))
  }
})

test_that("every shipped page using the guard also sources it", {
  pages <- documentation_sources()
  skip_if(length(pages) == 0L, "No documentation sources available")

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
