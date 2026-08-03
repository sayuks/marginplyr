# The generated function references are the canonical user contract, so the
# rules they state must be reachable from every topic that points at them.
# These tests read the Rd sources rather than any private roxygen structure.

rd_topics <- function() {
  # Prefer the source `man/` directory, because `tools::Rd_db()` reads whatever
  # marginplyr version happens to be installed and would silently test stale
  # documentation.
  man <- testthat::test_path("..", "..", "man")
  if (dir.exists(man)) {
    files <- list.files(man, pattern = "[.]Rd$", full.names = TRUE)
    topics <- lapply(files, function(file) {
      paste(readLines(file, warn = FALSE), collapse = "\n")
    })
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
