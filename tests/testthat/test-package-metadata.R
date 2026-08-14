# `DESCRIPTION`'s `Authors@R` records who holds copyright in this package, and
# both license files restate that name in their own format. Nothing compared the
# three, which is how `LICENSE` and `LICENSE.md` came to name a holder
# `Authors@R` never mentions (#115): each file reads as a settled statement on
# its own, and only reading them together shows the package giving two answers
# to one question. This test derives the expected name from `Authors@R` instead
# of restating it, so the holder is recorded in one place and the license files
# are checked against that record.

# `metadata_path()` and `dcf_field()` come from `helper-package-metadata.R`,
# because `test-documentation.R` asks the same two questions of DESCRIPTION and
# the generated README.

# `format()` renders a `person` the way a license notice names someone, so the
# expectation comes from the metadata rather than from a second copy of the
# name. Every `cph` is included, so adding a holder to `Authors@R` fails here
# until the license files name them too.
description_copyright_holders <- function() {
  authors_r <- dcf_field(metadata_path("DESCRIPTION"), "Authors@R")
  authors <- eval(parse(text = authors_r))
  holders <- Filter(function(author) "cph" %in% author$role, authors)
  if (length(holders) == 0L) {
    stop("DESCRIPTION declares no author with the `cph` role.", call. = FALSE)
  }
  format(holders, include = c("given", "family"))
}

# `LICENSE` is a DCF file, which is what R itself reads it as when expanding the
# `MIT + file LICENSE` template.
license_copyright_holder <- function() {
  dcf_field(metadata_path("LICENSE"), "COPYRIGHT HOLDER")
}

# `LICENSE.md` states the holder in prose, so match the line the MIT text puts
# it on rather than the whole notice: the contract is the identity, not the
# wording around it. The year is matched only to locate the line and is then
# discarded, because copyright-year policy is a separate question.
license_md_copyright_holder <- function(path) {
  notice <- "^Copyright \\(c\\) [0-9]{4} (.+)$"
  matched <- grep(notice, readLines(path, warn = FALSE), value = TRUE)
  if (length(matched) != 1L) {
    stop(
      sprintf(
        "%s states %d copyright notices; expected exactly one.",
        basename(path),
        length(matched)
      ),
      call. = FALSE
    )
  }
  trimws(sub(notice, "\\1", matched))
}

test_that("the license files name the copyright holder DESCRIPTION declares", {
  expected <- paste(description_copyright_holders(), collapse = ", ")

  expect_equal(license_copyright_holder(), expected)

  # `LICENSE.md` is `.Rbuildignore`d, so it is a repository file rather than a
  # shipped one and is absent whenever the tests run from a built tarball.
  # Reading it conditionally rather than skipping keeps `verify-backend.R`'s
  # rule that every skip names a backend its job withheld, and the shipped
  # `LICENSE` is asserted either way, so this test is never an empty one.
  license_md <- test_path("..", "..", "LICENSE.md")
  if (file.exists(license_md)) {
    expect_equal(license_md_copyright_holder(license_md), expected)
  }
})
