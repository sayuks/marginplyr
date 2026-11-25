#!/usr/bin/env Rscript

# Submits one retained source tarball to CRAN through its HTTPS form. Stage 11
# of `tools/cran-release.md` owns the approval a submission needs and the
# confirmation that only the maintainer can give.
#
#   Rscript tools/submit-cran.R --tarball PATH --sha256 DIGEST \
#     --comments cran-comments.md [--ca-bundle PATH] [--send]
#
# Without --send it verifies the tarball, fetches the form, and sends nothing.
# With it, it uploads, compares CRAN's review page with the tarball and the
# comments, and submits only when all of it agrees. Exit 0: the preview passed,
# or CRAN sent the confirmation e-mail. Exit 1: nothing was sent. Exit 2: an
# upload was attempted and the submission's outcome is unknown or incomplete. A
# run killed after it prints `Uploading:` has no exit of its own and means the
# same.

source(file.path("tools", "web-submission.R"))

cran_form <- "https://xmpalantir.wu.ac.at/cransubmit/"

# DESCRIPTION fields CRAN's review page shows under its own labels.
cran_review_labels <- c(
  Package = "Package", Version = "Version", Title = "Title",
  Description = "Description", Author = "Author(s)", License = "License",
  Depends = "Depends", Imports = "Imports", Suggests = "Suggests",
  LinkingTo = "Linking To"
)

one_line <- function(text) trimws(gsub("\\s+", " ", text))

html_text <- function(text) {
  entities <- c("&lt;" = "<", "&gt;" = ">", "&quot;" = "\"", "&#039;" = "'",
                "&#39;" = "'", "&amp;" = "&")
  for (entity in names(entities)) {
    text <- gsub(entity, entities[[entity]], text, fixed = TRUE)
  }
  text
}

# Answers the staged tarball's own DESCRIPTION, as built.
read_tarball_description <- function(directory, tarball) {
  utils::untar(file.path(directory, tarball$name),
               files = "marginplyr/DESCRIPTION", exdir = directory)
  read.dcf(file.path(directory, "marginplyr", "DESCRIPTION"))[1L, ]
}

# The submitter the form asks for, taken from the Maintainer field.
cran_submitter <- function(description) {
  maintainer <- one_line(description[["Maintainer"]])
  parts <- regmatches(maintainer, regexec("^(.+) <([^<>]+)>$", maintainer))[[1L]]
  if (length(parts) != 3L) stop_submission("Not one maintainer: ", maintainer)
  list(name = parts[[2L]], email = parts[[3L]])
}

# The value of the one read-only field the review page labels `label`, or NA.
cran_review_value <- function(html, label) {
  cell <- paste0("<td[^>]*>\\Q", label, ":\\E</td>\\s*<td>\\s*")
  input <- paste0(cell, "<input\\b[^>]*?\\bvalue=\"\\K[^\"]*(?=\"\\s+readonly\\s*>)")
  area <- paste0(cell, "<textarea\\b[^>]*\\breadonly\\s*>\\K[^<]*(?=</textarea>)")
  found <- c(
    html_matches(paste0("(?i)", input), html),
    html_matches(paste0("(?i)", area), html)
  )
  if (length(found) == 1L) html_text(found) else NA_character_
}

# Answers what disagrees between the review page and what was sent. The form
# gives line endings back as it stored them.
cran_review_differences <- function(html, description, submitter, comments, id) {
  fields <- names(cran_review_labels)
  expected <- ifelse(fields %in% names(description), description[fields], "")
  actual <- vapply(cran_review_labels, cran_review_value, character(1), html = html)
  same <- !is.na(actual) & one_line(actual) == one_line(expected)
  differing <- fields[!same]

  editable <- html_matches(
    "(?i)<textarea\\b[^>]*name='comment'[^>]*>\\K[^<]*(?=</textarea>)", html
  )
  lines <- function(text) trimws(gsub("\r", "", text, fixed = TRUE), "right")
  holds <- function(text) grepl(text, html, fixed = TRUE)
  checks <- c(
    "the review notice" = holds("Please review and submit."),
    "the submission number" =
      holds(paste0("name=\"pkg_id\" type=\"hidden\" value=\"", id, "\"")),
    "the submit button" =
      holds("type=\"submit\" value=\"Submit package\" name=\"submit\""),
    "Maintainer Name" =
      identical(cran_review_value(html, "Maintainer Name"), submitter$name),
    "Maintainer Email" =
      identical(cran_review_value(html, "Maintainer Email"), submitter$email),
    "the comments" = length(editable) == 1L &&
      identical(lines(html_text(editable)), lines(comments))
  )
  c(differing, names(checks)[!checks])
}

# The redirect target curl reported, which the form answers every post with.
cran_redirect <- function(result) {
  target <- trimws(result$stdout)
  if (startsWith(target, cran_form)) substring(target, nchar(cran_form) + 1L) else ""
}

cran_post_args <- function(common, fields, file) {
  c(
    common, "--cookie", "cookies.txt", "--cookie-jar", "cookies.txt",
    "--header", "Expect:",
    as.vector(rbind("--form-string", paste0(names(fields), "=", fields))),
    "-F", paste0("uploaded_file=", file),
    "-F", "comment=<comments.txt;type=text/plain",
    "--output", "response.html", "--write-out", "%{redirect_url}",
    paste0(cran_form, "index2.php")
  )
}

submit_cran <- function(args, run = run_submission_curl) {
  options <- parse_submission_args(args, c("--tarball", "--sha256", "--comments"))
  directory <- tempfile("cran-submit-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  tarball <- stage_submission_tarball(options$tarball, options$sha256, directory)
  description <- read_tarball_description(directory, tarball)
  submitter <- cran_submitter(description)
  staged_comments <- file.path(directory, "comments.txt")
  if (!file.exists(options$comments) ||
        !file.copy(options$comments, staged_comments)) {
    stop_submission("The comments cannot be read: ", options$comments)
  }
  comments <- paste(
    readLines(staged_comments, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  file.create(file.path(directory, c("empty", "cookies.txt")))
  common <- submission_curl_options(options$ca_bundle)
  fetch <- function(path, name) {
    run(
      c(common, "--cookie", "cookies.txt", "--cookie-jar", "cookies.txt",
        "--output", name, paste0(cran_form, path)),
      directory
    )
  }
  page <- function(name) read_submission_page(file.path(directory, name))

  fetched <- fetch("", "form.html")
  if (!identical(fetched$status, 0L)) {
    stop_submission(
      "The form cannot be fetched; nothing was sent. ",
      submission_curl_failure(fetched)
    )
  }
  form <- page("form.html")
  expected <- c(
    "<FORM action=\"index2.php\" method=\"post\" enctype=\"multipart/form-data\">",
    "name=\"pkg_id\" type=\"hidden\"", "name=\"name\"", "name=\"email\"",
    "name=\"uploaded_file\" type=\"file\"", "name='comment'",
    "value=\"Upload the package\" name=\"upload\""
  )
  if (!all(vapply(expected, grepl, logical(1), x = form, fixed = TRUE))) {
    stop_submission("The form is not the expected one; nothing was sent.")
  }

  cat(
    "Destination: ", cran_form, "\n",
    "Package: ", description[["Package"]], " ", description[["Version"]], "\n",
    "Submitter: ", submitter$name, " <", submitter$email, ">\n",
    "Tarball: ", normalizePath(options$tarball), "\n",
    "Size: ", tarball$size, " bytes\n",
    "SHA-256: ", options$sha256, " (verified)\n",
    "Comments: ", normalizePath(options$comments), " (SHA-256 ",
    unname(tools::sha256sum(staged_comments)), ")\n",
    sep = ""
  )
  if (!options$send) {
    cat("Preview only: nothing was sent. Rerun with --send after approval.\n")
    return(invisible("preview"))
  }

  identity <- c(name = submitter$name, email = submitter$email)
  cat("Uploading: ", utc_now(), "\n", sep = "")
  after_sending({
    uploaded <- run(
      cran_post_args(
        common, c(pkg_id = "", identity, upload = "Upload the package"),
        paste0("@", tarball$name, ";type=application/x-gzip")
      ),
      directory
    )
    if (!identical(uploaded$status, 0L)) stop(submission_curl_failure(uploaded))
    review <- cran_redirect(uploaded)
    if (!grepl("^index[.]php[?]strErr=99&edit=1&pkg_id=[0-9]+$", review)) {
      stop("CRAN did not answer the upload with its review page: ", review)
    }
    id <- sub("^.*pkg_id=", "", review)
    cat("Review page: ", cran_form, review, "\n", sep = "")

    reviewed <- fetch(review, "review.html")
    if (!identical(reviewed$status, 0L)) stop(submission_curl_failure(reviewed))
    differences <- cran_review_differences(
      page("review.html"), description, submitter, comments, id
    )
    if (length(differences) > 0L) {
      stop("Uploaded and not submitted; the review page differs in: ",
           paste(differences, collapse = ", "))
    }
    cat("Review page agrees.\nSubmitting: ", utc_now(), "\n", sep = "")

    submitted <- run(
      cran_post_args(
        common, c(pkg_id = id, identity, submit = "Submit package"),
        "@empty;filename=;type=application/octet-stream"
      ),
      directory
    )
    if (!identical(submitted$status, 0L)) stop(submission_curl_failure(submitted))
    outcome <- cran_redirect(submitted)
    if (!identical(outcome, "index.php?strErr=0&submit=1")) {
      stop("CRAN did not answer the submission as accepted: ", outcome)
    }
  })
  cat("Submitted. CRAN has sent the maintainer its confirmation e-mail.\n")
  invisible("submitted")
}

if (sys.nframe() == 0L) run_submission_command(submit_cran)
