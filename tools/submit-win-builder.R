#!/usr/bin/env Rscript

# Sends one retained source tarball to Win-Builder R-devel through its HTTPS
# form. Stage 9 of `tools/cran-release.md` owns the approval a send needs and
# what its result is evidence of.
#
#   Rscript tools/submit-win-builder.R --tarball PATH --sha256 DIGEST \
#     [--ca-bundle PATH] [--send]
#
# Without --send it verifies the tarball, fetches the form, and sends nothing.
# Exit 0: the preview passed, or the form accepted the upload. Exit 1: nothing
# was sent. Exit 2: an upload was attempted and its acceptance is unknown. A run
# killed after it prints `Sending:` has no exit of its own and means the same.

source(file.path("tools", "web-submission.R"))

win_builder_form <- "https://win-builder.r-project.org/upload.aspx"

# The named attribute of one tag, or NA where it has none.
html_attribute <- function(tag, name) {
  found <- html_matches(paste0("(?<=\\s", name, "=\")[^\"]*"), tag)
  if (length(found) == 1L) found else NA_character_
}

# Answers the page's hidden fields, every file field, and the R-devel section's
# own file field, submit field, submit label, and status text.
parse_win_builder_form <- function(html) {
  inputs <- html_matches("<input\\b[^>]*>", html)
  types <- vapply(inputs, html_attribute, character(1), "type")
  values <- function(tags, name) {
    unname(vapply(tags, html_attribute, character(1), name))
  }
  hidden <- inputs[types %in% "hidden"]

  section <- html_matches("(?s)<h2>\\s*R-devel\\s*</h2>.*?(?=<h2>|</form>)", html)
  if (length(section) != 1L) {
    stop_submission("The form does not hold exactly one R-devel section.")
  }
  own <- html_matches("<input\\b[^>]*>", section)
  own_types <- vapply(own, html_attribute, character(1), "type")
  file <- own[own_types %in% "file"]
  submit <- own[own_types %in% "submit"]
  status <- html_matches("(?s)(?<=>)[^>]*(?:<br\\s*/?>[^<]*)*(?=</span>)", section)

  form <- list(
    hidden = stats::setNames(values(hidden, "value"), values(hidden, "name")),
    files = values(inputs[types %in% "file"], "name"),
    file = values(file, "name"),
    submit = values(submit, "name"),
    submit_label = values(submit, "value"),
    status = paste(gsub("<br\\s*/?>", "; ", status), collapse = "")
  )
  # curl reads `=`, `@`, `<` and `;` in a field as its own syntax.
  names <- c(names(form$hidden), form$files, form$submit)
  required <- c("__VIEWSTATE", "__VIEWSTATEGENERATOR", "__EVENTVALIDATION")
  present <- form$hidden[required]
  if (anyNA(present) || !all(nzchar(present)) || anyNA(form$hidden) ||
        length(form$file) != 1L || length(form$submit) != 1L ||
        is.na(form$submit_label) || !all(grepl("^[A-Za-z0-9_]+$", names))) {
    stop_submission("The form's R-devel fields are not the expected ones.")
  }
  form
}

# curl arguments for one multipart submission from the staging directory, which
# holds the tarball and an empty file. A browser sends the sections it leaves
# empty as files without a name.
win_builder_upload_args <- function(form, name) {
  hidden <- paste0(names(form$hidden), "=", form$hidden)
  files <- vapply(form$files, function(field) {
    if (identical(field, form$file)) {
      paste0(field, "=@", name, ";type=application/x-gzip")
    } else {
      paste0(field, "=@empty;filename=;type=application/octet-stream")
    }
  }, character(1), USE.NAMES = FALSE)
  c(
    as.vector(rbind("--form-string", hidden)),
    as.vector(rbind("-F", files)),
    "--form-string", paste0(form$submit, "=", form$submit_label)
  )
}

# The form answers 200 with or without an upload. Only the R-devel section's
# own status text says that it took this tarball whole.
win_builder_accepted <- function(status, tarball) {
  startsWith(status, paste0("File name: ", tarball$name, "; ")) &&
    grepl(paste0("; File Size: ", tarball$size, " bytes;"), status, fixed = TRUE)
}

submit_win_builder <- function(args, run = run_submission_curl) {
  options <- parse_submission_args(args, c("--tarball", "--sha256"))
  directory <- tempfile("win-builder-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  tarball <- stage_submission_tarball(options$tarball, options$sha256, directory)
  file.create(file.path(directory, "empty"))
  common <- submission_curl_options(options$ca_bundle)
  read_form <- function(name) {
    parse_win_builder_form(read_submission_page(file.path(directory, name)))
  }

  fetched <- run(
    c(common, "--cookie-jar", "cookies.txt", "--output", "form.html",
      win_builder_form),
    directory
  )
  if (!identical(fetched$status, 0L)) {
    stop_submission(
      "The form cannot be fetched; nothing was sent. ",
      submission_curl_failure(fetched)
    )
  }
  form <- read_form("form.html")

  cat(
    "Destination: ", win_builder_form, " (R-devel: ", form$file, ", ",
    form$submit, ")\n",
    "Tarball: ", normalizePath(options$tarball), "\n",
    "Size: ", tarball$size, " bytes\n",
    "SHA-256: ", options$sha256, " (verified)\n",
    sep = ""
  )
  if (!options$send) {
    cat("Preview only: nothing was sent. Rerun with --send after approval.\n")
    return(invisible("preview"))
  }

  cat("Sending: ", utc_now(), "\n", sep = "")
  after_sending({
    sent <- run(
      c(common, "--cookie", "cookies.txt", "--header", "Expect:",
        win_builder_upload_args(form, tarball$name),
        "--output", "response.html", win_builder_form),
      directory
    )
    if (!identical(sent$status, 0L)) stop(submission_curl_failure(sent))
    status <- read_form("response.html")$status
    cat("Response: ", status, "\n", sep = "")
    if (!win_builder_accepted(status, tarball)) {
      stop("The response does not name this tarball and size.")
    }
    cat("Accepted.\n")
  })
  invisible("accepted")
}

if (sys.nframe() == 0L) run_submission_command(submit_win_builder)
