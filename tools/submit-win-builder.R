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

win_builder_form <- "https://win-builder.r-project.org/upload.aspx"

stop_win_builder <- function(..., status = 1L) {
  stop(structure(
    class = c("marginplyr_win_builder_error", "error", "condition"),
    list(message = paste0(...), call = NULL, status = status)
  ))
}

# Runs curl without a shell, so no argument is parsed or expanded by one.
run_win_builder_curl <- function(args, directory) {
  result <- tryCatch(
    processx::run("curl", args, wd = directory, error_on_status = FALSE),
    error = function(error) list(status = NA_integer_, stdout = "",
                                 stderr = conditionMessage(error))
  )
  result[c("status", "stdout", "stderr")]
}

parse_win_builder_args <- function(args) {
  options <- list(tarball = NULL, sha256 = NULL, ca_bundle = NULL, send = FALSE)
  while (length(args) > 0L) {
    if (identical(args[[1L]], "--send")) {
      options$send <- TRUE
      args <- args[-1L]
      next
    }
    name <- gsub("-", "_", sub("^--", "", args[[1L]]), fixed = TRUE)
    if (!args[[1L]] %in% c("--tarball", "--sha256", "--ca-bundle") ||
          length(args) < 2L || !is.null(options[[name]])) {
      stop_win_builder(
        "Usage: submit-win-builder.R --tarball PATH --sha256 DIGEST ",
        "[--ca-bundle PATH] [--send]"
      )
    }
    options[[name]] <- args[[2L]]
    args <- args[-(1:2)]
  }
  if (is.null(options$tarball) || is.null(options$sha256)) {
    stop_win_builder("Both --tarball and --sha256 are required.")
  }
  if (!is.null(options$ca_bundle)) {
    if (!file.exists(options$ca_bundle)) {
      stop_win_builder("The CA bundle does not exist: ", options$ca_bundle)
    }
    options$ca_bundle <- normalizePath(options$ca_bundle)
  }
  options
}

# Copies the tarball into `directory` and answers its name and size. The digest
# is taken from the copy, so the bytes verified are the bytes sent.
stage_win_builder_tarball <- function(path, sha256, directory) {
  name <- basename(path)
  if (!grepl("^marginplyr_[0-9]+([.][0-9]+)+[.]tar[.]gz$", name)) {
    stop_win_builder("Not a marginplyr source tarball: ", name)
  }
  staged <- file.path(directory, name)
  if (!file.exists(path) || !file.copy(path, staged)) {
    stop_win_builder("The tarball cannot be read: ", path)
  }
  actual <- unname(tools::sha256sum(staged))
  if (!identical(actual, sha256)) {
    stop_win_builder("SHA-256 is ", actual, ", not ", sha256, ".")
  }
  list(name = name, size = format(file.size(staged), scientific = FALSE))
}

html_matches <- function(pattern, html) {
  regmatches(html, gregexpr(pattern, html, perl = TRUE))[[1L]]
}

# The named attribute of one tag, or NA where it has none.
html_attribute <- function(tag, name) {
  found <- html_matches(paste0("(?<=\\s", name, "=\")[^\"]*"), tag)
  if (length(found) == 1L) found else NA_character_
}

# Answers the page's hidden fields, every file field, and the R-devel section's
# own file field, submit field, submit label, and status text. Comments are
# dropped first, because the page keeps a retired section in one.
parse_win_builder_form <- function(html) {
  html <- gsub("(?s)<!--.*?-->", "", paste(html, collapse = "\n"), perl = TRUE)
  inputs <- html_matches("<input\\b[^>]*>", html)
  types <- vapply(inputs, html_attribute, character(1), "type")
  values <- function(tags, name) {
    unname(vapply(tags, html_attribute, character(1), name))
  }
  hidden <- inputs[types %in% "hidden"]

  section <- html_matches("(?s)<h2>\\s*R-devel\\s*</h2>.*?(?=<h2>|</form>)", html)
  if (length(section) != 1L) {
    stop_win_builder("The form does not hold exactly one R-devel section.")
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
    stop_win_builder("The form's R-devel fields are not the expected ones.")
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

win_builder_curl_failure <- function(result) {
  paste0(
    "curl status ", result$status, ": ", trimws(result$stderr),
    if (identical(result$status, 60L)) {
      paste0(
        "\ncurl could not verify the certificate. If its CA list lacks the ",
        "site's root, rerun with --ca-bundle naming a current CA bundle."
      )
    }
  )
}

submit_win_builder <- function(args, run = run_win_builder_curl) {
  options <- parse_win_builder_args(args)
  directory <- tempfile("win-builder-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  tarball <- stage_win_builder_tarball(options$tarball, options$sha256, directory)
  file.create(file.path(directory, "empty"))

  # --disable keeps a .curlrc out, which could turn off verification or retry.
  common <- c(
    "--disable", "--silent", "--show-error", "--fail", "--proto", "=https",
    "--max-time", "300",
    if (!is.null(options$ca_bundle)) c("--cacert", options$ca_bundle)
  )
  read_page <- function(name) {
    path <- file.path(directory, name)
    if (file.exists(path)) readLines(path, warn = FALSE) else character()
  }

  fetched <- run(
    c(common, "--cookie-jar", "cookies.txt", "--output", "form.html",
      win_builder_form),
    directory
  )
  if (!identical(fetched$status, 0L)) {
    stop_win_builder(
      "The form cannot be fetched; nothing was sent. ",
      win_builder_curl_failure(fetched)
    )
  }
  form <- parse_win_builder_form(read_page("form.html"))

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

  cat("Sending: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "\n", sep = "")
  unknown <- function(condition) {
    reason <- conditionMessage(condition)
    stop_win_builder(
      "The upload was attempted and its acceptance is unknown; do not repeat ",
      "it without a new approval. ",
      if (nzchar(reason)) reason else class(condition)[[1L]],
      status = 2L
    )
  }
  tryCatch(
    {
      sent <- run(
        c(common, "--cookie", "cookies.txt", "--header", "Expect:",
          win_builder_upload_args(form, tarball$name),
          "--output", "response.html", win_builder_form),
        directory
      )
      if (!identical(sent$status, 0L)) stop(win_builder_curl_failure(sent))
      status <- parse_win_builder_form(read_page("response.html"))$status
      cat("Response: ", status, "\n", sep = "")
      if (!win_builder_accepted(status, tarball)) {
        stop("The response does not name this tarball and size.")
      }
    },
    error = unknown,
    interrupt = unknown
  )
  cat("Accepted.\n")
  invisible("accepted")
}

if (sys.nframe() == 0L) {
  tryCatch(
    submit_win_builder(commandArgs(trailingOnly = TRUE)),
    marginplyr_win_builder_error = function(error) {
      message(conditionMessage(error))
      quit(status = error$status)
    }
  )
}
